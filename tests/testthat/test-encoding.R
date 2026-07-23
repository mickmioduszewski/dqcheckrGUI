# Encoding normalisation + full-file sniff (the "encoding: ASCII" crash trap)

.write_bytes <- function(txt, from = NULL) {
  path <- tempfile(fileext = ".csv")
  bytes <- if (is.null(from)) charToRaw(enc2utf8(txt))
           else charToRaw(iconv(txt, from = "UTF-8", to = from))
  writeBin(bytes, path)
  path
}

test_that("normalise_encoding() maps ASCII aliases to UTF-8, case-insensitively", {
  expect_equal(normalise_encoding("ASCII"), "UTF-8")
  expect_equal(normalise_encoding("ascii"), "UTF-8")
  expect_equal(normalise_encoding(" US-ASCII "), "UTF-8")
  expect_equal(normalise_encoding("ANSI_X3.4-1968"), "UTF-8")
})

test_that("normalise_encoding() leaves real encodings and empties untouched", {
  expect_equal(normalise_encoding("UTF-8"), "UTF-8")
  expect_equal(normalise_encoding("Windows-1252"), "Windows-1252")
  expect_equal(normalise_encoding("UTF-16LE"), "UTF-16LE")
  expect_equal(normalise_encoding(""), "")
  expect_null(normalise_encoding(NULL))
})

test_that("read_config() normalises a legacy 'encoding: ASCII' to UTF-8", {
  path <- tempfile(fileext = ".yml")
  writeLines(c(
    "dataset_name: enc_ds",
    "format: csv",
    "encoding: ASCII",
    "delimiter: ','"
  ), path)
  cfg <- read_config(path)
  expect_equal(cfg$known$encoding, "UTF-8")
  unlink(path)
})

test_that("sniff_file_encoding() is certain about valid UTF-8 (incl. pure ASCII)", {
  utf8  <- .write_bytes("name,city\nZoë,München\n")
  ascii <- .write_bytes("name,city\nMick,Sydney\n")
  s1 <- sniff_file_encoding(utf8)
  s2 <- sniff_file_encoding(ascii)
  expect_true(s1$certain)
  expect_equal(s1$top, "UTF-8")
  expect_true(s2$certain)
  expect_equal(s2$top, "UTF-8")
  unlink(c(utf8, ascii))
})

test_that("sniff_file_encoding() flags a latin1 file as uncertain legacy", {
  lat <- .write_bytes("name,city\nZoë,München\n", from = "ISO-8859-1")
  s <- sniff_file_encoding(lat)
  expect_false(s$certain)
  expect_false(grepl("^UTF", s$top, ignore.case = TRUE))
  unlink(lat)
})

test_that("sniff_file_encoding() sees past a large all-ASCII head", {
  # The trap that motivated the full scan: guess_encoding()'s head sample says
  # "ASCII" when the first accented byte sits megabytes into the file.
  path <- tempfile(fileext = ".csv")
  con  <- file(path, open = "wb")
  writeBin(charToRaw("name,city\n"), con)
  filler <- charToRaw(strrep("Mick,Sydney\n", 1000))
  for (i in 1:200) writeBin(filler, con)  # ~2.4 MB of pure ASCII rows
  writeBin(charToRaw(iconv("Zoë,München\n", "UTF-8", "ISO-8859-1")), con)
  close(con)
  s <- sniff_file_encoding(path)
  expect_false(s$certain)
  expect_false(identical(toupper(s$top), "ASCII"))
  unlink(path)
})

test_that("sniff_file_encoding() never returns an NA top for a stray high byte (B-14)", {
  # A large, almost-entirely-ASCII window with a single non-ASCII byte is the
  # shape stri_enc_detect can answer with an NA-encoding row. That must not
  # propagate as top = NA (which crashed the edit-mode step-3 observer); the
  # NA-drop filter has to fall back to a concrete legacy encoding.
  path <- tempfile(fileext = ".csv")
  con  <- file(path, open = "wb")
  writeBin(charToRaw("name,city\n"), con)
  writeBin(charToRaw(strrep("Mick,Sydney\n", 100000)), con)  # ~1.2 MB ASCII
  writeBin(as.raw(0xFF), con)                                # one stray high byte
  writeBin(charToRaw("\n"), con)
  close(con)
  s <- sniff_file_encoding(path)
  expect_false(is.na(s$top))
  expect_true(nzchar(s$top))
  unlink(path)
})

# ── Chunked streaming: boundary correctness (B-10) ────────────────────────────
# sniff_file_encoding() streams the file in bounded chunks so a multi-GB file is
# not read into one raw vector. A multi-byte UTF-8 sequence split across a chunk
# boundary must be carried over, not false-flagged as invalid.

test_that("sniff_file_encoding() validates a multi-byte char split across a chunk boundary (B-10)", {
  # "aaa" + ë (0xC3 0xAB). chunk_size = 4 puts the ë lead byte at the end of
  # chunk 1 and its continuation byte in chunk 2.
  path <- tempfile(fileext = ".csv")
  con  <- file(path, open = "wb")
  writeBin(c(charToRaw("aaa"), as.raw(c(0xC3, 0xAB))), con)
  close(con)
  s <- sniff_file_encoding(path, chunk_size = 4L)
  expect_true(s$certain)
  expect_equal(s$top, "UTF-8")
  unlink(path)
})

test_that("sniff_file_encoding() still flags invalid bytes under a tiny chunk_size (B-10)", {
  path <- tempfile(fileext = ".csv")
  con  <- file(path, open = "wb")
  writeBin(c(charToRaw("Mick,Sydney"), as.raw(0xFF)), con)   # lone high byte: not UTF-8
  close(con)
  s <- sniff_file_encoding(path, chunk_size = 4L)
  expect_false(s$certain)
  expect_false(is.na(s$top))
  unlink(path)
})
