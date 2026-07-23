# Regression (B-07): the config-save critical section must be serialised so two
# concurrent same-machine saves cannot both pass the name-clash check and clobber.
# acquire_config_lock is an atomic dir.create() mutex with stale-lock reclaim.

test_that("acquire_config_lock is exclusive while held and reusable after release", {
  dir <- make_test_config_dir()

  lock1 <- acquire_config_lock(dir, "ds1")
  expect_false(is.null(lock1))        # first acquirer wins
  expect_true(dir.exists(lock1))

  expect_null(acquire_config_lock(dir, "ds1"))   # second is refused while held

  unlink(lock1, recursive = TRUE)                # release
  lock2 <- acquire_config_lock(dir, "ds1")
  expect_false(is.null(lock2))                   # now acquirable again
  unlink(lock2, recursive = TRUE)
})

test_that("acquire_config_lock does not contend across different dataset names", {
  dir <- make_test_config_dir()
  a <- acquire_config_lock(dir, "alpha")
  b <- acquire_config_lock(dir, "beta")
  expect_false(is.null(a))
  expect_false(is.null(b))            # independent names, independent locks
  unlink(a, recursive = TRUE); unlink(b, recursive = TRUE)
})

test_that("acquire_config_lock reclaims a stale lock", {
  dir <- make_test_config_dir()
  held <- acquire_config_lock(dir, "ds1")
  expect_null(acquire_config_lock(dir, "ds1", stale_seconds = 60))  # fresh: refused

  # Backdate the lock's mtime to simulate a crashed save that never released.
  Sys.setFileTime(held, Sys.time() - 120)
  reclaimed <- acquire_config_lock(dir, "ds1", stale_seconds = 60)
  expect_false(is.null(reclaimed))    # stale lock reclaimed
  unlink(reclaimed, recursive = TRUE)
})

# Regression (B-07): the *global* config save now takes the same mutex, keyed by
# the global config filename, so two concurrent global-config saves can't clobber.
test_that("global-config save uses a distinct, serialising lock", {
  dir <- make_test_config_dir()
  lock_name <- tools::file_path_sans_ext(basename(global_config_path(dir)))
  expect_identical(lock_name, "dqcheckr")   # keyed by the global config filename

  g1 <- acquire_config_lock(dir, lock_name)
  expect_false(is.null(g1))                  # first global save wins
  expect_null(acquire_config_lock(dir, lock_name))  # concurrent global save refused
  # A per-dataset save on a normal name is unaffected (independent lock).
  d <- acquire_config_lock(dir, "some_ds")
  expect_false(is.null(d))
  unlink(g1, recursive = TRUE)
  g2 <- acquire_config_lock(dir, lock_name)  # acquirable again after release
  expect_false(is.null(g2))
  unlink(g2, recursive = TRUE); unlink(d, recursive = TRUE)
})

# Regression (B-13): reclaiming a STALE lock must be atomic. The old
# unlink()-then-dir.create() was a check-then-act — two callers that both saw the
# stale lock both recreated it and both "acquired" it. Fork several contenders at
# a freshly-staled lock and assert at most one ever wins.
test_that("a stale lock is reclaimed by at most one concurrent caller (B-13)", {
  skip_on_cran()
  skip_on_os("windows")   # parallel::mclapply forks only on unix-alikes

  dir  <- make_test_config_dir()
  lock <- file.path(dir, ".raceds.yml.lock")

  double_grants <- 0L
  for (round in seq_len(20)) {
    unlink(lock, recursive = TRUE)
    dir.create(lock)
    Sys.setFileTime(lock, Sys.time() - 120)   # older than stale_seconds = 60

    res <- parallel::mclapply(1:8, function(i)
      acquire_config_lock(dir, "raceds", stale_seconds = 60), mc.cores = 8)
    holders <- Filter(Negate(is.null), res)
    if (length(holders) > 1L) double_grants <- double_grants + 1L
  }
  unlink(lock, recursive = TRUE)
  expect_equal(double_grants, 0L,
               info = "a stale lock must be reclaimed by at most one caller")
})
