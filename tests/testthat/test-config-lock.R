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
