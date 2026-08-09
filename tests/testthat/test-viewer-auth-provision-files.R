test_that("Windows path policy fails closed before any filesystem claim", {
  parent <- withr::local_tempdir()
  target <- file.path(parent, "viewer-auth")
  before <- list.files(parent, all.files = TRUE)
  state <- CerebroNexus:::.viewerAuthNewProvisionState(
    accounts = data.frame(
      user = "alice",
      password = "alice-password",
      admin = TRUE
    ),
    options = list(
      target_dir = target,
      passphrase_env = NULL,
      timeout_minutes = 15L,
      install_env = FALSE
    ),
    ops = viewer_auth_provision_test_ops(os_type = function() "windows")
  )
  expect_provision_error(
    CerebroNexus:::.viewerAuthProvisionPreflight(state),
    "unsafe_parent",
    "preflight"
  )
  expect_identical(list.files(parent, all.files = TRUE), before)
  expect_false(state$lock_claimed)
  expect_null(state$paths)
})

test_that("preflight freezes one private native parent without writing", {
  state <- viewer_auth_provision_test_state()
  before <- list.files(dirname(state$options$target_dir), all.files = TRUE)
  CerebroNexus:::.viewerAuthProvisionPreflight(state)
  expect_identical(
    list.files(dirname(state$options$target_dir), all.files = TRUE),
    before
  )
  expect_identical(
    state$preflight$target_path,
    file.path(normalizePath(dirname(state$options$target_dir)), "viewer-auth")
  )
})

test_that("preflight rejects unsafe path classes and parent proof", {
  fixture <- viewer_auth_provision_test_state()
  parent <- dirname(fixture$options$target_dir)
  invalid_utf8 <- rawToChar(as.raw(c(0x61, 0xff)))
  Encoding(invalid_utf8) <- "UTF-8"
  cases <- list(
    list(target = "relative/auth"),
    list(target = file.path(parent, paste0("auth", rawToChar(as.raw(1L))))),
    list(target = file.path(parent, strrep("a", 129L))),
    list(target = file.path(parent, ".cerebro-auth-reserved")),
    list(target = file.path(parent, invalid_utf8)),
    list(target = "C:\\private\\auth", os_type = "windows"),
    list(target = "//server/share/auth", os_type = "windows"),
    list(target = "\\\\?\\C:\\private\\auth", os_type = "windows")
  )
  for (case in cases) {
    local({
      selected <- case
      state <- viewer_auth_provision_test_state()
      safe_parent <- dirname(state$options$target_dir)
      before <- list.files(safe_parent, all.files = TRUE)
      state$options$target_dir <- selected$target
      if (!is.null(selected$os_type)) {
        state$ops$os_type <- function() selected$os_type
      }
      expect_provision_error(
        CerebroNexus:::.viewerAuthProvisionPreflight(state),
        "unsafe_parent",
        "preflight"
      )
      expect_identical(list.files(safe_parent, all.files = TRUE), before)
      expect_false(state$lock_claimed)
      expect_null(state$paths)
    })
  }
  bad_proofs <- list(
    list(uid = "502", effective_uid = "501", permissions = "rwx------"),
    list(uid = "501", effective_uid = NA_character_, permissions = "rwx------"),
    list(uid = "501", effective_uid = "501", permissions = "r-x------"),
    list(uid = "501", effective_uid = "501", permissions = "rw-------"),
    list(uid = "501", effective_uid = "501", permissions = "rwxrwx---"),
    list(uid = "501", effective_uid = "501", permissions = "rwx-----w-")
  )
  for (proof in bad_proofs) {
    local({
      selected <- proof
      state <- viewer_auth_provision_test_state(
        inspect_path = function(path) {
          list(
            exists = TRUE,
            type = "directory",
            permissions = selected$permissions,
            uid = selected$uid,
            device_id = "1",
            inode = "1",
            is_link = FALSE
          )
        },
        effective_uid = function() selected$effective_uid
      )
      expect_provision_error(
        CerebroNexus:::.viewerAuthProvisionPreflight(state),
        "unsafe_parent",
        "preflight"
      )
      expect_false(state$lock_claimed)
      expect_null(state$paths)
    })
  }
  existing <- viewer_auth_provision_test_state()
  dir.create(existing$options$target_dir)
  expect_provision_error(
    CerebroNexus:::.viewerAuthProvisionPreflight(existing),
    "target_exists",
    "preflight"
  )
  dangling <- viewer_auth_provision_test_state()
  expect_true(file.symlink("missing-target", dangling$options$target_dir))
  expect_provision_error(
    CerebroNexus:::.viewerAuthProvisionPreflight(dangling),
    "unsafe_parent",
    "preflight"
  )
})

test_that("paths include only deterministic operation-owned temp names", {
  state <- viewer_auth_provision_prepared_state()
  expect_identical(
    basename(state$paths$owner_tmp),
    paste0("owner.rds.", state$identity$operation_id, ".tmp")
  )
  expect_identical(
    basename(state$paths$manifest_tmp),
    paste0("provision.rds.", state$identity$operation_id, ".tmp")
  )
  expect_identical(
    basename(state$paths$secret_tmp),
    paste0("viewer-auth.env.", state$identity$operation_id, ".tmp")
  )
  expect_identical(
    basename(state$paths$release_receipt),
    paste0(
      ".cerebro-auth-",
      state$identity$target_hash,
      "-",
      state$identity$operation_id,
      ".released.rds"
    )
  )
})

test_that("atomic metadata rewrites rebind only the proven generation", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  claimed <- state$identities$owner
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  expect_false(identical(state$identities$owner, claimed))
  expect_null(state$identities$owner_tmp)
  old_manifest <- state$identities$manifest
  ready <- readRDS(state$paths$manifest)
  ready$state <- "ready"
  real_rename <- state$ops$rename
  state$ops$rename <- function(from, to) {
    result <- real_rename(from, to)
    if (identical(to, state$paths$manifest)) {
      stop("committed", call. = FALSE)
    }
    result
  }
  expect_silent(CerebroNexus:::.viewerAuthAtomicSaveRds(
    state,
    ready,
    state$paths$manifest,
    state$paths$manifest_tmp,
    "manifest",
    CerebroNexus:::.viewerAuthValidProvisionManifest,
    "artifact_publish_failed",
    "manifest"
  ))
  expect_false(identical(state$identities$manifest, old_manifest))
  expect_null(state$identities$manifest_tmp)
  expect_identical(readRDS(state$paths$manifest)$state, "ready")
})

test_that("an existing target-specific lock fails closed", {
  state <- viewer_auth_provision_prepared_state()
  dir.create(state$paths$lock, mode = "0700")
  expect_provision_error(
    CerebroNexus:::.viewerAuthAcquireProvisionLock(state),
    "lock_conflict",
    "lock"
  )
})

test_that("owner state and physical location matrix fails closed", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  dir.create(state$paths$target)
  condition <- CerebroNexus:::.viewerAuthCleanupProvision(state)
  expect_s3_class(condition, "cerebro_viewer_auth_cleanup_incomplete")
  expect_true(dir.exists(state$paths$stage))
  expect_true(dir.exists(state$paths$target))
})

test_that("every allowed owner location manifest transition is explicit", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  expect_true(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(state))
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  expect_true(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(state))
  ready <- readRDS(state$paths$manifest)
  ready$state <- "ready"
  CerebroNexus:::.viewerAuthAtomicSaveRds(
    state,
    ready,
    state$paths$manifest,
    state$paths$manifest_tmp,
    "manifest",
    CerebroNexus:::.viewerAuthValidProvisionManifest,
    "artifact_publish_failed",
    "manifest"
  )
  CerebroNexus:::.viewerAuthWriteOwnerState(state, "publishing")
  expect_true(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(state))
  expect_true(state$ops$rename(state$paths$stage, state$paths$target))
  CerebroNexus:::.viewerAuthFreezeProvisionPath(
    state,
    "target",
    state$paths$target,
    "directory",
    "rwx------"
  )
  CerebroNexus:::.viewerAuthWriteOwnerState(state, "published")
  expect_true(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(state))
})

test_that("cleanup never removes unknown or operation-mismatched files", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  unknown <- file.path(state$paths$stage, "do-not-delete")
  writeLines("foreign", unknown)
  condition <- CerebroNexus:::.viewerAuthCleanupProvision(state)
  expect_s3_class(condition, "cerebro_viewer_auth_cleanup_incomplete")
  expect_true(file.exists(unknown))
  expect_true(dir.exists(state$paths$lock))
})

test_that("lock release failure keeps valid operation recovery metadata", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  real_remove <- state$ops$remove_dir
  state$ops$remove_dir <- function(path) {
    if (identical(path, state$paths$lock)) FALSE else real_remove(path)
  }
  expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_identical(state$recovery_path, state$paths$release_receipt)
  expect_true(file.exists(state$paths$release_receipt))
  expect_true(dir.exists(state$paths$lock))
  state$ops$remove_dir <- real_remove
  expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_false(dir.exists(state$paths$lock))
  expect_null(state$recovery_path)
})

test_that("post-commit receipt housekeeping cannot create a false recovery error", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  real_remove <- state$ops$remove_file
  state$ops$remove_file <- function(path) {
    if (identical(path, state$paths$release_receipt)) {
      stop("receipt", call. = FALSE)
    }
    real_remove(path)
  }
  expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_false(state$lock_claimed)
  expect_false(dir.exists(state$paths$lock))
  expect_null(state$recovery_path)
})

test_that("every lock and stage begin failure remains cleanup-owned", {
  failing_ops <- c("dir_create", "save_rds", "chmod", "rename", "read_rds")
  for (phase in c("lock", "stage")) {
    for (op_name in failing_ops) {
      local({
        selected_phase <- phase
        selected_op <- op_name
        state <- viewer_auth_provision_prepared_state()
        if (identical(selected_phase, "stage")) {
          CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
        }
        calls <- 0L
        real <- state$ops[[selected_op]]
        state$ops[[selected_op]] <- function(...) {
          calls <<- calls + 1L
          if (calls == 1L) {
            stop("injected", call. = FALSE)
          }
          do.call(real, list(...))
        }
        primary <- tryCatch(
          if (identical(selected_phase, "lock")) {
            CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
          } else {
            CerebroNexus:::.viewerAuthCreateProvisionStage(state)
          },
          error = identity
        )
        info <- paste(selected_phase, selected_op, sep = "/")
        expect_s3_class(primary, "cerebro_viewer_auth_provision_error")
        expect_identical(
          primary$code,
          if (
            identical(selected_phase, "lock") &&
              identical(selected_op, "dir_create")
          ) {
            "lock_conflict"
          } else {
            "artifact_publish_failed"
          },
          info = info
        )
        expect_identical(
          primary$stage,
          if (
            identical(selected_phase, "stage") &&
              identical(selected_op, "dir_create")
          ) {
            "artifact_stage"
          } else {
            "lock"
          },
          info = info
        )
        state$primary_condition <- primary
        cleanup <- CerebroNexus:::.viewerAuthCleanupProvision(state)
        expect_true(is.null(cleanup) || isTRUE(cleanup), info = info)
        if (isTRUE(cleanup)) {
          expect_true(
            CerebroNexus:::.viewerAuthReleaseProvisionLock(state),
            info = info
          )
        }
        expect_false(dir.exists(state$paths$lock), info = info)
        expect_false(dir.exists(state$paths$stage), info = info)
        expect_false(state$lock_claimed, info = info)
      })
    }
  }
})

test_that("first staging manifest failures remain cleanup-owned", {
  faults <- c(
    "save_false_after_write",
    "save_throw_after_write",
    "chmod",
    "rename",
    "read_rds"
  )
  for (fault in faults) {
    local({
      selected <- fault
      state <- viewer_auth_provision_prepared_state()
      CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
      if (identical(selected, "save_false_after_write")) {
        real <- state$ops$save_rds
        state$ops$save_rds <- function(value, path) {
          value <- real(value, path)
          if (identical(path, state$paths$manifest_tmp)) FALSE else value
        }
      } else if (identical(selected, "save_throw_after_write")) {
        real <- state$ops$save_rds
        state$ops$save_rds <- function(value, path) {
          result <- real(value, path)
          if (identical(path, state$paths$manifest_tmp)) {
            stop("sentinel", call. = FALSE)
          }
          result
        }
      } else if (identical(selected, "chmod")) {
        real <- state$ops$chmod
        state$ops$chmod <- function(path, mode) {
          if (identical(path, state$paths$manifest_tmp)) {
            FALSE
          } else {
            real(path, mode)
          }
        }
      } else if (identical(selected, "rename")) {
        real <- state$ops$rename
        state$ops$rename <- function(from, to) {
          if (identical(to, state$paths$manifest)) FALSE else real(from, to)
        }
      } else {
        real <- state$ops$read_rds
        state$ops$read_rds <- function(path) {
          if (identical(path, state$paths$manifest)) {
            stop("sentinel", call. = FALSE)
          }
          real(path)
        }
      }
      primary <- tryCatch(
        CerebroNexus:::.viewerAuthCreateProvisionStage(state),
        error = identity
      )
      expect_s3_class(primary, "cerebro_viewer_auth_provision_error")
      expect_identical(primary$code, "artifact_publish_failed", info = selected)
      expect_identical(primary$stage, "manifest", info = selected)
      expect_identical(state$owner_state, "staging", info = selected)
      expect_true(dir.exists(state$paths$stage), info = selected)
      state$primary_condition <- primary
      expect_true(
        CerebroNexus:::.viewerAuthCleanupProvision(state),
        info = selected
      )
      expect_true(
        CerebroNexus:::.viewerAuthReleaseProvisionLock(state),
        info = selected
      )
      expect_false(dir.exists(state$paths$lock), info = selected)
      expect_false(dir.exists(state$paths$stage), info = selected)
    })
  }
})

test_that("contradictory states, mismatched manifests, and missing manifests fail closed", {
  claimed <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(claimed)
  dir.create(claimed$paths$stage, mode = "0700")
  CerebroNexus:::.viewerAuthFreezeProvisionPath(
    claimed,
    "stage",
    claimed$paths$stage,
    "directory",
    "rwx------"
  )
  expect_false(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(claimed))
  publishing <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(publishing)
  CerebroNexus:::.viewerAuthCreateProvisionStage(publishing)
  CerebroNexus:::.viewerAuthWriteOwnerState(publishing, "publishing")
  expect_false(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(publishing))
  published <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(published)
  CerebroNexus:::.viewerAuthWriteOwnerState(published, "published")
  expect_false(CerebroNexus:::.viewerAuthLayoutMatchesOwnerState(published))
  mismatched <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(mismatched)
  CerebroNexus:::.viewerAuthCreateProvisionStage(mismatched)
  manifest <- readRDS(mismatched$paths$manifest)
  manifest$operation_id <- paste(rep("f", 32L), collapse = "")
  saveRDS(manifest, mismatched$paths$manifest)
  expect_s3_class(
    CerebroNexus:::.viewerAuthCleanupProvision(mismatched),
    "cerebro_viewer_auth_cleanup_incomplete"
  )
  expect_true(dir.exists(mismatched$paths$stage))
  missing <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(missing)
  CerebroNexus:::.viewerAuthCreateProvisionStage(missing)
  file.remove(missing$paths$manifest)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), missing$paths$credentials)
  expect_s3_class(
    CerebroNexus:::.viewerAuthCleanupProvision(missing),
    "cerebro_viewer_auth_cleanup_incomplete"
  )
  expect_true(file.exists(missing$paths$credentials))
})

test_that("receipt housekeeping remains nonfatal for false throw and delete then throw", {
  for (mode in c("false", "throw", "delete_then_throw")) {
    local({
      selected <- mode
      state <- viewer_auth_provision_prepared_state()
      CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
      real <- state$ops$remove_file
      state$ops$remove_file <- function(path) {
        if (!identical(path, state$paths$release_receipt)) {
          return(real(path))
        }
        if (identical(selected, "false")) {
          return(FALSE)
        }
        if (identical(selected, "throw")) {
          stop("sentinel", call. = FALSE)
        }
        real(path)
        stop("sentinel", call. = FALSE)
      }
      expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
      expect_false(state$lock_claimed)
      expect_false(dir.exists(state$paths$lock))
      expect_null(state$recovery_path)
    })
  }
})

test_that("ownerless lock deletion exceptions retain or commit valid proof", {
  for (mode in c("observable", "unobservable")) {
    local({
      selected <- mode
      state <- viewer_auth_provision_prepared_state()
      real_save <- state$ops$save_rds
      calls <- 0L
      state$ops$save_rds <- function(...) {
        calls <<- calls + 1L
        if (calls == 1L) {
          stop("owner sentinel", call. = FALSE)
        }
        real_save(...)
      }
      expect_s3_class(
        tryCatch(
          CerebroNexus:::.viewerAuthAcquireProvisionLock(state),
          error = identity
        ),
        "cerebro_viewer_auth_provision_error"
      )
      expect_true(state$lock_claimed)
      expect_null(state$owner_state)
      real_remove <- state$ops$remove_dir
      real_inspect <- state$ops$inspect_path
      state$ops$remove_dir <- function(path) {
        if (!identical(path, state$paths$lock)) {
          return(real_remove(path))
        }
        expect_true(real_remove(path))
        stop("delete sentinel", call. = FALSE)
      }
      state$ops$inspect_path <- function(path) {
        if (
          identical(selected, "unobservable") &&
            identical(path, state$paths$lock) &&
            !dir.exists(path)
        ) {
          stop("inspect sentinel", call. = FALSE)
        }
        real_inspect(path)
      }
      released <- CerebroNexus:::.viewerAuthReleaseProvisionLock(state)
      expect_false(dir.exists(state$paths$lock))
      if (identical(selected, "observable")) {
        expect_true(released)
        expect_false(state$lock_claimed)
        expect_null(state$recovery_path)
      } else {
        expect_false(released)
        expect_true(state$lock_claimed)
        expect_identical(state$recovery_path, state$paths$release_receipt)
        expect_true(file.exists(state$paths$release_receipt))
        expect_identical(readRDS(state$paths$release_receipt)$state, "claimed")
      }
    })
  }
})

test_that("cleanup treats a stage inspection failure as incomplete", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  real_inspect <- state$ops$inspect_path
  state$ops$inspect_path <- function(path) {
    if (identical(path, state$paths$stage)) {
      stop("stage inspect sentinel", call. = FALSE)
    }
    real_inspect(path)
  }
  condition <- CerebroNexus:::.viewerAuthCleanupProvision(state)
  expect_s3_class(condition, "cerebro_viewer_auth_cleanup_incomplete")
  expect_true(dir.exists(state$paths$lock))
  expect_true(dir.exists(state$paths$stage))
  expect_identical(condition$recovery_path, state$paths$lock)
})

test_that("receipt-only release rejects a replacement lock directory", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  real_remove <- state$ops$remove_dir
  state$ops$remove_dir <- function(path) {
    if (identical(path, state$paths$lock)) FALSE else real_remove(path)
  }
  expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_true(file.exists(state$paths$release_receipt))
  expect_true(real_remove(state$paths$lock))
  dir.create(state$paths$lock, mode = "0700")
  state$ops$remove_dir <- real_remove
  expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_true(dir.exists(state$paths$lock))
  expect_true(file.exists(state$paths$release_receipt))
})

test_that("normal cleanup and release leave no target or receipt artifacts", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  CerebroNexus:::.viewerAuthCreateProvisionStage(state)
  expect_true(CerebroNexus:::.viewerAuthCleanupProvision(state))
  expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_false(dir.exists(state$paths$target))
  expect_false(file.exists(state$paths$release_receipt))
  expect_false(file.exists(state$paths$release_receipt_tmp))
})

test_that("release never treats failed or malformed lock listings as empty", {
  for (bad_listing in list(NULL, NA_character_)) {
    local({
      selected <- bad_listing
      state <- viewer_auth_provision_prepared_state()
      CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
      real_remove <- state$ops$remove_dir
      state$ops$remove_dir <- function(path) {
        if (identical(path, state$paths$lock)) FALSE else real_remove(path)
      }
      expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
      expect_true(file.exists(state$paths$release_receipt))
      state$ops$remove_dir <- real_remove
      real_list <- state$ops$list_files
      state$ops$list_files <- function(path) {
        if (identical(path, state$paths$lock)) selected else real_list(path)
      }
      expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
      expect_true(dir.exists(state$paths$lock))
      expect_true(file.exists(state$paths$release_receipt))
      expect_identical(state$recovery_path, state$paths$release_receipt)
    })
  }
})

test_that("release never deletes an unproven or replaced owner temp file", {
  for (mode in c("unproven", "replaced", "symlink")) {
    local({
      selected <- mode
      state <- viewer_auth_provision_prepared_state()
      CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
      if (identical(selected, "symlink")) {
        foreign <- file.path(state$preflight$parent, "foreign-owner-temp")
        writeLines("foreign", foreign)
        expect_true(file.symlink(foreign, state$paths$owner_tmp))
      } else {
        writeLines("owner temp", state$paths$owner_tmp)
        Sys.chmod(state$paths$owner_tmp, "0600")
        if (identical(selected, "replaced")) {
          CerebroNexus:::.viewerAuthFreezeProvisionPath(
            state,
            "owner_tmp",
            state$paths$owner_tmp,
            "file",
            "rw-------"
          )
          unlink(state$paths$owner_tmp)
          writeLines("replacement", state$paths$owner_tmp)
          Sys.chmod(state$paths$owner_tmp, "0600")
        }
      }
      expect_false(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
      expect_true(
        file.exists(state$paths$owner_tmp) ||
          isTRUE(fs::is_link(state$paths$owner_tmp))
      )
      expect_true(dir.exists(state$paths$lock))
    })
  }
})

test_that("postcommit receipt housekeeping preserves a replaced receipt", {
  state <- viewer_auth_provision_prepared_state()
  CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
  real_remove <- state$ops$remove_dir
  state$ops$remove_dir <- function(path) {
    result <- real_remove(path)
    if (identical(path, state$paths$lock)) {
      unlink(state$paths$release_receipt)
      writeLines("replacement", state$paths$release_receipt)
      Sys.chmod(state$paths$release_receipt, "0600")
    }
    result
  }
  expect_true(CerebroNexus:::.viewerAuthReleaseProvisionLock(state))
  expect_false(dir.exists(state$paths$lock))
  expect_true(file.exists(state$paths$release_receipt))
})

test_that("cleanup retains allowed-basename artifacts without this operation identity", {
  for (artifact in c("viewer-auth.env", "credentials.sqlite")) {
    local({
      selected <- artifact
      state <- viewer_auth_provision_prepared_state()
      CerebroNexus:::.viewerAuthAcquireProvisionLock(state)
      CerebroNexus:::.viewerAuthCreateProvisionStage(state)
      foreign <- file.path(state$paths$stage, selected)
      writeLines("foreign sentinel", foreign)
      condition <- CerebroNexus:::.viewerAuthCleanupProvision(state)
      expect_s3_class(condition, "cerebro_viewer_auth_cleanup_incomplete")
      expect_true(file.exists(foreign))
      expect_true(dir.exists(state$paths$stage))
      expect_true(dir.exists(state$paths$lock))
    })
  }
})
