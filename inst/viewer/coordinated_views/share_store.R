##----------------------------------------------------------------------------##
## Persistent storage for expiring linked-view share records.
##
## This file is sourced by the Viewer, not the package namespace. Keep its
## surface small: it receives canonical JSON and returns only opaque tokens.
##----------------------------------------------------------------------------##

CV_SHARE_TOKEN_BYTES <- 32L
CV_SHARE_TOKEN_CHARS <- 43L
CV_SHARE_TTL_SECONDS <- 90 * 24 * 60 * 60
CV_SHARE_MAX_BYTES <- 5L * 1024L * 1024L

cv_share_abort <- function(code, message) {
  stop(structure(
    list(message = message, call = NULL, code = code),
    class = c("cv_share_error", "error", "condition")
  ))
}

cv_share_time <- function(value = Sys.time()) {
  if (!inherits(value, "POSIXt") || length(value) != 1L || is.na(value)) {
    cv_share_abort("internal", "The share timestamp is invalid.")
  }
  format(as.POSIXct(value, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

cv_share_token <- function() {
  token <- openssl::base64_encode(openssl::rand_bytes(CV_SHARE_TOKEN_BYTES))
  token <- chartr("+/", "-_", sub("=+$", "", token))
  if (!identical(nchar(token, type = "bytes"), CV_SHARE_TOKEN_CHARS)) {
    cv_share_abort("internal", "The share token could not be created.")
  }
  token
}

cv_share_hash <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    cv_share_abort("invalid_receipt", "The share receipt is invalid.")
  }
  raw <- openssl::sha256(charToRaw(enc2utf8(value)))
  paste(sprintf("%02x", as.integer(raw)), collapse = "")
}

cv_share_token_input <- function(value, field) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !grepl("^[A-Za-z0-9_-]{43}$", value)
  ) {
    cv_share_abort("invalid_token", paste0("The share ", field, " is invalid."))
  }
  value
}

cv_share_store_open <- function(path) {
  if (
    !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
  ) {
    cv_share_abort("share_unavailable", "Share links are not configured here.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), path),
    error = function(error) {
      cv_share_abort("share_unavailable", "Share links are unavailable here.")
    }
  )
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS linked_view_shares (",
      "token TEXT PRIMARY KEY, receipt_hash TEXT NOT NULL, json TEXT NOT NULL,",
      "fingerprint TEXT NOT NULL, created_at TEXT NOT NULL, expires_at TEXT NOT NULL,",
      "revoked_at TEXT)"
    )
  )
  columns <- DBI::dbGetQuery(
    con,
    "PRAGMA table_info(linked_view_shares)"
  )$name
  if (!"creator" %in% columns) {
    DBI::dbExecute(
      con,
      "ALTER TABLE linked_view_shares ADD COLUMN creator TEXT"
    )
  }
  if (!"dataset_label" %in% columns) {
    DBI::dbExecute(
      con,
      "ALTER TABLE linked_view_shares ADD COLUMN dataset_label TEXT"
    )
  }
  DBI::dbExecute(
    con,
    "CREATE INDEX IF NOT EXISTS linked_view_shares_expiry ON linked_view_shares (expires_at)"
  )
  structure(list(con = con), class = "cv_share_store")
}

cv_share_store_cleanup <- function(store, now = Sys.time()) {
  DBI::dbExecute(
    store$con,
    "DELETE FROM linked_view_shares WHERE expires_at <= ?",
    params = list(cv_share_time(now))
  )
  invisible(NULL)
}

cv_share_store_create <- function(
  store,
  json,
  fingerprint,
  now = Sys.time(),
  token = NULL,
  receipt = NULL,
  creator = "",
  dataset_label = ""
) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  if (
    !is.character(json) ||
      length(json) != 1L ||
      is.na(json) ||
      nchar(enc2utf8(json), type = "bytes") > CV_SHARE_MAX_BYTES
  ) {
    cv_share_abort("invalid_config", "The shared configuration is invalid.")
  }
  if (
    !is.character(fingerprint) ||
      length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !nzchar(fingerprint)
  ) {
    cv_share_abort("invalid_dataset", "The shared cell population is invalid.")
  }
  audit_text <- function(value, field, maximum) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        nchar(enc2utf8(value), type = "bytes") > maximum
    ) {
      cv_share_abort(
        "invalid_audit",
        paste0("The share ", field, " is invalid.")
      )
    }
    value
  }
  creator <- audit_text(creator, "creator", 200L)
  dataset_label <- audit_text(dataset_label, "dataset label", 500L)
  created_at <- cv_share_time(now)
  expires_at <- cv_share_time(
    as.POSIXct(now, tz = "UTC") + CV_SHARE_TTL_SECONDS
  )
  supplied <- !is.null(token) || !is.null(receipt)
  if (supplied && (is.null(token) || is.null(receipt))) {
    cv_share_abort("invalid_token", "The share credentials are incomplete.")
  }
  if (supplied) {
    token <- cv_share_token_input(token, "link")
    receipt <- cv_share_token_input(receipt, "receipt")
  }
  for (attempt in seq_len(if (supplied) 1L else 3L)) {
    if (!supplied) {
      token <- cv_share_token()
      receipt <- cv_share_token()
    }
    written <- tryCatch(
      {
        DBI::dbExecute(
          store$con,
          paste(
            "INSERT INTO linked_view_shares",
            "(token, receipt_hash, json, fingerprint, created_at, expires_at,",
            "revoked_at, creator, dataset_label)",
            "VALUES (?, ?, ?, ?, ?, ?, NULL, ?, ?)"
          ),
          params = list(
            token,
            cv_share_hash(receipt),
            json,
            fingerprint,
            created_at,
            expires_at,
            creator,
            dataset_label
          )
        )
        TRUE
      },
      error = function(error) FALSE
    )
    if (isTRUE(written)) {
      cv_share_store_cleanup(store, now)
      return(list(token = token, receipt = receipt, expires_at = expires_at))
    }
  }
  cv_share_abort(
    if (supplied) "share_collision" else "internal",
    "The share link could not be created. Try again."
  )
}

cv_share_store_list <- function(store, now = Sys.time()) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  cv_share_store_cleanup(store, now)
  DBI::dbGetQuery(
    store$con,
    paste(
      "SELECT token, fingerprint, dataset_label, creator, created_at,",
      "expires_at, revoked_at FROM linked_view_shares",
      "WHERE revoked_at IS NULL ORDER BY created_at DESC"
    )
  )
}

cv_share_store_fetch <- function(store, token, fingerprint, now = Sys.time()) {
  token <- cv_share_token_input(token, "link")
  cv_share_store_cleanup(store, now)
  row <- DBI::dbGetQuery(
    store$con,
    "SELECT json, fingerprint, revoked_at FROM linked_view_shares WHERE token = ?",
    params = list(token)
  )
  if (!nrow(row)) {
    cv_share_abort(
      "share_unavailable",
      "This share link is unavailable or has expired."
    )
  }
  if (!is.na(row$revoked_at[[1L]])) {
    cv_share_abort("share_revoked", "This share link has been revoked.")
  }
  if (!identical(row$fingerprint[[1L]], fingerprint)) {
    cv_share_abort(
      "dataset_mismatch",
      "This configuration belongs to a different cell population."
    )
  }
  list(json = row$json[[1L]])
}

cv_share_store_revoke <- function(store, token, receipt, now = Sys.time()) {
  token <- cv_share_token_input(token, "link")
  receipt <- cv_share_token_input(receipt, "receipt")
  cv_share_store_cleanup(store, now)
  changed <- DBI::dbExecute(
    store$con,
    "UPDATE linked_view_shares SET revoked_at = ? WHERE token = ? AND receipt_hash = ? AND revoked_at IS NULL",
    params = list(cv_share_time(now), token, cv_share_hash(receipt))
  )
  if (!identical(as.integer(changed), 1L)) {
    cv_share_abort(
      "share_revoke_denied",
      "This link cannot be revoked from this browser."
    )
  }
  invisible(TRUE)
}

cv_share_store_revoke_admin <- function(store, token, now = Sys.time()) {
  if (!inherits(store, "cv_share_store")) {
    cv_share_abort("internal", "The share store is unavailable.")
  }
  token <- cv_share_token_input(token, "link")
  cv_share_store_cleanup(store, now)
  changed <- DBI::dbExecute(
    store$con,
    paste(
      "UPDATE linked_view_shares SET revoked_at = ?",
      "WHERE token = ? AND revoked_at IS NULL"
    ),
    params = list(cv_share_time(now), token)
  )
  if (!identical(as.integer(changed), 1L)) {
    cv_share_abort(
      "share_revoke_denied",
      "This share link is unavailable or already revoked."
    )
  }
  invisible(TRUE)
}
