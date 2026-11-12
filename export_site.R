#!/usr/bin/env Rscript
# Stage app/ for shinylive::export: keep data/tab5_counts.rds, drop tests/ and tools/.
# Used by .github/workflows/deploy-shinylive.yml and for local checks.
# Usage (from the repository root): Rscript export_site.R [_site] [app]

# Invisible visit counter only. count.js records a pageview and does not
# render a count. Insert this exact line immediately before </body> in each
# exported HTML entry page; every other byte stays as shinylive wrote it.
goatcounter_line <- '<script data-goatcounter="https://eco-sandbox.goatcounter.com/count" async src="//gc.zgo.at/count.js"></script>'

insert_goatcounter <- function(raw, line) {
  pos <- grepRaw("</body>", raw, fixed = TRUE, all = FALSE)
  if (length(pos) != 1L || pos < 1L) {
    return(raw)
  }
  pos <- as.integer(pos)
  line_start <- pos
  nl <- as.raw(c(0x0a, 0x0d))
  while (line_start > 1L && !(raw[[line_start - 1L]] %in% nl)) {
    line_start <- line_start - 1L
  }
  prefix <- if (pos > line_start) raw[line_start:(pos - 1L)] else raw(0)
  ws <- as.raw(c(0x20, 0x09))
  own_line <- length(prefix) == 0L || all(prefix %in% ws)
  payload <- charToRaw(line)
  lf <- as.raw(0x0a)
  if (own_line) {
    c(raw[seq_len(line_start - 1L)], payload, lf, raw[line_start:length(raw)])
  } else {
    c(raw[seq_len(pos - 1L)], lf, payload, lf, raw[pos:length(raw)])
  }
}

write_raw_file <- function(path, bytes) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(bytes, con)
}

inject_goatcounter <- function(destdir) {
  files <- list.files(
    destdir,
    pattern = "[.][Hh][Tt][Mm][Ll]$",
    recursive = TRUE,
    full.names = TRUE
  )
  n <- 0L
  for (f in files) {
    info <- file.info(f)
    if (is.na(info$size) || isTRUE(info$isdir) || info$size < 1) next
    if (info$size > .Machine$integer.max) {
      stop("HTML file too large to patch: ", f)
    }
    raw <- readBin(f, what = "raw", n = as.integer(info$size))
    if (length(grepRaw("data-goatcounter=", raw, fixed = TRUE, all = FALSE)) == 1L) next
    if (length(grepRaw("</body>", raw, fixed = TRUE, all = FALSE)) != 1L) next
    write_raw_file(f, insert_goatcounter(raw, goatcounter_line))
    n <- n + 1L
  }
  message("GoatCounter: inserted visit counter into ", n, " HTML file(s)")
  invisible(n)
}

args <- commandArgs(trailingOnly = TRUE)
dest <- if (length(args) >= 1L) args[[1]] else "_site"
app_src <- if (length(args) >= 2L) args[[2]] else "app"

rds <- file.path(app_src, "data", "tab5_counts.rds")
if (!file.exists(rds)) {
  stop(
    "Expected ", rds,
    " — tab ⑤ ships precomputed counts; the export must include data/."
  )
}

staging <- file.path(tempdir(), paste0("amf-export-", Sys.getpid()))
if (dir.exists(staging)) unlink(staging, recursive = TRUE)
dir.create(staging, recursive = TRUE)

skip <- c("tests", "tools", "README.md", ".Rhistory", ".RData", ".DS_Store")
items <- setdiff(list.files(app_src, all.files = TRUE, no.. = TRUE), skip)
ok <- file.copy(file.path(app_src, items), staging, recursive = TRUE, copy.date = TRUE)
if (any(!ok)) {
  stop("Failed to stage: ", paste(items[!ok], collapse = ", "))
}

staged_rds <- file.path(staging, "data", "tab5_counts.rds")
if (!file.exists(staged_rds)) {
  stop("Staging dropped data/tab5_counts.rds")
}

if (dir.exists(dest)) unlink(dest, recursive = TRUE)
dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)

message("shinylive::export(", staging, " -> ", dest, ")")
shinylive::export(appdir = staging, destdir = dest)
invisible(file.create(file.path(dest, ".nojekyll")))
inject_goatcounter(dest)
message("Export complete: ", dest)
