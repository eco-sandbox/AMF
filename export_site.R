#!/usr/bin/env Rscript
# Stage app/ for shinylive::export: keep data/tab5_counts.rds, drop tests/ and tools/.
# Used by .github/workflows/deploy-shinylive.yml and for local checks.
# Usage (from the repository root): Rscript export_site.R [_site] [app]

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
message("Export complete: ", dest)
