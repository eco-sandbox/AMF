# Regenerate ../glossary.md from glossary.json (the single glossary source).
# Run from the app/ folder:  Rscript tools/make_glossary_md.R
d <- jsonlite::fromJSON("glossary.json", simplifyVector = FALSE)
c_ <- function(x) gsub("|", "\\|", if (is.null(x)) "" else x, fixed = TRUE)
o <- c(paste0("# ", d$title), "",
  "Glossary and notation for the plant–AMF interaction-spectrum model. One notation is used in every tab, formula, slider label and hover text.", "",
  unlist(lapply(d$intro_md[-1], function(p) c(p, ""))),
  "## Symbols", "", "| Symbol | Meaning | Notes | Units | Tab(s) | Source | Source symbol |", "|---|---|---|---|---|---|---|")
for (g in d$symbols) o <- c(o, sprintf("| %s | %s | %s | %s | %s | %s | %s |", c_(g$md), c_(g$name), c_(g$notes), c_(g$units), c_(g$tabs), c_(g$src), c_(g$source_symbol)))
o <- c(o, "", d$tabs_note, "", "## Terms", "")
for (t in d$terms) o <- c(o, sprintf("- **%s**: %s", t$term, t$definition))
writeLines(o, "../glossary.md", useBytes = TRUE)
cat("wrote ../glossary.md:", length(d$symbols), "symbols,", length(d$terms), "terms\n")
