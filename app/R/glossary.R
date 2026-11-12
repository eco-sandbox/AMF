# Glossary: single data source = glossary.json (also used to generate ../glossary.md
# via tools/make_glossary_md.R; the v3 HTML embeds an identical copy, checked by tests/check_glossary_sync.js).
glossary_path <- function() {
  for (p in c("glossary.json", file.path("..", "glossary.json"), file.path("shiny", "glossary.json")))
    if (file.exists(p)) return(p)
  stop("glossary.json not found")
}
GLOSS <- jsonlite::fromJSON(glossary_path(), simplifyVector = FALSE)

# plain notation (K_P, α_MZ, H*_(s=0), λ_M^(Z)) -> HTML sub/superscripts (same rules as the v3 JS `subify`)
subify <- function(t) {
  t <- enc2utf8(as.character(t))
  t <- gsub("\\^\\(([^)]*)\\)", "<sup>(\\1)</sup>", t, perl = TRUE)
  gsub("(*UTF8)([A-Za-z\\x{0370}-\\x{03ff}*])_(?:\\(([^)]*)\\)|\\{([^}]*)\\}|([A-Za-z0-9]+))",
       "\\1<sub>\\2\\3\\4</sub>", t, perl = TRUE)
}

gloss_tip <- function(tab, key) {
  tk <- paste0(tab, ":", key)
  for (g in GLOSS$symbols) if (tk %in% unlist(g$keys)) {
    return(paste0(g$md, ": ", g$name, if (nzchar(g$notes)) paste0(" — ", g$notes) else "",
                  if (nzchar(g$src)) paste0(" [", g$src, "]") else ""))
  }
  ""
}

glossary_ui <- function() {
  rows <- lapply(GLOSS$symbols, function(g) tags$tr(
    tags$td(class = "gs", HTML(g$sym)), tags$td(HTML(subify(g$name))), tags$td(HTML(subify(g$notes))),
    tags$td(HTML(subify(g$units))), tags$td(g$tabs),
    tags$td(if (nzchar(g$src)) tags$span(class = "src paper", g$src)),
    tags$td(class = "k", HTML(subify(g$source_symbol)))))
  terms <- lapply(GLOSS$terms, function(t) tagList(tags$dt(HTML(subify(t$term))), tags$dd(HTML(subify(t$definition)))))
  div(class = "glosswrap",
    tags$h1(class = "gtitle", "Symbols"),
    card(card_header(GLOSS$title),
      div(class = "sub", HTML("Convention for competition: α<sub>ij</sub> = effect of species j on species i (so α<sub>MZ</sub> = effect of cheater Z on mutualist M).
        “Source” = short reference of the paper a symbol comes from (blank = introduced here or generic); “Source symbol” = the symbol used in the original source.")),
      tags$table(class = "gloss",
        tags$thead(tags$tr(lapply(c("Symbol", "Meaning", "Notes", "Units", "Tab(s)", "Source", "Source symbol"), tags$th))),
        tags$tbody(rows))),
    card(card_header("Terms"), tags$dl(class = "terms", terms)))
}
