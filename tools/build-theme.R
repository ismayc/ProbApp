# Builds www/theme.css, the app's theme as a precompiled override stylesheet.
#
#   Rscript tools/build-theme.R          # rewrite www/theme.css
#
# Why: a customized bslib theme is compiled from Sass every time the page is
# rendered. In the shinylive / webR build that compile runs in the browser and
# costs about a second of startup. The stock bslib theme ships precompiled, so
# ui.R uses the stock theme and inlines this file after it.
#
# How: run the app's UI twice, with the stock theme and with `custom_theme()`
# below, and for every stylesheet the page serves keep only the declarations the
# custom theme changes or adds. Each one is written under its original selector,
# so the file is a plain list of overrides for the stock theme. ui.R inlines it
# at the end of the page body, after every stylesheet it overrides.
#
# Rerun this after changing custom_theme() or upgrading bslib. The test in
# tests/testthat/test-theme.R fails when www/theme.css is out of date for the
# bslib version it was built with.

suppressMessages({
  library(shiny)
  library(bslib)
})

# The app's theme: Inter type, calm teal accent, light/dark capable.
custom_theme <- function() {
  bs_theme(
    version = 5,
    # ui.R links Inter from the Google Fonts CDN itself (local = FALSE here keeps
    # the compile from downloading the font files).
    base_font    = font_google("Inter", local = FALSE),
    heading_font = font_google("Inter", local = FALSE),
    primary = "#0d9488",
    # Links/accent text use a darker teal that meets WCAG AA (>=4.5:1) on white;
    # the lighter primary is kept for UI components (buttons/radios), which only
    # need 3:1.
    "link-color" = "#0f766e",
    "border-radius" = "0.6rem"
  )
}

# ---------------------------------------------------------------------------
# A small CSS splitter, enough for compiler output
# ---------------------------------------------------------------------------

# Split `txt` at the top level, ignoring anything inside quotes: into rules and
# statements (decls = FALSE: cut after a closing "}" or a ";" outside braces) or
# into the declarations of one rule body (decls = TRUE: cut at ";" outside
# parentheses).
css_split <- function(txt, decls = FALSE) {
  pos <- gregexpr("[\"'{}();\\\\]", txt)[[1]]
  pos <- pos[pos > 0]                                 # -1 when there is no match
  ch <- if (length(pos)) substring(txt, pos, pos) else character(0)
  pieces <- character(0)
  start <- 1; depth <- 0; paren <- 0; quote <- ""; skip <- 0
  cut <- function(end) {
    pieces[length(pieces) + 1] <<- substr(txt, start, end)
    start <<- end + 1
  }
  for (i in seq_along(pos)) {
    p <- pos[i]; c <- ch[i]
    if (p <= skip) next                               # escaped character
    if (c == "\\") { skip <- p + 1; next }
    if (quote != "") { if (c == quote) quote <- ""; next }
    if (c == "\"" || c == "'") quote <- c
    else if (c == "(") paren <- paren + 1
    else if (c == ")") paren <- paren - 1
    else if (c == "{") depth <- depth + 1
    else if (c == "}") { depth <- depth - 1; if (!decls && depth == 0) cut(p) }
    else if (depth == 0 && paren == 0) {              # c == ";"
      if (decls) { cut(p - 1); start <- p + 1 } else cut(p)
    }
  }
  if (start <= nchar(txt)) pieces <- c(pieces, substr(txt, start, nchar(txt)))
  pieces <- trimws(pieces)
  pieces[nzchar(pieces)]
}

# Read a stylesheet into a flat table of rules: one row per rule with its
# context (the enclosing @media / @supports chain, "" at the top level), its
# selector, and its body. Anything else (@font-face, @keyframes, @import) is
# kept whole, as a row with an empty selector. `n` numbers the occurrences of a
# selector within its context, since a stylesheet can repeat one.
css_rules <- function(txt) {
  walk <- function(txt, context) {
    rows <- lapply(css_split(txt), function(item) {
      brace <- regexpr("{", item, fixed = TRUE)
      head <- if (brace > 0) trimws(substr(item, 1, brace - 1)) else ""
      body <- if (brace > 0) substr(item, brace + 1, nchar(item) - 1) else ""
      if (grepl("^@(media|supports|container|layer)\\b", head))
        walk(body, paste0(context, head, "{"))
      else if (brace > 0 && !startsWith(head, "@"))
        data.frame(context = context, selector = head, body = body)
      else
        data.frame(context = context, selector = "", body = item)
    })
    do.call(rbind, c(list(data.frame(context = character(0), selector = character(0),
                                     body = character(0))), rows))
  }
  rules <- walk(gsub("/\\*[^*]*\\*+(?:[^/*][^*]*\\*+)*/", "", txt, perl = TRUE), "")   # drop comments
  key <- paste(rules$context, rules$selector, sep = "\r")
  rules$n <- stats::ave(seq_along(key), key, FUN = seq_along)
  rules
}

css_property <- function(decl) trimws(sub(":.*$", "", decl))

# The declarations `custom` adds or changes relative to `stock`, as CSS text.
# Stops if the custom stylesheet drops something the stock one has, because an
# override stylesheet cannot express a removal.
css_delta <- function(custom, stock, what = "stylesheet") {
  cr <- css_rules(custom); sr <- css_rules(stock)
  id <- function(r) paste(r$context, r$selector, ifelse(r$selector == "", r$body, r$n), sep = "\r")
  cr$id <- id(cr); sr$id <- id(sr)
  gone <- setdiff(sr$id, cr$id)
  if (length(gone))
    stop(what, ": the custom theme drops ", length(gone), " rule(s) of the stock theme, e.g. ",
         sQuote(sr$selector[match(gone[1], sr$id)]))
  out <- character(0)
  for (i in seq_len(nrow(cr))) {
    j <- match(cr$id[i], sr$id)
    if (cr$selector[i] == "") {
      if (is.na(j)) out <- c(out, paste0(cr$context[i], cr$body[i]))
      else next
    } else {
      decls <- css_split(cr$body[i], decls = TRUE)
      if (!is.na(j)) {
        old <- css_split(sr$body[j], decls = TRUE)
        dropped <- setdiff(css_property(old), css_property(decls))
        if (length(dropped))
          stop(what, ": the custom theme drops ", sQuote(dropped[1]), " from ", sQuote(cr$selector[i]))
        decls <- decls[!decls %in% old]
      }
      if (!length(decls)) next
      out <- c(out, paste0(cr$context[i], cr$selector[i], "{", paste(decls, collapse = ";"), "}"))
    }
    # close the @media / @supports blocks opened by the context
    out[length(out)] <- paste0(out[length(out)],
                               strrep("}", lengths(regmatches(cr$context[i], gregexpr("{", cr$context[i], fixed = TRUE)))))
  }
  out
}

# ---------------------------------------------------------------------------
# Collect the stylesheets the running app serves under a theme
# ---------------------------------------------------------------------------

# Shiny, bslib and selectize resolve their themed stylesheets only inside a
# running app, so this starts one: a temporary copy of the app's UI with
# `theme` swapped in (a function returning a bs_theme) and an empty server,
# run in a background R process. Returns a named character vector,
# "<dependency>" -> CSS text, for every stylesheet the page links locally.
theme_stylesheets <- function(theme, app_dir = ".") {
  tmp <- tempfile("theme-app-")
  dir.create(file.path(tmp, "www"), recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.copy(file.path(app_dir, "ui.R"), file.path(tmp, "ui_app.R"))
  file.copy(list.files(file.path(app_dir, "www"), full.names = TRUE), file.path(tmp, "www"))
  writeLines(c(
    "library(shiny); library(bslib)",
    "env <- new.env()",
    "ui <- source('ui_app.R', local = env)$value",
    paste0("env$prob_theme <- (", paste(deparse(theme), collapse = "\n"), ")()"),
    # The server renders these inputs later (renderUI); put one of each in the
    # page so their themed stylesheets are part of the comparison.
    "function(request) tagList(ui(request),",
    "  selectInput('theme_probe_select', 'x', c('a', 'b')),",
    "  radioButtons('theme_probe_radio', 'x', c('a', 'b')),",
    "  numericInput('theme_probe_number', 'x', 1))"
  ), file.path(tmp, "ui.R"))
  writeLines("function(input, output, session) {}", file.path(tmp, "server.R"))

  port <- httpuv::randomPort()
  app <- callr::r_bg(function(dir, port) shiny::runApp(dir, port = port, launch.browser = FALSE),
                     args = list(tmp, port), wd = tmp)
  on.exit(app$kill(), add = TRUE)
  base <- sprintf("http://127.0.0.1:%d/", port)
  get <- function(path) paste(readLines(paste0(base, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  html <- NULL
  for (i in 1:120) {
    html <- tryCatch(suppressWarnings(get("")), error = function(e) NULL)
    if (!is.null(html)) break
    if (!app$is_alive()) stop("the theme app failed to start:\n", app$read_all_error())
    Sys.sleep(0.25)
  }
  if (is.null(html)) stop("the theme app did not answer on ", base)

  links <- regmatches(html, gregexpr("<link[^>]*>", html))[[1]]
  hrefs <- sub('.*href="([^"]+)".*', "\\1", links[grepl("stylesheet", links, fixed = TRUE)])
  hrefs <- hrefs[!grepl("^https?:", hrefs)]
  # "selectize-0.15.2/selectize.css" -> "selectize": the file name inside a
  # dependency can differ between the precompiled and the compiled build
  sheets <- vapply(hrefs, get, character(1))
  names(sheets) <- sub("-[0-9][0-9.]*$", "", dirname(hrefs))
  sheets
}

build_theme_css <- function(app_dir = ".") {
  stock  <- theme_stylesheets(function() bslib::bs_theme(version = 5), app_dir)
  custom <- theme_stylesheets(custom_theme, app_dir)
  if (!setequal(names(stock), names(custom)))
    stop("the two themes link different stylesheets: ",
         paste(union(setdiff(names(stock), names(custom)), setdiff(names(custom), names(stock))), collapse = ", "))
  parts <- unlist(lapply(names(custom), function(nm) {
    # one rule per line (a selector list can span lines in compiler output)
    delta <- gsub("\\s*\n\\s*", " ", css_delta(custom[[nm]], stock[[nm]], nm))
    if (length(delta)) c(paste0("/* ", nm, " */"), delta)
  }))
  c(sprintf("/* Generated by tools/build-theme.R with bslib %s. Do not edit: change", packageVersion("bslib")),
    "   custom_theme() in that script and rerun it (Rscript tools/build-theme.R). */",
    parts)
}

if (sys.nframe() == 0) {
  css <- build_theme_css()
  writeLines(css, "www/theme.css")
  cat(sprintf("www/theme.css: %d rules, %d bytes\n", sum(!startsWith(css, "/*") & !startsWith(css, "   ")),
              file.size("www/theme.css")))
}
