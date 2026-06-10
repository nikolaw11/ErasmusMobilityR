#!/usr/bin/env Rscript

old_wd <- getwd()

if (basename(old_wd) == "tools") {
  setwd("..")
}
root <- getwd()

if (!file.exists("DESCRIPTION")) {
  stop("Błąd: Nie znaleziono pliku DESCRIPTION.", call. = FALSE)
}

message("Katalog roboczy ustawiony na: ", root)
on.exit(setwd(old_wd), add = TRUE)

require_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required to render documentation.", package), call. = FALSE)
  }
}

require_package("rmarkdown")
require_package("knitr")

ensure_pandoc <- function() {
  if (rmarkdown::pandoc_available()) {
    return(invisible(TRUE))
  }

  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC", unset = NA_character_),
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Positron/resources/app/quarto/bin/tools"
  )

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  candidates <- candidates[
    file.exists(file.path(candidates, "pandoc.exe")) |
      file.exists(file.path(candidates, "pandoc"))
  ]

  for (candidate in candidates) {
    Sys.setenv(RSTUDIO_PANDOC = candidate)
    rmarkdown::find_pandoc(cache = FALSE)
    if (rmarkdown::pandoc_available()) {
      return(invisible(TRUE))
    }
  }

  stop("Pandoc is required. Install Pandoc/Quarto or run the GitHub Actions workflow.", call. = FALSE)
}

ensure_pandoc()

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(root, quiet = TRUE)
} else if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(root, quiet = TRUE)
} else {
  message("pkgload/devtools not found; rendering will use an installed ErasmusMobilityR package.")
}

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)
unlink(Sys.glob("man/figures/README-*.png"))

message("Rendering README.md...")
rmarkdown::render(
  input = "README.Rmd",
  output_format = "github_document",
  encoding = "UTF-8",
  quiet = FALSE
)

message("Rendering index.html...")
source(file.path("tools", "render-site.R"), local = new.env(parent = globalenv()))

message("Rendering vignette preview...")
rmarkdown::render(
  input = "vignettes/poradnik_mcda.Rmd",
  output_format = rmarkdown::html_vignette(),
  output_dir = "doc",
  encoding = "UTF-8",
  quiet = FALSE
)
