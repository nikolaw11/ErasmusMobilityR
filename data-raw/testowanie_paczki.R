# data-raw/testowanie_paczki.R
#
# Uruchom po odswiezeniu danych skryptem:
#   Rscript data-raw/generowanie_danych.R
#
# Ten runner sprawdza, czy data/mcda_dane_surowe.rda istnieje i odpala testy
# pakietu, w tym pelny workflow MCDA z plikow tests/testthat.

if (!file.exists("data/mcda_dane_surowe.rda")) {
  stop(
    "Brakuje data/mcda_dane_surowe.rda. Najpierw uruchom data-raw/generowanie_danych.R.",
    call. = FALSE
  )
}

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop(
    "Do uruchomienia testow potrzebny jest pakiet testthat.",
    call. = FALSE
  )
}

testthat::test_local(reporter = "summary")
