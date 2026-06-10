skladnia_modelu_erasmus <- "
  Finanse      =~ wysokosc_stypendium + koszty_zycia_mies;
  Jakosc       =~ ranking_uczelni + kompatybilnosc_prog + ocena_biura_erasmus + jakosc_wykladowcow + dostepnosc_akademik;
  Zadowolenie  =~ satysfakcja_program + spolecznosc_int;
  Miasto       =~ odleglosc_od_macierzystej + bezpieczenstwo_miasta + atrakcyjnosc_miasta
"

kryteria_erasmus <- c("Finanse", "Jakosc", "Zadowolenie", "Miasto")
kierunki_erasmus <- rep("max", length(kryteria_erasmus))
ankieta_best_to_others <- c(1, 2, 3, 8)
ankieta_others_to_worst <- c(8, 4, 2, 1)

oczekiwane_nazwy <- c(
  "Hacettepe University",
  "Humboldt Universität zu Berlin",
  "Sapienza - Università di Roma",
  "Universidad Carlos III de Madrid",
  "Universidade do Porto"
)

zaladuj_dane_mcda <- function() {
  data("mcda_dane_surowe", package = "ErasmusMobilityR", envir = environment())
  mcda_dane_surowe
}

sprawdz_ranking_uczelni <- function(wynik, klasa, macierz_decyzyjna) {
  expect_s3_class(wynik, klasa)
  expect_s3_class(wynik$ranking, "data.frame")
  expect_equal(nrow(wynik$ranking), nrow(macierz_decyzyjna))
  expect_setequal(wynik$ranking$Uczelnia_Partnerska, rownames(macierz_decyzyjna))
  expect_setequal(wynik$ranking$Pozycja_w_Rankingu, seq_len(nrow(macierz_decyzyjna)))

  kolumny_numeryczne <- vapply(wynik$ranking, is.numeric, logical(1))
  expect_true(all(is.finite(unlist(wynik$ranking[kolumny_numeryczne]))))
}

test_that("mcda_dane_surowe ma strukture zgodna z generatorem", {
  dane <- zaladuj_dane_mcda()
  
  expect_s3_class(dane, "data.frame")
  expect_equal(dim(dane), c(15, 14))
  expect_named(
    dane,
    c(
      "Student_ID", "Uczelnia", "wysokosc_stypendium", "koszty_zycia_mies",
      "ranking_uczelni", "kompatybilnosc_prog", "ocena_biura_erasmus",
      "jakosc_wykladowcow", "dostepnosc_akademik", "satysfakcja_program",
      "spolecznosc_int", "odleglosc_od_macierzystej", "bezpieczenstwo_miasta",
      "atrakcyjnosc_miasta"
    ),
    ignore.order = FALSE
  )

  expect_equal(dane$Student_ID, seq_len(nrow(dane)))
  expect_equal(sort(
    unique(dane$Uczelnia)), 
     oczekiwane_nazwy <- c(
       "Hacettepe University",
       "Humboldt Universität zu Berlin",
       "Sapienza - Università di Roma",
       "Universidad Carlos III de Madrid",
       "Universidade do Porto"
     ))
  expect_true(all(table(dane$Uczelnia) == 3))

  kolumny_numeryczne <- vapply(dane, is.numeric, logical(1))
  expect_false(any(dane[kolumny_numeryczne] < 0, na.rm = TRUE))
  expect_true(all(dane$koszty_zycia_mies >= 1 & dane$koszty_zycia_mies <= 9))
  expect_true(all(dane$ranking_uczelni >= 1 & dane$ranking_uczelni <= 9))
  expect_true(all(dane$odleglosc_od_macierzystej >= 1 & dane$odleglosc_od_macierzystej <= 9))
  expect_true(all(dane$dostepnosc_akademik %in% c(0, 10)))

  expect_true(any(dane$kompatybilnosc_prog == 99))
  expect_true(any(is.na(dane$atrakcyjnosc_miasta)))
})

test_that("pelny workflow MCDA dziala na mcda_dane_surowe", {
  dane <- zaladuj_dane_mcda()

  macierz <- zbuduj_macierz_rozmyta(
    dane = dane,
    skladnia = skladnia_modelu_erasmus,
    kolumna_uczelni = "Uczelnia"
  )

  expect_type(macierz, "double")
  expect_equal(dim(macierz), c(5, length(kryteria_erasmus) * 3))
  expect_equal(rownames(macierz), oczekiwane_nazwy)
  expect_equal(attr(macierz, "nazwy_kryteriow"), kryteria_erasmus)
  expect_true(all(is.finite(macierz)))
  expect_true(all(macierz[, seq(1, ncol(macierz), 3)] <= macierz[, seq(2, ncol(macierz), 3)]))
  expect_true(all(macierz[, seq(2, ncol(macierz), 3)] <= macierz[, seq(3, ncol(macierz), 3)]))

  wagi_entropia <- wyznacz_wagi_obiektywne_entropia(macierz)
  expect_length(wagi_entropia, length(kryteria_erasmus))
  expect_equal(sum(wagi_entropia), 1, tolerance = 1e-8)
  expect_true(all(wagi_entropia >= 0))

  wagi_bwm <- wyznacz_wagi_kryteriow_bwm(
    nazwy_kryteriow = kryteria_erasmus,
    najwazniejszy_vs_reszta = ankieta_best_to_others,
    reszta_vs_najmniej_wazny = ankieta_others_to_worst
  )
  expect_named(
    wagi_bwm,
    c("badane_filary", "obliczone_wagi", "wskaznik_spojnosci_cr", "blad_optymalizacji_ksi")
  )
  expect_equal(sum(wagi_bwm$obliczone_wagi), 1, tolerance = 1e-8)
  expect_true(all(wagi_bwm$obliczone_wagi >= 0))
  expect_true(is.finite(wagi_bwm$wskaznik_spojnosci_cr))

  wynik_topsis <- suppressMessages(wyznacz_ranking_topsis(
    rozmyta_macierz_decyzyjna = macierz,
    kierunki_kryteriow = kierunki_erasmus,
    nazwy_kryteriow_bwm = kryteria_erasmus,
    bwm_najlepsze = ankieta_best_to_others,
    bwm_najgorsze = ankieta_others_to_worst
  ))
  sprawdz_ranking_uczelni(wynik_topsis, "topsis_erasmus_wynik", macierz)

  wynik_vikor <- suppressMessages(wyznacz_ranking_vikor(
    rozmyta_macierz_decyzyjna = macierz,
    kierunki_kryteriow = kierunki_erasmus,
    nazwy_kryteriow_bwm = kryteria_erasmus,
    bwm_najlepsze = ankieta_best_to_others,
    bwm_najgorsze = ankieta_others_to_worst
  ))
  sprawdz_ranking_uczelni(wynik_vikor, "vikor_erasmus_wynik", macierz)

  wynik_waspas <- suppressMessages(wyznacz_ranking_waspas(
    rozmyta_macierz_decyzyjna = macierz,
    kierunki_kryteriow = kierunki_erasmus,
    nazwy_kryteriow_bwm = kryteria_erasmus,
    bwm_najlepsze = ankieta_best_to_others,
    bwm_najgorsze = ankieta_others_to_worst
  ))
  sprawdz_ranking_uczelni(wynik_waspas, "waspas_erasmus_wynik", macierz)

  meta_wynik <- suppressMessages(wyznacz_meta_ranking_erasmus(
    rozmyta_macierz_decyzyjna = macierz,
    kierunki_kryteriow = kierunki_erasmus,
    nazwy_kryteriow_bwm = kryteria_erasmus,
    bwm_najlepsze = ankieta_best_to_others,
    bwm_najgorsze = ankieta_others_to_worst
  ))

  expect_s3_class(meta_wynik$porownanie, "data.frame")
  expect_equal(nrow(meta_wynik$porownanie), nrow(macierz))
  expect_setequal(meta_wynik$porownanie$Uczelnia_Partnerska, rownames(macierz))
  expect_equal(dim(meta_wynik$zgodnosc_metod_korelacja), c(3, 3))
  expect_true(all(is.finite(meta_wynik$zgodnosc_metod_korelacja)))

  kolumny_rang <- c(
    "Miejsce_TOPSIS",
    "Miejsce_VIKOR",
    "Miejsce_WASPAS",
    "Meta_Srednia_Pozycja",
    "Meta_Dominacja",
    "Meta_Konsensus_RA"
  )
  for (kolumna in kolumny_rang) {
    expect_setequal(meta_wynik$porownanie[[kolumna]], seq_len(nrow(macierz)))
  }
})
