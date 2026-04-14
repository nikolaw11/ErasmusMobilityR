#' @title Wewnętrzna Funkcja Dominacji dla Meta-Rankingu
#' @description
#' Funkcja pomocnicza wyznaczająca ranking konsensusu na podstawie reguły większości.
#' Rozstrzyga, która uczelnia najczęściej wygrywa na danej pozycji w trzech różnych algorytmach.
#' @param r1 Wektor numeryczny rang metody TOPSIS.
#' @param r2 Wektor numeryczny rang metody VIKOR.
#' @param r3 Wektor numeryczny rang metody WASPAS.
#' @return Wektor numeryczny z finalnym rankingiem.
#' @keywords internal
.oblicz_dominacje_erasmus <- function(r1, r2, r3) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)
  macierz_rang <- cbind(r1, r2, r3)
  dostepne <- rep(TRUE, n)
  
  for (obecna_pozycja in 1:n) {
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf
    
    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])
    najlepszy_r3 <- which.min(obecna_macierz[, 3])
    
    kandydaci <- c(najlepszy_r1, najlepszy_r2, najlepszy_r3)
    tabela_czestosci <- table(kandydaci)
    zwyciezca_idx <- as.numeric(names(tabela_czestosci)[which.max(tabela_czestosci)])
    
    # Obsługa remisu (gdy każdy algorytm wskazuje inną uczelnię)
    if (length(tabela_czestosci) == 3) {
      c1 <- najlepszy_r1; c2 <- najlepszy_r2; c3 <- najlepszy_r3
      
      c1_wygrane <- sum(macierz_rang[c1, ] < macierz_rang[c2, ]) + sum(macierz_rang[c1, ] < macierz_rang[c3, ])
      c2_wygrane <- sum(macierz_rang[c2, ] < macierz_rang[c1, ]) + sum(macierz_rang[c2, ] < macierz_rang[c3, ])
      c3_wygrane <- sum(macierz_rang[c3, ] < macierz_rang[c1, ]) + sum(macierz_rang[c3, ] < macierz_rang[c2, ])
      
      wygrane <- c(c1_wygrane, c2_wygrane, c3_wygrane)
      
      if (which.max(wygrane) == 1) zwyciezca_idx <- c1
      else if (which.max(wygrane) == 2) zwyciezca_idx <- c2
      else zwyciezca_idx <- c3
    }
    
    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] <- FALSE
  }
  return(finalny_ranking)
}


#' @title Ostateczny Meta-Ranking Uczelni (Konsensus z trzech metod)
#' @description
#' Agreguje wyniki z metod Fuzzy TOPSIS, Fuzzy VIKOR i Fuzzy WASPAS, aby stworzyć
#' jeden, absolutnie odporny ranking końcowy dla mobilności studenckiej.
#'
#' @param rozmyta_macierz_decyzyjna Rozmyta macierz danych ze wskaźnikami uczelni.
#' @param kierunki_kryteriow Wektor kierunków optymalizacji ("min", "max").
#' @param wagi_odgorne (Opcjonalnie) Wagi kryteriów wyliczone np. metodą Entropii.
#' @param nazwy_kryteriow_bwm (Opcjonalnie) Nazwy kryteriów do modelu BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor priorytetów Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor priorytetów Others-to-Worst.
#' @param wspolczynnik_kompromisu_v Parametr dla VIKOR (domyślnie 0.5).
#' @param wspolczynnik_lambda Parametr dla WASPAS (domyślnie 0.5).
#'
#' @return Lista zawierająca ramkę danych z zestawieniem wszystkich rankingów oraz macierz korelacji.
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#' @export
wyznacz_meta_ranking_erasmus <- function(
    rozmyta_macierz_decyzyjna,
    kierunki_kryteriow,
    wagi_odgorne = NULL,
    nazwy_kryteriow_bwm = NULL,
    bwm_najlepsze = NULL,
    bwm_najgorsze = NULL,
    wspolczynnik_kompromisu_v = 0.5,
    wspolczynnik_lambda = 0.5) 
{
  
  # 1. Zabezpieczenie wag
  if (is.null(wagi_odgorne) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak zadeklarowanych wag. Uruchamiam autmatyczne wyliczanie obiektywne (Entropia Shannona)...")
    wagi_odgorne <- wyznacz_wagi_obiektywne_entropia(rozmyta_macierz_decyzyjna)
  }
  
  if (!is.null(bwm_najlepsze) && is.null(nazwy_kryteriow_bwm)) {
    nazwy_kryteriow_bwm <- attr(rozmyta_macierz_decyzyjna, "nazwy_kryteriow")
  }
  
  # 2. Uruchomienie metody TOPSIS
  message("Generowanie rankingu Fuzzy TOPSIS...")
  res_topsis <- wyznacz_ranking_topsis(
    rozmyta_macierz_decyzyjna = rozmyta_macierz_decyzyjna,
    kierunki_kryteriow = kierunki_kryteriow,
    wagi_odgorne = wagi_odgorne,
    nazwy_kryteriow_bwm = nazwy_kryteriow_bwm,
    bwm_najlepsze = bwm_najlepsze,
    bwm_najgorsze = bwm_najgorsze
  )
  
  # 3. Uruchomienie metody VIKOR
  message("Generowanie rankingu Fuzzy VIKOR...")
  res_vikor <- wyznacz_ranking_vikor(
    rozmyta_macierz_decyzyjna = rozmyta_macierz_decyzyjna,
    kierunki_kryteriow = kierunki_kryteriow,
    wspolczynnik_kompromisu_v = wspolczynnik_kompromisu_v,
    wagi_odgorne = wagi_odgorne,
    nazwy_kryteriow_bwm = nazwy_kryteriow_bwm,
    bwm_najlepsze = bwm_najlepsze,
    bwm_najgorsze = bwm_najgorsze
  )
  
  # 4. Uruchomienie metody WASPAS
  message("Generowanie rankingu Fuzzy WASPAS...")
  res_waspas <- wyznacz_ranking_waspas(
    rozmyta_macierz_decyzyjna = rozmyta_macierz_decyzyjna,
    kierunki_kryteriow = kierunki_kryteriow,
    wspolczynnik_lambda = wspolczynnik_lambda,
    wagi_odgorne = wagi_odgorne,
    nazwy_kryteriow_bwm = nazwy_kryteriow_bwm,
    bwm_najlepsze = bwm_najlepsze,
    bwm_najgorsze = bwm_najgorsze
  )
  
  # 5. Ekstrakcja i sortowanie ostatecznych miejsc
  tabela_topsis <- res_topsis$ranking[order(res_topsis$ranking$Uczelnia_Partnerska), ]
  tabela_vikor <- res_vikor$ranking[order(res_vikor$ranking$Uczelnia_Partnerska), ]
  tabela_waspas <- res_waspas$ranking[order(res_waspas$ranking$Uczelnia_Partnerska), ]
  
  r_topsis <- tabela_topsis$Pozycja_w_Rankingu
  r_vikor <- tabela_vikor$Pozycja_w_Rankingu
  r_waspas <- tabela_waspas$Pozycja_w_Rankingu
  nazwy_uczelni <- tabela_topsis$Uczelnia_Partnerska
  
  # 6. Agregacja Rankingów
  
  # A. Borda Count (Suma Rang)
  suma_pozycji <- r_vikor + r_topsis + r_waspas
  ranking_suma <- rank(suma_pozycji, ties.method = "first")
  
  # B. Teoria Dominacji (Głosowanie większościowe)
  ranking_dominacja <- .oblicz_dominacje_erasmus(r_topsis, r_vikor, r_waspas)
  
  # C. RankAggreg (Algorytm konsensusu)
  macierz_dla_ra <- rbind(order(r_topsis), order(r_vikor), order(r_waspas))
  liczba_uczelni <- nrow(rozmyta_macierz_decyzyjna)
  
  if (liczba_uczelni <= 10) {
    ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, liczba_uczelni, distance = "Spearman")
  } else {
    ra_wynik <- RankAggreg::RankAggreg(macierz_dla_ra, liczba_uczelni, method = "GA", distance = "Spearman", verbose = FALSE)
  }
  
  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(liczba_uczelni)
  
  for(pozycja in 1:liczba_uczelni) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }
  
  # 7. Zestawienie wyników
  porownanie_df <- data.frame(
    Uczelnia_Partnerska = nazwy_uczelni,
    Miejsce_TOPSIS = r_topsis,
    Miejsce_VIKOR = r_vikor,
    Miejsce_WASPAS = r_waspas,
    Meta_Srednia_Pozycja = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Konsensus_RA = wektor_ra
  )
  
  # Sortowanie po głównym konsensusie algorytmicznym
  porownanie_df <- porownanie_df[order(porownanie_df$Meta_Konsensus_RA), ]
  rownames(porownanie_df) <- NULL
  
  # Macierz Korelacji Spearmana dla 3 metod
  macierz_kor <- cor(porownanie_df[, c("Miejsce_TOPSIS", "Miejsce_VIKOR", "Miejsce_WASPAS")], method = "spearman")
  
  wynik_finałowy <- list(
    porownanie = porownanie_df,
    zgodnosc_metod_korelacja = macierz_kor
  )
  
  return(wynik_finałowy)
}