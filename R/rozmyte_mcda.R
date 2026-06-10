#' Obliczanie obiektywnych wag metodą Entropii Shannona
#'
#' @description Wyznacza obiektywne wagi kryteriów na podstawie danych z ankiet studenckich,
#' mierząc stopień rozproszenia ocen. Im większa zmienność opinii o danym kryterium, tym wyższa jego waga.
#'
#' @param rozmyta_macierz_decyzyjna Rozmyta macierz uczelni (wynik funkcji `zbuduj_macierz_rozmyta`).
#' @return Wektor numeryczny wag kryteriów sumujący się do 1 (100%).
#' @export
wyznacz_wagi_obiektywne_entropia <- function(rozmyta_macierz_decyzyjna) {
  
  # Od-rozmycie macierzy uczelni do obliczeń entropii (oczekiwana wartość z l, m, u)
  n_kolumn <- ncol(rozmyta_macierz_decyzyjna)
  macierz_srednich_ocen <- matrix(0, nrow = nrow(rozmyta_macierz_decyzyjna), ncol = n_kolumn/3)
  
  k <- 1
  for(j in seq(1, n_kolumn, 3)) {
    # Standardowe odrozmycie TFN: (l + 4m + u) / 6
    macierz_srednich_ocen[, k] <- (rozmyta_macierz_decyzyjna[, j] + 4*rozmyta_macierz_decyzyjna[, j+1] + rozmyta_macierz_decyzyjna[, j+2]) / 6
    k <- k + 1
  }
  
  # Normalizacja danych (P_ij)
  sumy_kolumn <- colSums(macierz_srednich_ocen)
  sumy_kolumn[sumy_kolumn == 0] <- 1 # Zabezpieczenie przed dzieleniem przez zero
  macierz_znormalizowana <- sweep(macierz_srednich_ocen, 2, sumy_kolumn, "/")
  
  # Obliczanie wskaźnika Entropii (E_j)
  stala_entropii <- 1 / log(nrow(rozmyta_macierz_decyzyjna))
  wektor_entropii <- numeric(ncol(macierz_znormalizowana))
  
  for(j in 1:ncol(macierz_znormalizowana)) {
    wartosci_p <- macierz_znormalizowana[, j]
    wartosci_p <- wartosci_p[wartosci_p > 0] # Ignorujemy zera dla logarytmu naturalnego
    if(length(wartosci_p) == 0) {
      wektor_entropii[j] <- 1
    } else {
      wektor_entropii[j] <- -stala_entropii * sum(wartosci_p * log(wartosci_p))
    }
  }
  
  # Wyznaczenie ostatecznych wag obiektywnych (Stopień rozbieżności d_j)
  stopien_rozbieznosci <- 1 - wektor_entropii
  if(sum(stopien_rozbieznosci) == 0) return(rep(1/length(stopien_rozbieznosci), length(stopien_rozbieznosci))) # Bezpiecznik
  wagi_obiektywne <- stopien_rozbieznosci / sum(stopien_rozbieznosci)
  
  return(wagi_obiektywne)
}

#' @title Wewnętrzny menedżer priorytetów ankiet
#' @description Decyduje, skąd pobrać wagi dla kryteriów wyjazdu (np. ustalone odgórnie vs z modelu BWM).
#' @keywords internal
.ustal_wagi_kryteriow_mobilnosci <- function(
    rozmyta_macierz_decyzyjna, 
    wagi_odgorne, 
    nazwy_kryteriow_bwm, 
    ankieta_najwazniejszy_vs_reszta, 
    ankieta_reszta_vs_najmniej_wazny) 
{
  
  liczba_kryteriow <- ncol(rozmyta_macierz_decyzyjna) / 3
  
  # Opcja 1: Wagi podane bezpośrednio (np. wyliczone z Entropii lub narzucone przez badacza)
  if (!missing(wagi_odgorne) && !is.null(wagi_odgorne)) {
    if (length(wagi_odgorne) == liczba_kryteriow) {
      # Przekształcamy wagi "ostre" na format rozmyty (w, w, w) dopasowany do macierzy uczelni
      return(rep(wagi_odgorne, each = 3))
    }
    if (length(wagi_odgorne) != ncol(rozmyta_macierz_decyzyjna)) {
      stop("Błąd w strukturze wag. Długość wektora 'wagi_odgorne' musi odpowiadać liczbie kryteriów.")
    }
    return(wagi_odgorne)
  }
  
  # Opcja 2: Dynamiczne wyliczenie wag na podstawie ankiet studenckich BWM
  if (!missing(ankieta_najwazniejszy_vs_reszta) && !missing(ankieta_reszta_vs_najmniej_wazny)) {
    
    # Identyfikacja nazw kryteriów do analizy BWM
    if (missing(nazwy_kryteriow_bwm)) {
      if (!is.null(attr(rozmyta_macierz_decyzyjna, "nazwy_kryteriow"))) {
        nazwy_kryteriow_bwm <- attr(rozmyta_macierz_decyzyjna, "nazwy_kryteriow")
      } else {
        nazwy_kryteriow_bwm <- paste0("Kryterium_", 1:liczba_kryteriow)
        message("Brak metadanych o kryteriach. Stosuję nazwy zastępcze: ", paste(nazwy_kryteriow_bwm, collapse=", "))
      }
    }
    
    message("Przetwarzanie preferencji studenta metodą BWM...")
    
    # Wywołanie naszej dedykowanej funkcji BWM!
    wyniki_analizy_bwm <- wyznacz_wagi_kryteriow_bwm(
      nazwy_kryteriow = nazwy_kryteriow_bwm, 
      najwazniejszy_vs_reszta = ankieta_najwazniejszy_vs_reszta, 
      reszta_vs_najmniej_wazny = ankieta_reszta_vs_najmniej_wazny
    )
    
    wagi_ostre_bwm <- wyniki_analizy_bwm$obliczone_wagi
    
    if (length(wagi_ostre_bwm) != liczba_kryteriow) {
      stop("Krytyczny błąd: Liczba wag wygenerowanych przez model BWM nie pasuje do rozmiaru macierzy uczelni.")
    }
    
    # Klonowanie wag do formatu rozmytego dla zgodności z trójkami TFN
    wagi_rozmyte_bwm <- rep(wagi_ostre_bwm, each = 3)
    return(wagi_rozmyte_bwm)
  }
  
  stop("Decyzja niemożliwa: Musisz dostarczyć wektor 'wagi_odgorne' LUB wypełnione ankiety BWM ('ankieta_najwazniejszy_vs_reszta' i 'ankieta_reszta_vs_najmniej_wazny').")
}



#' Rozmyta Metoda TOPSIS dla Wyboru Uczelni
#'
#' @description Implementacja algorytmu Fuzzy TOPSIS w kontekście mobilności studenckiej.
#' Oblicza odległość każdej badanej uczelni od wyidealizowanego rozwiązania perfekcyjnego (FPIS)
#' oraz najgorszego rozwiązania anty-idealnego (FNIS).
#'
#' @param rozmyta_macierz_decyzyjna Macierz m x 3n.
#' @param kierunki_kryteriow Wektor znakowy ("max" dla kryteriów pozytywnych np. stypendium, "min" dla negatywnych np. koszty).
#' @param wagi_odgorne (Opcjonalnie) Gotowy wektor wag dla kryteriów (np. z metody Entropii).
#' @param nazwy_kryteriow_bwm (Opcjonalnie) Nazwy kryteriów do dynamicznego wyliczenia wag z ankiet BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `topsis_erasmus_wynik` z rankingiem uczelni wyjazdowych.
#' @export
wyznacz_ranking_topsis <- function(
    rozmyta_macierz_decyzyjna, 
    kierunki_kryteriow, 
    wagi_odgorne = NULL,
    nazwy_kryteriow_bwm, 
    bwm_najlepsze, 
    bwm_najgorsze) 
{
  
  if (!is.matrix(rozmyta_macierz_decyzyjna)) 
    stop("Błąd: 'rozmyta_macierz_decyzyjna' musi być formacie macierzy.")
  
  # 1. Integracja z silnikiem wag: Ustalenie priorytetów kryteriów
  wagi_finalne <- .ustal_wagi_kryteriow_mobilnosci(
    rozmyta_macierz_decyzyjna = rozmyta_macierz_decyzyjna, 
    wagi_odgorne = wagi_odgorne, 
    nazwy_kryteriow_bwm = nazwy_kryteriow_bwm, 
    ankieta_najwazniejszy_vs_reszta = bwm_najlepsze, 
    ankieta_reszta_vs_najmniej_wazny = bwm_najgorsze 
  )
  
  # 2. Przeniesienie logiki "zysk/koszt" (max/min) na wszystkie granice rozmycia
  liczba_kolumn <- ncol(rozmyta_macierz_decyzyjna)
  kierunki_rozmyte <- character(liczba_kolumn)
  indeks_kryterium <- 1
  
  for (j in seq(1, liczba_kolumn, 3)) {
    kierunki_rozmyte[j:(j+2)] <- kierunki_kryteriow[indeks_kryterium]
    indeks_kryterium <- indeks_kryterium + 1
  }
  
  # 3. Normalizacja wektorowa ocen
  znormalizowana_macierz_uczelni <- matrix(nrow = nrow(rozmyta_macierz_decyzyjna), ncol = liczba_kolumn)
  moduly_wektorow <- sqrt(apply(rozmyta_macierz_decyzyjna^2, 2, sum))
  
  for (i in seq(1, liczba_kolumn, 3)) {
    znormalizowana_macierz_uczelni[, i]   <- rozmyta_macierz_decyzyjna[, i]   / moduly_wektorow[i + 2]
    znormalizowana_macierz_uczelni[, i+1] <- rozmyta_macierz_decyzyjna[, i+1] / moduly_wektorow[i + 1]
    znormalizowana_macierz_uczelni[, i+2] <- rozmyta_macierz_decyzyjna[, i+2] / moduly_wektorow[i]
  }
  
  # 4. Nałożenie wag na znormalizowane oceny uczelni
  macierz_przekatniowa_wag <- diag(wagi_finalne)
  wazona_macierz_uczelni <- znormalizowana_macierz_uczelni %*% macierz_przekatniowa_wag
  
  # 5. Wyznaczenie punktów odniesienia (Rozwiązania FPIS i FNIS)
  idealna_uczelnia_pozytywna <- ifelse(kierunki_rozmyte == "max", apply(wazona_macierz_uczelni, 2, max), apply(wazona_macierz_uczelni, 2, min))
  idealna_uczelnia_negatywna <- ifelse(kierunki_rozmyte == "min", apply(wazona_macierz_uczelni, 2, max), apply(wazona_macierz_uczelni, 2, min))
  
  # 6. Obliczenie odległości metodą geometryczną (Wierzchołkową)
  roznice_do_idealu_poz <- (wazona_macierz_uczelni - matrix(idealna_uczelnia_pozytywna, nrow=nrow(rozmyta_macierz_decyzyjna), ncol=liczba_kolumn, byrow=TRUE))^2
  roznice_do_idealu_neg <- (wazona_macierz_uczelni - matrix(idealna_uczelnia_negatywna, nrow=nrow(rozmyta_macierz_decyzyjna), ncol=liczba_kolumn, byrow=TRUE))^2
  
  dystans_poz_rozmyty <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  dystans_neg_rozmyty <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  
  # Sumowanie odchyleń dla wartości dolnych, środkowych i górnych (l, m, u)
  dystans_poz_rozmyty[,1] <- sqrt(apply(roznice_do_idealu_poz[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, sum))
  dystans_poz_rozmyty[,2] <- sqrt(apply(roznice_do_idealu_poz[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, sum))
  dystans_poz_rozmyty[,3] <- sqrt(apply(roznice_do_idealu_poz[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, sum))
  
  dystans_neg_rozmyty[,1] <- sqrt(apply(roznice_do_idealu_neg[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, sum))
  dystans_neg_rozmyty[,2] <- sqrt(apply(roznice_do_idealu_neg[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, sum))
  dystans_neg_rozmyty[,3] <- sqrt(apply(roznice_do_idealu_neg[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, sum))
  
  # 7. Obliczenie współczynnika bliskości CC (Closeness Coefficient)
  mianownik_odleglosci <- dystans_neg_rozmyty + dystans_poz_rozmyty
  wspolczynnik_cc_rozmyty <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  
  # Rozmyte dzielenie interwałów
  wspolczynnik_cc_rozmyty[,1] <- dystans_neg_rozmyty[,1] / mianownik_odleglosci[,3]
  wspolczynnik_cc_rozmyty[,2] <- dystans_neg_rozmyty[,2] / mianownik_odleglosci[,2]
  wspolczynnik_cc_rozmyty[,3] <- dystans_neg_rozmyty[,3] / mianownik_odleglosci[,1]
  
  # 8. Defuzzyfikacja końcowa - wyciągnięcie ostrej wartości do rankingu (Metoda GMI)
  ostateczna_ocena_rankingowa <- (wspolczynnik_cc_rozmyty[,1] + 4*wspolczynnik_cc_rozmyty[,2] + wspolczynnik_cc_rozmyty[,3]) / 6
  
  # Przygotowanie czytelnej tabeli dla analityka
  skalar_dystansu_poz <- rowMeans(dystans_poz_rozmyty)
  skalar_dystansu_neg <- rowMeans(dystans_neg_rozmyty)
  nazwy_ocenianych_uczelni <- rownames(rozmyta_macierz_decyzyjna)
  
  tabela_podsumowujaca <- data.frame(
    Uczelnia_Partnerska = nazwy_ocenianych_uczelni,
    Dystans_Od_Idealu = skalar_dystansu_poz,
    Dystans_Od_Antyidealu = skalar_dystansu_neg,
    Wskaznik_CC = ostateczna_ocena_rankingowa,
    Pozycja_w_Rankingu = rank(-ostateczna_ocena_rankingowa, ties.method = "first")
  )
  
  # Posortowanie od najlepszej do najgorszej uczelni
  tabela_podsumowujaca <- tabela_podsumowujaca[order(tabela_podsumowujaca$Pozycja_w_Rankingu), ]
  rownames(tabela_podsumowujaca) <- NULL
  
  wynik_ewaluacji <- list(
    ranking = tabela_podsumowujaca,
    wykorzystana_metoda = "Fuzzy TOPSIS (Wybór Mobilności)"
  )
  
  class(wynik_ewaluacji) <- "topsis_erasmus_wynik"
  return(wynik_ewaluacji)
}



#' Rozmyta Metoda VIKOR dla Wyboru Uczelni
#'
#' @description Implementacja kompromisowej metody Fuzzy VIKOR. Oblicza wskaźniki S (maksymalna użyteczność grupowa),
#' R (indywidualny żal decydenta) oraz Q (ostateczny indeks kompromisu) dla uczelni partnerskich.
#'
#' @param rozmyta_macierz_decyzyjna Macierz m x 3n wygenerowana ze wskaźników uczelni.
#' @param kierunki_kryteriow Wektor znakowy ("max" dla kryteriów pozytywnych, "min" dla negatywnych).
#' @param wspolczynnik_kompromisu_v Waga strategii "większości kryteriów" (domyślnie 0.5 oznacza równowagę).
#' @param wagi_odgorne (Opcjonalnie) Gotowy wektor wag dla kryteriów.
#' @param nazwy_kryteriow_bwm (Opcjonalnie) Nazwy kryteriów do dynamicznego wyliczenia wag z ankiet BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `vikor_erasmus_wynik` z rankingiem uczelni.
#' @export
wyznacz_ranking_vikor <- function(
    rozmyta_macierz_decyzyjna, 
    kierunki_kryteriow, 
    wspolczynnik_kompromisu_v = 0.5, 
    wagi_odgorne = NULL,
    nazwy_kryteriow_bwm, 
    bwm_najlepsze, 
    bwm_najgorsze) 
{
  
  # 1. Integracja z silnikiem wag (z poprawnym mapowaniem zmiennych!)
  wagi_finalne <- .ustal_wagi_kryteriow_mobilnosci(
    rozmyta_macierz_decyzyjna, 
    wagi_odgorne, 
    nazwy_kryteriow_bwm, 
    bwm_najlepsze, 
    bwm_najgorsze 
  )
  
  liczba_kolumn <- ncol(rozmyta_macierz_decyzyjna)
  
  # Rozszerzenie typów zysk/koszt na trójki rozmyte
  kierunki_rozmyte <- character(liczba_kolumn)
  indeks_kryterium <- 1
  for (j in seq(1, liczba_kolumn, 3)) {
    kierunki_rozmyte[j:(j+2)] <- kierunki_kryteriow[indeks_kryterium]
    indeks_kryterium <- indeks_kryterium + 1
  }
  
  # 2. Wyznaczenie Rozwiązań Idealnych dla uczelni
  idealna_uczelnia_pozytywna <- ifelse(kierunki_rozmyte == "max", apply(rozmyta_macierz_decyzyjna, 2, max), apply(rozmyta_macierz_decyzyjna, 2, min))
  idealna_uczelnia_negatywna <- ifelse(kierunki_rozmyte == "min", apply(rozmyta_macierz_decyzyjna, 2, max), apply(rozmyta_macierz_decyzyjna, 2, min))
  
  # 3. Normalizacja liniowa (specyficzna dla VIKOR) i ważenie
  macierz_znormalizowana <- matrix(0, nrow = nrow(rozmyta_macierz_decyzyjna), ncol = liczba_kolumn)
  
  for (i in seq(1, liczba_kolumn, 3)) {
    if (kierunki_rozmyte[i] == "max") {
      mianownik <- idealna_uczelnia_pozytywna[i+2] - idealna_uczelnia_negatywna[i]
      if(mianownik == 0) mianownik <- 1e-9
      
      macierz_znormalizowana[, i]   <- (idealna_uczelnia_pozytywna[i]   - rozmyta_macierz_decyzyjna[, i+2]) / mianownik
      macierz_znormalizowana[, i+1] <- (idealna_uczelnia_pozytywna[i+1] - rozmyta_macierz_decyzyjna[, i+1]) / mianownik
      macierz_znormalizowana[, i+2] <- (idealna_uczelnia_pozytywna[i+2] - rozmyta_macierz_decyzyjna[, i])   / mianownik
    } else {
      mianownik <- idealna_uczelnia_negatywna[i+2] - idealna_uczelnia_pozytywna[i]
      if(mianownik == 0) mianownik <- 1e-9
      
      macierz_znormalizowana[, i]   <- (rozmyta_macierz_decyzyjna[, i]   - idealna_uczelnia_pozytywna[i+2]) / mianownik
      macierz_znormalizowana[, i+1] <- (rozmyta_macierz_decyzyjna[, i+1] - idealna_uczelnia_pozytywna[i+1]) / mianownik
      macierz_znormalizowana[, i+2] <- (rozmyta_macierz_decyzyjna[, i+2] - idealna_uczelnia_pozytywna[i])   / mianownik
    }
  }
  
  # Mnożenie znormalizowanych odległości przez wagi kryteriów
  macierz_przekatniowa_wag <- diag(wagi_finalne)
  wazona_macierz_uczelni <- macierz_znormalizowana %*% macierz_przekatniowa_wag
  
  # 4. Wartości S (Użyteczność Grupowa) i R (Indywidualny Żal)
  uzytecznosc_grupowa_S <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  indywidualny_zal_R <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  
  uzytecznosc_grupowa_S[,1] <- apply(wazona_macierz_uczelni[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, sum)
  uzytecznosc_grupowa_S[,2] <- apply(wazona_macierz_uczelni[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, sum)
  uzytecznosc_grupowa_S[,3] <- apply(wazona_macierz_uczelni[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, sum)
  
  indywidualny_zal_R[,1] <- apply(wazona_macierz_uczelni[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, max)
  indywidualny_zal_R[,2] <- apply(wazona_macierz_uczelni[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, max)
  indywidualny_zal_R[,3] <- apply(wazona_macierz_uczelni[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, max)
  
  # 5. Indeks kompromisu Q
  s_minimum <- min(uzytecznosc_grupowa_S[,1])
  s_maksimum <- max(uzytecznosc_grupowa_S[,3])
  r_minimum <- min(indywidualny_zal_R[,1])
  r_maksimum <- max(indywidualny_zal_R[,3])
  
  mianownik_s <- s_maksimum - s_minimum
  mianownik_r <- r_maksimum - r_minimum
  if (mianownik_s == 0) mianownik_s <- 1
  if (mianownik_r == 0) mianownik_r <- 1
  
  czlon_grupowy <- (uzytecznosc_grupowa_S - s_minimum) / mianownik_s
  czlon_indywidualny <- (indywidualny_zal_R - r_minimum) / mianownik_r
  
  indeks_kompromisu_Q <- wspolczynnik_kompromisu_v * czlon_grupowy + (1 - wspolczynnik_kompromisu_v) * czlon_indywidualny
  
  # 6. Defuzzyfikacja końcowa - wyciągnięcie ostrej wartości do rankingu (VIKOR używa innego wzoru niż TOPSIS)
  ostre_S <- (uzytecznosc_grupowa_S[,1] + 2*uzytecznosc_grupowa_S[,2] + uzytecznosc_grupowa_S[,3]) / 4
  ostre_R <- (indywidualny_zal_R[,1] + 2*indywidualny_zal_R[,2] + indywidualny_zal_R[,3]) / 4
  ostre_Q <- (indeks_kompromisu_Q[,1] + 2*indeks_kompromisu_Q[,2] + indeks_kompromisu_Q[,3]) / 4
  
  nazwy_ocenianych_uczelni <- rownames(rozmyta_macierz_decyzyjna)
  
  # W metodzie VIKOR zasada jest odwrotna niż w TOPSIS: im mniejszy indeks Q, tym lepsza pozycja!
  tabela_podsumowujaca <- data.frame(
    Uczelnia_Partnerska = nazwy_ocenianych_uczelni,
    Wskaznik_S = ostre_S,
    Wskaznik_R = ostre_R,
    Indeks_Q = ostre_Q,
    Pozycja_w_Rankingu = rank(ostre_Q, ties.method = "first") 
  )
  
  # Posortowanie od najlepszej do najgorszej uczelni
  tabela_podsumowujaca <- tabela_podsumowujaca[order(tabela_podsumowujaca$Pozycja_w_Rankingu), ]
  rownames(tabela_podsumowujaca) <- NULL
  
  wynik_ewaluacji <- list(
    ranking = tabela_podsumowujaca,
    detale_rozmyte = list(S = uzytecznosc_grupowa_S, R = indywidualny_zal_R, Q = indeks_kompromisu_Q),
    wykorzystana_metoda = "Fuzzy VIKOR (Wybór Mobilności)",
    parametry_modelu = list(waga_v = wspolczynnik_kompromisu_v)
  )
  
  class(wynik_ewaluacji) <- "vikor_erasmus_wynik"
  return(wynik_ewaluacji)
}


#' Rozmyta Metoda WASPAS dla Wyboru Uczelni
#'
#' @description Implementacja kompromisowej metody Fuzzy WASPAS w kontekście mobilności studenckiej.
#' Łączy dwa skrajne podejścia matematyczne: addytywne (Suma Ważona - WSM) 
#' oraz multiplikatywne (Iloczyn Ważony - WPM), co znacznie zwiększa dokładność 
#' i stabilność ostatecznego rankingu.
#'
#' @param rozmyta_macierz_decyzyjna Macierz m x 3n wygenerowana ze wskaźników uczelni.
#' @param kierunki_kryteriow Wektor znakowy ("max" dla kryteriów pozytywnych, "min" dla negatywnych).
#' @param wspolczynnik_lambda Parametr równowagi między WSM a WPM (domyślnie 0.5 oznacza równy wpływ).
#' @param wagi_odgorne (Opcjonalnie) Gotowy wektor wag dla kryteriów.
#' @param nazwy_kryteriow_bwm (Opcjonalnie) Nazwy kryteriów do modelu BWM.
#' @param bwm_najlepsze (Opcjonalnie) Wektor Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor Others-to-Worst.
#' @return Obiekt klasy `waspas_erasmus_wynik` z rankingiem uczelni.
#' @export
wyznacz_ranking_waspas <- function(
    rozmyta_macierz_decyzyjna, 
    kierunki_kryteriow, 
    wspolczynnik_lambda = 0.5, 
    wagi_odgorne = NULL,
    nazwy_kryteriow_bwm = NULL, 
    bwm_najlepsze = NULL, 
    bwm_najgorsze = NULL) 
{
  
  # 1. Integracja z naszym wewnętrznym silnikiem wag
  wagi_finalne <- .ustal_wagi_kryteriow_mobilnosci(
    rozmyta_macierz_decyzyjna = rozmyta_macierz_decyzyjna, 
    wagi_odgorne = wagi_odgorne, 
    nazwy_kryteriow_bwm = nazwy_kryteriow_bwm, 
    ankieta_najwazniejszy_vs_reszta = bwm_najlepsze, 
    ankieta_reszta_vs_najmniej_wazny = bwm_najgorsze
  )
  
  liczba_kolumn <- ncol(rozmyta_macierz_decyzyjna)
  
  # Rozszerzanie typów zysk/koszt na pełne trójki rozmyte
  kierunki_rozmyte <- character(liczba_kolumn)
  indeks_kryterium <- 1
  for (j in seq(1, liczba_kolumn, 3)) {
    kierunki_rozmyte[j:(j+2)] <- kierunki_kryteriow[indeks_kryterium]
    indeks_kryterium <- indeks_kryterium + 1
  }
  
  # 2. Normalizacja liniowa (charakterystyczna dla WASPAS)
  baza_normalizacji <- ifelse(kierunki_rozmyte == "max", apply(rozmyta_macierz_decyzyjna, 2, max), apply(rozmyta_macierz_decyzyjna, 2, min))
  znormalizowana_macierz_uczelni <- matrix(0, nrow(rozmyta_macierz_decyzyjna), liczba_kolumn)
  
  for (j in seq(1, liczba_kolumn, 3)) {
    if (kierunki_rozmyte[j] == "max") {
      znormalizowana_macierz_uczelni[, j]   <- rozmyta_macierz_decyzyjna[, j]   / baza_normalizacji[j+2]
      znormalizowana_macierz_uczelni[, j+1] <- rozmyta_macierz_decyzyjna[, j+1] / baza_normalizacji[j+2]
      znormalizowana_macierz_uczelni[, j+2] <- rozmyta_macierz_decyzyjna[, j+2] / baza_normalizacji[j+2]
    } else {
      znormalizowana_macierz_uczelni[, j]   <- baza_normalizacji[j] / rozmyta_macierz_decyzyjna[, j+2]
      znormalizowana_macierz_uczelni[, j+1] <- baza_normalizacji[j] / rozmyta_macierz_decyzyjna[, j+1]
      znormalizowana_macierz_uczelni[, j+2] <- baza_normalizacji[j] / rozmyta_macierz_decyzyjna[, j]
    }
  }
  
  # 3. Model WSM (Suma ważona ocen)
  macierz_przekatniowa_wag <- diag(wagi_finalne)
  macierz_dodawania <- znormalizowana_macierz_uczelni %*% macierz_przekatniowa_wag
  
  WSM_rozmyte <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  WSM_rozmyte[,1] <- apply(macierz_dodawania[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, sum)
  WSM_rozmyte[,2] <- apply(macierz_dodawania[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, sum)
  WSM_rozmyte[,3] <- apply(macierz_dodawania[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, sum)
  
  # 4. Model WPM (Iloczyn ważony ocen -> potęgowanie do wagi)
  macierz_mnozenia <- matrix(0, nrow(rozmyta_macierz_decyzyjna), liczba_kolumn)
  for (j in seq(1, liczba_kolumn, 3)) {
    # Podnoszenie liczby rozmytej do potęgi wagi 
    macierz_mnozenia[, j]   <- znormalizowana_macierz_uczelni[, j]   ^ wagi_finalne[j+2]
    macierz_mnozenia[, j+1] <- znormalizowana_macierz_uczelni[, j+1] ^ wagi_finalne[j+1]
    macierz_mnozenia[, j+2] <- znormalizowana_macierz_uczelni[, j+2] ^ wagi_finalne[j]
  }
  
  WPM_rozmyte <- matrix(0, nrow(rozmyta_macierz_decyzyjna), 3)
  WPM_rozmyte[,1] <- apply(macierz_mnozenia[, seq(1, liczba_kolumn, 3), drop=FALSE], 1, prod)
  WPM_rozmyte[,2] <- apply(macierz_mnozenia[, seq(2, liczba_kolumn, 3), drop=FALSE], 1, prod)
  WPM_rozmyte[,3] <- apply(macierz_mnozenia[, seq(3, liczba_kolumn, 3), drop=FALSE], 1, prod)
  
  # 5. Łączny wskaźnik Q i defuzzyfikacja do formy ostrej
  ostre_wsm <- rowSums(WSM_rozmyte) / 3
  ostre_wpm <- rowSums(WPM_rozmyte) / 3
  
  # Wzór kompromisowy WASPAS
  ostateczna_ocena_waspas <- wspolczynnik_lambda * ostre_wsm + (1 - wspolczynnik_lambda) * ostre_wpm
  
  # Przygotowanie ostatecznej tabeli
  nazwy_ocenianych_uczelni <- rownames(rozmyta_macierz_decyzyjna)
  
  tabela_podsumowujaca <- data.frame(
    Uczelnia_Partnerska = nazwy_ocenianych_uczelni,
    Wynik_WSM = ostre_wsm,
    Wynik_WPM = ostre_wpm,
    Wskaznik_Q_WASPAS = ostateczna_ocena_waspas,
    # Im wyższy wskaźnik WASPAS, tym lepsza uczelnia
    Pozycja_w_Rankingu = rank(-ostateczna_ocena_waspas, ties.method = "first")
  )
  
  # Posortowanie od najlepszej do najgorszej uczelni
  tabela_podsumowujaca <- tabela_podsumowujaca[order(tabela_podsumowujaca$Pozycja_w_Rankingu), ]
  rownames(tabela_podsumowujaca) <- NULL
  
  wynik_ewaluacji <- list(
    ranking = tabela_podsumowujaca,
    wykorzystana_metoda = "Fuzzy WASPAS (Wybór Mobilności)",
    parametry_modelu = list(waga_lambda = wspolczynnik_lambda)
  )
  
  class(wynik_ewaluacji) <- "waspas_erasmus_wynik"
  return(wynik_ewaluacji)
}

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
