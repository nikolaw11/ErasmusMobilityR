#' @title Wewnętrzne asercje logiki BWM
#' @description Sprawdza, czy preferencje studentów (wagi kryteriow) mają sens matematyczny.
#' @keywords internal
.wymus_logike_bwm <- function(wyrazenie, komunikat) {
  if (!all(wyrazenie)) {
    stop(if (is.null(komunikat)) "Błąd w priorytetach ankiety." else komunikat)
  }
}

#' @title Wewnętrzna walidacja ankiety BWM
#' @description Sprawdza, czy wektory porównań od ankietowanych spełniają zasady Best-Worst.
#' @keywords internal
.waliduj_priorytety_studenta <- function(najwazniejszy_vs_reszta, reszta_vs_najmniej_wazny, nazwy_kryteriow) {
  .wymus_logike_bwm(length(najwazniejszy_vs_reszta) > 1, "Musisz ocenić więcej niż jedno kryterium wyjazdu.")
  .wymus_logike_bwm(length(najwazniejszy_vs_reszta) == length(reszta_vs_najmniej_wazny), "Niezgodność długości wektorów ocen ankiety.")
  .wymus_logike_bwm(length(najwazniejszy_vs_reszta) == length(nazwy_kryteriow), "Liczba kryteriów nie zgadza się z podanymi ocenami.")
  .wymus_logike_bwm(1 %in% najwazniejszy_vs_reszta, "Wektor 'najwazniejszy_vs_reszta' musi zawierać wartość 1 (dla najważniejszego kryterium).")
  .wymus_logike_bwm(1 %in% reszta_vs_najmniej_wazny, "Wektor 'reszta_vs_najmniej_wazny' musi zawierać wartość 1 (dla najmniej ważnego kryterium).")
  .wymus_logike_bwm(all(najwazniejszy_vs_reszta >= 1 & najwazniejszy_vs_reszta <= 9), "Oceny przewagi kryteriów muszą być z przedziału 1-9 (skala Saaty'ego).")
  
  list(best_to_others = najwazniejszy_vs_reszta, others_to_worst = reszta_vs_najmniej_wazny, criteria_names = nazwy_kryteriow)
}

#' @title Wewnętrzna ocena spójności wywiadów
#' @keywords internal
.ocen_spojnosc_odpowiedzi <- function(model_badawczy) {
  indeks_najmniej_waznego <- match(1, model_badawczy$others_to_worst)
  najwazniejszy_nad_najgorszym <- model_badawczy$best_to_others[indeks_najmniej_waznego]
  
  # Sprawdzenie idealnej spojnosci logicznej studenta: a_bj * a_jw = a_bw
  list(
    jest_spojny = all(model_badawczy$best_to_others * model_badawczy$others_to_worst == najwazniejszy_nad_najgorszym),
    a_bw = najwazniejszy_nad_najgorszym
  )
}

#' @title Pomocnik budowania ograniczeń dla solvera LP
#' @keywords internal
.buduj_rownanie_lp <- function(macierz_ograniczen, nowe_rownanie) {
  idx <- length(macierz_ograniczen) + 1
  macierz_ograniczen[[idx]] <- nowe_rownanie
  list(macierz_ograniczen = macierz_ograniczen, dodano = TRUE)
}

#' Wyznaczanie Wag Kryteriów Metodą BWM (Erasmus+)
#'
#' @description Oblicza optymalne wagi ważności dla głównych kryteriów oceny uczelni (np. Finanse, Jakość)
#' metodą Best-Worst (BWM) przy użyciu programowania liniowego. Minimalizuje błąd niespójności decydenta (ksi).
#'
#' @param nazwy_kryteriow Wektor znakowy z nazwami kryteriów (np. c("Finanse", "Jakosc")).
#' @param najwazniejszy_vs_reszta Wektor numeryczny (1-9). Przewaga NAJWAŻNIEJSZEGO kryterium nad pozostałymi.
#' @param reszta_vs_najmniej_wazny Wektor numeryczny (1-9). Przewaga pozostałych kryteriów nad NAJMNIEJ WAŻNYM.
#' @return Lista zawierająca: `wagi_kryteriow`, `wskaznik_spojnosci` (CR) oraz wartość błędu `ksi`.
#' @import Rglpk
#' @export
wyznacz_wagi_kryteriow_bwm <- function(
    nazwy_kryteriow, 
    najwazniejszy_vs_reszta, 
    reszta_vs_najmniej_wazny) 
  {
  
  # 1. Walidacja ankiety i budowa modelu badawczego
  dane_ankietowe <- .waliduj_priorytety_studenta(najwazniejszy_vs_reszta, reszta_vs_najmniej_wazny, nazwy_kryteriow)
  logika_studenta <- .ocen_spojnosc_odpowiedzi(dane_ankietowe)
  
  liczba_zmiennych_lp <- length(najwazniejszy_vs_reszta) + 1 # Liczba wag kryteriow + 1 zmienna bledu (ksi)
  indeks_bledu_ksi <- liczba_zmiennych_lp
  
  # Budowanie macierzy ograniczen dla algorytmu Rglpk
  
  # Ograniczenie 1: Suma wag kryteriow musi wynosić równe 1 (100%)
  lewa_strona_suma <- c(rep(1, liczba_zmiennych_lp - 1), 0) # 0 przy ksi, bo ksi nie jest wagą
  lista_rownan <- list(
    list(lhs = lewa_strona_suma, dir = "==", rhs = 1)
  )
  
  # Ograniczenia wynikające z porównań do NAJWAŻNIEJSZEGO kryterium
  indeks_najlepszego <- match(1, najwazniejszy_vs_reszta)
  
  for (j in seq_along(najwazniejszy_vs_reszta)) {
    if (j != indeks_najlepszego) {
      # Równanie liniowe A (górna granica błędu)
      lhs1 <- rep(0, liczba_zmiennych_lp)
      lhs1[indeks_najlepszego] <- 1
      lhs1[j] <- -najwazniejszy_vs_reszta[j]
      lhs1[indeks_bledu_ksi] <- -1 
      lista_rownan <- .buduj_rownanie_lp(lista_rownan, list(lhs = lhs1, dir = "<=", rhs = 0))$macierz_ograniczen
      
      # Równanie liniowe B (dolna granica błędu)
      lhs2 <- lhs1 * -1
      lhs2[indeks_bledu_ksi] <- -1 
      lista_rownan <- .buduj_rownanie_lp(lista_rownan, list(lhs = lhs2, dir = "<=", rhs = 0))$macierz_ograniczen
    }
  }
  
  # Ograniczenia wynikające z porównań do NAJMNIEJ WAŻNEGO kryterium
  indeks_najgorszego <- match(1, reszta_vs_najmniej_wazny)
  
  for (j in seq_along(reszta_vs_najmniej_wazny)) {
    if (j != indeks_najgorszego) {
      # Równanie liniowe A
      lhs1 <- rep(0, liczba_zmiennych_lp)
      lhs1[j] <- 1
      lhs1[indeks_najgorszego] <- -reszta_vs_najmniej_wazny[j]
      lhs1[indeks_bledu_ksi] <- -1
      lista_rownan <- .buduj_rownanie_lp(lista_rownan, list(lhs = lhs1, dir = "<=", rhs = 0))$macierz_ograniczen
      
      # Równanie liniowe B
      lhs2 <- lhs1 * -1
      lhs2[indeks_bledu_ksi] <- -1
      lista_rownan <- .buduj_rownanie_lp(lista_rownan, list(lhs = lhs2, dir = "<=", rhs = 0))$macierz_ograniczen
    }
  }
  
  # 2. Konfiguracja i uruchomienie Solvera LP
  macierz_lewej_strony <- t(sapply(lista_rownan, function(x) x$lhs))
  kierunki_nierownosci <- sapply(lista_rownan, function(x) x$dir)
  wektor_prawej_strony <- unlist(sapply(lista_rownan, function(x) x$rhs))
  
  # Funkcja celu: Chcemy zminimalizować wyłącznie wartość błędu (ksi)
  funkcja_celu <- rep(0, liczba_zmiennych_lp)
  funkcja_celu[indeks_bledu_ksi] <- 1
  
  wynik_optymalizacji <- Rglpk::Rglpk_solve_LP(funkcja_celu, macierz_lewej_strony, kierunki_nierownosci, wektor_prawej_strony, max = FALSE)
  
  # 3. Przetwarzanie i opis wyników analitycznych
  ostateczne_wagi <- wynik_optymalizacji$solution[1:(liczba_zmiennych_lp - 1)]
  uzyskany_blad_ksi <- wynik_optymalizacji$solution[liczba_zmiennych_lp]
  
  # Tabela Indeksu Spójności (Consistency Index) wg Rezaei (2015) dla skali 1-9
  tabela_rezaei_ci <- c(0, 0.44, 1.0, 1.63, 2.30, 3.00, 3.73, 4.47, 5.23)
  
  # Wyciągnięcie relacji między Najważniejszym a Najmniej Ważnym do oceny spójności
  odchylenie_bw <- as.integer(logika_studenta$a_bw)
  odchylenie_bw <- ifelse(odchylenie_bw > 9, 9, odchylenie_bw) # Zabezpieczenie skali
  
  # Obliczenie wskaźnika spójności studenta (CR - Consistency Ratio)
  wspolczynnik_cr <- uzyskany_blad_ksi / tabela_rezaei_ci[odchylenie_bw]
  if (odchylenie_bw == 1) wspolczynnik_cr <- 0
  
  list(
    badane_filary = nazwy_kryteriow,
    obliczone_wagi = ostateczne_wagi,
    wskaznik_spojnosci_cr = wspolczynnik_cr,
    blad_optymalizacji_ksi = uzyskany_blad_ksi
  )
}