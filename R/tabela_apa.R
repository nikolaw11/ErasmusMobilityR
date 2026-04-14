#' @title Generowanie Tabeli APA
#' @description
#' Funkcja przekształca wyniki analizy MCDA (TOPSIS, VIKOR, WASPAS, Meta-Ranking)
#' w sformatowaną tabelę zgodną ze standardem APA, gotową do publikacji w Wordzie.
#'
#' @param x Obiekt wynikowy z funkcji pakietu (np. `topsis_erasmus_wynik`).
#' @param tytul Opcjonalny tytuł tabeli.
#' @return Obiekt klasy `flextable` gotowy do druku lub zapisu do Worda.
#' @importFrom rempsyc nice_table
#' @export
tabela_apa <- function(x, tytul = NULL) {
  UseMethod("tabela_apa")
}

#' @export
tabela_apa.topsis_erasmus_wynik <- function(x, tytul = "Wyniki metody Fuzzy TOPSIS") {
  df <- x$ranking
  
  # Formatowanie nazw kolumn dla czytelnika
  names(df) <- c("Uczelnia Partnerska", "D+ (Odległość od Ideału)", "D- (Odległość od Antyideału)", "Współczynnik (CC)", "Pozycja w Rankingu")
  
  # Zaokrąglenia dla elegancji
  df$`D+ (Odległość od Ideału)` <- round(df$`D+ (Odległość od Ideału)`, 3)
  df$`D- (Odległość od Antyideału)`   <- round(df$`D- (Odległość od Antyideału)`, 3)
  df$`Współczynnik (CC)`      <- round(df$`Współczynnik (CC)`, 4)
  
  # Tworzenie tabeli APA
  rempsyc::nice_table(
    df, 
    title = c("Tabela 1", tytul),
    note = c("Uwaga. CC - Coefficient of Closeness (Współczynnik Bliskości). Im wyższa wartość, tym lepsza alternatywa do wyjazdu na Erasmusa.")
  )
}

#' @export
tabela_apa.vikor_erasmus_wynik <- function(x, tytul = "Wyniki metody Fuzzy VIKOR") {
  df <- x$ranking
  
  names(df) <- c("Uczelnia Partnerska", "Wskaźnik S (Użyteczność)", "Wskaźnik R (Żal)", "Indeks Q (Kompromis)", "Pozycja w Rankingu")
  
  df$`Wskaźnik S (Użyteczność)` <- round(df$`Wskaźnik S (Użyteczność)`, 3)
  df$`Wskaźnik R (Żal)`       <- round(df$`Wskaźnik R (Żal)`, 3)
  df$`Indeks Q (Kompromis)` <- round(df$`Indeks Q (Kompromis)`, 4)
  
  rempsyc::nice_table(
    df, 
    title = c("Tabela 2", tytul),
    note = c("Uwaga. S: maksymalizacja użyteczności grupowej, R: minimalizacja indywidualnego żalu, Q: ostateczny indeks kompromisu (im mniej, tym lepiej).")
  )
}

#' @export
tabela_apa.waspas_erasmus_wynik <- function(x, tytul = "Wyniki metody Fuzzy WASPAS") {
  df <- x$ranking
  
  names(df) <- c("Uczelnia Partnerska", "WSM (Model Sumaryczny)", "WPM (Model Iloczynowy)", "Wskaźnik Q (WASPAS)", "Pozycja w Rankingu")
  
  df$`WSM (Model Sumaryczny)`   <- round(df$`WSM (Model Sumaryczny)`, 3)
  df$`WPM (Model Iloczynowy)` <- round(df$`WPM (Model Iloczynowy)`, 3)
  df$`Wskaźnik Q (WASPAS)`   <- round(df$`Wskaźnik Q (WASPAS)`, 4)
  
  rempsyc::nice_table(
    df, 
    title = c("Tabela 3", tytul),
    note = c("Uwaga. Model WASPAS łączy podejście sumowane (WSM) oraz multiplikatywne (WPM) w jeden wskaźnik użyteczności.")
  )
}

#' @export
tabela_apa.list <- function(x, tytul = "Ostateczny Meta-Ranking Uczelni (Konsensus)") {
  # Zabezpieczenie: sprawdzamy czy to na pewno lista z naszego Meta-Rankingu
  if(is.null(x$porownanie)) stop("To nie jest prawidłowy obiekt meta-rankingu z pakietu ErasmusMobilityR.")
  
  df <- x$porownanie
  
  # Automatyczne czyszczenie podkreślników z nazw kolumn na spacje
  names(df) <- gsub("_", " ", names(df))
  # Dodatkowe upiększenie nazw
  names(df)[names(df) == "Meta Konsensus RA"] <- "Algorytm Genetyczny (RA)"
  names(df)[names(df) == "Meta Dominacja"] <- "Reguła Dominacji"
  names(df)[names(df) == "Meta Srednia Pozycja"] <- "Suma Pozycji (Borda)"
  
  rempsyc::nice_table(
    df, 
    title = c("Tabela 4", tytul),
    note = c("Uwaga. Zestawienie rang uzyskanych z trzech niezależnych algorytmów (TOPSIS, VIKOR, WASPAS) oraz ostateczne wyznaczenie lidera za pomocą algorytmu konsensusu.")
  )
}