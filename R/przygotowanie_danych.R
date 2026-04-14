#' @title Wewnętrzny parser modelu badawczego
#' @description Funkcja pomocnicza dekodująca definicje kryteriów ustalone przez badacza.
#' Przekształca zapis "Kryterium =~ pytanie1 + pytanie2" w strukturę listową.
#' @keywords internal
.analizuj_skladnia_mcda <- function(skladnia) {
  # usuniecie znaków nowej linii
  skladnia_czysta <- gsub("\n", "", skladnia)
  # podzielenie na linie po sredniku
  linie <- strsplit(skladnia_czysta, ";")[[1]]
  struktura_kryteriow <- list()
  
  for (linia in linie) {
    if (trimws(linia) == "") 
      next # ominięcie pustych linii
    # dzielenie wg operatora "=~"
    czesci <- strsplit(linia, "=~")[[1]]
    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      # dzielenie zmiennych skladowych wg "+"
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      struktura_kryteriow[[nazwa_kryterium]] <- elementy
    }
  }
  return(struktura_kryteriow)
}

#' @title Wewnętrzny Skaler Saaty'ego - funkcja wewnętrzna 2
#' @description Przekształca dowolną skalę (np. Likert 1-5, wartości ciągłe)
#' na skalę Saaty'ego 1-9.
#' @keywords internal
.skaluj_do_saaty <- function(odpowiedzi_studentow) {
  # zabezpieczenie przed ujemnymi (chyba ze to specyfika danych, tu zakladamy blad)
  if (any(odpowiedzi_studentow < 0, na.rm = TRUE)) 
    stop("Wykryto wartości ujemne w danych wejściowych.")
  
  # obsluga kodow bledow (np. 99) i brakow danych (NA) -> zamiana na 0
  odpowiedzi_studentow[is.na(odpowiedzi_studentow) | odpowiedzi_studentow == 99] <- 0
  
  # Utworzenie maski logicznej chroniącej puste odpowiedzi przed sztucznym skalowaniem
  odpowiedzi_wazne <- odpowiedzi_studentow > 0
  wartosci_do_przeliczenia <- odpowiedzi_studentow[odpowiedzi_wazne]
  
  # Bezpiecznik: jeśli nikt nie odpowiedział na to pytanie, zwróć zera
  if (length(wartosci_do_przeliczenia) == 0) 
    return(odpowiedzi_studentow)
  
  min_odp <- min(wartosci_do_przeliczenia)
  max_odp <- max(wartosci_do_przeliczenia)
  
  # Matematyczne skalowanie liniowe do przedziału [1, 9]
  if (min_odp == max_odp) {
    odpowiedzi_studentow[odpowiedzi_wazne] <- 1
  } else {
    # Zastosowanie wzoru transformacyjnego
    odpowiedzi_studentow[odpowiedzi_wazne] <- 1 + (wartosci_do_przeliczenia - min_odp) * (8 / (max_odp - min_odp))
  }
  return(odpowiedzi_studentow)
}

#' @title Wewnętrzny Fuzzifier wyników uczelni
#' @description Wprowadza niepewność ocen studenckich poprzez konwersję tzw. oceny ostrej (Crisp) 
#' na Trójkątną Liczbę Rozmytą (TFN - Triangular Fuzzy Number).
#' @keywords internal
.transformuj_na_tfn <- function(srednie_ostre) {
  # Dolna granica niepewności (ograniczona od dołu przez 1)
  l <- pmax(1, srednie_ostre - 1)
  # Wartość najbardziej prawdopodobna (właściwa średnia)
  m <- srednie_ostre
  # Górna granica niepewności (ograniczona od góry przez 9)
  u <- pmin(9, srednie_ostre + 1)
  
  # Ochrona braków danych przed transformacją
  brak_danych <- (srednie_ostre == 0)
  l[brak_danych] <- 0; m[brak_danych] <- 0; u[brak_danych] <- 0
  
  return(cbind(l, m, u))
}

#' funkcja publiczna
#' Przygotowanie Danych do Rozmytej Analizy MCDA
#'
#' @description Funkcja przekształca surowe dane ankietowe w rozmytą macierz decyzyjną.
#' Oblicza wyniki zmiennych kompozytowych na podstawie składni, skaluje je do przedziału 1-9,
#' agreguje odpowiedzi ekspertów (jeśli dotyczy) i dokonuje rozmycia (fuzzification).
#'
#' @param dane Ramka danych (data frame) zawierająca surowe zmienne.
#' @param skladnia Ciąg znaków definiujący kryteria (np. "Koszt =~ k1 + k2").
#' @param kolumna_uczelni Nazwa kolumny identyfikującej alternatywy.
#'        Jeśli NULL, każdy wiersz traktowany jest jako osobna alternatywa.
#' @param agregacja_studenci Funkcja używana do scalania opinii studentów (domyślnie: mean).
#' @return Macierz o wymiarach ($m \times 3n$), gdzie m to liczba uczelni.
#' @export
zbuduj_macierz_rozmyta <- function(
    dane, 
    skladnia, 
    kolumna_uczelni = NULL, 
    agregacja_studenci = mean) 
  {
  
  if (!is.data.frame(dane)) 
    stop("Argument 'dane' musi być ramką danych (data frame).")
  
  # 1. Parsowanie składni
  struktura_kryteriow <- .analizuj_skladnia_mcda(skladnia)
  nazwy_kryteriow <- names(struktura_kryteriow)
  
  # 2. Obliczanie zmiennych kompozytowych i skalowanie (dla każdego wiersza/studenta)
  tymczasowe_wyniki <- data.frame(row_id = 1:nrow(dane))
  
  for (kryt in nazwy_kryteriow) {
    zmienne <- struktura_kryteriow[[kryt]]
    # Sprawdzenie czy zmienne istnieja w danych
    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) 
      stop(paste("Brakuje zmiennych w danych:", paste(brakujace, collapse=", ")))
    
    # Obliczanie sredniej dla kryterium (Composite Score)
    if (length(zmienne) > 1) {
      surowy_wynik <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy_wynik <- dane[[zmienne]]
    }
    
    # Skalowanie do 1-9
    tymczasowe_wyniki[[kryt]] <- .skaluj_do_saaty(surowy_wynik)
  }
  
  # 3. Agregacja (Studenci -> Uczelnie)
  if (!is.null(kolumna_uczelni)) {
    if (!kolumna_uczelni %in% names(dane)) 
      stop("Nie znaleziono kolumny alternatyw (uczelni) w danych.")
    
    tymczasowe_wyniki$ID_Uczelnie <- dane[[kolumna_uczelni]]
    
    # Agregacja wg ID Alternatywy (np. srednia z ocen 5 studentow dla danej uczelni)
    dane_zagregowane <- aggregate(
      . ~ ID_Uczelnie, 
      data = tymczasowe_wyniki[, -1], 
      FUN = agregacja_studenci
      )
    
    # Sortowanie i czyszczenie
    dane_zagregowane <- dane_zagregowane[order(dane_zagregowane$ID_Uczelnie), ]
    nazwy_wierszy <- dane_zagregowane$ID_Uczelnie
    macierz_wynikow <- as.matrix(dane_zagregowane[, nazwy_kryteriow])
    
  } else {
    # Brak agregacji (1 wiersz = 1 uczelnia)
    macierz_wynikow <- as.matrix(tymczasowe_wyniki[, nazwy_kryteriow])
    nazwy_wierszy <- 1:nrow(macierz_wynikow)
  }
  
  # 4. Rozmywanie (Crisp -> Fuzzy Triangular)
  lista_decyzyjna <- list()
  for (i in seq_along(nazwy_kryteriow)) {
    kryt <- nazwy_kryteriow[i]
    lista_decyzyjna[[kryt]] <- .transformuj_na_tfn(macierz_wynikow[, i])
  }
  
  macierz_rozmyta <- do.call(cbind, lista_decyzyjna)
  rownames(macierz_rozmyta) <- nazwy_wierszy
  # Zapisujemy metadane (nazwy kryteriow) jako atrybut macierzy
  attr(macierz_rozmyta, "nazwy_kryteriow") <- nazwy_kryteriow
  
  return(macierz_rozmyta)
}