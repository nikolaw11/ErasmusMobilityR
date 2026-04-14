# data-raw/generowanie_danych.R

# ziarno losowosci - aby wyniki byly powtarzalne
set.seed(123)

# ramka danych symulujaca problem decyzyjny
# 15 ankiet (wierszy), co da nam 5 docelowych alternatyw (uczelni)
# 4 glowne kryteria, ktore skladaja sie z podkryteriow (zmiennych)

mcda_dane_surowe <- data.frame(
  # --- Identyfikatory ---
  Student_ID = 1:15, # Symulacja 5 ekspertow oceniajacych po 3 warianty
  Uczelnia = rep(paste0("Uczelnia_", LETTERS[1:5]), each = 3),
  
  # --- Kryterium 1: Finanse (Dane ciagle, liczbowe) ---
  # kwoty podajemy w euro
  wysokosc_stypendium = runif(15, 400, 800),
  koszty_zycia_mies   = runif(15, 600, 1200),
  
  # --- Kryterium 2: Jakosc (Dane mieszane - Ranking, Skala Likerta 1-5, Binarne) ---
  ranking_uczelni     = sample(1:1200, 15, replace = TRUE),
  # wartosc 99 jako blad/brak danych, zeby przetestowac czyszczenie
  kompatybilnosc_prog = sample(c(1:5, 99), 15, replace = TRUE, prob = c(rep(0.18, 5), 0.1)),
  ocena_biura_erasmus = sample(1:5, 15, replace = TRUE),
  jakosc_wykladowcow  = sample(1:5, 15, replace = TRUE),
  dostepnosc_akademik = sample(0:1, 15, replace = TRUE) * 10, # 0 lub 10 pkt
  
  # --- Kryterium 3: Zadowolenie (Skala Likerta 1-10) ---
  satysfakcja_program = sample(1:10, 15, replace = TRUE),      
  spolecznosc_int     = sample(1:10, 15, replace = TRUE),
  
  # --- Kryterium 4: Miasto (Dane mieszane - Dane Liczbowe, Skala Likerta 1-7) ---
  odleglosc_od_macierzystej = runif(15, 100, 5000),
  bezpieczenstwo_miasta     = sample(1:7, 15, replace = TRUE),
  # Wprowadzamy NA (braki danych) dla testow
  atrakcyjnosc_miasta       = sample(c(1:7, NA), 15, replace = TRUE)
)

# Im nizsza wartosc, tym lepiej
mcda_dane_surowe$koszty_zycia_mies <- max(mcda_dane_surowe$koszty_zycia_mies) - mcda_dane_surowe$koszty_zycia_mies
mcda_dane_surowe$ranking_uczelni <- max(mcda_dane_surowe$ranking_uczelni) - mcda_dane_surowe$ranking_uczelni
mcda_dane_surowe$odleglosc_od_macierzystej <- max(mcda_dane_surowe$odleglosc_od_macierzystej) - mcda_dane_surowe$odleglosc_od_macierzystej

# Zapisanie danych do folderu pakietu /data
# Funkcja use_data automatycznie kompresuje dane do formatu .rda
usethis::use_data(mcda_dane_surowe, overwrite = TRUE)