# data-raw/generowanie_danych.R

# Formalny DGP dla przykladowego zbioru danych pakietu.
# Skrypt celowo zachowuje 5 uczelni x 3 opinie studentow, aby przyklady
# i testy pakietu mialy ten sam ksztalt co dotychczas.

# --- PARAMETRY DGP ---------------------------------------------------------

set.seed(123)

liczba_uczelni <- 5
liczba_ocen_na_uczelnie <- 3
nazwy_uczelni <- paste0("Uczelnia_", LETTERS[seq_len(liczba_uczelni)])
liczba_obserwacji <- liczba_uczelni * liczba_ocen_na_uczelnie

# --- DANE FAKTUALNE DO UZUPELNIENIA --------------------------------------

# Ponizsze wektory mozna zastapic prawdziwymi danymi dla wybranych uczelni.
# Kolejnosc wartosci musi odpowiadac kolejnosci w obiekcie `nazwy_uczelni`.
#
# Przyklady zmiennych faktualnych:
# - ranking_uczelni_surowy: miejsce w rankingu, nizsze jest lepsze,
# - koszty_zycia_mies_eur: sredni miesieczny koszt utrzymania, nizszy jest lepszy,
# - odleglosc_od_macierzystej_km: odleglosc od uczelni macierzystej, nizsza jest lepsza,
# - stypendium_bazowe_eur: bazowa wysokosc stypendium, wyzsza jest lepsza.
ranking_uczelni_surowy <- c(420, 760, 210, 530, 980)
koszty_zycia_mies_eur <- c(820, 690, 1050, 760, 910)
odleglosc_od_macierzystej_km <- c(540, 1260, 980, 2100, 430)
stypendium_bazowe_eur <- c(520, 560, 640, 600, 500)

# --- FUNKCJE POMOCNICZE ---------------------------------------------------

sprawdz_dlugosc <- function(x, nazwa) {
  if (length(x) != liczba_uczelni) {
    stop(
      sprintf(
        "Wektor `%s` musi miec dlugosc %s.",
        nazwa,
        liczba_uczelni
      ),
      call. = FALSE
    )
  }
}

skaluj_do_korzysci <- function(x, nizsza_lepsza = FALSE) {
  if (any(!is.finite(x))) {
    stop("Dane faktualne musza byc skonczonymi liczbami.", call. = FALSE)
  }

  min_x <- min(x)
  max_x <- max(x)

  if (min_x == max_x) {
    return(rep(5, length(x)))
  }

  if (nizsza_lepsza) {
    return(1 + (max_x - x) * 8 / (max_x - min_x))
  }

  1 + (x - min_x) * 8 / (max_x - min_x)
}

rhalf_normal <- function(n, sd = 1) {
  abs(stats::rnorm(n, mean = 0, sd = sd))
}

rhalf_student <- function(n, df = 4, scale = 1) {
  abs(stats::rt(n, df = df)) * scale
}

ogranicz <- function(x, dol, gora) {
  pmin(pmax(x, dol), gora)
}

likert_beta <- function(latent, min_wynik, max_wynik, koncentracja = 20) {
  latent <- ogranicz(latent, 0.02, 0.98)
  surowe <- stats::rbeta(
    n = length(latent),
    shape1 = latent * koncentracja,
    shape2 = (1 - latent) * koncentracja
  )

  as.integer(round(min_wynik + surowe * (max_wynik - min_wynik)))
}

sprawdz_zakres <- function(x, nazwa, dol, gora, dodatkowo_dozwolone = NULL, na_ok = FALSE) {
  poprawne <- rep(FALSE, length(x))
  if (na_ok) {
    poprawne <- poprawne | is.na(x)
  }
  if (!is.null(dodatkowo_dozwolone)) {
    poprawne <- poprawne | x %in% dodatkowo_dozwolone
  }
  poprawne <- poprawne | (!is.na(x) & x >= dol & x <= gora)

  if (any(!poprawne)) {
    stop(sprintf("Kolumna `%s` ma wartosci poza oczekiwanym zakresem.", nazwa), call. = FALSE)
  }
}

# --- WALIDACJA DANYCH FAKTUALNYCH ----------------------------------------

sprawdz_dlugosc(ranking_uczelni_surowy, "ranking_uczelni_surowy")
sprawdz_dlugosc(koszty_zycia_mies_eur, "koszty_zycia_mies_eur")
sprawdz_dlugosc(odleglosc_od_macierzystej_km, "odleglosc_od_macierzystej_km")
sprawdz_dlugosc(stypendium_bazowe_eur, "stypendium_bazowe_eur")

if (
  any(ranking_uczelni_surowy <= 0) ||
    any(koszty_zycia_mies_eur <= 0) ||
    any(odleglosc_od_macierzystej_km <= 0) ||
    any(stypendium_bazowe_eur <= 0)
) {
  stop("Dane faktualne musza byc dodatnie.", call. = FALSE)
}

# --- WARSTWA UCZELNI I ZMIENNYCH LATENTNYCH ------------------------------

id_uczelni <- seq_len(liczba_uczelni)
student_uczelnia <- rep(id_uczelni, each = liczba_ocen_na_uczelnie)
uczelnia <- nazwy_uczelni[student_uczelnia]

ranking_korzysc_uczelnia <- skaluj_do_korzysci(
  ranking_uczelni_surowy,
  nizsza_lepsza = TRUE
)
koszt_korzysc_uczelnia <- skaluj_do_korzysci(
  koszty_zycia_mies_eur,
  nizsza_lepsza = TRUE
)
odleglosc_korzysc_uczelnia <- skaluj_do_korzysci(
  odleglosc_od_macierzystej_km,
  nizsza_lepsza = TRUE
)

normalizuj_1_9 <- function(x) (x - 1) / 8

jakosc_latentna_uczelnia <- ogranicz(
  0.70 * normalizuj_1_9(ranking_korzysc_uczelnia) +
    0.30 * stats::rbeta(liczba_uczelni, shape1 = 7, shape2 = 3),
  0.05,
  0.95
)

miasto_latentne_uczelnia <- ogranicz(
  0.40 * normalizuj_1_9(koszt_korzysc_uczelnia) +
    0.25 * normalizuj_1_9(odleglosc_korzysc_uczelnia) +
    0.35 * stats::rbeta(liczba_uczelni, shape1 = 5, shape2 = 3),
  0.05,
  0.95
)

dopasowanie_latentne_uczelnia <- ogranicz(
  0.55 * jakosc_latentna_uczelnia +
    0.45 * stats::rbeta(liczba_uczelni, shape1 = 6, shape2 = 4),
  0.05,
  0.95
)

spolecznosc_latentna_uczelnia <- ogranicz(
  0.35 * jakosc_latentna_uczelnia +
    0.30 * miasto_latentne_uczelnia +
    0.35 * stats::rbeta(liczba_uczelni, shape1 = 5, shape2 = 3),
  0.05,
  0.95
)

# --- WARSTWA ODPOWIEDZI STUDENTOW ----------------------------------------

znak_szumu <- sample(c(-1, 1), liczba_obserwacji, replace = TRUE)
szum_opinii <- znak_szumu * rhalf_normal(liczba_obserwacji, sd = 0.06)
szok_adaptacyjny <- rhalf_student(liczba_obserwacji, df = 4, scale = 0.035)

jakosc_obs <- ogranicz(
  jakosc_latentna_uczelnia[student_uczelnia] + szum_opinii,
  0.02,
  0.98
)
miasto_obs <- ogranicz(
  miasto_latentne_uczelnia[student_uczelnia] + szum_opinii / 2,
  0.02,
  0.98
)
dopasowanie_obs <- ogranicz(
  dopasowanie_latentne_uczelnia[student_uczelnia] + szum_opinii,
  0.02,
  0.98
)
spolecznosc_obs <- ogranicz(
  spolecznosc_latentna_uczelnia[student_uczelnia] + szum_opinii,
  0.02,
  0.98
)

stypendium_mu <- stypendium_bazowe_eur[student_uczelnia]
stypendium_sd <- 20 + 0.08 * stypendium_mu
wysokosc_stypendium <- round(
  stats::rgamma(
    liczba_obserwacji,
    shape = (stypendium_mu / stypendium_sd)^2,
    scale = stypendium_sd^2 / stypendium_mu
  ),
  0
)

koszt_z_szumem <- koszty_zycia_mies_eur[student_uczelnia] +
  rhalf_student(liczba_obserwacji, df = 5, scale = 35)
odleglosc_z_szumem <- odleglosc_od_macierzystej_km[student_uczelnia] +
  rhalf_normal(liczba_obserwacji, sd = 20)

kompatybilnosc_prog <- likert_beta(dopasowanie_obs, 1, 5, koncentracja = 18)
ocena_biura_erasmus <- likert_beta(
  0.65 * dopasowanie_obs + 0.35 * miasto_obs,
  1,
  5,
  koncentracja = 16
)
jakosc_wykladowcow <- likert_beta(jakosc_obs, 1, 5, koncentracja = 22)
dostepnosc_akademik <- stats::rbinom(
  liczba_obserwacji,
  size = 1,
  prob = ogranicz(0.20 + 0.70 * miasto_obs, 0.05, 0.95)
) * 10

satysfakcja_program <- likert_beta(
  ogranicz(
    0.45 * dopasowanie_obs +
      0.35 * jakosc_obs +
      0.20 * miasto_obs -
      szok_adaptacyjny,
    0.02,
    0.98
  ),
  1,
  10,
  koncentracja = 24
)
spolecznosc_int <- likert_beta(spolecznosc_obs, 1, 10, koncentracja = 18)
bezpieczenstwo_miasta <- likert_beta(
  ogranicz(0.30 * miasto_obs + 0.70 * stats::rbeta(liczba_obserwacji, 6, 3), 0.02, 0.98),
  1,
  7,
  koncentracja = 20
)
atrakcyjnosc_miasta <- likert_beta(
  ogranicz(0.65 * miasto_obs + 0.35 * stats::rbeta(liczba_obserwacji, 5, 4), 0.02, 0.98),
  1,
  7,
  koncentracja = 18
)

# Kontrolowane bledy/braki danych do testowania czyszczenia.
kompatybilnosc_prog[c(4, 12)] <- 99
atrakcyjnosc_miasta[c(8, 14)] <- NA_integer_

mcda_dane_surowe <- data.frame(
  Student_ID = seq_len(liczba_obserwacji),
  Uczelnia = uczelnia,
  wysokosc_stypendium = wysokosc_stypendium,
  koszty_zycia_mies = round(skaluj_do_korzysci(koszt_z_szumem, nizsza_lepsza = TRUE), 2),
  ranking_uczelni = round(ranking_korzysc_uczelnia[student_uczelnia], 2),
  kompatybilnosc_prog = kompatybilnosc_prog,
  ocena_biura_erasmus = ocena_biura_erasmus,
  jakosc_wykladowcow = jakosc_wykladowcow,
  dostepnosc_akademik = dostepnosc_akademik,
  satysfakcja_program = satysfakcja_program,
  spolecznosc_int = spolecznosc_int,
  odleglosc_od_macierzystej = round(
    skaluj_do_korzysci(odleglosc_z_szumem, nizsza_lepsza = TRUE),
    2
  ),
  bezpieczenstwo_miasta = bezpieczenstwo_miasta,
  atrakcyjnosc_miasta = atrakcyjnosc_miasta,
  stringsAsFactors = FALSE
)

# --- WALIDACJA ZBIORU WYJSCIOWEGO ----------------------------------------

oczekiwane_kolumny <- c(
  "Student_ID",
  "Uczelnia",
  "wysokosc_stypendium",
  "koszty_zycia_mies",
  "ranking_uczelni",
  "kompatybilnosc_prog",
  "ocena_biura_erasmus",
  "jakosc_wykladowcow",
  "dostepnosc_akademik",
  "satysfakcja_program",
  "spolecznosc_int",
  "odleglosc_od_macierzystej",
  "bezpieczenstwo_miasta",
  "atrakcyjnosc_miasta"
)

if (!identical(names(mcda_dane_surowe), oczekiwane_kolumny)) {
  stop("Zbior danych ma nieoczekiwany zestaw kolumn.", call. = FALSE)
}

if (nrow(mcda_dane_surowe) != liczba_obserwacji) {
  stop("Nieoczekiwana liczba obserwacji.", call. = FALSE)
}

if (length(unique(mcda_dane_surowe$Uczelnia)) != liczba_uczelni) {
  stop("Nieoczekiwana liczba uczelni.", call. = FALSE)
}

numeryczne <- vapply(mcda_dane_surowe, is.numeric, logical(1))
if (any(mcda_dane_surowe[numeryczne] < 0, na.rm = TRUE)) {
  stop("W zbiorze wykryto wartosci ujemne.", call. = FALSE)
}

sprawdz_zakres(mcda_dane_surowe$koszty_zycia_mies, "koszty_zycia_mies", 1, 9)
sprawdz_zakres(mcda_dane_surowe$ranking_uczelni, "ranking_uczelni", 1, 9)
sprawdz_zakres(
  mcda_dane_surowe$odleglosc_od_macierzystej,
  "odleglosc_od_macierzystej",
  1,
  9
)
sprawdz_zakres(
  mcda_dane_surowe$kompatybilnosc_prog,
  "kompatybilnosc_prog",
  1,
  5,
  dodatkowo_dozwolone = 99
)
sprawdz_zakres(mcda_dane_surowe$ocena_biura_erasmus, "ocena_biura_erasmus", 1, 5)
sprawdz_zakres(mcda_dane_surowe$jakosc_wykladowcow, "jakosc_wykladowcow", 1, 5)
if (!all(mcda_dane_surowe$dostepnosc_akademik %in% c(0, 10))) {
  stop("Kolumna `dostepnosc_akademik` musi zawierac tylko 0 albo 10.", call. = FALSE)
}
sprawdz_zakres(mcda_dane_surowe$satysfakcja_program, "satysfakcja_program", 1, 10)
sprawdz_zakres(mcda_dane_surowe$spolecznosc_int, "spolecznosc_int", 1, 10)
sprawdz_zakres(mcda_dane_surowe$bezpieczenstwo_miasta, "bezpieczenstwo_miasta", 1, 7)
sprawdz_zakres(
  mcda_dane_surowe$atrakcyjnosc_miasta,
  "atrakcyjnosc_miasta",
  1,
  7,
  na_ok = TRUE
)

if (!any(mcda_dane_surowe$kompatybilnosc_prog == 99)) {
  stop("Brakuje kontrolowanego kodu bledu 99.", call. = FALSE)
}

if (!any(is.na(mcda_dane_surowe$atrakcyjnosc_miasta))) {
  stop("Brakuje kontrolowanego braku danych NA.", call. = FALSE)
}

# --- ZAPIS DO PAKIETU -----------------------------------------------------

usethis::use_data(mcda_dane_surowe, overwrite = TRUE)
