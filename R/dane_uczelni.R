#' Dane do macierzy decyzyjnej MCDA
#'
#' Zbiór danych zawierający syntetyczne odpowiedzi lub pomiary dla 5 uczelni
#' partnerskich ocenianych w ramach 12 zmiennych reprezentujących 4 główne
#' kryteria: Finanse, Jakość, Zadowolenie i Miasto. Dane zostały wygenerowane
#' z formalnego procesu DGP opisanego w `data-raw/generowanie_danych.R`.
#'
#' Generator zawiera sekcję z wektorami danych faktualnych, które można zastąpić
#' prawdziwymi wartościami dla konkretnych uczelni. Zmienne kosztu, rankingu i
#' odległości są w zbiorze zapisane jako skale korzyściowe 1-9: wyższa wartość
#' oznacza lepszy wynik dla studenta.
#'
#' @format Ramka danych (data frame) z 15 wierszami i 14 zmiennymi:
#' \describe{
#'   \item{Student_ID}{Identyfikator osoby oceniającej.}
#'   \item{Uczelnia}{Uczelnia podlegająca ocenie.}
#'   \item{wysokosc_stypendium}{Wysokość stypendium Erasmus w euro.}
#'   \item{koszty_zycia_mies}{Skala korzyściowa 1-9 wyliczona z miesięcznych kosztów życia; wyżej oznacza korzystniej.}
#'   \item{ranking_uczelni}{Skala korzyściowa 1-9 wyliczona z miejsca w rankingu uczelni; wyżej oznacza korzystniej.}
#'   \item{kompatybilnosc_prog}{Kompatybilność programu studiów w skali 1-5; zawiera 99 jako kontrolowany kod błędu.}
#'   \item{ocena_biura_erasmus}{Ocena biura Erasmus w skali Likerta 1-5.}
#'   \item{jakosc_wykladowcow}{Ocena wykładowców w skali Likerta 1-5.}
#'   \item{dostepnosc_akademik}{Dostępność akademików zapisana jako 0 albo 10 punktów.}
#'   \item{satysfakcja_program}{Satysfakcja z programu w skali Likerta 1-10.}
#'   \item{spolecznosc_int}{Ocena społeczności międzynarodowej w skali Likerta 1-10.}
#'   \item{odleglosc_od_macierzystej}{Skala korzyściowa 1-9 wyliczona z odległości od uczelni macierzystej; wyżej oznacza korzystniej.}
#'   \item{bezpieczenstwo_miasta}{Ocena bezpieczeństwa miasta w skali Likerta 1-7.}
#'   \item{atrakcyjnosc_miasta}{Ocena atrakcyjności miasta w skali Likerta 1-7; zawiera kontrolowane braki danych NA.}
#' }
#' @usage data(mcda_dane_surowe)
#' @source Wygenerowano skryptem `data-raw/generowanie_danych.R`.
#' @name mcda_dane_surowe
NULL
