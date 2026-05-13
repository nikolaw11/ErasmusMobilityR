
# ErasmusMobilityR

**ErasmusMobilityR** to kompleksowe narzędzie analityczne przeznaczone
do Wielokryterialnej Analizy Decyzyjnej (MCDA) w środowisku rozmytym.
Pakiet został zaprojektowany z myślą o ewaluacji i rankingu uczelni
partnerskich w ramach programu Erasmus+.

Umożliwia pełną ścieżkę badawczą: od agregacji surowych ankiet
studenckich, przez wyznaczanie wag metodą **BWM (Best-Worst Method)**,
aż po zaawansowane rankingi metodami **Fuzzy TOPSIS, Fuzzy VIKOR i Fuzzy
WASPAS**, zwieńczone autorskim **Meta-Rankingiem**.

## Instalacja

Najwygodniej zainstalować wersję deweloperską razem z zależnościami
potrzebnymi do poradnika i renderowania tabel:

``` r
install.packages("remotes")

remotes::install_github(
  "nikolaw11/ErasmusMobilityR",
  dependencies = TRUE,
  build_vignettes = TRUE
)
```

Po instalacji pełny poradnik można otworzyć poleceniem:

``` r
browseVignettes("ErasmusMobilityR")
vignette("poradnik_mcda", package = "ErasmusMobilityR")
```

## Szybki start

Poniższy przykład jest renderowany automatycznie z `README.Rmd`, dlatego
tabele i wykresy w README na GitHubie są aktualizowane razem z kodem
pakietu.

``` r
library(ErasmusMobilityR)

data("mcda_dane_surowe")

skladnia_modelu_erasmus <- "
  Finanse      =~ wysokosc_stypendium + koszty_zycia_mies;
  Jakosc       =~ ranking_uczelni + kompatybilnosc_prog + ocena_biura_erasmus + jakosc_wykladowcow + dostepnosc_akademik;
  Zadowolenie  =~ satysfakcja_program + spolecznosc_int;
  Miasto       =~ odleglosc_od_macierzystej + bezpieczenstwo_miasta + atrakcyjnosc_miasta
"

rozmyta_macierz <- zbuduj_macierz_rozmyta(
  dane = mcda_dane_surowe,
  skladnia = skladnia_modelu_erasmus,
  kolumna_uczelni = "Uczelnia"
)

kryteria_erasmus <- c("Finanse", "Jakosc", "Zadowolenie", "Miasto")
kierunki_erasmus <- rep("max", length(kryteria_erasmus))
ankieta_best_to_others <- c(1, 2, 3, 8)
ankieta_others_to_worst <- c(8, 4, 2, 1)

wyniki_topsis <- wyznacz_ranking_topsis(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
)

wyniki_vikor <- wyznacz_ranking_vikor(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
)

wyniki_waspas <- wyznacz_ranking_waspas(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
)

meta_wynik <- wyznacz_meta_ranking_erasmus(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
)

print(meta_wynik$porownanie)
#>   Uczelnia_Partnerska Miejsce_TOPSIS Miejsce_VIKOR Miejsce_WASPAS
#> 1          Uczelnia_C              1             1              1
#> 2          Uczelnia_D              2             2              2
#> 3          Uczelnia_B              3             3              3
#> 4          Uczelnia_E              4             4              4
#> 5          Uczelnia_A              5             5              5
#>   Meta_Srednia_Pozycja Meta_Dominacja Meta_Konsensus_RA
#> 1                    1              1                 1
#> 2                    2              2                 2
#> 3                    3              3                 3
#> 4                    4              4                 4
#> 5                    5              5                 5
```

## Tabele APA

Tabele poniżej są renderowane w formacie Markdown zgodnym z GitHubem.
Pełne tabele `tabela_apa()` w formacie HTML/Word są dostępne w vignette
pakietu.

**Tabela 1. Wyniki metody Fuzzy TOPSIS**

| Uczelnia Partnerska |    D+ |    D- |     CC | Pozycja |
|:--------------------|------:|------:|-------:|--------:|
| Uczelnia_C          | 0.101 | 0.293 | 0.7667 |       1 |
| Uczelnia_D          | 0.146 | 0.197 | 0.6013 |       2 |
| Uczelnia_B          | 0.193 | 0.154 | 0.4706 |       3 |
| Uczelnia_E          | 0.272 | 0.061 | 0.1884 |       4 |
| Uczelnia_A          | 0.309 | 0.044 | 0.1288 |       5 |

*Uwaga. CC oznacza współczynnik bliskości. Im wyższa wartość, tym lepsza
alternatywa.*

**Tabela 2. Wyniki metody Fuzzy VIKOR**

| Uczelnia Partnerska |     S |     R |      Q | Pozycja |
|:--------------------|------:|------:|-------:|--------:|
| Uczelnia_C          | 0.163 | 0.147 | 0.2786 |       1 |
| Uczelnia_D          | 0.319 | 0.177 | 0.3841 |       2 |
| Uczelnia_B          | 0.350 | 0.254 | 0.4778 |       3 |
| Uczelnia_E          | 0.608 | 0.310 | 0.6575 |       4 |
| Uczelnia_A          | 0.621 | 0.380 | 0.7361 |       5 |

*Uwaga. S opisuje użyteczność grupową, R indywidualny żal, a Q indeks
kompromisu. Niższe Q oznacza lepszą pozycję.*

**Tabela 3. Wyniki metody Fuzzy WASPAS**

| Uczelnia Partnerska |   WSM |   WPM | Q WASPAS | Pozycja |
|:--------------------|------:|------:|---------:|--------:|
| Uczelnia_C          | 0.735 | 0.682 |   0.7086 |       1 |
| Uczelnia_D          | 0.592 | 0.565 |   0.5789 |       2 |
| Uczelnia_B          | 0.563 | 0.547 |   0.5548 |       3 |
| Uczelnia_E          | 0.356 | 0.338 |   0.3473 |       4 |
| Uczelnia_A          | 0.339 | 0.303 |   0.3208 |       5 |

*Uwaga. WASPAS łączy model sumaryczny WSM i iloczynowy WPM w jeden
wskaźnik użyteczności.*

**Tabela 4. Ostateczny Meta-Ranking Uczelni**

| Uczelnia Partnerska | TOPSIS | VIKOR | WASPAS | Borda | Dominacja | Konsensus RA |
|:--------------------|-------:|------:|-------:|------:|----------:|-------------:|
| Uczelnia_C          |      1 |     1 |      1 |     1 |         1 |            1 |
| Uczelnia_D          |      2 |     2 |      2 |     2 |         2 |            2 |
| Uczelnia_B          |      3 |     3 |      3 |     3 |         3 |            3 |
| Uczelnia_E          |      4 |     4 |      4 |     4 |         4 |            4 |
| Uczelnia_A          |      5 |     5 |      5 |     5 |         5 |            5 |

*Uwaga. Tabela zestawia rangi z trzech metod oraz trzy sposoby agregacji
wyniku końcowego.*

## Wizualizacja

Wykresy są generowane podczas renderowania README, więc pliki PNG w
`man/figures/` nie wymagają ręcznego odświeżania.

<img src="man/figures/README-plot-topsis-1.png" alt="" width="100%" />

<img src="man/figures/README-plot-vikor-1.png" alt="" width="100%" />

<img src="man/figures/README-plot-waspas-1.png" alt="" width="100%" />

## Synchronizacja dokumentacji

Lokalnie README i vignette można odtworzyć jednym poleceniem:

``` bash
Rscript tools/render-docs.R
```

Ten sam krok działa w GitHub Actions. Workflow renderuje `README.Rmd`,
buduje vignette oraz sprawdza, czy `README.md` i pliki
`man/figures/README-*.png` są zsynchronizowane.
