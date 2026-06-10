
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
#>                Uczelnia_Partnerska Miejsce_TOPSIS Miejsce_VIKOR Miejsce_WASPAS
#> 1            Universidade do Porto              1             1              3
#> 2    Sapienza - Università di Roma              2             2              2
#> 3   Humboldt Universität zu Berlin              3             3              1
#> 4             Hacettepe University              4             4              4
#> 5 Universidad Carlos III de Madrid              5             5              5
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

| Uczelnia Partnerska              |    D+ |    D- |     CC | Pozycja |
|:---------------------------------|------:|------:|-------:|--------:|
| Universidade do Porto            | 0.101 | 0.193 | 0.6779 |       1 |
| Sapienza - Università di Roma    | 0.122 | 0.149 | 0.5896 |       2 |
| Humboldt Universität zu Berlin   | 0.132 | 0.143 | 0.5556 |       3 |
| Hacettepe University             | 0.220 | 0.020 | 0.0885 |       4 |
| Universidad Carlos III de Madrid | 0.233 | 0.004 | 0.0155 |       5 |

*Uwaga. CC oznacza współczynnik bliskości. Im wyższa wartość, tym lepsza
alternatywa.*

**Tabela 2. Wyniki metody Fuzzy VIKOR**

| Uczelnia Partnerska              |     S |     R |      Q | Pozycja |
|:---------------------------------|------:|------:|-------:|--------:|
| Universidade do Porto            | 0.232 | 0.146 | 0.2771 |       1 |
| Sapienza - Università di Roma    | 0.275 | 0.219 | 0.3737 |       2 |
| Humboldt Universität zu Berlin   | 0.254 | 0.239 | 0.3859 |       3 |
| Hacettepe University             | 0.634 | 0.320 | 0.6420 |       4 |
| Universidad Carlos III de Madrid | 0.685 | 0.339 | 0.6848 |       5 |

*Uwaga. S opisuje użyteczność grupową, R indywidualny żal, a Q indeks
kompromisu. Niższe Q oznacza lepszą pozycję.*

**Tabela 3. Wyniki metody Fuzzy WASPAS**

| Uczelnia Partnerska              |   WSM |   WPM | Q WASPAS | Pozycja |
|:---------------------------------|------:|------:|---------:|--------:|
| Humboldt Universität zu Berlin   | 0.692 | 0.673 |   0.6823 |       1 |
| Sapienza - Università di Roma    | 0.679 | 0.670 |   0.6747 |       2 |
| Universidade do Porto            | 0.695 | 0.643 |   0.6690 |       3 |
| Hacettepe University             | 0.410 | 0.398 |   0.4043 |       4 |
| Universidad Carlos III de Madrid | 0.373 | 0.365 |   0.3690 |       5 |

*Uwaga. WASPAS łączy model sumaryczny WSM i iloczynowy WPM w jeden
wskaźnik użyteczności.*

**Tabela 4. Ostateczny Meta-Ranking Uczelni**

| Uczelnia Partnerska | TOPSIS | VIKOR | WASPAS | Borda | Dominacja | Konsensus RA |
|:---|---:|---:|---:|---:|---:|---:|
| Universidade do Porto | 1 | 1 | 3 | 1 | 1 | 1 |
| Sapienza - Università di Roma | 2 | 2 | 2 | 2 | 2 | 2 |
| Humboldt Universität zu Berlin | 3 | 3 | 1 | 3 | 3 | 3 |
| Hacettepe University | 4 | 4 | 4 | 4 | 4 | 4 |
| Universidad Carlos III de Madrid | 5 | 5 | 5 | 5 | 5 | 5 |

*Uwaga. Tabela zestawia rangi z trzech metod oraz trzy sposoby agregacji
wyniku końcowego.*

## Wizualizacja

Wykresy są generowane podczas renderowania README, więc pliki PNG w
`man/figures/` nie wymagają ręcznego odświeżania.

<img src="man/figures/README-plot-topsis-1.png" width="100%" />

<img src="man/figures/README-plot-vikor-1.png" width="100%" />

<img src="man/figures/README-plot-waspas-1.png" width="100%" />

## Synchronizacja dokumentacji

Lokalnie README i vignette można odtworzyć jednym poleceniem:

``` bash
Rscript tools/render-docs.R
```

Ten sam krok działa w GitHub Actions. Workflow renderuje `README.Rmd`,
buduje vignette oraz sprawdza, czy `README.md` i pliki
`man/figures/README-*.png` są zsynchronizowane.
