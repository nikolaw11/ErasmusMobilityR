#!/usr/bin/env Rscript

if (!file.exists("DESCRIPTION")) {
  stop("Run tools/render-site.R from the package root.", call. = FALSE)
}

if (!exists("zbuduj_macierz_rozmyta", mode = "function")) {
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(".", quiet = TRUE)
  } else if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    stop("Package must be loaded with pkgload or devtools before rendering the site.", call. = FALSE)
  }
}

if (!requireNamespace("htmltools", quietly = TRUE)) {
  stop("Package 'htmltools' is required to render index.html.", call. = FALSE)
}

escape_html <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

fmt <- function(x, digits = 2) {
  if (is.numeric(x)) {
    formatC(x, format = "f", digits = digits)
  } else {
    as.character(x)
  }
}

render_table <- function(df, numeric_digits = 2) {
  headers <- paste0(
    '<th class="px-4 py-3 text-left text-[11px] font-semibold uppercase tracking-[0.18em] text-cyan-200">',
    escape_html(names(df)),
    '</th>',
    collapse = "\n"
  )

  rows <- apply(df, 1, function(row) {
    cells <- vapply(seq_along(row), function(i) {
      value <- row[[i]]
      numeric_value <- suppressWarnings(as.numeric(value))
      if (!is.na(numeric_value) && grepl("^[0-9.-]+$", value)) {
        value <- fmt(numeric_value, numeric_digits)
      }

      paste0(
        '<td class="border-t border-zinc-800 px-4 py-3 text-sm text-zinc-100">',
        escape_html(value),
        '</td>'
      )
    }, character(1))

    paste0('<tr class="hover:bg-white/[0.04]">', paste(cells, collapse = "\n"), '</tr>')
  })

  paste0(
    '<div class="overflow-x-auto rounded border border-zinc-800 bg-zinc-950/60">',
    '<table class="w-full min-w-[640px] border-collapse">',
    '<thead class="bg-zinc-900/90"><tr>',
    headers,
    '</tr></thead>',
    '<tbody>',
    paste(rows, collapse = "\n"),
    '</tbody></table></div>'
  )
}

render_plot <- function(src, alt) {
  paste0(
    '<figure class="rounded border border-zinc-800 bg-zinc-950/70 p-3">',
    '<img class="w-full rounded border border-zinc-800 bg-white" src="',
    escape_html(src),
    '" alt="',
    escape_html(alt),
    '">',
    '</figure>'
  )
}

js_strings <- function(x) {
  escaped <- gsub('(["\\\\])', '\\\\\\1', as.character(x))
  paste0('["', paste(escaped, collapse = '","'), '"]')
}

js_numbers <- function(x, digits = 4) {
  paste0("[", paste(formatC(as.numeric(x), format = "f", digits = digits), collapse = ","), "]")
}

data("mcda_dane_surowe")

skladnia_modelu_erasmus <- "
  Finanse      =~ wysokosc_stypendium + koszty_zycia_mies;
  Jakosc       =~ ranking_uczelni + kompatybilnosc_prog + ocena_biura_erasmus + jakosc_wykladowcow + dostepnosc_akademik;
  Zadowolenie  =~ satysfakcja_program + spolecznosc_int;
  Miasto       =~ odleglosc_od_macierzystej + bezpieczenstwo_miasta + atrakcyjnosc_miasta
"

kryteria_erasmus <- c("Finanse", "Jakosc", "Zadowolenie", "Miasto")
kryteria_etykiety <- c("Finanse", "Jakość", "Zadowolenie", "Miasto")
kierunki_erasmus <- rep("max", length(kryteria_erasmus))
ankieta_best_to_others <- c(1, 2, 3, 8)
ankieta_others_to_worst <- c(8, 4, 2, 1)

rozmyta_macierz <- zbuduj_macierz_rozmyta(
  dane = mcda_dane_surowe,
  skladnia = skladnia_modelu_erasmus,
  kolumna_uczelni = "Uczelnia"
)

wyniki_bwm <- wyznacz_wagi_kryteriow_bwm(
  nazwy_kryteriow = kryteria_erasmus,
  najwazniejszy_vs_reszta = ankieta_best_to_others,
  reszta_vs_najmniej_wazny = ankieta_others_to_worst
)

wyniki_topsis <- suppressMessages(wyznacz_ranking_topsis(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
))

wyniki_vikor <- suppressMessages(wyznacz_ranking_vikor(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
))

wyniki_waspas <- suppressMessages(wyznacz_ranking_waspas(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
))

meta_wynik <- suppressMessages(wyznacz_meta_ranking_erasmus(
  rozmyta_macierz_decyzyjna = rozmyta_macierz,
  kierunki_kryteriow = kierunki_erasmus,
  nazwy_kryteriow_bwm = kryteria_erasmus,
  bwm_najlepsze = ankieta_best_to_others,
  bwm_najgorsze = ankieta_others_to_worst
))

dane_preview <- mcda_dane_surowe[
  order(mcda_dane_surowe$Uczelnia, mcda_dane_surowe$Student_ID),
  c("Uczelnia", "wysokosc_stypendium", "ocena_biura_erasmus", "spolecznosc_int", "atrakcyjnosc_miasta")
]
dane_preview <- head(dane_preview, 5)
names(dane_preview) <- c("Uczelnia", "Stypendium", "Biuro Erasmus", "Społeczność", "Miasto")

ranking_preview <- meta_wynik$porownanie
names(ranking_preview) <- c(
  "Uczelnia",
  "TOPSIS",
  "VIKOR",
  "WASPAS",
  "Borda",
  "Dominacja",
  "Konsensus"
)

top_school <- gsub("_", " ", ranking_preview$Uczelnia[[1]])
cor_values <- meta_wynik$zgodnosc_metod_korelacja[upper.tri(meta_wynik$zgodnosc_metod_korelacja)]
min_cor <- min(cor_values, na.rm = TRUE)
stability <- if (is.finite(min_cor) && min_cor >= 0.9) "Tak" else "Do sprawdzenia"

site_html <- paste0(
'<!DOCTYPE html>
<html lang="pl">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Nikola Waliczek | ErasmusMobilityR</title>
  <script src="https://cdn.tailwindcss.com"></script>
  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
  <style>
    html { scroll-behavior: smooth; }
    body { background: #050505; }
    .chart-box { position: relative; width: 100%; height: 360px; min-height: 300px; max-height: 460px; }
    code, pre { font-variant-ligatures: none; }
  </style>
</head>
<body class="bg-black text-zinc-100 selection:bg-cyan-300 selection:text-black">
  <nav class="sticky top-0 z-50 border-b border-zinc-800 bg-black/90 backdrop-blur">
    <div class="mx-auto flex max-w-7xl items-center justify-between px-4 py-3 sm:px-6 lg:px-8">
      <div class="flex items-center gap-3">
        <div class="flex h-9 w-9 items-center justify-center rounded border border-cyan-300 bg-cyan-300 font-black text-black">ER</div>
        <div>
          <p class="text-sm font-bold uppercase tracking-[0.18em] text-zinc-50">ErasmusMobilityR</p>
          <p class="text-xs text-zinc-400">MCDA, fuzzy logic i ranking uczelni partnerskich Erasmus+</p>
        </div>
      </div>
      <div class="hidden items-center gap-5 text-sm font-medium text-zinc-300 md:flex">
        <a href="#problem" class="hover:text-cyan-200">Problem</a>
        <a href="#dane" class="hover:text-cyan-200">Dane</a>
        <a href="#wyniki" class="hover:text-cyan-200">Wyniki</a>
        <a href="#odtworzenie" class="hover:text-cyan-200">Odtworzenie</a>
      </div>
    </div>
  </nav>

  <main class="mx-auto max-w-7xl px-4 py-8 sm:px-6 lg:px-8">
    <section id="problem" class="grid gap-6 lg:grid-cols-[1.25fr_0.75fr]">
      <div class="rounded border border-zinc-800 bg-zinc-950 p-6 shadow-2xl shadow-black">
        <p class="mb-3 text-xs font-bold uppercase tracking-[0.22em] text-cyan-300">Prezentacja pakietu</p>
        <h1 class="max-w-4xl text-4xl font-black tracking-tight text-white sm:text-5xl">ErasmusMobilityR</h1>
        <p class="mt-4 max-w-3xl text-lg leading-8 text-zinc-300">
          Narzędzie do wielokryterialnej analizy uczelni partnerskich Erasmus+. Pakiet prowadzi od danych ankietowych przez rozmywanie ocen, wagi BWM oraz rankingi Fuzzy TOPSIS, VIKOR i WASPAS do końcowego meta-rankingu.
        </p>
        <div class="mt-6 grid gap-3 sm:grid-cols-3">
          <div class="rounded border border-zinc-800 bg-black p-4">
            <p class="text-xs font-semibold uppercase tracking-[0.18em] text-cyan-300">Zakres</p>
            <p class="mt-2 text-sm leading-6 text-zinc-300">Finanse, jakość, zadowolenie i miasto jako główne kryteria wyboru.</p>
          </div>
          <div class="rounded border border-zinc-800 bg-black p-4">
            <p class="text-xs font-semibold uppercase tracking-[0.18em] text-lime-300">Metody</p>
            <p class="mt-2 text-sm leading-6 text-zinc-300">BWM, Fuzzy TOPSIS, Fuzzy VIKOR, Fuzzy WASPAS i konsensus rankingów.</p>
          </div>
          <div class="rounded border border-zinc-800 bg-black p-4">
            <p class="text-xs font-semibold uppercase tracking-[0.18em] text-amber-300">Wynik</p>
            <p class="mt-2 text-sm leading-6 text-zinc-300">Aktualny lider demonstracji: <strong class="text-white">', escape_html(top_school), '</strong>.</p>
          </div>
        </div>
      </div>

      <aside class="rounded border border-zinc-800 bg-zinc-950 p-6">
        <h2 class="text-lg font-bold text-white">Szybki odczyt wyniku</h2>
        <dl class="mt-5 space-y-4">
          <div>
            <dt class="text-xs uppercase tracking-[0.18em] text-zinc-500">Najlepsza alternatywa</dt>
            <dd class="mt-1 text-3xl font-black text-cyan-200">', escape_html(top_school), '</dd>
          </div>
          <div>
            <dt class="text-xs uppercase tracking-[0.18em] text-zinc-500">Stabilność rankingu</dt>
            <dd class="mt-1 text-3xl font-black text-lime-200">', escape_html(stability), '</dd>
            <dd class="mt-1 text-sm text-zinc-400">minimalna korelacja Spearmana: ', fmt(min_cor, 2), '</dd>
          </div>
          <div>
            <dt class="text-xs uppercase tracking-[0.18em] text-zinc-500">Źródło</dt>
            <dd class="mt-1 text-sm leading-6 text-zinc-300">Wyniki są liczone podczas renderowania strony przez <code class="rounded bg-zinc-900 px-1.5 py-0.5 text-cyan-200">tools/render-docs.R</code>.</dd>
          </div>
        </dl>
      </aside>
    </section>

    <section id="dane" class="mt-8 grid gap-6 lg:grid-cols-2">
      <div class="rounded border border-zinc-800 bg-zinc-950 p-6">
        <p class="mb-3 text-xs font-bold uppercase tracking-[0.22em] text-cyan-300">Dane</p>
        <h2 class="text-2xl font-bold text-white">Przykładowe dane wejściowe</h2>
        <p class="mt-3 text-sm leading-6 text-zinc-300">Tabela jest generowana z aktualnego zbioru <code class="rounded bg-zinc-900 px-1.5 py-0.5 text-cyan-200">mcda_dane_surowe</code>.</p>
        <div class="mt-5">', render_table(dane_preview, numeric_digits = 0), '</div>
      </div>

      <div class="rounded border border-zinc-800 bg-zinc-950 p-6">
        <p class="mb-3 text-xs font-bold uppercase tracking-[0.22em] text-lime-300">Wagi</p>
        <h2 class="text-2xl font-bold text-white">Priorytety BWM</h2>
        <p class="mt-3 text-sm leading-6 text-zinc-300">Wagi kryteriów są liczone z tych samych wektorów preferencji, których używa poradnik.</p>
        <div class="chart-box mt-5">
          <canvas id="weightsChart"></canvas>
        </div>
      </div>
    </section>

    <section id="wyniki" class="mt-8 rounded border border-zinc-800 bg-zinc-950 p-6">
      <div class="flex flex-col gap-3 sm:flex-row sm:items-end sm:justify-between">
        <div>
          <p class="mb-3 text-xs font-bold uppercase tracking-[0.22em] text-amber-300">Wyniki</p>
          <h2 class="text-2xl font-bold text-white">Aktualny meta-ranking</h2>
        </div>
        <p class="max-w-2xl text-sm leading-6 text-zinc-400">Tabela poniżej jest generowana z obiektu <code class="rounded bg-zinc-900 px-1.5 py-0.5 text-cyan-200">meta_wynik$porownanie</code>, więc aktualizuje się razem z README i vignette.</p>
      </div>
      <div class="mt-5">', render_table(ranking_preview, numeric_digits = 0), '</div>
      <div class="mt-6 grid gap-4 lg:grid-cols-3">',
        render_plot("man/figures/README-plot-topsis-1.png", "Mapa TOPSIS"),
        render_plot("man/figures/README-plot-vikor-1.png", "Mapa VIKOR"),
        render_plot("man/figures/README-plot-waspas-1.png", "Mapa WASPAS"),
      '</div>
    </section>

    <section id="odtworzenie" class="mt-8 rounded border border-zinc-800 bg-zinc-950 p-6">
      <p class="mb-3 text-xs font-bold uppercase tracking-[0.22em] text-cyan-300">Odtworzenie</p>
      <h2 class="text-2xl font-bold text-white">Instalacja i pełny poradnik</h2>
      <pre class="mt-5 overflow-x-auto rounded border border-zinc-800 bg-black p-5 text-sm leading-6 text-zinc-100"><code>install.packages("remotes")

remotes::install_github(
  "nikolaw11/ErasmusMobilityR",
  dependencies = TRUE,
  build_vignettes = TRUE
)

browseVignettes("ErasmusMobilityR")
vignette("poradnik_mcda", package = "ErasmusMobilityR")</code></pre>
    </section>
  </main>

  <footer class="mt-12 border-t border-zinc-800 bg-black px-4 py-8 text-center text-sm text-zinc-500">
    Nikola Waliczek 2026
  </footer>

  <script>
    const grid = { color: "rgba(255,255,255,0.12)" };
    const tick = { color: "#d4d4d8" };

    new Chart(document.getElementById("weightsChart"), {
      type: "bar",
      data: {
        labels: ', js_strings(kryteria_etykiety), ',
        datasets: [{
          label: "Waga",
          data: ', js_numbers(wyniki_bwm$obliczone_wagi), ',
          backgroundColor: ["#67e8f9", "#bef264", "#fcd34d", "#f0abfc"],
          borderColor: "#09090b",
          borderWidth: 1,
          borderRadius: 4
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: { legend: { display: false } },
        scales: {
          y: { beginAtZero: true, grid, ticks: tick },
          x: { grid: { display: false }, ticks: tick }
        }
      }
    });
  </script>
</body>
</html>
')

site_html <- sub("[[:space:]]+$", "", site_html)
writeLines(site_html, "index.html", useBytes = TRUE)
message("Rendered index.html")
