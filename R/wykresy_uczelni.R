#' @title Wewnętrzny motyw graficzny
#' @description Ujednolicony styl wykresów dla całego pakietu.
#' @import ggplot2
#' @keywords internal
.motyw_mcda <- function() {
  list(
    theme_light(base_size = 12),
    scale_fill_gradient(low = "#90A4AE", high = "#0066CC"),
    scale_size_continuous(range = c(4, 16)),
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(color = "grey40", size = 11),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position = "right",
      axis.title = element_text(face = "bold")
    )
  )
}

#' Mapa Strategiczna VIKOR
#' 
#' @description Wizualizacja typu cIPMA.
#' Oś X: Efektywność grupowa (odwrócone S). Oś Y: Ryzyko/Żal (R).
#' Wielkość bąbla: Siła kompromisu (zależna od Q).
#' 
#' @param x Obiekt klasy `rozmyty_vikor_wynik`.
#' @param ... Dodatkowe argumenty (ignorowane).
#' @import ggplot2
#' @import ggrepel
#' @export
plot.vikor_erasmus_wynik <- function(x, ...) {
  df <- x$ranking
  
  # 1. Matematyka wykresu: Odwracamy S (żeby im więcej tym lepiej na osi X)
  s_min <- min(df$Wskaznik_S); s_max <- max(df$Wskaznik_S)
  # Normalizacja do 0-100
  df$Wydajnosc <- ((s_max - df$Wskaznik_S) / (s_max - s_min)) * 100
  
  # Wielkość bąbla (odwrócone Q - im mniejsze Q tym większy bąbel)
  q_inv <- 1 - ((df$Indeks_Q - min(df$Indeks_Q)) / (max(df$Indeks_Q) - min(df$Indeks_Q)))
  df$Rozmiar <- (q_inv + 0.1)^3 
  
  # Środki do wyznaczenia ćwiartek
  srodek_perf <- stats::median(df$Wydajnosc, na.rm=TRUE)
  srodek_ryzyko <- stats::median(df$Wskaznik_R, na.rm=TRUE)
  
  ggplot(df, aes(x = Wydajnosc, y = Wskaznik_R)) +
    # Tło dla strefy Lidera
    annotate("rect", xmin=srodek_perf, xmax=Inf, ymin=-Inf, ymax=srodek_ryzyko, fill="#E8F5E9", alpha=0.5) +
    
    # Linie podziału
    geom_vline(xintercept = srodek_perf, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_ryzyko, linetype = "dashed", color = "grey50") +
    
    # Etykiety stref
    annotate("text", x = max(df$Wydajnosc), y = min(df$Wskaznik_R), label = "STABILNY LIDER\n(Wysoka Efekt., Niskie Ryzyko)", 
             hjust=1, vjust=0, size=3, fontface="bold.italic", color="darkgreen") +
    annotate("text", x = min(df$Wydajnosc), y = max(df$Wskaznik_R), label = "UNIKAĆ\n(Niska Efekt., Wysokie Ryzyko)", 
             hjust=0, vjust=1, size=3, fontface="italic", color="#B71C1C") +
    
    # Bąble (Zamiast paste0("Alt", Alternatywa) dajemy od razu naszą kolumnę Uczelnia_Partnerska)
    geom_point(aes(size = Rozmiar, fill = Wydajnosc), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = Uczelnia_Partnerska), box.padding = 0.5, seed = 20260513) +
    
    scale_x_continuous(expand = expansion(mult = 0.2)) +
    
    labs(
      title = "Mapa Strategiczna Uczelni (Fuzzy VIKOR)",
      subtitle = "Zielona strefa = Najlepszy kompromis do wyjazdu na Erasmusa.",
      x = "Indeks Zadowolenia Ogółu (Wydajność)",
      y = "Indeks Ryzyka / Wad (Żal)",
      size = "Siła kompromisu",
      fill = "Wynik"
    ) +
    .motyw_mcda()
}

#' Mapa Efektywności TOPSIS
#' 
#' @description Pokazuje odległość od ideału. Oś X: Dystans od Najgorszego (D-).
#' Oś Y: Dystans do Najlepszego (D+).
#' Cel: Chcemy być w prawym dolnym rogu (Daleko od D-, Blisko D+).
#' 
#' @param x Obiekt klasy `rozmyty_topsis_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.topsis_erasmus_wynik <- function(x, ...) {
  df <- x$ranking
  df$Rozmiar <- (df$Wskaznik_CC)^4 
  
  # Punkt Idealny na wykresie (Target)
  cel_x <- max(df$Dystans_Od_Antyidealu) * 1.02
  cel_y <- min(df$Dystans_Od_Idealu) * 0.98
  
  # Obliczenie wizualnej odległości euklidesowej na wykresie
  df$OdlegloscWizualna <- sqrt((df$Dystans_Od_Antyidealu - cel_x)^2 + (df$Dystans_Od_Idealu - cel_y)^2)
  
  ggplot(df, aes(x = Dystans_Od_Antyidealu, y = Dystans_Od_Idealu)) +
    geom_segment(aes(xend = cel_x, yend = cel_y), linetype = "dotted", color = "grey50") +
    
    geom_label(aes(x = (Dystans_Od_Antyidealu + cel_x) / 2, y = (Dystans_Od_Idealu + cel_y) / 2, 
                   label = sprintf("%.3f", OdlegloscWizualna)), 
               size = 2.5, color = "grey30", label.size = 0, alpha = 0.7) +
    
    geom_point(aes(size = Rozmiar, fill = Wskaznik_CC), shape = 21, color = "black", alpha = 0.9) +
    geom_text_repel(aes(label = Uczelnia_Partnerska), box.padding = 0.6, seed = 20260513) +
    
    annotate("point", x = cel_x, y = cel_y, shape=18, size=6, color="#FFD700") +
    annotate("text", x = cel_x, y = cel_y, label="UCZELNIA IDEALNA", vjust=2, size=3.5, fontface="bold") +

    scale_x_continuous(expand = expansion(mult = c(0.08, 0.18))) +
    scale_y_continuous(expand = expansion(mult = c(0.25, 0.08))) +

    labs(
      title = "Mapa Odległości od Ideału (Fuzzy TOPSIS)",
      subtitle = "Linie przerywane pokazują geometryczną odległość od uczelni perfekcyjnej.",
      x = "Dystans od Uczelni Najgorszej (D-)",
      y = "Dystans do Uczelni Wzorcowej (D+)",
      size = "Bliskość^4",
      fill = "Wynik (CC)"
    ) +
    .motyw_mcda()
}

#' Mapa Balansu WASPAS
#' 
#' @description Pokazuje relację między modelem sumarycznym (WSM) a iloczynowym (WPM).
#' Oś X: Suma Ważona (WSM). Oś Y: Iloczyn Ważony (WPM).
#' Uczelnie w prawym górnym rogu są najlepsze w obu modelach.
#' 
#' @param x Obiekt klasy `waspas_erasmus_wynik`.
#' @param ... Dodatkowe argumenty.
#' @export
plot.waspas_erasmus_wynik <- function(x, ...) {
  df <- x$ranking
  
  # Potęgowanie do rozmiaru bąbla dla lepszego efektu wizualnego
  df$Rozmiar <- (df$Wskaznik_Q_WASPAS)^4 
  
  # Środki do wyznaczenia analitycznych ćwiartek
  srodek_wsm <- stats::median(df$Wynik_WSM, na.rm=TRUE)
  srodek_wpm <- stats::median(df$Wynik_WPM, na.rm=TRUE)
  
  ggplot(df, aes(x = Wynik_WSM, y = Wynik_WPM)) +
    # Tło dla strefy Lidera (Prawa górna ćwiartka - wysokie WSM i wysokie WPM)
    annotate("rect", xmin=srodek_wsm, xmax=Inf, ymin=srodek_wpm, ymax=Inf, fill="#E8F5E9", alpha=0.5) +
    
    # Linie podziału na ćwiartki
    geom_vline(xintercept = srodek_wsm, linetype = "dashed", color = "grey50") +
    geom_hline(yintercept = srodek_wpm, linetype = "dashed", color = "grey50") +
    
    # Etykieta strefy
    annotate("text", x = max(df$Wynik_WSM), y = min(df$Wynik_WPM), label = "SŁABE WPM\n(Uważać na wartości skrajne)", 
             hjust=1, vjust=0, size=3, fontface="italic", color="grey40") +
    annotate("text", x = srodek_wsm + 0.03, y = max(df$Wynik_WPM), label = "PODWÓJNA PRZEWAGA\n(Mocne WSM i WPM)",
             hjust=0, vjust=1, size=3, fontface="bold.italic", color="darkblue") +
    
    # Bąble reprezentujące uczelnie
    geom_point(aes(size = Rozmiar, fill = Wskaznik_Q_WASPAS), shape = 21, color = "black", alpha = 0.8) +
    geom_text_repel(aes(label = Uczelnia_Partnerska), box.padding = 0.5, seed = 20260513) +
    
    labs(
      title = "Mapa Balansu Uczelni (Fuzzy WASPAS)",
      subtitle = "Prawy górny róg to uczelnie o najwyższej stabilności ocen.",
      x = "Model Sumaryczny (WSM)",
      y = "Model Multiplikatywny (WPM)",
      size = "Wskaźnik Q^4",
      fill = "Ostateczne Q"
    ) +
    .motyw_mcda()
}

utils::globalVariables(c("Wskaznik_S", "Wskaznik_R", "Dystans_Od_Idealu", "Dystans_Od_Antyidealu", 
                         "Wskaznik_CC", "Wydajnosc", "Rozmiar", "OdlegloscWizualna", 
                         "Uczelnia_Partnerska", "Wynik_WSM", "Wynik_WPM", "Wskaznik_Q_WASPAS"))
