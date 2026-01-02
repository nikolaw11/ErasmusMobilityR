#' Plot Fuzzy VIKOR Results
#'
#' @description Visualizes the VIKOR results using a bubble chart.
#' The X-axis represents Group Utility (S), the Y-axis represents Individual Regret (R).
#' Bubble size is inversely proportional to the Compromise Solution (Q) - smaller Q (better rank) = larger bubble.
#'
#' @param x An object of class `fuzzy_vikor_res` returned by the `fuzzy_vikor_student_mobility()` function.
#' @param ... Additional arguments.
#' 
#' @return A ggplot object.
#' 
#' @import ggplot2
#' @import ggrepel
#' 
#' @export

plot.fuzzy_vikor_res <- function(x, ...) {
  
  # 1. Data preparation
  
  df <- x$results
  
  df$Uni_Label <- paste("Uni", df$Alternative)
  df$Uni_Group <- ifelse(df$Ranking <= 3, "Top 3 Universities", "Other Universities")
  
  
  # 2. Size Logic
  
  total_rank <- max(df$Ranking)
  df$PlotSize <- (total_rank + 1) - df$Ranking
  
  
  # 3. Reference Points
  
  center_S <- mean(df$Def_S, na.rm = TRUE)
  center_R <- mean(df$Def_R, na.rm = TRUE)
  
  
  # 4. Plot Building
  
  p <- ggplot(df, aes(x = Def_S, y = Def_R)) +
    
    # Layer A: Quadrant lines
    geom_vline(xintercept = center_S, linetype = "longdash", color = "slategray", alpha = 0.6) +
    geom_hline(yintercept = center_R, linetype = "longdash", color = "slategray", alpha = 0.6) +
    
    # Layer B: Bubbles
    geom_point(aes(size = PlotSize, fill = Uni_Group),
                shape = 21,         
                color = "white",    
                stroke = 0.8,
                alpha = 0.9) +
    
    # Layer C: Text Labels
    geom_text_repel(aes(label = Uni_Label),
                    size = 3.5,
                    box.padding = 0.4,
                    point.padding = 0.5,
                    min.segment.length = 0,
                    max.overlaps = 15,
                    color = "black") +
    
    # Layer E: Scale and Colors
    scale_fill_manual(values = c("Top 3 Universities" = "#2E86C1", "Other Universities" = "#D5D8DC")) +
    scale_size_continuous(range = c(5, 14), guide = "none") +
    
    # Layer E: Axis and Title Description
    labs(
      title = "Fuzzy VIKOR Compromise Analysis",
      subtitle = "X: Group Utility (S), Y: Individual Regret (R). Lower values are better.\nBlue bubbles represent the top-ranked universities.",
      x = "Group Utility (S) -> [Lower is Better]",
      y = "Individual Regret (R) -> [Lower is Better]",
      fill = "Rank Group"
    ) +
    
    #Layer F: Aesthetics
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16, family = "sans"),
      plot.subtitle = element_text(size = 11, color = "grey40"),
      axis.title = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# Fix for R CMD check global variable warnings
utils::globalVariables(c("Def_S", "Def_R", "PlotSize", "Uni_Group", "Uni_Label", "Alternative"))