#' Fuzzy VIKOR Method
#'
#' @description Implements Fuzzy VIKOR. Returns an object for plotting.
#' 
#' @inheritParams fuzzy_topsis
#' 
#' @param mobility_matrix Matrix (m x 3n). Triangular fuzzy numbers.
#' @param criteria_types Character vector. "max" for benefit, "min" for cost.
#' @param v_strategy Numeric (0-1). Compromise weight (default 0.5).
#' @param criteria_weights Numeric vector. Fuzzy weights (must be length equal to columns).
#' 
#' @return An object of class `student_mobility_fuzzy_vikor` containing S, R, and Q indices.
#' @export

student_mobility_fuzzy_vikor <- function(mobility_matrix, criteria_types, v_strategy = 0.5, criteria_weights) {
  
  
  # 1. Input Validation and Weight Preparation
  
  if (!is.matrix(mobility_matrix)) 
    stop("'mobility_matrix' must be a matrix.")
  
  fuzzy_weights <- .get_final_weights(mobility_matrix, criteria_weights)
  n_cols <- ncol(mobility_matrix)
  
  
  # 2. Expand Criteria Types to Fuzzy Columns
  
  fuzzy_cba <- character(n_cols)
  
  k <- 1
  for (j in seq(1, n_cols, 3)) {
   fuzzy_cba[j:(j+2)] <- criteria_types[k]
    k <- k + 1
  }
  
  
  # 3. Determine Ideal Solutions
  positive_ideal <- ifelse(fuzzy_cba == "max", apply(mobility_matrix, 2, max), apply(mobility_matrix, 2, min))
  negative_ideal <- ifelse(fuzzy_cba == "min", apply(mobility_matrix, 2, max), apply(mobility_matrix, 2, min))
  
  
  # 4. Calculate Normalized Fuzzy Distances (Linear Normalization)
  norm_dist_matrix <- matrix(0, nrow = nrow(mobility_matrix), ncol = n_cols)
  
  for (i in seq(1, n_cols, 3)) {
    if (fuzzy_cba[i] == "max") {
      denominator <- positive_ideal[i+2] - negative_ideal[i]
      if(denominator == 0) denominator <- 1e-9
      norm_dist_matrix[, i]   <- (positive_ideal[i]   - mobility_matrix[, i+2]) / denominator
      norm_dist_matrix[, i+1] <- (positive_ideal[i+1] - mobility_matrix[, i+1]) / denominator
      norm_dist_matrix[, i+2] <- (positive_ideal[i+2] - mobility_matrix[, i])   / denominator
    } else {
      denominator <- negative_ideal[i+2] - positive_ideal[i]
      if(denominator == 0) denominator <- 1e-9
      norm_dist_matrix[, i]   <- (mobility_matrix[, i]   - positive_ideal[i+2]) / denominator
      norm_dist_matrix[, i+1] <- (mobility_matrix[, i+1] - positive_ideal[i+1]) / denominator
      norm_dist_matrix[, i+2] <- (mobility_matrix[, i+2] - positive_ideal[i])   / denominator
    }
  }
  
  
  # 5. Apply Fuzzy Weights to Normalized Matrix
  
  weighted_matrix <- norm_dist_matrix %*% diag(fuzzy_weights)
  
  
  # 6. Compute Group Utility (S) and Individual Regret (R)
  S_fuzzy <- matrix(0, nrow(mobility_matrix), 3)
  R_fuzzy <- matrix(0, nrow(mobility_matrix), 3)
  
  S_fuzzy[,1] <- apply(weighted_matrix[, seq(1, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,2] <- apply(weighted_matrix[, seq(2, n_cols, 3), drop=FALSE], 1, sum)
  S_fuzzy[,3] <- apply(weighted_matrix[, seq(3, n_cols, 3), drop=FALSE], 1, sum)
  
  R_fuzzy[,1] <- apply(weighted_matrix[, seq(1, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,2] <- apply(weighted_matrix[, seq(2, n_cols, 3), drop=FALSE], 1, max)
  R_fuzzy[,3] <- apply(weighted_matrix[, seq(3, n_cols, 3), drop=FALSE], 1, max)
  
  
  # 7. Calculate Fuzzy Compromise Index (Q)
  
  S_best <- min(S_fuzzy[,1])
  S_worst <- max(S_fuzzy[,3])
  R_best <- min(R_fuzzy[,1])
  R_worst <- max(R_fuzzy[,3])
  
  denominator_S <- ifelse((S_worst - S_best) == 0, 1, (S_worst - S_best))
  denominator_R <- ifelse((R_worst - R_best) == 0, 1, (R_worst - R_best))
  
  utility_part <- (S_fuzzy - S_best) / denominator_S
  regret_part  <- (R_fuzzy - R_best) / denominator_R
  
  Q_fuzzy <- v_strategy * utility_part + (1 - v_strategy) * regret_part
  
  
  # 8. Defuzzification (Center of Area Method)
  
  crisp_S <- (S_fuzzy[,1] + 2*S_fuzzy[,2] + S_fuzzy[,3]) / 4
  crisp_R <- (R_fuzzy[,1] + 2*R_fuzzy[,2] + R_fuzzy[,3]) / 4
  crisp_Q <- (Q_fuzzy[,1] + 2*Q_fuzzy[,2] + Q_fuzzy[,3]) / 4
  
  
  # 9. Structure and Return Final Results
  
  result_df <- data.frame(
    Alternative = 1:nrow(mobility_matrix),
    Def_S = crisp_S,
    Def_R = crisp_R,
    Def_Q = crisp_Q,
    Ranking = rank(crisp_Q, ties.method = "first")
  )
  
  output_list <- list(
    results = result_df,
    details = list(S_fuzzy = S_fuzzy, R_fuzzy = R_fuzzy, Q_fuzzy = Q_fuzzy),
    params = list(v = v_strategy)
  )
  
  class(output_list) <- "fuzzy_vikor_res"
  return(output_list)
}