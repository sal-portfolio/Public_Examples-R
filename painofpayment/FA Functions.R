
# Pain of Payment Study 6
# Author: Amanda
##This file creates functions for running factor analysis, inter-factor corrs, and regressions
##can be used for individual-level (j) or individual-product level (ij) data, filtered by KMO results

  # KMO_results <- function(items, KMO_threshold) {
  # kmo_result <- KMO(items)
  # low_kmo_items <- names(kmo_result$MSAi[kmo_result$MSAi < KMO_threshold])
  # }

  factor_analysis = function(items) {
  #cortest.bartlett(items)
  pa_result <- fa.parallel(items, fa = "fa")
  fa_result <- fa(items, nfactors = pa_result$nfact, rotate = "oblimin", fm = "ml")
  #rename factors according to top item loadings
  loadings_mat <- unclass(fa_result$loadings)
  top_items <- apply(loadings_mat, 2, function(col) {
    
  top_idx <- which.max(abs(col))
  item_name <- rownames(loadings_mat)[top_idx]
  if (col[top_idx] < 0) paste0("neg_", item_name) else item_name
    })
  top_items <- make.unique(top_items, sep = "_")

  colnames(fa_result$loadings) <- paste0("f_", top_items)
  colnames(fa_result$scores) <- paste0("f_", top_items)
  
fa_result
}



  
  corr_results <- function(vars, factor_data, prod_pooled, id) {
  pairs <- combn(vars, 2, simplify = FALSE)
  corr_results <- list()
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    pair_name <- paste0(var1, "_", var2)
    if (prod_pooled == TRUE) {result <- cor.test(factor_data[[var1]], factor_data[[var2]])}
    else {  result <- rmcorr(participant = factor_data[[id]], 
                    measure1 = factor_data[[var1]], 
                    measure2 = factor_data[[var2]], 
                    dataset = factor_data)}
    corr_results[[pair_name]] <- result
  }
  #save as csv a table showing corr results

  corr_summary <- map_dfr(corr_results, function(x) {
    if (prod_pooled) {
      tibble(r = x$estimate, df = x$parameter, p_value = x$p.value)
    } else {
      tibble(r = x$r, df = x$df, p_value = x$p)
    }
  }, .id = "pair")

  corr_summary <- corr_summary %>%
    mutate(
      p_value = case_when(
       p_value < .001 ~ "p<.001",
       TRUE ~ paste0("p=", round(p_value, 3))
     ),
     r = round(r, 3)
    )
  corr_summary %>% as.data.frame()  
  }
  

reg_results = function(factor_data, y, x_vars, prod_pooled, id) {
  factor_data[[y]] <- as.ordered(factor_data[[y]])
  if (prod_pooled) {
    ordinal_formula <- as.formula(paste(y, "~", paste(x_vars, collapse = " + ")))
    ordinal_model <- polr(ordinal_formula, data = factor_data, Hess = TRUE)
    
    coef_table <- coef(summary(ordinal_model))
    p_values <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
    coef_table <- cbind(coef_table, p_value = p_values)
    
        # McFadden pseudo-R^2: intercept-only null with same data
    null_model <- polr(as.formula(paste(y, "~ 1")), data = factor_data, Hess = TRUE)
    mcfadden_r2 <- as.numeric(1 - logLik(ordinal_model) / logLik(null_model))

  } else {
    ordinal_formula <- as.formula(paste(y, "~", paste(x_vars, collapse = " + "), "+ (1 |", id, ")"))
    ordinal_model <- clmm(ordinal_formula, data = factor_data, control = clmm.control(maxIter = 1000, gradTol = 1e-6, maxLineIter = 100))
    
    coef_table <- coef(summary(ordinal_model))
    coef_table <- cbind(coef_table, p_value = coef_table[, "Pr(>|z|)"])

     # McFadden pseudo-R^2: random-intercept-only null (same random structure)
    null_formula <- as.formula(paste(y, "~ 1 + (1 |", id, ")"))
    null_model <- clmm(null_formula, data = factor_data,
                       control = clmm.control(maxIter = 1000, gradTol = 1e-6, maxLineIter = 100))
    mcfadden_r2 <- as.numeric(1 - logLik(ordinal_model) / logLik(null_model))
  }


  ordinal_stats <- as.data.frame(coef_table)
  ordinal_stats$term <- rownames(ordinal_stats)
  
  if ("Estimate" %in% names(ordinal_stats)) {
    ordinal_stats <- ordinal_stats %>% rename(Value = Estimate)
  }

  ordinal_stats <- ordinal_stats %>%
    dplyr::select(term, Value, `Std. Error`, p_value) %>%
    mutate(
      Value = round(Value, 3),
      `Std. Error` = round(`Std. Error`, 3),
      p_value = case_when(
        p_value < .001 ~ "p<.001",
        TRUE ~ paste0("p=", round(p_value, 3))
      )
    )
  # Create table output; filter to just the predictor rows, using the same  f_vars list

  out <- ordinal_stats %>% filter(term %in% x_vars)
  attr(out, "mcfadden_r2") <- round(mcfadden_r2, 3)
  out
}

