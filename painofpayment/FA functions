##This file creates functions for running factor analysis, inter-factor corrs, and regressions
##can be used for individual-level (j) or individual-product level (ij) data, filtered by KMO results

  KMO_results = function(items, KMO_threshold) {
  kmo_result <- KMO(items)
  low_kmo_items <- names(kmo_result$MSAi[kmo_result$MSAi < KMO_threshold])
  }

  factor_analysis = function(items) {
  #cortest.bartlett(items)
  pa_result <- fa.parallel(items, fa = "fa")
  fa_result <- fa(items, nfactors = pa_result$nfact, rotate = "oblimin", fm = "ml")
  }
  
  corr_results = function(vars, factor_data, prod_pooled, id) {
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
  } 
  else {
  ordinal_formula <- as.formula(paste(y, "~", paste(x_vars, collapse = " + "), "+ (1 |", id, ")"))
  ordinal_model <- clmm(ordinal_formula, data = factor_data)
  }
#output and format main results
  coef_table <- coef(summary(ordinal_model))
  p_values <- pnorm(abs(coef_table[, "t value"]), lower.tail = FALSE) * 2
  coef_table <- cbind(coef_table, p_value = p_values)

  ordinal_stats <- as.data.frame(coef_table)
  ordinal_stats$term <- rownames(ordinal_stats)

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
  # Create table output; filter to just the predictor rows, using the same dynamic f_vars list
  stats_predictors_only <- ordinal_stats %>%
    filter(term %in% x_vars)
    }
