
# Pain of Payment Study 6
# Author: Amanda
##This file creates functions for running factor analysis, inter-factor corrs, and regressions
## at the individual-product level (ij) 

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



  
  corr_results <- function(vars, factor_data, id) {
  pairs <- combn(vars, 2, simplify = FALSE)
  corr_results <- list()
  for (pair in pairs) {
    var1 <- pair[1]
    var2 <- pair[2]
    pair_name <- paste0(var1, "_", var2)
    result <- rmcorr(participant = factor_data[[id]], 
                    measure1 = factor_data[[var1]], 
                    measure2 = factor_data[[var2]], 
                    dataset = factor_data)
    corr_results[[pair_name]] <- result
  }
  #save as csv a table showing corr results

  corr_summary <- map_dfr(corr_results, function(x) {
      tibble(r = x$r, df = x$df, p_value = x$p)
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
  

reg_results = function(factor_data, y, x_vars, id) {
    # ---- Ordinal mixed model: respondent-level data, clustered by id ----
    factor_data[[y]] <- as.ordered(factor_data[[y]])

    ordinal_formula <- as.formula(paste(y, "~", paste(x_vars, collapse = " + "), "+ (1 |", id, ")"))
    full_model <- clmm(ordinal_formula, data = factor_data,
                  control = clmm.control(maxIter = 1000, gradTol = 1e-6, maxLineIter = 100))

    ###compare against a null model to estimate a proxy of r2
    null_formula <- as.formula(paste(y, "~ 1 + (1 |", id, ")"))
    null_model <- clmm(null_formula, data = factor_data,
                        control = clmm.control(maxIter = 1000, gradTol = 1e-6, maxLineIter = 100))
    
    ###save results
    coef_table <- coef(summary(full_model))
    coef_table <- cbind(coef_table, p_value = coef_table[, "Pr(>|z|)"])

    
    r2_value <- as.numeric(1 - logLik(full_model) / logLik(null_model))
    r2_type  <- "mcfadden_r2"
    model_label <- "Ordinal logistic regression with random per-person intercepts"
  

  reg_stats <- as.data.frame(coef_table)
  reg_stats$term <- rownames(reg_stats)
##in case of  different naming conventions in the package
  if ("Estimate" %in% names(reg_stats)) {
    reg_stats <- reg_stats %>% rename(Value = Estimate)
  }

  reg_stats <- reg_stats %>%
    dplyr::select(term, Value, `Std. Error`, p_value) %>%
    mutate(
      Value = round(Value, 3),
      `Std. Error` = round(`Std. Error`, 3),
      p_value = case_when(
        p_value < .001 ~ "p<.001",
        TRUE ~ paste0("p=", round(p_value, 3))
      )
    )

  out <- reg_stats %>% filter(term %in% x_vars)
  attr(out, "r2") <- round(r2_value, 3)
  attr(out, "r2_type") <- r2_type
  attr(out, "Model_type") <- model_label
  out
}

write_output <- function(reg_writing, corr_writing, filename, fa_object = fa_prod1) {

  loadings_matrix <- unclass(fa_object$loadings)
  loadings_df <- as.data.frame(loadings_matrix) %>%
    tibble::rownames_to_column(var = "item")

  # Pull the R^2 off the regression output and make it its own row
  r2_data <- tibble::tibble(
    term     = attr(reg_writing, "r2_type"),
    Value    = attr(reg_writing, "r2"),
    analysis = attr(reg_writing, "Model_type")
  )

  output <- bind_rows(
    loadings_df %>% mutate(analysis = "factor analysis"),
    corr_writing %>% mutate(analysis = "factor score correlations"),
    reg_writing %>% mutate(analysis = attr(reg_writing, "Model_type")),
    r2_data
  )

  write.csv(output, filename, row.names = FALSE, na = "")

  invisible(output)
}