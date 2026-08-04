
# Pain of Payment Study 6
# Author: Amanda
#####Analysis

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)

source("painofpayment/FA Functions.R")
source("painofpayment/FA ETL.R")

################################################################################
#Factor analysis across product i and person j (multiple i per j)#
################################################################################

# select items to run factor analysis on person_level or person-product level #
# items_j <- person_level_long %>%
#   dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
#   -typ_charge, -fair_price, -like_1, -value_1, -purchased)  
items_ij <- df_long %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -like_1, -value_1, -purchased, -Item, -Item2, 
  -Item3, -Category )  
items_prod1 <- df_long %>%
  dplyr::filter(Category == "prod_1") %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -like_1, -value_1, -purchased, -Item, -Item2,
  -Item3, -Category)
#running factor analysis w/ or w/out low kmo results
# low_kmo_j <- KMO_results(items_j, 0.7)
# low_kmo_ij <- KMO_results(items_ij, 0.7)
# items_j_highkmo <- items_j[, !(names(items_j) %in% low_kmo_j)]
# items_ij_highkmo <- items_ij[, !(names(items_ij) %in% low_kmo_ij)]


# fa_j_all <- as.data.frame(factor_analysis(items_j)$scores)
# # fa_j_highkmo <- as.data.frame(factor_analysis(items_j_highkmo)$scores)
# fa_ij_all <- as.data.frame(factor_analysis(items_ij)$scores)

fa_prod1 <- factor_analysis(items_prod1)
#applying the factor loadings to all of the items (not just product 1)
scores_full <- psych::factor.scores(items_ij, fa_prod1)$scores

# fa_ij_highkmo <- as.data.frame(factor_analysis(items_ij_highkmo)$scores)

#print(factor_analysis(items_ij)$model, cut = 0.5)  

# person_level_long_kmo <- cbind(person_level_long, fa_j_highkmo)
# df_long_kmo <- cbind(df_long, fa_ij_highkmo)
# person_level_long_all <- cbind(person_level_long, fa_j_all)
# df_long_all <- cbind(df_long, fa_ij_all)
df_long_scored <- df_long %>%
  dplyr::bind_cols(as.data.frame(scores_full))


#checking inter-factor correlations to avoid multicollinearity (+ check w/ PoP)

# corr_j_highkmo <- corr_results(names(fa_j_highkmo), person_level_long_kmo, TRUE, "ResponseId")
#corr_j_all <- corr_results(names(fa_j_all), person_level_long_all, TRUE, "ResponseId")
# corr_ij_highkmo <- corr_results(names(fa_ij_highkmo), df_long_kmo, FALSE, "ResponseId")
#corr_ij_all <- corr_results(names(fa_ij_all), df_long_all, FALSE, "ResponseId")

corr_prod1fa <- corr_results(c("painful_1", colnames(fa_prod1$scores),"ln_typ_charge",
"ln_disutility","like_1","value_1"), df_long_scored, FALSE, "ResponseId")


# df_select <- df_long_scored %>%
#   dplyr::select(dplyr::all_of(c("painful_1", colnames(fa_prod1$scores))))

# cor_matrix <- cor(df_select, use = "pairwise.complete.obs")
# round(cor_matrix, 2)
# cor_df <- as.data.frame(cor_matrix) %>%
#   tibble::rownames_to_column(var = "variable")
# write.csv(cor_df, "correlation_matrix_prod1FA.csv", row.names = FALSE, na = "")


################################################################################
#Running regressions
################################################################################

#reg_pooled_all <- reg_results(person_level_long_all,"painful_1", names(fa_j_all), TRUE, "ResponseId")
# reg_pooled_kmo <- reg_results(person_level_long_kmo,"painful_1", names(fa_j_highkmo), TRUE, "ResponseId")
#reg_RE_all <- reg_results(df_long_all,"painful_1", names(fa_ij_all), FALSE, "ResponseId")
# reg_RE_kmo <- reg_results(df_long_kmo,"painful_1", names(fa_ij_highkmo), FALSE, "ResponseId")

ind_vars <- c(colnames(fa_prod1$scores), "value_1", "purchased", "ln_disutility")
#drop <- c( "f_expect_free") 
#ind_vars <- ind_vars[!(ind_vars %in% drop)]
reg_RE_prod1fa <- reg_results(df_long_scored,"painful_1", ind_vars, FALSE, "ResponseId")


#######writing outputs

loadings_matrix <- unclass(fa_prod1$loadings)
loadings_df <- as.data.frame(loadings_matrix) %>%
  tibble::rownames_to_column(var = "item")

# Pull the McFadden R^2 off the regression output and make it its own row
  r2_prod1fa <- tibble::tibble(
  term = "McFadden's pseudo-R2",
  Value = attr(reg_RE_prod1fa, "mcfadden_r2"),
  analysis = "ordinal logistic regression, person REs"
)

FA_onProd1_output <- bind_rows(
  loadings_df %>% mutate (analysis = "factor anaysis"),
  corr_prod1fa %>% mutate(analysis = "factor score correlations"),
  reg_RE_prod1fa %>% mutate(analysis = "ordinal logistic regression, person REs"),
  r2_prod1fa
)

write.csv(FA_onProd1_output, "painofpayment/output/1.drop_expectfree.csv", row.names = FALSE, na = "")
#write.csv(FA_onProd1_output, "painofpayment/output/FAonProd1_output.csv", row.names = FALSE, na = "")



# corr_all_combined <- bind_rows(
#   #corr_j_highkmo   %>% mutate(analysis = "person_level_highkmo"),
#   corr_j_all       %>% mutate(analysis = "person_level_all"),
#   #corr_ij_highkmo  %>% mutate(analysis = "person_product_highkmo"),
#   corr_ij_all      %>% mutate(analysis = "person_product_all"),
#   corr_prod1fa %>% mutate(analysis = "person_product_FAonProd1")
# )

# write.csv(corr_all_combined, "painofpayment/output/FA_correlation_results.csv", row.names = FALSE)

# reg_all_combined <- bind_rows(
#   reg_pooled_all %>% mutate(analysis = "pooled_all"),
#  # reg_pooled_kmo %>% mutate(analysis = "pooled_highkmo"),
#   reg_RE_all     %>% mutate(analysis = "RE_FAonAllProds"),
#   # reg_RE_kmo     %>% mutate(analysis = "random_effects_highkmo")
#   reg_RE_prod1fa %>% mutate(analysis = "RE_FAonProd1")
# )

# write.csv(reg_all_combined, "painofpayment/output/FAregression_results.csv", row.names = FALSE)

