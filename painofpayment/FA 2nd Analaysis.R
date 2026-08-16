
# Pain of Payment Study 6
# Author: Amanda
#####Analysis

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)

source("painofpayment/FA Functions.R")
source("painofpayment/ETL 2nd factor analysis.R")

################################################################################
#Factor analysis across product i and person j (multiple i per j)#
################################################################################

 
items_prod1 <- df_long %>%
  dplyr::filter(Category == "prod_1") %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -value_1, -purchased, -Item, -Item2,
  -Item3, -Category, -item_focal)

fa_prod1 <- factor_analysis(items_prod1)

#applying the factor loadings to all of the items (not just product 1) using tenBerge method for weights


items_ij <- df_long %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -value_1, -purchased, -Item, -Item2, 
  -Item3, -Category, -item_focal)  
items_i <- product_level_long %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -value_1, -purchased, -item_focal)


scores_ij <- psych::factor.scores(items_ij, fa_prod1)$scores
scores_i <- psych::factor.scores(items_i, fa_prod1)$scores


df_long_scored <- df_long %>%
  dplyr::bind_cols(as.data.frame(scores_ij))

product_level_long_scored <- product_level_long %>%
  dplyr::bind_cols(as.data.frame(scores_i))



#monthsubset_df <- df_long_scored %>% filter(item_focal == "Monthly car payment")
#cor.test(monthsubset_df$painful_1, monthsubset_df$f_worthless_alone)


********Done through here
corr_prod1fa <- corr_results(c("painful_1", colnames(fa_prod1$scores),"ln_typ_charge",
"ln_disutility","like_1","value_1"), df_long_scored, FALSE, "ResponseId")



################################################################################
#Running regressions
################################################################################

#reg_pooled_all <- reg_results(person_level_long_all,"painful_1", names(fa_j_all), TRUE, "ResponseId")
# reg_pooled_kmo <- reg_results(person_level_long_kmo,"painful_1", names(fa_j_highkmo), TRUE, "ResponseId")
#reg_RE_all <- reg_results(df_long_all,"painful_1", names(fa_ij_all), FALSE, "ResponseId")
# reg_RE_kmo <- reg_results(df_long_kmo,"painful_1", names(fa_ij_highkmo), FALSE, "ResponseId")



ind_vars <- c(colnames(fa_prod1$scores), "value_1", "purchased", "ln_disutility")
drop <- c( "f_overpriced") 
ind_vars <- ind_vars[!(ind_vars %in% drop)]
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

write.csv(FA_onProd1_output, "painofpayment/output/1.drop_overpriced.csv", row.names = FALSE, na = "")
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

