
# Pain of Payment Study 6
# Author: Amanda
#####Analysis

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)

source("painofpayment/FA Functions 2nd Analysis.R")
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



corr_dflong <- corr_results(c("painful_1", colnames(fa_prod1$scores),"ln_typ_charge",
"ln_disutility","purchased","value_1"), df_long_scored, FALSE, "ResponseId")

corr_prodlevel <- corr_results(c("painful_1", colnames(fa_prod1$scores),"ln_typ_charge",
"ln_disutility","purchased","value_1"), product_level_long_scored, TRUE, "ResponseId")

 
################################################################################
#Running regressions
################################################################################


full_xvars <- c(colnames(fa_prod1$scores), "value_1", "purchased", "ln_disutility")
partial_xvars <- c("f_worthless_alone", "f_unavoidable_req", "f_physical_product" )

reg_dflong <- reg_results(df_long_scored,"painful_1", full_xvars, FALSE, "ResponseId")
reg_prodlevel <- reg_results(product_level_long_scored,"painful_1", partial_xvars, TRUE, "ResponseId")


#######write output

dflong_output <- write_output(reg_dflong, corr_dflong, "painofpayment/output/Study_2_unpooled.csv", fa_object = fa_prod1)
prodlevel_output <- write_output(reg_prodlevel, corr_prodlevel, "painofpayment/output/Study_2_prodpooled.csv", fa_object = fa_prod1)
