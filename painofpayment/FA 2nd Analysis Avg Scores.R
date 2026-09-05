
# Pain of Payment Study 6
# Author: Amanda
#####Analysis

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)

source("painofpayment/Functions 2nd analysis Avg Scores.R")
source("painofpayment/ETL 2nd factor analysis Avg scores.R")

################################################################################
#Factor analysis across product i and person j (multiple i per j)#
################################################################################

 
items_prod1 <- df_long %>%
  dplyr::filter(Category == "prod_1") %>%
  dplyr::select(-ResponseId, -age, -gender, -attention_check, -painful_1,
  -typ_charge, -fair_price, -ln_disutility, -ln_typ_charge, -value_1, -purchased, -Item, -Item2,
  -Item3, -Category)

fa_prod1 <- factor_analysis(items_prod1)



# For each factor, build the list of items loading >= 0.6 in absolute value,
# named after that factor's highest-loading item
loads <- unclass(loadings(fa_prod1))   
factor_item_lists <- list()

for (f in colnames(loads)) {
  col <- loads[, f]
  factor_item_lists[[f]] <- names(col)[abs(col) >= 0.6]
}

#taking the average (at the ij level) of each list of high-loading items, now for the entire data set (all 3 j products for every i person)
for (nm in names(factor_item_lists)) {
  items <- factor_item_lists[[nm]]
  df_long[[nm]] <- rowMeans(df_long[, items, drop = FALSE], na.rm = TRUE)
}



################################################################################
#Analyses
################################################################################

corr_dflong <- corr_results(c("painful_1", colnames(fa_prod1$scores),"ln_typ_charge",
"ln_disutility","purchased","value_1"), df_long, "ResponseId")

 
xvars <- c(colnames(fa_prod1$scores), "value_1", "purchased", "ln_disutility")
reg_dflong <- reg_results(df_long,"painful_1", xvars, "ResponseId")


#######write output

dflong_output <- write_output(reg_dflong, corr_dflong, "painofpayment/output/final.csv", fa_object = fa_prod1)
