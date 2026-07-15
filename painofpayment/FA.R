# Pain of Payment Study 6
# Author: Amanda

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)


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



################ETL###########################################
# Read raw header without deduplication, to preserve true positions
# of repeated column names (prod_1_1 through prod_1_46 appear 3 times)
raw_names <- read_csv("painofpayment/data/PoP+Factor+Analysis+10-16-25_July+9,+2026_17.33.csv", n_max = 0, name_repair = "minimal") %>% names()

#  Load the data & take out two junk rows/descriptions
df <- read_csv("painofpayment/data/PoP+Factor+Analysis+10-16-25_July+9,+2026_17.33.csv", name_repair = "minimal") 

# Find the position of each occurrence of prod_1_x across the 3 blocks
prod_items <- c(1:9, 11:46)
prod_pattern <- paste0("prod_1_", prod_items)

positions <- purrr::map(prod_pattern, ~ which(raw_names == .x))

block2_idx <- purrr::map_int(positions, 2)  # 2nd occurrence of each item
block3_idx <- purrr::map_int(positions, 3)  # 3rd occurrence of each item

# Rename block 2 and block 3 by position (block 1 stays as prod_1_x)
names(df)[block2_idx] <- paste0("prod_2_", prod_items)
names(df)[block3_idx] <- paste0("prod_3_", prod_items)

# take away two description rows and convert type
df <- df %>%
  dplyr::slice(-c(1, 2)) %>%
  type_convert()

# Rename the standalone Q-numbered columns
df <- df %>%
  rename(
    prod_2_painful_1 = Q587_1,
    prod_2_typ_charge = Q589,
    prod_2_fair_price = Q590,
    prod_2_like_1 = Q591_1,
    prod_2_value_1 = Q592_1,
    prod_2_purchased = Q593,
    prod_3_painful_1 = Q597_1,
    prod_3_typ_charge = Q599,
    prod_3_fair_price = Q600,
    prod_3_like_1 = Q601_1,
    prod_3_value_1 = Q602_1,
    prod_3_purchased = Q603
  )

# Create attention_check variable
df <- df %>%
  mutate(attention_check = ifelse(shark_screeners_1 == 1 & shark_screeners_3 == 0 & shark_screeners_4 == 0, 1, 0),
         attention_check = ifelse(is.na(attention_check), 0, attention_check))

#  Keep only relevant columns, filtered to attention_check == 1
df_final <- df %>%
  filter(attention_check == 1) %>%
  dplyr::select(ResponseId, age, gender, attention_check,
         starts_with("prod_1"), starts_with("prod_2"), starts_with("prod_3"),
        starts_with("Item"))

glimpse(df_final)

# Pivot into long format
# Prefix (prod_1/prod_2/prod_3) becomes "Category"
# Suffix (1-46, painful_1, typ_charge, fair_price, like_1, value_1, purchased) becomes its own column

pivot_cols <- df_final %>%
  dplyr::select(starts_with("prod_1"), starts_with("prod_2"), starts_with("prod_3")) %>%
  names()

df_long <- df_final %>%
  pivot_longer(
    cols = all_of(pivot_cols),
    names_to = c("Category", ".value"),
    names_pattern = "^(prod_[123])_(.*)$"
  )

glimpse(df_long)

# renaming variables 
df_long <- df_long %>%
  rename(
    too_high = `1`,
    overpriced = `2`,
    ripoff = `3`,
    expensive = `4`,
    worth_price = `5`,
    cheaper_not_different = `6`,
    budget_strain = `7`,
    needs_wealth = `8`,
    extra_money_to_spare = `9`,
    helps_other_things = `11`,
    worthless_alone = `12`,
    value_with_others = `13`,
    wouldnt_buy_if_not_required = `14`,
    should_come_included = `15`,
    unnecessary = `16`,
    have_choice = `17`,
    have_other_options = `18`,
    no_real_alternatives = `19`,
    negative_consequences_if_not = `20`,
    feel_powerless = `21`,
    cant_control_timing = `22`,
    purchase_regularly = `23`,
    buy_similar_soon = `24`,
    always_pretty_much_same = `25`,
    recurring_bill = `26`,
    very_aware_when_charged = `27`,
    always_have_to_pay = `28`,
    can_hold_touch = `29`,
    physical_product = `30`,
    can_physically_keep = `31`,
    use_long_period = `32`,
    lasts_a_while = `33`,
    feels_like_investment = `34`,
    businesses_give_away_free = `35`,
    people_dont_pay_money = `36`,
    expect_free = `37`,
    should_be_free = `38`,
    roughly_the_same_anywhere = `39`,
    many_different_types = `40`,
    unsure_long_term_benefit = `41`,
    unsure_before_using = `42`,
    learn_quality_after_purchase = `43`,
    saves_time = `44`,
    makes_life_easier = `45`,
    makes_life_more_convenient = `46`
  )

######## Pooling by product (person-level j analysis)##########
#create person-level df
person_level_long <- df_long %>%
  dplyr::select(-Item, -Item2, -Item3, -Category) %>%
  group_by(ResponseId) %>%
  summarize(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), ~ first(.x)),
    .groups = "drop"
  )
################################################################################
#Factor analysis across product i and person j (multiple i per j)#
################################################################################

# select items to run factor analysis on person_level or person-product level #
items_j <- person_level_long %>%
  dplyr::select(too_high:makes_life_more_convenient)  
items_ij <- df_long %>%
  dplyr::select(too_high:makes_life_more_convenient)  

#running factor analysis w/ or w/out low kmo results
low_kmo_j <- KMO_results(items_j, 0.7)
low_kmo_ij <- KMO_results(items_ij, 0.7)
items_j_highkmo <- items_j[, !(names(items_j) %in% low_kmo_j)]
items_ij_highkmo <- items_ij[, !(names(items_ij) %in% low_kmo_ij)]

fa_j_all <- as.data.frame(factor_analysis(items_j)$scores)
fa_j_highkmo <- as.data.frame(factor_analysis(items_j_highkmo)$scores)
fa_ij_all <- as.data.frame(factor_analysis(items_ij)$scores)
fa_ij_highkmo <- as.data.frame(factor_analysis(items_ij_highkmo)$scores)

#######check this######
fa_j_highkmo$communality
fa_j_all$communality
person_level_long <- person_level_long %>%
  bind_cols(factor_scores) %>%
  rename(
    f_convenience = ML1,
    f_choice = ML8,
    f_expectfree = ML2,
    f_overpriced = ML4,
    f_tangible = ML6,
    f_longlasting = ML3,
    f_budget_strain = ML7,
    f_complementary = ML5
  )
#checking inter-factor correlations to avoid multicollinearity (+ check w/ PoP)

vars_j_highkmo <- grep("^hf_", names(person_level_long), value = TRUE)
vars_j_all <- grep("^f_", names(person_level_long), value = TRUE)
vars_ij_highkmo <- grep("^hf_", names(df_long), value = TRUE)
vars_ij_all <- grep("^f_", names(df_long), value = TRUE)

corr_j_highkmo <- corr_results(vars_j_highkmo, person_level_long, TRUE, ResponseId)
corr_j_all <- corr_results(vars_j_all, person_level_long, TRUE, ResponseId)
corr_ij_highkmo <- corr_results(vars_ij_highkmo, df_long, FALSE, ResponseId)
corr_ij_all <- corr_results(vars_ij_all, df_long, FALSE, ResponseId)

write_csv(corr_summary, "painofpayment/output/Study6_j_corrs.csv")

################################################################################
#Running regressions
################################################################################

reg_pooled_all <- reg_results(person_level_long,painful_1, vars_j_all, TRUE, ResponseId)
reg_pooled_kmo <- reg_results(person_level_long,painful_1, vars_j_highkmo, TRUE, ResponseId)
reg_RE_all <- reg_results(df_long,painful_1, vars_ij_all, FALSE, ResponseId)
reg_RE_kmo <- reg_results(df_long,painful_1, vars_ij_highkmo, FALSE, ResponseId)


write_csv(ordinal_stats_predictors_only, "painofpayment/output/Study6_j_model.csv")


