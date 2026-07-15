# Pain of Payment Study 6
# Author: Amanda

library(MASS)
library(tidyverse)
library(psych)
library(ordinal)
library(rmcorr)

source("painofpayment/FA functions.R")

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

#print(factor_analysis(items_j)$model, cut = 0.5)  

person_level_long_kmo <- cbind(person_level_long, fa_j_highkmo)
df_long_kmo <- cbind(df_long, fa_ij_highkmo)
person_level_long_all <- cbind(person_level_long, fa_j_all)
df_long_all <- cbind(df_long, fa_ij_all)


#checking inter-factor correlations to avoid multicollinearity (+ check w/ PoP)

corr_j_highkmo <- corr_results(names(fa_j_highkmo), person_level_long_kmo, TRUE, "ResponseId")
corr_j_all <- corr_results(names(fa_j_all), person_level_long_all, TRUE, "ResponseId")
corr_ij_highkmo <- corr_results(names(fa_ij_highkmo), df_long_kmo, FALSE, "ResponseId")
corr_ij_all <- corr_results(names(fa_ij_all), df_long_all, FALSE, "ResponseId")


################################################################################
#Running regressions
################################################################################

reg_pooled_all <- reg_results(person_level_long_all,"painful_1", names(fa_j_all), TRUE, "ResponseId")
reg_pooled_kmo <- reg_results(person_level_long_kmo,"painful_1", names(fa_j_highkmo), TRUE, "ResponseId")
reg_RE_all <- reg_results(df_long_all,"painful_1", names(fa_ij_all), FALSE, "ResponseId")
reg_RE_kmo <- reg_results(df_long_kmo,"painful_1", names(fa_ij_highkmo), FALSE, "ResponseId")

#######writing outputs
corr_all_combined <- bind_rows(
  corr_j_highkmo   %>% mutate(analysis = "person_level_highkmo"),
  corr_j_all       %>% mutate(analysis = "person_level_all"),
  corr_ij_highkmo  %>% mutate(analysis = "person_product_highkmo"),
  corr_ij_all      %>% mutate(analysis = "person_product_all")
)

write.csv(corr_all_combined, "painofpayment/output/FA_correlation_results.csv", row.names = FALSE)

reg_all_combined <- bind_rows(
  reg_pooled_all %>% mutate(analysis = "pooled_all"),
  reg_pooled_kmo %>% mutate(analysis = "pooled_highkmo"),
  reg_RE_all     %>% mutate(analysis = "random_effects_all"),
  reg_RE_kmo     %>% mutate(analysis = "random_effects_highkmo")
)

write.csv(reg_all_combined, "painofpayment/output/FAregression_results.csv", row.names = FALSE)