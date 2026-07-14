# Pain of Payment Study 6
# Author: Amanda

library(MASS)
library(tidyverse)
library(psych)


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

#for factor analysis, average scores within person across products
person_level_long <- df_long %>%
  dplyr::select(-Item, -Item2, -Item3, -Category) %>%
  group_by(ResponseId) %>%
  summarize(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), ~ first(.x)),
    .groups = "drop"
  )

glimpse(person_level_long)

# run factor analysis on person_level data

items <- person_level_long %>%
  dplyr::select(too_high:makes_life_more_convenient)  
KMO(items)
cortest.bartlett(items)
fa.parallel(items, fa = "fa")
fa_result <- fa(items, nfactors = 8, rotate = "oblimin", fm = "ml")
print(fa_result, cut = 0.55, sort = TRUE)
fa_result$communality

####look at this later
factor_scores <- as.data.frame(fa_result$scores)

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
#these should be RM corrs***
vars <- c("painful_1", grep("^f_", names(person_level_long), value = TRUE))
pairs <- combn(vars, 2, simplify = FALSE)

corr_results <- list()

for (pair in pairs) {
  var1 <- pair[1]
  var2 <- pair[2]
  pair_name <- paste0(var1, "_", var2)
  
  result <- cor.test(person_level_long[[var1]], person_level_long[[var2]])
  
  corr_results[[pair_name]] <- result
}

#save as csv a table showing corr results
corr_summary <- map_dfr(corr_results, ~ tibble(
  r = .x$estimate,
  df = .x$parameter,
  p_value = .x$p.value
), .id = "pair")

corr_summary <- corr_summary %>%
  mutate(
    p_value = case_when(
      p_value < .001 ~ "p<.001",
      TRUE ~ paste0("p=", round(p_value, 3))
    ),
    r = round(r, 3)
  )

corr_summary %>% as.data.frame()
write_csv(corr_summary, "painofpayment/output/Study6corr_summary.csv")

#running reg 
model_data <- person_level_long %>%
  mutate(painful_1 = as.ordered(painful_1))

# Dynamically grab every column starting with "f_"
f_vars <- grep("^f_", names(model_data), value = TRUE)

# Build the formula using all f_ predictors
formula_str <- paste("painful_1 ~", paste(f_vars, collapse = " + "))
ordinal_formula <- as.formula(formula_str)

ordinal_model <- polr(ordinal_formula, data = model_data, Hess = TRUE)

summary(ordinal_model)

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

# Filter to just the predictor rows, using the same dynamic f_vars list
ordinal_stats_predictors_only <- ordinal_stats %>%
  filter(term %in% f_vars)

dir.create("painofpayment/output", recursive = TRUE, showWarnings = FALSE)

write_csv(ordinal_stats_predictors_only, "painofpayment/output/Study6ordinal_model_stats.csv")