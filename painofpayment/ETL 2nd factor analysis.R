
# Pain of Payment Study 6
# Author: Amanda
#####ETL Only####

data_name <- "painofpayment/data/PoP+Factor+Analysis+7-29-26_August+14,+2026_20.05.csv"
# Read raw header without deduplication, to preserve true positions
# of repeated column names (prod_1_1 through prod_1_46 appear 3 times)
raw_names <- read_csv(data_name, n_max = 0, name_repair = "minimal") %>% names()

#  Load the data & take out two junk rows/descriptions
df <- read_csv(data_name, name_repair = "minimal") 
#### weird naming conventions from Aug 14 survey
###which(names(df) == "prod_2_21")
###which(names(df) == "prod_1_2")
names(df)[23:23] <- gsub("_2", "_1", names(df)[23:23])
names(df)[24:43] <- gsub("prod_2", "prod_1", names(df)[24:43])
names(df)[53:53] <- gsub("prod_1_2", "prod_2_1", names(df)[53:53])
names(df)[87:87] <- gsub("prod_1_2", "prod_3_1", names(df)[87:87])
names(df)[88:107] <- gsub("prod_2", "prod_3", names(df)[88:107])

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
    prod_2_value_1 = Q592_1,
    prod_2_purchased = Q593,
    prod_3_painful_1 = Q597_1,
    prod_3_typ_charge = Q599,
    prod_3_fair_price = Q600,
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
    overpriced = `1`,
    more_expect = `2`,
    expensive = `3`,
    worthless_alone = `4`,
    use_withothers = `5`,
    enable_other = `6`,
    allows_something = `7`,
    have_choice = `8`,
    have_options = `9`,
    cannot_control = `10`,
    unavoidable_req = `11`,
    noreal_choice = `12`,
    hold_touch = `13`,
    physical_product = `14`,
    tangible_concrete = `15`,
    long_period = `16`,
    lasts_awhile = `17`,
    giveaway_free = `18`,
    usually_dontpay = `19`,
    expect_free = `20`,
    shouldbe_free = `21`
  )
df_long <- df_long %>% mutate(ln_disutility =  ifelse(fair_price > typ_charge, log(1), log(typ_charge - fair_price + 1)))
df_long <- df_long %>% mutate(ln_typ_charge = log(typ_charge)) 

# ### result filtering 
# drop_items <- c(
#   "too_high", "ripoff"
# )
# df_long <- df_long %>%
#   dplyr::select(-all_of(drop_items)
#   )


######## Pooling by product (product-level i analysis)##########
df_long <- df_long %>%
  mutate(item_focal = case_when(
    Category == "prod_1" ~ Item,
    Category == "prod_2" ~ Item2,
    Category == "prod_3" ~ Item3
  ))
product_level_long <- df_long %>%
  dplyr::select(-Item, -Item2, -Item3, -Category) %>%
  group_by(item_focal) %>%
  summarize(
    across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
    across(where(is.character), ~ first(.x)),
    .groups = "drop"
  )
