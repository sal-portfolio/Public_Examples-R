library(tidyverse)

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
  select(ResponseId, age, gender, attention_check,
         starts_with("prod_1"), starts_with("prod_2"), starts_with("prod_3"),
        starts_with("Item"))

glimpse(df_final)

# Pivot into long format
# Prefix (prod_1/prod_2/prod_3) becomes "Category"
# Suffix (1-46, painful_1, typ_charge, fair_price, like_1, value_1, purchased) becomes its own column

pivot_cols <- df_final %>%
  select(starts_with("prod_1"), starts_with("prod_2"), starts_with("prod_3")) %>%
  names()

df_long <- df_final %>%
  pivot_longer(
    cols = all_of(pivot_cols),
    names_to = c("Category", ".value"),
    names_pattern = "^(prod_[123])_(.*)$"
  )

glimpse(df_long)