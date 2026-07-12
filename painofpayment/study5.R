# Pain of Payment Study 5
# Author: Amanda
# Date: 2026-07-04

library(tidyverse)
library(ordinal)
library(rmcorr)


# Load data, ignoring the two description rows after the header
df <- read_csv("painofpayment/data/PoPv2_6.21.2026_July+3,+2026_14.48.csv") %>%
  dplyr::slice(-c(1, 2)) %>%
  type_convert()
  
# Explore
glimpse(df)


#rename pain of payment vars
df <- df %>%
  rename(HotelWifiQuestion_PoP = PoPGeneralQuestion_1, InflightWifiQuestion_PoP = PoPGeneralQuestion_2,
  ParkingQuestion_PoP = PoPGeneralQuestion_3, HotelBFquestion_PoP = PoPGeneralQuestion_4,
  AppetizerQuestion_PoP = PoPGeneralQuestion_5, PrinterInkQuestion_PoP = PoPGeneralQuestion_6,
  CargasQuestion_PoP = PoPGeneralQuestion_7, Fragrance_PoP = PoPGeneralQuestion_8)

#Based on prelim analysis (high inter-item corr), create composite scores
all_names <- c()
for (prefix in c("HotelWifiQuestion", "InflightWifiQuestion", "ParkingQuestion",
"HotelBFquestion", "AppetizerQuestion", "PrinterInkQuestion", "CargasQuestion", "Fragrance")) {
  concrete <- paste0(prefix, "_1")
  touch <- paste0(prefix, "_2")
  unavoidable <- paste0(prefix, "_3")
  norealchoice <- paste0(prefix, "_4")
  notvalued <- paste0(prefix, "_5")
  notenjoyed <- paste0(prefix, "_6")
  
  df[[paste0(prefix, "_tangibility")]] <- rowMeans(df[, c(concrete, touch)], na.rm = TRUE)
  df[[paste0(prefix, "_nocontrol")]] <- rowMeans(df[, c(unavoidable, norealchoice)], na.rm = TRUE)
  df[[paste0(prefix, "_meanstoend")]] <- rowMeans(df[, c(notvalued, notenjoyed)], na.rm = TRUE)
  
  all_names <- c(all_names, paste0(prefix, "_tangibility"), 
  paste0(prefix, "_nocontrol"), paste0(prefix, "_meanstoend"),
  paste0(prefix, "_PoP"))
}

# Pass attention check? (var)
df <- df %>%
  mutate(attentive = ifelse(Attention_1 == 1 & Attention_3 == 2 & 
  Attention_4 == 2 & Attention_5 == 1, 1, 0))

#keeping only relevant columns (& filtering by attn check), pivot the data into long format
df_long <- df %>%
  filter(attentive == 1) %>%
  select(ResponseId, Age, Gender, attentive, all_of(all_names)) %>%
  pivot_longer(
    cols = all_of(all_names),
    names_to = c("Category", ".value"),
    names_sep = "_"
  )
#Double-check the coding/reverse-coding
#PoP - as written in survey, lower numbers mean more pain, so we need to reverse code
#To stay consistent with past tests, we'll reverse code nocontrol--> control
#tangibility and meanstoend are fine per survey (higher number, more tangible/meanstoend)
df_long <- df_long %>%
  mutate(PoP = 6 - PoP,
  control = 6 - nocontrol)

#check correlations of main vars (pooling across categories, within-person)
vars <- c("tangibility", "control", "meanstoend", "PoP")
pairs <- combn(vars, 2, simplify = FALSE)

rmcorr_results <- list()

for (pair in pairs) {
  var1 <- pair[1]
  var2 <- pair[2]
  pair_name <- paste0(var1, "_", var2)
  
  result <- rmcorr(participant = ResponseId, 
                    measure1 = df_long[[var1]], 
                    measure2 = df_long[[var2]], 
                    dataset = df_long)
  
  rmcorr_results[[pair_name]] <- result
}

#save as csv a table showing corr results
rmcorr_summary <- map_dfr(rmcorr_results, ~ tibble(r = .x$r, df = .x$df, p_value = .x$p), .id = "pair")
rmcorr_summary <- rmcorr_summary %>%
  mutate(p_value = case_when(
    p_value < .001 ~ "p<.001",
    TRUE ~ paste0("p=", round(p_value, 3))), 
  r = round(r,3))

rmcorr_summary %>% as.data.frame()
write_csv(rmcorr_summary, "rmcorr_summary.csv")

#run an ordinal logistic regression of PoP on potential mechanisms
#incl random effects at the individual level 
df_long$PoP <- as.ordered(df_long$PoP)
model <- clmm(PoP ~ tangibility + meanstoend + (1 | ResponseId), 
              data = df_long)
summary(model)

#output a csv file of main results
model_summary <- summary(model)
coef_table <- as.data.frame(model_summary$coefficients)
coef_table$term <- rownames(coef_table)

predictor_stats <- coef_table %>%
  filter(term %in% c("tangibility", "meanstoend")) %>%
  select(term, Estimate, `Std. Error`, `Pr(>|z|)`) %>%
  mutate(
    Estimate = round(Estimate, 3),
    `Std. Error` = round(`Std. Error`, 3),
    `Pr(>|z|)` = case_when(
      `Pr(>|z|)` < .001 ~ "p<.001",
      TRUE ~ as.character(round(`Pr(>|z|)`, 3))
    )
  )

write_csv(predictor_stats, "reg_stats.csv")

lnull_model <- clmm(PoP ~ 1 + (1 | ResponseId), data = df_long)
logLik_full <- as.numeric(logLik(model))
logLik_null <- as.numeric(logLik(null_model))
n <- nobs(model)

# Cox & Snell R2
r2_cs <- 1 - exp((2/n) * (logLik_null - logLik_full))

# Nagelkerke R2 (adjusts Cox & Snell to max out at 1)
r2_nagelkerke <- r2_cs / (1 - exp((2/n) * logLik_null))

r2_nagelkerke