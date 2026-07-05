# Pain of Payment Study 5
# Author: Amanda
# Date: 2026-07-04

library(tidyverse)
library(ordinal)

# Load data, ignoring the two description rows after the header
df <- read_csv("painofpayment/data/PoPv2_6.21.2026_July+3,+2026_14.48.csv") %>%
  dplyr::slice(-c(1, 2)) %>%
  type_convert()
  
# Explore
glimpse(df)

# Pass attention check? (var)
df <- df %>%
  mutate(attentive = ifelse(Attention_1 == 1 & Attention_3 == 2 & 
  Attention_4 == 2 & Attention_5 == 1, 1, 0))

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
#keeping only relevant columns, pivot the data into long format
df_long <- df %>%
  select(ResponseId, Age, Gender, attentive, all_of(all_names)) %>%
  pivot_longer(
    cols = all_of(all_names),
    names_to = c("Category", ".value"),
    names_sep = "_"
  )
#
#check to see how correlated tangibility no control and meanstoend are, lining up responseID and category type
#or, for every category (random effect), estimate a model of tangibility based on control pairing within person...then average across categories


#run an ordinal logistic regression of PoP on potential mechanisms
#incl random effects at the individual level 
model_data <- df_long %>%
  filter(attentive == 1)
model_data$PoP <- as.ordered(model_data$PoP)
model <- clmm(PoP ~ tangibility + nocontrol + meanstoend + (1 | ResponseId), 
              data = model_data)
summary(model)

