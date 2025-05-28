library(dplyr)
library(ggplot2)
library(rdrobust)
library(modelsummary)
library(stargazer)
library(tidyr)
library(rio)
library(janitor)
library(lubridate)
library(pdftools)
library(stringr)
library(readxl)
#### CONTRACTUAL DATA
#Transaction Data preperation for merging - the data is orginally transaction level and not contract meaning that I had to summarise the data
award_level <- transaction_usagov %>%
  group_by(award_id_piid) %>%
  summarise(
    total_federal_action_obligation = sum(federal_action_obligation, na.rm = TRUE),
    total_dollars_obligated = sum(total_dollars_obligated, na.rm = TRUE),
    total_outlayed = sum(total_outlayed_amount_for_overall_award, na.rm = TRUE),
    
    first_action_date = min(as.Date(action_date), na.rm = TRUE),
    last_end_date = max(as.Date(period_of_performance_current_end_date), na.rm = TRUE),
    
    most_common_contract_type = {
      type_table <- table(type_of_contract_pricing)
      if (length(type_table) == 0) NA_character_ else names(sort(type_table, decreasing = TRUE))[1]
    },
    
    num_modifications = n_distinct(modification_number),
    num_transactions = n()
  )



# Standardize column names to lowercase and underscores
names(award_level) <- tolower(gsub(" ", "_", names(award_level)))
names(prime_2020) <- tolower(gsub(" ", "_", names(prime_2020)))

# Example for key columns; repeat as needed
award_level <- award_level %>%
  mutate(across(where(is.character), ~str_trim(.))) %>%
  mutate(across(where(is.character), tolower))

prime_2020 <- prime_2020 %>%
  mutate(across(where(is.character), ~str_trim(.))) %>%
  mutate(across(where(is.character), tolower))

# Replace 'action_date' and 'obligation' with actual column names
colnames(award_level)
colnames(prime_2020)

award_level <- award_level %>%
  mutate(action_date = ymd(first_action_date),
         federal_action_obligation = as.numeric(total_federal_action_obligation))

prime_2020 <- prime_2020 %>%
  mutate(period_of_performance_start_date = ymd(period_of_performance_start_date),
         total_obligation = as.numeric(total_obligated_amount))

award_level <- award_level %>%
  distinct(award_id_piid, .keep_all = TRUE)

prime_2020 <- prime_2020 %>%
  distinct(award_id_piid, .keep_all = TRUE)

usagov <- award_level %>%
  left_join(prime_2020, by = "award_id_piid")

  
###INSTITUTIONAL QUALITY
## GOA
# gao21_dta<- import("gao_high_risk_list_2021.csv")
# # gao23_dta<- import("gao_high_risk_list_2023.csv")
# 
# gao21_dta <- gao21_dta %>%
#   mutate(risk_score = case_when(
#     Change_Status == "Progress" ~ 1,
#     Change_Status == "No Change" ~ 0,
#     Change_Status == "Decline" ~ -1,
#     TRUE ~ NA_real_))
# 
# #rename high risk area to agency name
# names(gao21_dta)[1] <- "agency_name"

# gao23_dta <- gao23_dta %>%
#   mutate(risk_score = case_when(
#     Change_Status == "Progress" ~ 1,
#     Change_Status == "No Change" ~ 0,
#     Change_Status == "Decline" ~ -1,
#     TRUE ~ NA_real_))
# 
# gao21_dta$year <- 2021
# gao23_dta$year <- 2023
# 
# gao2020 <- gao21_dta
# gao2020$year <- 2020
# 
# gao2022 <- gao23_dta
# gao2022$year <- 2022
# 
# gao_all_years <- bind_rows(gao2020, gao21_dta, gao2022, gao23_dta)

##Best place to work data-set 
BPTW <- import("BPTW.xlsx")

names(BPTW)[2] <- "agency_name"
names(BPTW)[3] <- "2021_score"
names(BPTW)[4] <- "2020_score"
names(BPTW)[5] <- "agency_size"

#dataset imported weird so it was adjusted
BPTW <- BPTW[-1, ]
BPTW <- BPTW[ , -1]  

BPTW$"2020_score"=factor(BPTW$"2020_score")
BPTW$"2020_score" <- as.numeric(BPTW$"2020_score")
summary(BPTW$"2020_score")

BPTW$"2021_score"=factor(BPTW$"2021_score")
BPTW$"2021_score" <- as.numeric(BPTW$"2021_score")
summary(BPTW$"2021_score")


BPTW$`2021_score_normalized` <- round(BPTW$`2021_score` / 100, 3)
BPTW$`2020_score_normalized` <- round(BPTW$`2020_score` / 100, 3)

# Create a new column for the change in score
BPTW <- BPTW %>%
  mutate(bptw_score_change = `2021_score_normalized` - `2020_score_normalized`)

BPTW <- BPTW[ , -2] #repeat 2 times 

### USE CASE DATA
#Data prep for merging on GSA 
colnames(SAM_)

# Keep only rows where classification is "Firm"
df_firm <- subset(SAM_, Classification == "Firm")

df_firm$Creation_Date <- as.Date(df_firm$Creation_Date, format = "%Y-%m-%d")

df_clean <- df_firm[!duplicated(df_firm[c("Name")]), ]

#Prep for data merging 
df_clean <- df_clean %>% janitor::clean_names()

df_clean$country <- toupper(df_clean$country) # Make sure country codes are upper case
df_clean$state_province <- toupper(df_clean$state_province)

# df_clean <-df_clean[ , -3] #run this line for times to remove column with null obs


#Merging all the datasets into one
#Clean agency name in usagov and BPTW
usagov <- usagov %>%
  mutate(agency_name = str_to_lower(str_trim(awarding_sub_agency_name)))  # or correct column

BPTW <- BPTW %>%
  mutate(agency_name = str_to_lower(str_trim(agency_name)))

# Clean firm names in usagov and df_clean
usagov <- usagov %>%
  mutate(recipient_uei = str_to_lower(str_trim(recipient_uei)))  # or vendor name column

df_clean <- df_clean %>%
  mutate(recipient_uei = str_to_lower(str_trim(unique_entity_id)))

usagov_bptw <- usagov %>%
  left_join(BPTW, by = "agency_name")

#Not doing a full join because we just want to indicate if they are on the list or not
usagov_combined <- usagov_bptw %>%
  mutate(is_excluded_vendor = recipient_uei %in% df_clean$recipient_uei)

table(usagov_combined$is_excluded_vendor)

write.csv(data.frame(usagov_combined),
          "usagov_combined.csv",
          row.names = FALSE)

# Setting up the Model 

bptw_threshold <- quantile(usagov_combined$"2020_score_normalized", 0.25, na.rm = TRUE)

colnames(usagov_combined)
