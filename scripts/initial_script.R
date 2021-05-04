library("tidyverse")
library("here")

controls <- read_csv(here("raw_data", "controls.csv")) %>%
  drop_na(LocalPatientIdentifier)

eligible_controls <- controls %>%
  rename("mrn" = LocalPatientIdentifier,
         "postcode" = Postcode,
         "current_daily" = CurrentEveryDaySmoker,
         "current_intermittent" = "Current Some Day Smoker",
         "former" = "Former Smoker",
         "heavy_smoker" = "Heavy Tobacco Smoker",
         "light_smoker" = "Light Tobacco Smoker",
         "other_smoker_status" = "Other Smoking Status",
         "episode_id" = EpisodeID,
         "icd_10" = DiagnosisCode) %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(EpisodeStartDate), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25,
         current_smoker = ifelse(current_daily == "Yes", "yes",
                                 ifelse(current_intermittent == "Yes", "yes",
                                        ifelse(heavy_smoker == "Yes", "yes",
                                               ifelse(light_smoker == "Yes", "yes", NA)))),
         former_smoker = ifelse(former == "Yes", "yes", NA),
         never_smoker = ifelse(other_smoker_status == "Never Smoker", "yes",
                               ifelse(other_smoker_status == "no", NA,
                                      ifelse(other_smoker_status == "Passive Smoke Exposure - Never Smoker", "yes",
                                             NA))),
         unknown_smoker = ifelse(other_smoker_status == "*Unknown", "yes",
                                 ifelse(other_smoker_status == "Unknown If Ever Smoked", "yes",
                                        ifelse(other_smoker_status == "Smoker, Current Status Unknown", "yes",
                                               NA))),
         never_assessed = ifelse(other_smoker_status == "Never Assessed", "yes",
                                 NA),
         sex = recode(Sex, "Female" = "female", "Male" = "male")) %>%
  select(mrn, birthdate, age_at_admission, admission_date, sex, icd_10, current_smoker, former_smoker, never_smoker, unknown_smoker, never_assessed) %>%
  filter(!age_at_admission < 18) %>%
  pivot_longer(cols = 7:11, names_to = "system_smoking_status", values_drop_na = T) %>%
  arrange(admission_date) 

interim_analysis_eligible <- eligible_controls %>%
  filter(admission_date >= "2019-01-01")

interim_analysis_ineligible_age <- controls %>%
  filter(!LocalPatientIdentifier %in% eligible_controls$mrn) %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(EpisodeStartDate), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25) %>%
  filter(admission_date >= "2019-01-01")

excluded_controls_age <- controls %>%
  filter(!LocalPatientIdentifier %in% eligible_controls$mrn) %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(EpisodeStartDate), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25)

write_csv(eligible_controls, here("raw_data", "data_collection_controls.csv"))

cases <- read_csv(here("raw_data", "cases.csv")) %>%
  drop_na(Primary_MRN)

eligible_cases <- cases %>%
  rename("mrn" = Primary_MRN,
         "postcode" = Postcode,
         "sex" = Sex,
         "current_daily" = CurrentEveryDaySmoker,
         "current_intermittent" = "Current Some Day Smoker",
         "former" = "Former Smoker",
         "heavy_smoker" = "Heavy Tobacco Smoker",
         "light_smoker" = "Light Tobacco Smoker",
         "other_smoker_status" = "Other Smoking Status") %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(IP_Admission_Dttm), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25,
         current_smoker = ifelse(current_daily == "Yes", "yes",
                                 ifelse(current_intermittent == "Yes", "yes",
                                        ifelse(heavy_smoker == "Yes", "yes",
                                               ifelse(light_smoker == "Yes", "yes", NA)))),
         former_smoker = ifelse(former == "Yes", "yes", NA),
         never_smoker = ifelse(other_smoker_status == "Never Smoker", "yes",
                               ifelse(other_smoker_status == "no", NA,
                                      ifelse(other_smoker_status == "Passive Smoke Exposure - Never Smoker", "yes",
                                             NA))),
         unknown_smoker = ifelse(other_smoker_status == "*Unknown", "yes",
                                 ifelse(other_smoker_status == "Unknown If Ever Smoked", "yes",
                                        ifelse(other_smoker_status == "Smoker, Current Status Unknown", "yes",
                                               NA))),
         never_assessed = ifelse(other_smoker_status == "Never Assessed", "yes",
                                 NA),
         sex = recode(sex, "Female" = "female", "Male" = "male")) %>%
  filter(!age_at_admission < 18) %>%
  select(mrn, birthdate, age_at_admission, admission_date, sex, current_smoker, former_smoker, never_smoker, unknown_smoker, never_assessed) %>%
  pivot_longer(cols = 6:10, names_to = "smoking_status", values_drop_na = T) %>%
  arrange(admission_date)

excluded_cases <- cases %>%
  filter(!Primary_MRN %in% eligible_cases$mrn)  %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(IP_Admission_Dttm), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25)

interim_analysis_cases <- eligible_cases %>%
  filter(admission_date <= "2020-04-08")

interim_analysis_ineligible_cases <- cases %>%
  filter(!Primary_MRN %in% eligible_cases$mrn) %>%
  mutate(birthdate = as.Date(as.character(BirthDate), "%d/%m/%Y"),
         admission_date = as.Date(as.character(IP_Admission_Dttm), "%d/%m/%Y"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25) %>%
  filter(admission_date <= "2020-04-08")

  
write_csv(eligible_cases, here("raw_data", "data_collection_cases.csv"))
