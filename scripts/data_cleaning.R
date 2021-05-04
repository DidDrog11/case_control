library("tidyverse")
library("readxl")
library("here")
source(here("scripts", "split_multiple.r"))
source(here("scripts", "comorbidities.r"))

cases <- read_xlsx(here("raw_data", "cases_extracted.xlsx")) %>%
  mutate(cohort = "cases")
controls <- read_xlsx(here("raw_data", "controls_extracted.xlsx")) %>%
  mutate(cohort = "controls")

system_smoking_status <- bind_rows(
  read_csv(here("raw_data", "data_collection_cases.csv")) %>%
    mutate(cohort = "cases",
           icd_10 = "U07.1") %>%
    rename("system_smoking_status" = smoking_status),
  read_csv(here("raw_data", "data_collection_controls.csv")) %>%
    mutate(cohort = "controls")
)

data <- bind_rows(
  cases %>%
    select(mrn, birthdate, admission_date, sex, postcode, ethnicity, comorbidity, diagnosis, length_admission, smoking_status, pack_years, mortality, include, other, specimen_collection, cohort),
  controls %>%
    select(mrn, birthdate, admission_date, sex, postcode, ethnicity, comorbidity, diagnosis, length_admission, smoking_status, pack_years, mortality, include, other, specimen_collection, cohort))

data <- data %>%
  left_join(.,
            system_smoking_status %>%
              select(mrn, birthdate, age_at_admission, admission_date, system_smoking_status, cohort, icd_10)) %>%
  drop_na(include)


exclude_time_from_admission <- data %>%
  filter(!specimen_collection == is.na(specimen_collection)) %>%
  mutate(time_from_admission = as.numeric(difftime(specimen_collection, admission_date, units = "days")),
         time_from_admission = ifelse(is.na(time_from_admission), 0, time_from_admission),
         smoking_status = ifelse(smoking_status == "occasional_smoker", "current_smoker", smoking_status),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25) %>%
  filter(time_from_admission > 4)

exclude_no_smoking <- data %>%
  filter(smoking_status %in% c("no_record", "no_data")) 

exclude_other <- data %>%
  filter(include == "n" | other == "already_admitted") %>%
  anti_join(., exclude_time_from_admission, by = c("mrn", "birthdate", "admission_date")) %>%
  anti_join(., exclude_no_smoking, by = c("mrn", "birthdate", "admission_date"))

eligible_data <- data %>%
  anti_join(., exclude_time_from_admission, by = c("mrn", "birthdate", "admission_date")) %>%
  anti_join(., exclude_no_smoking, by = c("mrn", "birthdate", "admission_date")) %>%
  anti_join(., exclude_other, by = c("mrn", "birthdate", "admission_date")) %>%
  bind_cols(split_into_multiple(.$comorbidity, ", ", "comorbidity")) %>%
  mutate(mrn = as_factor(mrn),
         birthdate = as.Date(birthdate, "%Y-%m-%d"),
         admission_date = as.Date(admission_date, "%Y-%m-%d"),
         age_at_admission = as.numeric(difftime(admission_date, birthdate, units = "weeks"))/52.25,
         sex = as_factor(sex %>%
                           recode("m" = "male",
                                  "f" = "female")),
         postcode = as_factor(postcode),
         ethnicity = ifelse(is.na(ethnicity), "not_stated", ethnicity),
         ethnicity = as_factor(ethnicity),
         diagnosis = as_factor(diagnosis),
         smoking_status = as_factor(smoking_status),
         pack_years = as_factor(pack_years),
         mortality = as_factor(mortality),
         system_smoking_status = ifelse(is.na(system_smoking_status), "unknown_smoker", system_smoking_status),
         system_smoking_status = as_factor(system_smoking_status),
         cohort = as_factor(cohort),
         icd_10 = as_factor(icd_10)) %>%
  select(-c(comorbidity, include, specimen_collection))
         
eligible_data <- eligible_data %>%
  mutate(grouped_age = as_factor(cut(age_at_admission, breaks = c(18, 24, 34, 44, 54, 64, max(age_at_admission)))),
         mortality = fct_explicit_na(mortality, "alive"),
         cohort = relevel(cohort, ref = "controls"))


levels(eligible_data$smoking_status) <- c("never_smoker", "former_smoker", "never_smoker", "current_smoker", "current_smoker", "current_smoker")
levels(eligible_data$system_smoking_status) <- c("never_smoker", "former_smoker", "current_smoker", "unknown_smoker", "unknown_smoker")
levels(eligible_data$grouped_age) <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
levels(eligible_data$ethnicity) <- c("black_african", "white_british", "not_stated", "black_caribbean", "other_asian", "other_white", "other", "asian",
                                     "white_irish", "chinese", "mixed", "other", "asian", "asian", "mixed", "not_stated", "other", "mixed")

eligible_data$comorbidity_1[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_1[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_1 <- replace_na(eligible_data$comorbidity_1, "nil")
eligible_data$comorbidity_1[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_1[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_1[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_1[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_1[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_1[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_1[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_1[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_1, ignore.case=FALSE)] <- "renal"

eligible_data$comorbidity_2[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_2[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_2 <- replace_na(eligible_data$comorbidity_2, "nil")
eligible_data$comorbidity_2[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_2[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_2[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_2[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_2[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_2[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_2[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_2[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_2, ignore.case=FALSE)] <- "renal"

eligible_data$comorbidity_3[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_3[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_3 <- replace_na(eligible_data$comorbidity_3, "nil")
eligible_data$comorbidity_3[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_3[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_3[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_3[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_3[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_3[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_3[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_3[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_3, ignore.case=FALSE)] <- "renal"

eligible_data$comorbidity_4[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_4[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_4 <- replace_na(eligible_data$comorbidity_4, "nil")
eligible_data$comorbidity_4[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_4[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_4[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_4[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_4[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_4[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_4[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_4[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_4, ignore.case=FALSE)] <- "renal"

eligible_data$comorbidity_5[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_5[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_5 <- replace_na(eligible_data$comorbidity_5, "nil")
eligible_data$comorbidity_5[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_5[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_5[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_5[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_5[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_5[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_5[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_5[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_5, ignore.case=FALSE)] <- "renal"

eligible_data$comorbidity_6[grepl(paste(cancers, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "cancer"
eligible_data$comorbidity_6[grepl(paste(no_comorbidity, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "nil"
eligible_data$comorbidity_6 <- replace_na(eligible_data$comorbidity_6, "nil")
eligible_data$comorbidity_6[grepl(paste(auto_immune, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "auto_immune"
eligible_data$comorbidity_6[grepl(paste(metabolic, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "metabolic"
eligible_data$comorbidity_6[grepl(paste(haematological, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "haematological"
eligible_data$comorbidity_6[grepl(paste(cardiac, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "cardiac"
eligible_data$comorbidity_6[grepl(paste(neurological, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "neurological"
eligible_data$comorbidity_6[grepl(paste(psychiatric, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "psychiatric"
eligible_data$comorbidity_6[grepl(paste(respiratory, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "respiratory"
eligible_data$comorbidity_6[grepl(paste(renal, collapse = "|"), eligible_data$comorbidity_6, ignore.case=FALSE)] <- "renal"

eligible_data <- eligible_data %>%
  mutate(comorbidity_1 = as_factor(comorbidity_1),
         comorbidity_2 = as_factor(comorbidity_2),
         comorbidity_3 = as_factor(comorbidity_3),
         comorbidity_4 = as_factor(comorbidity_4),
         comorbidity_5 = as_factor(comorbidity_5),
         comorbidity_6 = as_factor(comorbidity_6),
         cancer = ifelse(eligible_data$comorbidity_1 == "cancer", 1,
                          ifelse(eligible_data$comorbidity_2  == "cancer", 1,
                                 ifelse(eligible_data$comorbidity_3  == "cancer", 1,
                                        ifelse(eligible_data$comorbidity_4  == "cancer", 1,
                                               ifelse(eligible_data$comorbidity_5  == "cancer", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "cancer", 1,
                                                             0)))))),
         cancer = as_factor(ifelse(is.na(cancer), 0, cancer)),
         auto_immune = ifelse(eligible_data$comorbidity_1 == "auto_immune", 1,
                          ifelse(eligible_data$comorbidity_2  == "auto_immune", 1,
                                 ifelse(eligible_data$comorbidity_3  == "auto_immune", 1,
                                        ifelse(eligible_data$comorbidity_4  == "auto_immune", 1,
                                               ifelse(eligible_data$comorbidity_5  == "auto_immune", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "auto_immune", 1,
                                                             0)))))),
         auto_immune = as_factor(ifelse(is.na(auto_immune), 0, auto_immune)),
         metabolic = ifelse(eligible_data$comorbidity_1 == "metabolic", 1,
                          ifelse(eligible_data$comorbidity_2  == "metabolic", 1,
                                 ifelse(eligible_data$comorbidity_3  == "metabolic", 1,
                                        ifelse(eligible_data$comorbidity_4  == "metabolic", 1,
                                               ifelse(eligible_data$comorbidity_5  == "metabolic", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "metabolic", 1,
                                                             0)))))),
         metabolic = as_factor(ifelse(is.na(metabolic), 0, metabolic)),
         haematological = ifelse(eligible_data$comorbidity_1 == "haematological", 1,
                            ifelse(eligible_data$comorbidity_2  == "haematological", 1,
                                   ifelse(eligible_data$comorbidity_3  == "haematological", 1,
                                          ifelse(eligible_data$comorbidity_4  == "haematological", 1,
                                                 ifelse(eligible_data$comorbidity_5  == "haematological", 1,
                                                        ifelse(eligible_data$comorbidity_6  == "haematological", 1,
                                                               0)))))),
         haematological = as_factor(ifelse(is.na(haematological), 0, haematological)),
         cardiac = ifelse(eligible_data$comorbidity_1 == "cardiac", 1,
                          ifelse(eligible_data$comorbidity_2  == "cardiac", 1,
                                 ifelse(eligible_data$comorbidity_3  == "cardiac", 1,
                                        ifelse(eligible_data$comorbidity_4  == "cardiac", 1,
                                               ifelse(eligible_data$comorbidity_5  == "cardiac", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "cardiac", 1,
                                 0)))))),
         cardiac = as_factor(ifelse(is.na(cardiac), 0, cardiac)),
         neurological = ifelse(eligible_data$comorbidity_1 == "neurological", 1,
                          ifelse(eligible_data$comorbidity_2  == "neurological", 1,
                                 ifelse(eligible_data$comorbidity_3  == "neurological", 1,
                                        ifelse(eligible_data$comorbidity_4  == "neurological", 1,
                                               ifelse(eligible_data$comorbidity_5  == "neurological", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "neurological", 1,
                                                             0)))))),
         neurological = as_factor(ifelse(is.na(neurological), 0, neurological)),
         psychiatric = ifelse(eligible_data$comorbidity_1 == "psychiatric", 1,
                          ifelse(eligible_data$comorbidity_2  == "psychiatric", 1,
                                 ifelse(eligible_data$comorbidity_3  == "psychiatric", 1,
                                        ifelse(eligible_data$comorbidity_4  == "psychiatric", 1,
                                               ifelse(eligible_data$comorbidity_5  == "psychiatric", 1,
                                                      ifelse(eligible_data$comorbidity_6  == "psychiatric", 1,
                                                             0)))))),
         psychiatric = as_factor(ifelse(is.na(psychiatric), 0, psychiatric)),
         respiratory = ifelse(eligible_data$comorbidity_1 == "respiratory", 1,
                              ifelse(eligible_data$comorbidity_2  == "respiratory", 1,
                                     ifelse(eligible_data$comorbidity_3  == "respiratory", 1,
                                            ifelse(eligible_data$comorbidity_4  == "respiratory", 1,
                                                   ifelse(eligible_data$comorbidity_5  == "respiratory", 1,
                                                          ifelse(eligible_data$comorbidity_6  == "respiratory", 1,
                                                                 0)))))),
         respiratory = as_factor(ifelse(is.na(respiratory), 0, respiratory)),
         renal = ifelse(eligible_data$comorbidity_1 == "renal", 1,
                              ifelse(eligible_data$comorbidity_2  == "renal", 1,
                                     ifelse(eligible_data$comorbidity_3  == "renal", 1,
                                            ifelse(eligible_data$comorbidity_4  == "renal", 1,
                                                   ifelse(eligible_data$comorbidity_5  == "renal", 1,
                                                          ifelse(eligible_data$comorbidity_6  == "renal", 1,
                                                                 0)))))),
         renal = as_factor(ifelse(is.na(renal), 0, renal)),
         HIV = ifelse(eligible_data$comorbidity_1 == "HIV", 1,
                      ifelse(eligible_data$comorbidity_2  == "HIV", 1,
                             ifelse(eligible_data$comorbidity_3  == "HIV", 1,
                                    ifelse(eligible_data$comorbidity_4  == "HIV", 1,
                                           ifelse(eligible_data$comorbidity_5  == "HIV", 1,
                                                  ifelse(eligible_data$comorbidity_6  == "HIV", 1,
                                                         0)))))),
         HIV = as_factor(ifelse(is.na(HIV), 0, HIV)),
         nil = ifelse(eligible_data$comorbidity_1 == "nil", 1,
                      0),
         nil = as_factor(ifelse(is.na(nil), 0, nil))
  )

write_rds(eligible_data, here("clean_data", "cleaned_data.rds"))