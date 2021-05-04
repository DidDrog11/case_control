library("tidyverse")
library("readxl")
library("here")

data <- read_rds(here("clean_data", "cleaned_data.rds")) 

unadjusted <- glm(cohort ~ smoking_status, data = data, family = binomial(link = "logit"))
minimally_adjusted <- glm(cohort ~ smoking_status + sex + age_at_admission, data = data, family = binomial(link = "logit"))
fully_adjusted <- glm(cohort ~ smoking_status + sex + age_at_admission + 
                         cancer + auto_immune + metabolic + haematological + cardiac + neurological + respiratory + renal + HIV,
                       data = data, family = binomial(link = "logit"))

summary(unadjusted)
summary(minimally_adjusted)
summary(fully_adjusted)

confint(minimally_adjusted)

exp(cbind(OR = coef(unadjusted), confint(unadjusted)))
exp(cbind(OR = coef(minimally_adjusted), confint(minimally_adjusted)))
exp(cbind(OR = coef(fully_adjusted), confint(fully_adjusted)))

#Z-test
z_tests <- read_rds(here("clean_data", "cleaned_data.rds")) %>%
  mutate(length_admission = ifelse(mortality == "died", NA, length_admission),
         three_age = as.factor(ifelse(grouped_age %in% c("18-24", "25-34", "35-44"), 1,
                                      ifelse(grouped_age %in% c("45-54", "55-64"), 2, 3)))) %>%
  group_by(three_age, smoking_status, cohort)

london_smoking <- read_xlsx(here("gov_data", "london_smoking.xlsx")) %>%
  select(sex, `Age group`, current_smoker, former_smoker, never_smoker, sample) %>%
  rename("grouped_age" = `Age group`) %>%
  filter(grouped_age != "All 18+") %>%
  mutate(cohort = "London population",
         three_age = as.factor(ifelse(grouped_age %in% c("18-24", "25-34", "35-44"), 1,
                                      ifelse(grouped_age %in% c("45-54", "55-64"), 2, 3))),
         three_age = factor(three_age, labels = c("18-44", "45-64", "65+")),
         number_current = current_smoker/100*sample,
         number_former =  former_smoker/100*sample,
         number_never =  never_smoker/100*sample) %>%
  group_by(three_age, sex) %>%
  summarise(sample = sum(sample),
            number_current = sum(number_current),
            number_former = sum(number_former),
            number_never = sum(number_never)) %>%
  mutate(prop_current = number_current/sample * 100,
         prop_former = number_former/sample * 100,
         prop_never = number_never/sample * 100) %>%
  filter(sex == "Persons") %>%
  select(-c("number_current", "number_former", "number_never"))

current_controls_age_1 <- prop.test(2, n = 12, p = 0.13, alternative = "less") #unstable
current_controls_age_2 <- prop.test(2, n = 16, p = 0.15, alternative = "less") #unstable
current_controls_age_3 <- prop.test(5, n = 12, p = 0.14, alternative = "less") #unstable
current_controls_age_4 <- prop.test(6, n = 29, p = 0.14, alternative = "less") #unstable
current_controls_age_5 <- prop.test(6, n = 43, p = 0.13, alternative = "less")
current_controls_age_6 <- prop.test(15, n = 99, p = 0.08, alternative = "less")


current_cases_age_2 <- prop.test(3, n = 10, p = 0.15, alternative = "less") #unstable
current_cases_age_3 <- prop.test(3, n = 20, p = 0.14, alternative = "less") #unstable
current_cases_age_4 <- prop.test(6, n = 51, p = 0.14, alternative = "less")
current_cases_age_5 <- prop.test(2, n = 57, p = 0.13, alternative = "less")
current_cases_age_6 <- prop.test(13, n = 173, p = 0.8, alternative = "less")

current_controls_all_age <- prop.test(36, n = 211, p = 0.13, alternative = "less")
current_cases_all_age <- prop.test(27, n = 313, p = 0.13, alternative = "less")

table(z_tests$cohort, z_tests$three_age)
table(z_tests$cohort, z_tests$smoking_status, z_tests$three_age)

current_controls_three_1 <- prop.test(9, n = 40, p = 0.14, alternative = "less")
current_controls_three_2 <- prop.test(12, n = 72, p = 0.136, alternative = "less")
current_controls_three_3 <- prop.test(15, n = 99, p = 0.078, alternative = "less")

current_cases_three_1 <- prop.test(6, n = 32, p = 0.14, alternative = "less")
current_cases_three_2 <- prop.test(8, n = 108, p = 0.136, alternative = "less")
current_cases_three_3 <- prop.test(13, n = 173, p = 0.078, alternative = "less")

former_cases_three_1 <- prop.test(5, n = 32, p = 0.135, alternative = "greater")
former_cases_three_2 <- prop.test(33, n = 108, p = 0.237, alternative = "greater")
former_cases_three_3 <- prop.test(82, n = 173, p = 0.335, alternative = "greater")

#Recorded smoking status
discrepancies <- data %>%
  droplevels(.$cohort) %>%
  select(mrn, smoking_status, system_smoking_status, cohort) %>%
  pivot_longer(., 2:3, names_to = "record_type") %>%
  mutate(record_type = as_factor(record_type)) %>%
  group_by(cohort)
levels(discrepancies$cohort) <- c("Controls", "Cases")
levels(discrepancies$record_type) <- c("Medical Notes", "Summary EHR")
levels(discrepancies$value) <- c("Never smoker", "Former smoker", "Current smoker", "Unknown")

discrepancies_cases <- discrepancies %>%
  ungroup() %>%
  filter(cohort == "cases")

discrepancies_controls <- discrepancies %>%
  ungroup() %>%
  filter(cohort == "controls")

ggsave("discrepancies.png",
  ggplot(discrepancies, aes(x = record_type, fill = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~ cohort) +
    theme_minimal() +
    labs(title = "Concordance between medical records and summary EHR",
         fill = "Smoking status") +
    scale_color_discrete(labels = c("Controls", "Cases")) +
    ylab("Proportion") +
    xlab("Record"),
  path = here("output"))

table_discrepancies <- table(discrepancies$record_type, discrepancies$value)
table_discrepancies_cases <- table(discrepancies_cases$record_type, discrepancies_cases$value)
table_discrepancies_controls <- table(discrepancies_controls$record_type, discrepancies_controls$value)
chisq.test(table_discrepancies)
chisq.test(table_discrepancies_cases)
chisq.test(table_discrepancies_controls)
