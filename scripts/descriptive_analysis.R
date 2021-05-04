library("tidyverse")
library("readxl")
library("here")
library("arsenal")

data <- read_rds(here("clean_data", "cleaned_data.rds")) %>%
  mutate(length_admission = ifelse(mortality == "died", NA, length_admission),
         three_age = as.factor(ifelse(grouped_age %in% c("18-24", "25-34", "35-44"), 1,
                            ifelse(grouped_age %in% c("45-54", "55-64"), 2, 3))))
london_smoking <- read_xlsx(here("gov_data", "london_smoking.xlsx")) %>%
  select(sex, `Age group`, current_smoker, former_smoker, never_smoker) %>%
  rename("grouped_age" = `Age group`) %>%
  mutate(cohort = "London population")

write_rds(data, here("clean_data", "data_for_analysis.rds"))

table(data$cohort)  

levels(data$sex) <- c("Male", "Female")
levels(data$ethnicity) <- c("Black African or Black Caribbean", "White British or White Other", "Not stated",
                            "Black African or Black Caribbean", "Asian", "White British or White Other", "Other",
                            "Asian", "White British or White Other", "Asian", "Other")
levels(data$smoking_status) <- c("Never smoker", "Former smoker", "Current smoker")
levels(data$system_smoking_status) <- c("Never smoker", "Former smoker", "Current smoker", "Unknown")
levels(data$diagnosis) <- c("SARS-CoV-2", "Influenza A", "Multiple non-SARS-CoV-2", "Influenza B",
                            "RSV", "Multiple non-SARS-CoV-2", "Adenovirus", "Coronavirus (non-SARS-CoV-2)",
                            "Parainfluenza", "Influenza A", "Rhinovirus", "Influenza A", "Multiple non-SARS-CoV-2",
                            "Metapneumovirus", "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2",
                            "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2",
                            "Multiple non-SARS-CoV-2", "Multiple non-SARS-CoV-2")
levels(data$mortality) <- c("Died during admission", "Survived admission")
levels(data$cancer) <- c("Absent", "Present")
levels(data$auto_immune) <- c("Absent", "Present")
levels(data$metabolic) <- c("Absent", "Present")
levels(data$haematological) <- c("Absent", "Present")
levels(data$cardiac) <- c("Absent", "Present")
levels(data$neurological) <- c("Absent", "Present")
levels(data$respiratory) <- c("Absent", "Present")
levels(data$renal) <- c("Absent", "Present")
levels(data$HIV) <- c("Absent", "Present")
levels(data$nil) <- c("At least one", "No comorbidities")
levels(data$three_age) <- c("18-44", "45-64", "65+")
levels(data$cohort) <- c("Controls", "Cases")

mylabels <- list(sex = "Female",
                 age_at_admission = "Age",
                 grouped_age = "Categorical age",
                 ethnicity = "Ethnicity",
                 smoking_status = "Smoking status",
                 system_smoking_status = "System recorded smoking status",
                 diagnosis = "Diagnosis",
                 length_admission = "Length of admission for survivors (days)",
                 mortality = "Survival to discharge",
                 cancer = "Cancer (current or past)",
                 auto_immune = "Auto-immune disease (present)",
                 metabolic = "Metabolic disease (present)",
                 haematological = "Haematological disease (present)",
                 cardiac = "Cardiac disease (present)",
                 neurological = "Neurological disease (present)",
                 respiratory = "Respiratory disease (present)",
                 renal = "Renal disease (present)",
                 HIV = "HIV (present)",
                 nil = "No relevant comorbidities",
                 three_age = "Grouped age",
                 cohort = "Cohort")

t1 <- tableby(cohort ~ sex +
                age_at_admission +
                trend(grouped_age) +
                ethnicity +
                smoking_status +
                system_smoking_status +
                notest(diagnosis) +
                mortality +
                anova(length_admission, "median", "q1q3") +
                cancer +
                auto_immune +
                metabolic +
                haematological +
                cardiac +
                neurological +
                respiratory +
                renal +
                HIV +
                nil,
              data = data,
              numeric.stats=c("median","q1q3"),
              cat.simplify = T)

summary(t1, text = T, labelTranslations = mylabels,  pfootnote=TRUE, digits = 2)
write2word(t1, here("output", "descriptive_table.doc"), title = "Descriptive Table")

t1 <- table1(~ sex +
                age_at_admission +
                grouped_age +
                ethnicity +
                smoking_status + system_smoking_status + 
                diagnosis + length_admission +
                mortality +
                cancer + auto_immune + metabolic +
                haematological + cardiac + neurological +
                respiratory + renal + HIV +
                nil | cohort, data = data,
              render.continuous = "Median [Q1, Q3]")

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- data[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ data$cohort)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(data$cohort)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}


t1a <- table1(~ sex +
         age_at_admission +
         grouped_age +
         ethnicity +
         smoking_status + system_smoking_status + 
         diagnosis + length_admission +
         mortality +
         cancer + auto_immune + metabolic +
         haematological + cardiac + neurological +
         respiratory + renal + HIV +
         nil | cohort, data = data,
       droplevels=F, render=rndr, render.strat=rndr.strat)

t2 <- table1(~ three_age | cohort * smoking_status, data = data, overall = F)

ggsave("age_distribution.png",
       ggplot(data = data, aes(x = age_at_admission, colour = cohort)) +
         geom_density() +
         theme_minimal() +
         labs(title = "Age at hospital admission for controls and cases",
              colour = "Group") +
         scale_color_discrete(labels = c("Controls", "Cases")) +
         xlab("Age at admission") +
         ylab("Density"),
       path = here("output"))

ggplot(data = data, aes(x = age_at_admission, colour = cohort)) +
  stat_ecdf(geom = "step")

ggsave("sex_distribution.png",
       ggplot(data, aes(x = cohort, fill = sex)) +
         geom_bar(position = "fill") +
         theme_minimal() +
         labs(title = "Sex of patients admitted",
              fill = "Sex") +
         xlab("Cohort") +
         ylab("Proportion"),
       path = here("output"))

ggsave("smoking_status.png",
       ggplot(data, aes(x = cohort, fill = smoking_status)) +
         geom_bar(position = "fill") +
         geom_hline(aes(yintercept = 0.129)) +
         geom_hline(aes(yintercept = 0.123), linetype = "dashed") +
         geom_hline(aes(yintercept = 0.136), linetype = "dashed") +
         theme_minimal() +
         labs(title = "Smoking status of patients admitted",
              fill = "Smoking status") +
         xlab("Cohort") +
         ylab("Proportion"),
       path = here("output"))

ggplot(data, aes(x = smoking_status, fill = cohort)) +
  geom_bar()

ggplot(data, aes(x = admission_date, fill = cohort)) +
  geom_histogram()


