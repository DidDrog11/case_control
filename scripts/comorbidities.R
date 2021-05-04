no_comorbidity <- c(
  "nil",
  NA,
  "none",
  "alcohol_excess",
  "alcohol_abuse",
  "achalasia",
  "sciatica",
  "drug_misuse",
  "BPH",
  "frailty"
)
cancers <- c(
  "cancer",
  "carcinoma",
  "myeoysplasia",
  "myelodysplastic",
  "waldenstrom",
  "myeloma",
  "lymphoma",
  "leukaemia",
  "prostate_ca",
  "adenocarcinoma",
  "glioma",
  "sarcoma",
  "melenoma",
  "AML",
  "glioblastoma",
  "CLL"
)
auto_immune <- c(
  "myasthenia",
  "lupus",
  "SLE",
  "crohns",
  "UC",
  "SLE",
  "granulomatosis",
  "sjogrens",
  "cold_agglutinin",
  "graves",
  "RA",
  "sjogren"
)
metabolic <- c(
  "obesity",
  "pbesity",
  "T1DM",
  "T2DM",
  "SIADH",
  "obese",
  "hypothyroidism",
  "multinodular",
  "addison",
  "pituitary",
  "pancreatitis"
)
haematological <- c(
  "sickle",
  "thalassaemia",
  "TTP",
  "anaemia"
)
cardiac <- c(
  "HTN",
  "hypertension",
  "IHD",
  "CAD",
  "AF",
  "CCF",
  "heart failure",
  "NSTEMI",
  "myocarditis",
  "atrial",
  "CABG",
  "aortic",
  "pacemaker",
  "cardiomyopathy",
  "HOCM",
  "foramen",
  "AS"
)
neurological <- c(
  "stroke",
  "parkinsons",
  "TIA",
  "sclerosis",
  "epilepsy",
  "palsy",
  "dementia",
  "autism",
  "ataxia"
)
psychiatric <- c(
  "schizophrenia",
  "anorexia_nervosa"
)
respiratory <- c(
  "OSA",
  "pulmonary_hypertension",
  "COPD",
  "asthma",
  "fibrosis",
  "bronchiectasis",
  "interstitial",
  "emphysema",
  "TB",
  "sarcoidosis",
  "PE",
  "apnoea",
  "prev_tb"
)
renal <- c(
  "CKD",
  "AKI",
  "renal_transplant"
)

source(here::here("scripts", "cbind_na.R"))
library("tidyverse")
comorbidity_list <- cbind.na(auto_immune, cancers, cardiac, haematological, metabolic, neurological, no_comorbidity, psychiatric, renal, respiratory) %>%
  write.csv(., here::here("output", "comorbidity_list.csv"))