############################################################
### PRECLINICAL / CLINICAL OBESITY AMONG LANCET-OBESE ONLY
############################################################
stopifnot(exists("cohort"))
stopifnot(exists("hesin_long_reg"))

stopifnot(all(c(
  "eid", "sex", "age", "baseline_date", "obesity_lancet",
  "hypertension_baseline", "HbA1c_mmol_mol", "tg_mmol_L",
  "HDL_mmol_L", "creatinine_umol_L"
) %in% names(cohort)))

stopifnot(all(c("eid", "diag", "diag_date", "baseline_date") %in% names(hesin_long_reg)))

# =========================================================
# Restrict to participants already classified as obese
# according to Lancet obesity criteria
# =========================================================
cohort_obese_lancet <- cohort %>%
  filter(obesity_lancet == 1L)

cat("Participants with obesity_lancet == 1:", nrow(cohort_obese_lancet), "\n")

# =========================================================
# ICD-based clinical obesity criteria
# =========================================================
# Included:
# G93.2  Benign intracranial hypertension
# G47.3  Obstructive sleep apnea
# E66.2  Obesity hypoventilation syndrome
# I50    Heart failure
# I48    Atrial fibrillation/flutter
# I27    Pulmonary hypertension
# I26    Pulmonary embolism
# I27.82 Pulmonary embolism (if coded as I2782 after removing dot)
# I82    Venous thromboembolism
# I10    Hypertension
# K76.0  MASLD / fatty liver
# K74    Hepatic fibrosis/cirrhosis
# N18    Chronic kidney disease
# N39.4  Urinary incontinence
# R32    Urinary incontinence
# E28.2  PCOS
# N91    Amenorrhea / oligomenorrhea
# E29.1  Male hypogonadism
# M16    Hip osteoarthritis
# M17    Knee osteoarthritis
# I89.0  Lymphedema

clinical_obesity_regex <- paste0(
  "^(G932|G473|E662|I50|I48|I27|I2782|I26|I82|I10|",
  "K760|K74|N18|N394|R32|E282|N91|E291|M16|M17|I890)",
  "([0-9A-Z].*)?$"
)

clinical_icd <- hesin_long_reg %>%
  mutate(
    diag = gsub("\\.", "", toupper(trimws(diag)))
  ) %>%
  filter(
    !is.na(diag),
    !is.na(diag_date),
    !is.na(baseline_date),
    diag_date <= baseline_date,
    grepl(clinical_obesity_regex, diag)
  ) %>%
  distinct(eid) %>%
  mutate(clinical_obesity_icd = 1L)

# =========================================================
# CKD by ICD only (separate variable, useful for transparency)
# =========================================================
ckd_icd <- hesin_long_reg %>%
  mutate(
    diag = gsub("\\.", "", toupper(trimws(diag)))
  ) %>%
  filter(
    !is.na(diag),
    !is.na(diag_date),
    !is.na(baseline_date),
    diag_date <= baseline_date,
    grepl("^N18([0-9A-Z].*)?$", diag)
  ) %>%
  distinct(eid) %>%
  mutate(ckd_icd = 1L)

# =========================================================
# Join ICD components
# =========================================================
n0 <- nrow(cohort_obese_lancet)

cohort_obese_lancet <- cohort_obese_lancet %>%
  select(-any_of(c(
    "clinical_obesity_icd",
    "ckd_icd",
    "creat_mg_dl",
    "kappa",
    "alpha",
    "sex_factor",
    "egfr_ckdepi_2021",
    "hyperglycaemia",
    "high_triglycerides",
    "low_hdl",
    "metabolic_cluster_n",
    "metabolic_cluster",
    "clinical_obesity_hypertension",
    "clinical_obesity_ckd",
    "clinical_obesity_any",
    "lancet_obesity_stage",
    "obesity_preclinical",
    "obesity_clinical"
  ))) %>%
  left_join(clinical_icd, by = "eid") %>%
  left_join(ckd_icd, by = "eid")

stopifnot(nrow(cohort_obese_lancet) == n0)
stopifnot(n_distinct(cohort_obese_lancet$eid) == nrow(cohort_obese_lancet))

# =========================================================
# Define components:
# - eGFR (CKD-EPI 2021 creatinine equation)
# - metabolic cluster = ALL 3 present simultaneously
# - hypertension also by your study definition
# - CKD by ICD OR eGFR <60
# =========================================================
cohort_obese_lancet <- cohort_obese_lancet %>%
  mutate(
    clinical_obesity_icd = ifelse(is.na(clinical_obesity_icd), 0L, clinical_obesity_icd),
    ckd_icd = ifelse(is.na(ckd_icd), 0L, ckd_icd),
    
    HbA1c_mmol_mol = as.numeric(HbA1c_mmol_mol),
    tg_mmol_L = as.numeric(tg_mmol_L),
    HDL_mmol_L = as.numeric(HDL_mmol_L),
    creatinine_umol_L = as.numeric(creatinine_umol_L),
    age = as.numeric(age),
    
    hypertension_baseline = ifelse(is.na(hypertension_baseline), 0L, as.integer(hypertension_baseline)),
    
    # -----------------------------------------------------
    # eGFR CKD-EPI 2021
    # creatinine in mg/dL
    # -----------------------------------------------------
    creat_mg_dl = creatinine_umol_L / 88.4,
    kappa = ifelse(sex == "female", 0.7, ifelse(sex == "male", 0.9, NA_real_)),
    alpha = ifelse(sex == "female", -0.241, ifelse(sex == "male", -0.302, NA_real_)),
    sex_factor = ifelse(sex == "female", 1.012, ifelse(sex == "male", 1, NA_real_)),
    
    egfr_ckdepi_2021 = ifelse(
      !is.na(creat_mg_dl) & !is.na(kappa) & !is.na(alpha) & !is.na(sex_factor) & !is.na(age),
      142 *
        (pmin(creat_mg_dl / kappa, 1) ^ alpha) *
        (pmax(creat_mg_dl / kappa, 1) ^ (-1.200)) *
        (0.9938 ^ age) *
        sex_factor,
      NA_real_
    ),
    
    # -----------------------------------------------------
    # Metabolic cluster
    # ALL 3 conditions required
    # HbA1c >= 39 mmol/mol
    # TG > 2.3 mmol/L
    # low HDL sex-specific
    # -----------------------------------------------------
    hyperglycaemia = as.integer(
      !is.na(HbA1c_mmol_mol) & HbA1c_mmol_mol >= 39
    ),
    
    high_triglycerides = as.integer(
      !is.na(tg_mmol_L) & tg_mmol_L > 2.3
    ),
    
    low_hdl = as.integer(
      (!is.na(HDL_mmol_L) & sex == "male" & HDL_mmol_L < 1.0) |
        (!is.na(HDL_mmol_L) & sex == "female" & HDL_mmol_L < 1.3)
    ),
    
    metabolic_cluster_n = hyperglycaemia + high_triglycerides + low_hdl,
    
    metabolic_cluster = as.integer(
      hyperglycaemia == 1L &
        high_triglycerides == 1L &
        low_hdl == 1L
    ),
    
    # -----------------------------------------------------
    # Additional operational criteria
    # -----------------------------------------------------
    clinical_obesity_hypertension = as.integer(
      hypertension_baseline == 1L
    ),
    
    clinical_obesity_ckd = as.integer(
      ckd_icd == 1L |
        (!is.na(egfr_ckdepi_2021) & egfr_ckdepi_2021 < 60)
    ),
    
    # -----------------------------------------------------
    # Final clinical obesity
    # -----------------------------------------------------
    clinical_obesity_any = as.integer(
      clinical_obesity_icd == 1L |
        clinical_obesity_hypertension == 1L |
        clinical_obesity_ckd == 1L |
        metabolic_cluster == 1L
    ),
    
    lancet_obesity_stage = case_when(
      obesity_lancet == 1L & clinical_obesity_any == 0L ~ "Preclinical obesity",
      obesity_lancet == 1L & clinical_obesity_any == 1L ~ "Clinical obesity",
      TRUE ~ NA_character_
    ),
    
    lancet_obesity_stage = factor(
      lancet_obesity_stage,
      levels = c("Preclinical obesity", "Clinical obesity")
    ),
    
    obesity_preclinical = as.integer(lancet_obesity_stage == "Preclinical obesity"),
    obesity_clinical = as.integer(lancet_obesity_stage == "Clinical obesity")
  )

# =========================================================
# Checks
# =========================================================
cat("\n--- CHECKS ---\n")

cat("\nICD-based clinical obesity:\n")
print(table(cohort_obese_lancet$clinical_obesity_icd, useNA = "ifany"))

cat("\nHypertension criterion:\n")
print(table(cohort_obese_lancet$clinical_obesity_hypertension, useNA = "ifany"))

cat("\nCKD ICD:\n")
print(table(cohort_obese_lancet$ckd_icd, useNA = "ifany"))

cat("\neGFR <60:\n")
print(table(ifelse(!is.na(cohort_obese_lancet$egfr_ckdepi_2021) &
                     cohort_obese_lancet$egfr_ckdepi_2021 < 60, 1, 0),
            useNA = "ifany"))

cat("\nMetabolic components count:\n")
print(table(cohort_obese_lancet$metabolic_cluster_n, useNA = "ifany"))

cat("\nMetabolic cluster (all 3 simultaneously):\n")
print(table(cohort_obese_lancet$metabolic_cluster, useNA = "ifany"))

cat("\nFinal clinical obesity:\n")
print(table(cohort_obese_lancet$clinical_obesity_any, useNA = "ifany"))

cat("\nLancet obesity stage among obesity_lancet == 1:\n")
print(table(cohort_obese_lancet$lancet_obesity_stage, useNA = "ifany"))

cat("\nLogical checks:\n")
cat("Clinical + preclinical overlap:",
    sum(cohort_obese_lancet$obesity_preclinical == 1 &
          cohort_obese_lancet$obesity_clinical == 1, na.rm = TRUE), "\n")

cat("Missing stage among obesity_lancet==1:",
    sum(is.na(cohort_obese_lancet$lancet_obesity_stage)), "\n")

summary(cohort_obese_lancet$egfr_ckdepi_2021)

cohort <- cohort %>%
  select(-any_of(c(
    "egfr_ckdepi_2021",
    "clinical_obesity_icd",
    "ckd_icd",
    "hyperglycaemia",
    "high_triglycerides",
    "low_hdl",
    "metabolic_cluster_n",
    "metabolic_cluster",
    "clinical_obesity_hypertension",
    "clinical_obesity_ckd",
    "clinical_obesity_any",
    "lancet_obesity_stage",
    "obesity_preclinical",
    "obesity_clinical"
  ))) %>%
  left_join(
    cohort_obese_lancet %>%
      select(
        eid,
        egfr_ckdepi_2021,
        clinical_obesity_icd,
        ckd_icd,
        hyperglycaemia,
        high_triglycerides,
        low_hdl,
        metabolic_cluster_n,
        metabolic_cluster,
        clinical_obesity_hypertension,
        clinical_obesity_ckd,
        clinical_obesity_any,
        lancet_obesity_stage,
        obesity_preclinical,
        obesity_clinical
      ),
    by = "eid"
  )

cohort <- cohort %>%
  mutate(
    lancet_obesity_stage_full = case_when(
      obesity_lancet == 0L ~ "No obesity",
      obesity_lancet == 1L & obesity_preclinical == 1L ~ "Preclinical obesity",
      obesity_lancet == 1L & obesity_clinical == 1L ~ "Clinical obesity",
      TRUE ~ NA_character_
    ),
    lancet_obesity_stage_full = factor(
      lancet_obesity_stage_full,
      levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
    )
  )

table(cohort$lancet_obesity_stage_full, useNA = "ifany")
table(cohort$obesity_lancet, cohort$lancet_obesity_stage_full, useNA = "ifany")
