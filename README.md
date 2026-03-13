install.packages(c("readr", "dplyr", "janitor",
                   "stringr", "tidyr", "readxl", "plotly", "flextable",
                   "officer", "parallel", "fpc", "survival", "cmprsk",
                   "broom", "survminer", "lubridate", "cluster", "tidyverse"))

library(plotly)
library(scales)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)
library(lubridate)
library(cluster)
library(officer)
library(flextable)
library(parallel)
library(fpc)
library(survival)
library(cmprsk)
library(broom)
library(survminer)
library(tidyverse)

df_31 <- read.csv("C:/Users/nicol/Desktop/ukbb_obesity/data/df_31.csv", 
                  stringsAsFactors = FALSE, colClasses = "character")
raw <- df_31[[1]]
eid <- as.integer(sub("\\..*", "", raw))
sex_code <- ifelse(grepl("\\.1$", raw), 1L,ifelse(grepl("\\.", raw), NA_integer_, 0L))

df_31 <- data.frame(eid = eid, sex_code = sex_code)

# Ricodifica (UKBB standard: 0=female, 1=male) (=i0)
df_31$sex <- ifelse(df_31$sex_code == 1, "male",ifelse(df_31$sex_code == 0, "female", NA))

table(df_31$sex_code, useNA = "ifany")
table(df_31$sex, useNA = "ifany")

# FILTRO EUROPEI
eur <- read.table(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/european_eid.txt",
  header = FALSE,
  stringsAsFactors = FALSE
)
colnames(eur) <- "eid"
eur$eid <- as.integer(eur$eid)

master <- df_31 %>%
  filter(eid %in% eur$eid)

# Check univocità eid nella coorte di lavoro
stopifnot(nrow(master) == n_distinct(master$eid))

n0 <- nrow(master)

# 1) carica età all'assessment (field 21003) (=i0)
df_21003 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_21003.csv",
  stringsAsFactors = FALSE
)

age <- df_21003 %>%
  mutate(
    eid = as.integer(eid),
    age = as.numeric(p21003_i0)
  ) %>%
  select(eid, age) %>%
  distinct(eid, .keep_all = TRUE)

summary(age$age)
stopifnot(n_distinct(age$eid) == nrow(age))

master <- master %>% left_join(age, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$age)
sum(is.na(master$age))

# 2) carica data assessment (field 53) (=i0)
df_53 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_53.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

baseline_date <- df_53 %>%
  mutate(
    eid = as.integer(eid),
    baseline_date = as.Date(parse_date_time(p53_i0, orders = c("ymd", "dmy", "mdy")))
  ) %>%
  select(eid, baseline_date) %>%
  distinct(eid, .keep_all = TRUE)

summary(baseline_date$baseline_date)
range(baseline_date$baseline_date, na.rm = TRUE)
sum(is.na(baseline_date$baseline_date))
stopifnot(n_distinct(baseline_date$eid) == nrow(baseline_date))

master <- master %>% left_join(baseline_date, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$baseline_date)
range(master$baseline_date, na.rm = TRUE)
sum(is.na(master$baseline_date))

# 3) carica standing height (field 50) (=i0)
df_50 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_50.csv",
  stringsAsFactors = FALSE
)

height <- df_50 %>%
  mutate(
    eid = as.integer(eid),
    height = as.numeric(p50_i0)
  ) %>%
  select(eid, height) %>%
  distinct(eid, .keep_all = TRUE)

summary(height$height)
stopifnot(n_distinct(height$eid) == nrow(height))

master <- master %>% left_join(height, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$height)
sum(is.na(master$height))

# 4) carica body weight (field 21002) (=i0)
df_21002 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_21002.csv",
  stringsAsFactors = FALSE
)

weight <- df_21002 %>%
  mutate(
    eid = as.integer(eid),
    weight = as.numeric(p21002_i0)
  ) %>%
  select(eid, weight) %>%
  distinct(eid, .keep_all = TRUE)

summary(weight$weight)
stopifnot(n_distinct(weight$eid) == nrow(weight))

master <- master %>% left_join(weight, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$weight)
sum(is.na(master$weight))

# 5) carica BMI (field 21001) (=i0)
df_21001 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_21001.csv",
  stringsAsFactors = FALSE
)

bmi <- df_21001 %>%
  mutate(
    eid = as.integer(eid),
    bmi = as.numeric(p21001_i0)
  ) %>%
  select(eid, bmi) %>%
  distinct(eid, .keep_all = TRUE)

summary(bmi$bmi)
stopifnot(n_distinct(bmi$eid) == nrow(bmi))

master <- master %>% left_join(bmi, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$bmi)
sum(is.na(master$bmi))

# 6) carica waist circumference (field 48) (=i0)
df_48 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_48.csv",
  stringsAsFactors = FALSE
)

waist <- df_48 %>%
  mutate(
    eid = as.integer(eid),
    waist_c = as.numeric(p48_i0)
  ) %>%
  select(eid, waist_c) %>%
  distinct(eid, .keep_all = TRUE)

summary(waist$waist_c)
stopifnot(n_distinct(waist$eid) == nrow(waist))

master <- master %>% left_join(waist, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$waist_c)
sum(is.na(master$waist_c))

# 7) carica hip circumference (field 49) (=i0)
df_49 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_49.csv",
  stringsAsFactors = FALSE
)

hip <- df_49 %>%
  mutate(
    eid = as.integer(eid),
    hip_c = as.numeric(p49_i0)
  ) %>%
  select(eid, hip_c) %>%
  distinct(eid, .keep_all = TRUE)

summary(hip$hip_c)
stopifnot(n_distinct(hip$eid) == nrow(hip))

master <- master %>% left_join(hip, by = "eid")
stopifnot(nrow(master) == n0)

summary(master$hip_c)
sum(is.na(master$hip_c))

##DERIVED ANTHROPOMETRIC RATIOS (WHR, WHtR)
master <- master %>%
  mutate(
    whr  = waist_c / hip_c,
    whtr = waist_c / height
  )

summary(master$whr)
summary(master$whtr)

range(master$whr, na.rm = TRUE)
range(master$whtr, na.rm = TRUE)

sum(is.na(master$whr))
sum(is.na(master$whtr))

# 8) PHYSICAL ACTIVITY (field 884) (=i0)
df_884 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_884.csv",
  stringsAsFactors = FALSE
)

pa_cols <- grep("^p884(_i0|_0_)", names(df_884), value = TRUE)
stopifnot(length(pa_cols) > 0)

phys_act <- df_884 %>%
  mutate(
    eid = as.integer(eid),
    phys_act = suppressWarnings(as.numeric(.data[[pa_cols[1]]])),
    phys_act = ifelse(phys_act %in% c(-3, -1), NA, phys_act)
  ) %>%
  select(eid, phys_act) %>%
  distinct(eid, .keep_all = TRUE)

summary(phys_act$phys_act)
sum(is.na(phys_act$phys_act))
stopifnot(n_distinct(phys_act$eid) == nrow(phys_act))

# 9) SMOKING (field 20116) (=i0)
df_20116 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_20116.csv",
  stringsAsFactors = FALSE
)

smk_cols <- grep("^p20116(_i0|_0_)", names(df_20116), value = TRUE)
stopifnot(length(smk_cols) > 0)

smoking <- df_20116 %>%
  mutate(
    eid = as.integer(eid),
    smoking = suppressWarnings(as.numeric(.data[[smk_cols[1]]])),
    smoking = ifelse(smoking %in% c(-3, -1), NA, smoking)
  ) %>%
  select(eid, smoking) %>%
  distinct(eid, .keep_all = TRUE)

summary(smoking$smoking)
sum(is.na(smoking$smoking))
stopifnot(n_distinct(smoking$eid) == nrow(smoking))

# 10) SYSTOLIC BP MEAN (field 4080) (=i0)
df_4080 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_4080.csv",
  stringsAsFactors = FALSE
)

sbp <- df_4080 %>%
  mutate(eid = as.integer(eid))

sbp_cols <- grep("^p4080(_i0|_0_)", names(sbp), value = TRUE)
stopifnot(length(sbp_cols) > 0)

sbp_mat <- suppressWarnings(sapply(sbp[sbp_cols], as.numeric))

sbp_mean <- ifelse(
  rowSums(!is.na(sbp_mat)) == 0,
  NA,
  rowMeans(sbp_mat, na.rm = TRUE)
)

sbp <- data.frame(
  eid = sbp$eid,
  sbp_m = sbp_mean
) %>%
  distinct(eid, .keep_all = TRUE)

summary(sbp$sbp_m)
stopifnot(n_distinct(sbp$eid) == nrow(sbp))

# 11) DIASTOLIC BP MEAN (field 4079) (=i0)
df_4079 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_4079.csv",
  stringsAsFactors = FALSE
)

dbp <- df_4079 %>%
  mutate(eid = as.integer(eid))

dbp_cols <- grep("^p4079(_i0|_0_)", names(dbp), value = TRUE)
stopifnot(length(dbp_cols) > 0)

dbp_mat <- suppressWarnings(sapply(dbp[dbp_cols], as.numeric))

dbp_mean <- ifelse(
  rowSums(!is.na(dbp_mat)) == 0,
  NA,
  rowMeans(dbp_mat, na.rm = TRUE)
)

dbp <- data.frame(
  eid = dbp$eid,
  dbp_m = dbp_mean
) %>%
  distinct(eid, .keep_all = TRUE)

summary(dbp$dbp_m)
stopifnot(n_distinct(dbp$eid) == nrow(dbp))

# 12) PREGNANCY (field 3140) (=i0)
df_3140 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_3140.csv",
  stringsAsFactors = FALSE
)

preg_cols <- grep("^p3140(_i0|_0_)", names(df_3140), value = TRUE)
stopifnot(length(preg_cols) > 0)

preg <- df_3140 %>%
  mutate(
    eid = as.integer(eid),
    pregnancy = suppressWarnings(as.numeric(.data[[preg_cols[1]]]))
  ) %>%
  select(eid, pregnancy) %>%
  distinct(eid, .keep_all = TRUE)

summary(preg$pregnancy)
stopifnot(n_distinct(preg$eid) == nrow(preg))

n0 <- nrow(master)

master <- master %>%
  left_join(phys_act, by = "eid") %>%
  left_join(smoking, by = "eid") %>%
  left_join(sbp, by = "eid") %>%
  left_join(dbp, by = "eid") %>%
  left_join(preg, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

summary(master$phys_act)
summary(master$smoking)
summary(master$sbp_m)
summary(master$dbp_m)
summary(master$pregnancy)

sum(is.na(master$phys_act))
sum(is.na(master$smoking))
sum(is.na(master$sbp_m))
sum(is.na(master$dbp_m))
sum(is.na(master$pregnancy))

# 13) ALCOHOL INTAKE (g/day) -- precomputed dataset
alcohol <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/alcohol_0.csv",
  stringsAsFactors = FALSE
)

names(alcohol)
head(alcohol)
alcohol <- alcohol %>%
  mutate(
    eid = as.integer(eid)
  )

alc_col <- setdiff(names(alcohol), "eid")

if(length(alc_col) != 1){
  stop("Alcohol file should contain eid + exactly one alcohol column. Found: ",
       paste(names(alcohol), collapse = ", "))
}

alcohol <- alcohol %>%
  mutate(
    alcohol_g_day = suppressWarnings(as.numeric(.data[[alc_col]])),
    alcohol_g_day = ifelse(alcohol_g_day < 0, NA, alcohol_g_day)
  ) %>%
  select(eid, alcohol_g_day) %>%
  distinct(eid, .keep_all = TRUE)

summary(alcohol$alcohol_g_day)
sum(is.na(alcohol$alcohol_g_day))
stopifnot(n_distinct(alcohol$eid) == nrow(alcohol))

n0 <- nrow(master)

master <- master %>%
  select(-any_of("alcohol_g_day")) %>%
  left_join(alcohol, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

summary(master$alcohol_g_day)
sum(is.na(master$alcohol_g_day))

# 14) TRIGLYCERIDES (30870)
df_30870 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30870.csv",
  stringsAsFactors = FALSE
)

tg_cols <- grep("^p30870(_i0|_0_)", names(df_30870), value = TRUE)
stopifnot(length(tg_cols) > 0)

tg <- df_30870 %>%
  mutate(
    eid = as.integer(eid),
    tg_mmol_L = as.numeric(.data[[tg_cols[1]]]),
    tg_mmol_L = ifelse(tg_mmol_L < 0, NA, tg_mmol_L)
  ) %>%
  select(eid, tg_mmol_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(tg$tg_mmol_L)
stopifnot(n_distinct(tg$eid) == nrow(tg))

# 15) APOB (30640)
df_30640 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30640.csv",
  stringsAsFactors = FALSE
)

apo_cols <- grep("^p30640(_i0|_0_)", names(df_30640), value = TRUE)
stopifnot(length(apo_cols) > 0)

apoB <- df_30640 %>%
  mutate(
    eid = as.integer(eid),
    apoB_g_L = as.numeric(.data[[apo_cols[1]]]),
    apoB_g_L = ifelse(apoB_g_L < 0, NA, apoB_g_L)
  ) %>%
  select(eid, apoB_g_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(apoB$apoB_g_L)
stopifnot(n_distinct(apoB$eid) == nrow(apoB))

# 16) HDL (30760)
df_30760 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30760.csv",
  stringsAsFactors = FALSE
)

hdl_cols <- grep("^p30760(_i0|_0_)", names(df_30760), value = TRUE)
stopifnot(length(hdl_cols) > 0)

hdl <- df_30760 %>%
  mutate(
    eid = as.integer(eid),
    HDL_mmol_L = as.numeric(.data[[hdl_cols[1]]]),
    HDL_mmol_L = ifelse(HDL_mmol_L < 0, NA, HDL_mmol_L)
  ) %>%
  select(eid, HDL_mmol_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(hdl$HDL_mmol_L)
stopifnot(n_distinct(hdl$eid) == nrow(hdl))

# 17) TOTAL CHOLESTEROL (30690)
df_30690 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30690.csv",
  stringsAsFactors = FALSE
)

tc_cols <- grep("^p30690(_i0|_0_)", names(df_30690), value = TRUE)
stopifnot(length(tc_cols) > 0)

totC <- df_30690 %>%
  mutate(
    eid = as.integer(eid),
    tot_C_mmol_L = as.numeric(.data[[tc_cols[1]]]),
    tot_C_mmol_L = ifelse(tot_C_mmol_L < 0, NA, tot_C_mmol_L)
  ) %>%
  select(eid, tot_C_mmol_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(totC$tot_C_mmol_L)
stopifnot(n_distinct(totC$eid) == nrow(totC))

# 18) LDL (30780)
df_30780 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30780.csv",
  stringsAsFactors = FALSE
)

ldl_cols <- grep("^p30780(_i0|_0_)", names(df_30780), value = TRUE)
stopifnot(length(ldl_cols) > 0)

ldl <- df_30780 %>%
  mutate(
    eid = as.integer(eid),
    LDL_mmol_L = as.numeric(.data[[ldl_cols[1]]]),
    LDL_mmol_L = ifelse(LDL_mmol_L < 0, NA, LDL_mmol_L)
  ) %>%
  select(eid, LDL_mmol_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(ldl$LDL_mmol_L)
stopifnot(n_distinct(ldl$eid) == nrow(ldl))

# 19) HBA1C (30750)
df_30750 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30750.csv",
  stringsAsFactors = FALSE
)

hba1c_cols <- grep("^p30750(_i0|_0_)", names(df_30750), value = TRUE)
stopifnot(length(hba1c_cols) > 0)

hba1c <- df_30750 %>%
  mutate(
    eid = as.integer(eid),
    HbA1c_mmol_mol = as.numeric(.data[[hba1c_cols[1]]]),
    HbA1c_mmol_mol = ifelse(HbA1c_mmol_mol < 0, NA, HbA1c_mmol_mol)
  ) %>%
  select(eid, HbA1c_mmol_mol) %>%
  distinct(eid, .keep_all = TRUE)

summary(hba1c$HbA1c_mmol_mol)
stopifnot(n_distinct(hba1c$eid) == nrow(hba1c))

# 20) CREATININE (30700)
df_30700 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_30700.csv",
  stringsAsFactors = FALSE
)

creat_cols <- grep("^p30700(_i0|_0_)", names(df_30700), value = TRUE)
stopifnot(length(creat_cols) > 0)

creat <- df_30700 %>%
  mutate(
    eid = as.integer(eid),
    creatinine_umol_L = as.numeric(.data[[creat_cols[1]]]),
    creatinine_umol_L = ifelse(creatinine_umol_L < 0, NA, creatinine_umol_L)
  ) %>%
  select(eid, creatinine_umol_L) %>%
  distinct(eid, .keep_all = TRUE)

summary(creat$creatinine_umol_L)
stopifnot(n_distinct(creat$eid) == nrow(creat))

n0 <- nrow(master)

master <- master %>%
  left_join(tg, by = "eid") %>%
  left_join(apoB, by = "eid") %>%
  left_join(hdl, by = "eid") %>%
  left_join(totC, by = "eid") %>%
  left_join(ldl, by = "eid") %>%
  left_join(hba1c, by = "eid") %>%
  left_join(creat, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

sum(is.na(master$tg_mmol_L))
sum(is.na(master$apoB_g_L))
sum(is.na(master$HDL_mmol_L))
sum(is.na(master$tot_C_mmol_L))
sum(is.na(master$LDL_mmol_L))
sum(is.na(master$HbA1c_mmol_mol))
sum(is.na(master$creatinine_umol_L))

# =========================================================
# UKBB medications (field 20003) -- BASELINE instance (i0)
# =========================================================
df_20003 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_20003.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

df_20003 <- df_20003 %>% clean_names()
med_cols <- grep("^p20003_i0_a\\d+$", names(df_20003), value = TRUE)
stopifnot(length(med_cols) > 0)
med_long <- df_20003 %>%
  transmute(
    eid = as.integer(eid),
    across(all_of(med_cols), as.character)
  ) %>%
  pivot_longer(
    cols = all_of(med_cols),
    names_to = "var",
    values_to = "coding"
  ) %>%
  filter(!is.na(coding), coding != "") %>%
  mutate(
    coding = trimws(coding),
    coding = gsub("\\.0$", "", coding)
  ) %>%
  filter(!coding %in% c("99999", "-1", "-3")) %>%
  distinct(eid, coding)

stopifnot(n_distinct(med_long$eid) <= nrow(df_20003))

cat("Medication columns used (instance i0 only):", length(med_cols), "\n")
print(head(med_cols, 10))
cat("Participants with >=1 medication code at i0:", n_distinct(med_long$eid), "\n")

file_med <- "C:/Users/nicol/Desktop/ukbb_obesity/data/UKB_Medications_df_20003_Federica.xlsx"

map_dm  <- read_excel(file_med, sheet = 1) %>% clean_names()
map_bp  <- read_excel(file_med, sheet = 2) %>% clean_names()
map_lip <- read_excel(file_med, sheet = 3) %>% clean_names()

map_dm <- map_dm %>%
  transmute(
    coding = gsub("\\.0$", "", as.character(coding)),
    class = toupper(class)
  ) %>%
  distinct()

map_bp <- map_bp %>%
  transmute(
    coding = gsub("\\.0$", "", as.character(coding)),
    class = toupper(class)
  ) %>%
  distinct()

map_lip <- map_lip %>%
  transmute(
    coding = gsub("\\.0$", "", as.character(coding)),
    class = toupper(class),
    statin_type = toupper(statin_type)
  ) %>%
  distinct()

names(map_dm)
names(map_bp)
names(map_lip)

dm_codes  <- map_dm$coding
bp_codes  <- map_bp$coding
lip_codes <- map_lip$coding

med_long <- med_long %>%
  mutate(coding = gsub("\\.0$", "", as.character(coding)))

med_flags <- med_long %>%
  group_by(eid) %>%
  summarise(
    on_diabetes_meds_any    = as.integer(any(coding %in% dm_codes)),
    on_antihypertensive_any = as.integer(any(coding %in% bp_codes)),
    on_lipid_lowering_any   = as.integer(any(coding %in% lip_codes)),
    .groups = "drop"
  )

n0 <- nrow(master)
master <- master %>%
  select(-any_of(c(
    "on_diabetes_meds_any",
    "on_antihypertensive_any",
    "on_lipid_lowering_any"
  ))) %>%
  left_join(med_flags, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

table(master$on_diabetes_meds_any, useNA = "ifany")
table(master$on_antihypertensive_any, useNA = "ifany")
table(master$on_lipid_lowering_any, useNA = "ifany")

sum(is.na(master$on_diabetes_meds_any))
sum(is.na(master$on_antihypertensive_any))
sum(is.na(master$on_lipid_lowering_any))

master <- master %>%
  mutate(
    on_diabetes_meds_any    = ifelse(is.na(on_diabetes_meds_any), 0L, on_diabetes_meds_any),
    on_antihypertensive_any = ifelse(is.na(on_antihypertensive_any), 0L, on_antihypertensive_any),
    on_lipid_lowering_any   = ifelse(is.na(on_lipid_lowering_any), 0L, on_lipid_lowering_any)
  )

table(master$on_diabetes_meds_any, useNA = "ifany")
table(master$on_antihypertensive_any, useNA = "ifany")
table(master$on_lipid_lowering_any, useNA = "ifany")

sum(is.na(master$on_diabetes_meds_any))
sum(is.na(master$on_antihypertensive_any))
sum(is.na(master$on_lipid_lowering_any))

# =========================================================
# CANCER REGISTRY
# 40006 = cancer ICD code
# 40005 = cancer diagnosis date
# Pre-post assessment + icd10
# =========================================================
df_40006 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_40006.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

df_40006 <- df_40006 %>%
  mutate(eid = as.integer(eid))

cancer_cols <- grep("^p40006", names(df_40006), value = TRUE)
stopifnot(length(cancer_cols) > 0)

cancer_icd <- df_40006 %>%
  select(eid, all_of(cancer_cols)) %>%
  pivot_longer(
    cols = all_of(cancer_cols),
    names_to = "var",
    values_to = "icd_code"
  ) %>%
  mutate(
    icd_code = trimws(icd_code),
    icd_code = na_if(icd_code, "")
  ) %>%
  filter(!is.na(icd_code)) %>%
  distinct(eid, icd_code)

n_distinct(cancer_icd$eid)
head(cancer_icd)

cancer_any <- cancer_icd %>%
  group_by(eid) %>%
  summarise(
    cancer_any = 1L,
    .groups = "drop"
  )

df_40005 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_40005.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

df_40005 <- df_40005 %>%
  mutate(eid = as.integer(eid))

date_cols <- grep("^p40005", names(df_40005), value = TRUE)
stopifnot(length(date_cols) > 0)

cancer_date <- df_40005 %>%
  select(eid, all_of(date_cols)) %>%
  pivot_longer(
    cols = all_of(date_cols),
    names_to = "var",
    values_to = "dx_date_raw"
  ) %>%
  mutate(
    dx_date_raw = trimws(dx_date_raw),
    dx_date_raw = na_if(dx_date_raw, ""),
    dx_date = as.Date(parse_date_time(dx_date_raw, orders = c("dmy","ymd","mdy")))
  ) %>%
  filter(!is.na(dx_date)) %>%
  group_by(eid) %>%
  summarise(
    cancer_date_first = min(dx_date, na.rm = TRUE),
    .groups = "drop"
  )
summary(cancer_date$cancer_date_first)
range(cancer_date$cancer_date_first, na.rm = TRUE)

n0 <- nrow(master)
master <- master %>%
  select(-any_of(c(
    "cancer_any",
    "cancer_date_first",
    "cancer_pre_baseline",
    "cancer_incident"
  ))) %>%
  left_join(cancer_any, by = "eid") %>%
  left_join(cancer_date, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

master <- master %>%
  mutate(
    cancer_any = dplyr::coalesce(cancer_any, 0L),
    cancer_any = ifelse(
      cancer_any == 1L | !is.na(cancer_date_first),
      1L, 0L
    )
  )

master <- master %>%
  mutate(
    
    cancer_pre_baseline = ifelse(
      cancer_any == 1 &
        !is.na(cancer_date_first) &
        cancer_date_first <= baseline_date,
      1L, 0L
    ),
    
    cancer_incident = ifelse(
      cancer_any == 1 &
        !is.na(cancer_date_first) &
        cancer_date_first > baseline_date,
      1L, 0L
    )
    
  )

table(master$cancer_any)
table(master$cancer_pre_baseline)
table(master$cancer_incident)

table(master$cancer_any, is.na(master$cancer_date_first))

sum(is.na(master$cancer_date_first))
range(master$cancer_date_first, na.rm = TRUE)

saveRDS(cancer_icd,
        "C:/Users/nicol/Desktop/ukbb_obesity/data/cancer_icd_long.rds")

# =========================================================
# ADD HESIN DIAGNOSES
# baseline anchor = baseline_date
# =========================================================
hesin_diag_raw <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/hesin_diag_gp.txt",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

hesin_long <- hesin_diag_raw %>%
  mutate(
    eid = as.integer(eid),
    diag_icd10 = toupper(trimws(diag_icd10)),
    diag_date = as.Date(parse_date_time(epistart, orders = c("dmy", "ymd", "mdy")))
  ) %>%
  filter(!is.na(eid), !is.na(diag_icd10), diag_icd10 != "", !is.na(diag_date)) %>%
  transmute(
    eid,
    diag = gsub("\\.", "", diag_icd10),
    diag_date
  ) %>%
  distinct() %>%
  left_join(master %>% select(eid, baseline_date), by = "eid") %>%
  filter(!is.na(baseline_date))

# A) CARDIOVASCULAR DISEASE
ascvd_regex <- paste0(
  "^(I70|I71|I74|I75|",
  "I200|I201|I202|I208|I209|",
  "I210|I211|I212|I213|I214|I219|",
  "I220|I221|I228|I229|",
  "I230|I231|I232|I233|I234|I235|I236|I238|",
  "I240|I241|I248|I249|",
  "I250|I251|I252|I253|I254|I255|I256|I257|I258|I259|",
  "I63|I64|G45",
  ")([0-9A-Z].*)?$"
)

ascvd_eid <- hesin_long %>%
  filter(grepl(ascvd_regex, diag)) %>%
  group_by(eid) %>%
  summarise(
    ascvd_related_event = 1L,
    ascvd_related_event_date = min(diag_date, na.rm = TRUE),
    ascvd_related_event_prebaseline  = as.integer(any(diag_date <= baseline_date, na.rm = TRUE)),
    ascvd_related_event_postbaseline = as.integer(any(diag_date >  baseline_date, na.rm = TRUE)),
    .groups = "drop"
  )

ascvd_post_dates <- hesin_long %>%
  filter(grepl(ascvd_regex, diag)) %>%
  filter(diag_date > baseline_date) %>%
  group_by(eid) %>%
  summarise(
    ascvd_related_event_date_postbaseline = min(diag_date, na.rm = TRUE),
    .groups = "drop"
  )

n0 <- nrow(master)

master <- master %>%
  select(-any_of(c(
    "ascvd_related_event",
    "ascvd_related_event_date",
    "ascvd_related_event_prebaseline",
    "ascvd_related_event_postbaseline",
    "ascvd_related_event_date_postbaseline"
  ))) %>%
  left_join(ascvd_eid, by = "eid") %>%
  left_join(ascvd_post_dates, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

master <- master %>%
  mutate(
    ascvd_related_event = ifelse(is.na(ascvd_related_event), 0L, ascvd_related_event),
    ascvd_related_event_prebaseline = ifelse(is.na(ascvd_related_event_prebaseline), 0L, ascvd_related_event_prebaseline),
    ascvd_related_event_postbaseline = ifelse(is.na(ascvd_related_event_postbaseline), 0L, ascvd_related_event_postbaseline)
  )

table(master$ascvd_related_event, useNA = "ifany")
table(master$ascvd_related_event_prebaseline, useNA = "ifany")
table(master$ascvd_related_event_postbaseline, useNA = "ifany")
summary(master$ascvd_related_event_date)
summary(master$ascvd_related_event_date_postbaseline)

#B) LIVER-RELATED EVENTS
liver_regex <- paste0(
  "^(K721|K729|",
  "K766|K7682|",
  "K652|",
  "R18|",
  "K767|",
  "I850|I851|I859|I864|",
  "I81|I82|",
  "Z944|",
  "C220",
  ")([0-9A-Z].*)?$"
)

liver_eid <- hesin_long %>%
  filter(grepl(liver_regex, diag)) %>%
  group_by(eid) %>%
  summarise(
    liver_related_event = 1L,
    liver_related_event_date = min(diag_date, na.rm = TRUE),
    liver_related_event_prebaseline  = as.integer(any(diag_date <= baseline_date, na.rm = TRUE)),
    liver_related_event_postbaseline = as.integer(any(diag_date >  baseline_date, na.rm = TRUE)),
    .groups = "drop"
  )

liver_post_dates <- hesin_long %>%
  filter(grepl(liver_regex, diag)) %>%
  filter(diag_date > baseline_date) %>%
  group_by(eid) %>%
  summarise(
    liver_related_event_date_postbaseline = min(diag_date, na.rm = TRUE),
    .groups = "drop"
  )

n0 <- nrow(master)

master <- master %>%
  select(-any_of(c(
    "liver_related_event",
    "liver_related_event_date",
    "liver_related_event_prebaseline",
    "liver_related_event_postbaseline",
    "liver_related_event_date_postbaseline"
  ))) %>%
  left_join(liver_eid, by = "eid") %>%
  left_join(liver_post_dates, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

master <- master %>%
  mutate(
    liver_related_event = ifelse(is.na(liver_related_event), 0L, liver_related_event),
    liver_related_event_prebaseline = ifelse(is.na(liver_related_event_prebaseline), 0L, liver_related_event_prebaseline),
    liver_related_event_postbaseline = ifelse(is.na(liver_related_event_postbaseline), 0L, liver_related_event_postbaseline)
  )

table(master$liver_related_event, useNA = "ifany")
table(master$liver_related_event_prebaseline, useNA = "ifany")
table(master$liver_related_event_postbaseline, useNA = "ifany")
summary(master$liver_related_event_date)
summary(master$liver_related_event_date_postbaseline)

# =========================================================
# DEATH REGISTRY
# Use PRIMARY cause of death only
# Coherent with HESIN definitions 
#==========================================================
death_reg <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/death_cause.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

death_reg <- death_reg %>%
  mutate(
    eid = as.integer(eid),
    death_date = as.Date(parse_date_time(date_of_death, orders = c("dmy", "ymd", "mdy"))),
    primary_cause = toupper(trimws(cause_icd10)),
    primary_cause = gsub("\\.", "", primary_cause)
  ) %>%
  filter(!is.na(eid), !is.na(death_date))

cvd_death_regex <- paste0(
  "^(I70|I71|I74|I75|",
  "I200|I201|I202|I208|I209|",
  "I210|I211|I212|I213|I214|I219|",
  "I220|I221|I228|I229|",
  "I230|I231|I232|I233|I234|I235|I236|I238|",
  "I240|I241|I248|I249|",
  "I250|I251|I252|I253|I254|I255|I256|I257|I258|I259|",
  "I63|I64|G45",
  ")([0-9A-Z].*)?$"
)

liver_death_regex <- paste0(
  "^(K721|K729|",
  "K766|K7682|",
  "K652|",
  "R18|",
  "K767|",
  "I850|I851|I859|I864|",
  "I81|I82|",
  "Z944|",
  "C220",
  ")([0-9A-Z].*)?$"
)

death_by_eid <- death_reg %>%
  group_by(eid) %>%
  summarise(
    death_any = 1L,
    death_date = min(death_date, na.rm = TRUE),
    death_cv_postbaseline = as.integer(any(
      !is.na(primary_cause) & grepl(cvd_death_regex, primary_cause)
    )),
    death_liver_postbaseline = as.integer(any(
      !is.na(primary_cause) & grepl(liver_death_regex, primary_cause)
    )),
    .groups = "drop"
  )

n0 <- nrow(master)

master <- master %>%
  select(-any_of(c(
    "death_any",
    "death_date",
    "death_cv_postbaseline",
    "death_liver_postbaseline"
  ))) %>%
  left_join(death_by_eid, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

master <- master %>%
  mutate(
    death_any = ifelse(is.na(death_any), 0L, death_any),
    death_cv_postbaseline = ifelse(is.na(death_cv_postbaseline), 0L, death_cv_postbaseline),
    death_liver_postbaseline = ifelse(is.na(death_liver_postbaseline), 0L, death_liver_postbaseline)
  )

table(master$death_any, useNA = "ifany")
table(master$death_cv_postbaseline, useNA = "ifany")
table(master$death_liver_postbaseline, useNA = "ifany")

summary(master$death_date)
sum(master$death_any == 0 & !is.na(master$death_date))

# =========================================================
# SELF-REPORTED DIAGNOSES (field 20002) -- BASELINE instance i0
# =========================================================

df_20002 <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/df_20002.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

code_cols <- grep("^p20002_i0_a\\d+$", names(df_20002), value = TRUE)
stopifnot(length(code_cols) > 0)

df_20002_long_i0 <- df_20002 %>%
  transmute(
    eid = as.integer(eid),
    across(all_of(code_cols), ~ suppressWarnings(as.integer(trimws(.x))))
  ) %>%
  pivot_longer(cols = all_of(code_cols), values_to = "illness_code") %>%
  filter(!is.na(eid), !is.na(illness_code)) %>%
  distinct(eid, illness_code)

stopifnot(n_distinct(df_20002_long_i0$eid) <= nrow(df_20002))

# =========================================================
# DEFINIZIONE COMORBIDITA' METABOLICHE
# =========================================================

stopifnot(exists("master"), exists("hesin_long"))
stopifnot(all(c("eid", "baseline_date") %in% names(master)))
stopifnot(all(c("eid", "diag", "diag_date", "baseline_date") %in% names(hesin_long)))

hesin_long_reg <- hesin_long %>%
  mutate(
    diag = gsub("\\.", "", toupper(trimws(diag)))
  )

### 1) HYPERTENSION (ICD10 I10 + self-report + antihypertensive meds)
# - ICD10 I10 pre-baseline
htn_icd <- hesin_long_reg %>%
  filter(!is.na(diag), grepl("^I10([0-9A-Z].*)?$", diag)) %>%
  group_by(eid) %>%
  summarise(
    htn_icd_prebaseline = as.integer(any(diag_date <= baseline_date, na.rm = TRUE)),
    .groups = "drop"
  )
# - Self-reported hypertension (20002 i0: 1065, 1072)
htn_selfrep <- df_20002_long_i0 %>%
  filter(illness_code %in% c(1065, 1072)) %>%
  distinct(eid) %>%
  mutate(htn_self_reported = 1L)
# - Meds already in master
master <- master %>%
  mutate(
    on_antihypertensive_any = ifelse(is.na(on_antihypertensive_any), 0L, as.integer(on_antihypertensive_any))
  )

n0 <- nrow(master)
master <- master %>%
  select(-any_of(c("htn_icd_prebaseline", "htn_self_reported", "hypertension_prebaseline"))) %>%
  left_join(htn_icd, by = "eid") %>%
  left_join(htn_selfrep, by = "eid")
stopifnot(nrow(master) == n0)
master <- master %>%
  mutate(
    htn_icd_prebaseline = ifelse(is.na(htn_icd_prebaseline), 0L, htn_icd_prebaseline),
    htn_self_reported = ifelse(is.na(htn_self_reported), 0L, htn_self_reported),
    hypertension_prebaseline = as.integer(
      htn_icd_prebaseline == 1L |
        htn_self_reported == 1L |
        on_antihypertensive_any == 1L
    )
  )

table(master$hypertension_prebaseline, useNA = "ifany")

### 2) DYSLIPIDEMIA (LAB + meds + ICD + self-report)
master <- master %>%
  mutate(
    LDL_mmol_L = as.numeric(LDL_mmol_L),
    tg_mmol_L = as.numeric(tg_mmol_L),
    HDL_mmol_L = as.numeric(HDL_mmol_L),
    apoB_g_L = as.numeric(apoB_g_L),
    on_lipid_lowering_any = ifelse(is.na(on_lipid_lowering_any), 0L, as.integer(on_lipid_lowering_any))
  )

# - Lab definition
master <- master %>%
  mutate(
    dyslipidemia_lab = as.integer(
      (!is.na(LDL_mmol_L) & LDL_mmol_L >= 3.4) |
        (!is.na(tg_mmol_L) & tg_mmol_L >= 2.0) |
        (!is.na(apoB_g_L) & apoB_g_L >= 1.0) |
        (!is.na(HDL_mmol_L) & sex == "male" & HDL_mmol_L < 1.0) |
        (!is.na(HDL_mmol_L) & sex == "female" & HDL_mmol_L < 1.3)
    )
  )

# - ICD10 E78.5 pre-baseline
dyslip_icd <- hesin_long_reg %>%
  filter(!is.na(diag), grepl("^E785([0-9A-Z].*)?$", diag)) %>%
  group_by(eid) %>%
  summarise(
    dyslip_icd_prebaseline = as.integer(any(diag_date <= baseline_date, na.rm = TRUE)),
    .groups = "drop"
  )

# - Self-reported dyslipidemia (20002 i0: 1473)
dyslip_selfrep <- df_20002_long_i0 %>%
  filter(illness_code == 1473) %>%
  distinct(eid) %>%
  transmute(eid, dyslip_self_reported = 1L)

n0 <- nrow(master)
master <- master %>%
  select(-any_of(c(
    "dyslip_icd_prebaseline",
    "dyslip_self_reported",
    "dyslipidemia_prebaseline"
  ))) %>%
  left_join(dyslip_icd, by = "eid") %>%
  left_join(dyslip_selfrep, by = "eid")
stopifnot(nrow(master) == n0)
master <- master %>%
  mutate(
    dyslip_icd_prebaseline = ifelse(is.na(dyslip_icd_prebaseline), 0L, dyslip_icd_prebaseline),
    dyslip_self_reported = ifelse(is.na(dyslip_self_reported), 0L, dyslip_self_reported),
    dyslipidemia_prebaseline = as.integer(
      dyslipidemia_lab == 1L |
        on_lipid_lowering_any == 1L |
        dyslip_icd_prebaseline == 1L |
        dyslip_self_reported == 1L
    )
  )

table(master$dyslipidemia_prebaseline, useNA = "ifany")

### 3) TYPE 2 DIABETES (HbA1c + meds + ICD + self-report + Exclude DM1)
master <- master %>%
  mutate(
    HbA1c_mmol_mol = as.numeric(HbA1c_mmol_mol),
    on_diabetes_meds_any = ifelse(is.na(on_diabetes_meds_any), 0L, as.integer(on_diabetes_meds_any)),
    dm_hba1c = as.integer(!is.na(HbA1c_mmol_mol) & HbA1c_mmol_mol >= 48)
  )

# - DM2 ICD pre-baseline: E11/E14 (exclude E10)
dm2_icd <- hesin_long_reg %>%
  filter(
    !is.na(diag),
    (grepl("^E11", diag) | grepl("^E14", diag)),
    !grepl("^E10", diag),
    !is.na(diag_date),
    diag_date <= baseline_date
  ) %>%
  distinct(eid) %>%
  transmute(eid, dm2_icd_prebaseline = 1L)

# - DM1 ICD pre-baseline
dm1_icd <- hesin_long_reg %>%
  filter(
    !is.na(diag),
    grepl("^E10", diag),
    !is.na(diag_date),
    diag_date <= baseline_date
  ) %>%
  distinct(eid) %>%
  transmute(eid, dm1_icd_prebaseline = 1L)

# - Self-reported type 2 diabetes (20002 i0: 1223)
dm2_selfrep <- df_20002_long_i0 %>%
  filter(illness_code == 1223) %>%
  distinct(eid) %>%
  transmute(eid, dm2_self_reported = 1L)

n0 <- nrow(master)
master <- master %>%
  select(-any_of(c(
    "dm2_icd_prebaseline",
    "dm1_icd_prebaseline",
    "dm2_self_reported",
    "dm2_prebaseline_raw",
    "dm2_prebaseline"
  ))) %>%
  left_join(dm2_icd, by = "eid") %>%
  left_join(dm1_icd, by = "eid") %>%
  left_join(dm2_selfrep, by = "eid")
stopifnot(nrow(master) == n0)
master <- master %>%
  mutate(
    dm2_icd_prebaseline = ifelse(is.na(dm2_icd_prebaseline), 0L, dm2_icd_prebaseline),
    dm1_icd_prebaseline = ifelse(is.na(dm1_icd_prebaseline), 0L, dm1_icd_prebaseline),
    dm2_self_reported = ifelse(is.na(dm2_self_reported), 0L, dm2_self_reported),
    dm2_prebaseline_raw = as.integer(
      dm2_self_reported == 1L |
        dm2_icd_prebaseline == 1L |
        dm_hba1c == 1L |
        on_diabetes_meds_any == 1L
    ),
    dm2_prebaseline = as.integer(dm2_prebaseline_raw == 1L & dm1_icd_prebaseline == 0L)
  )

table(master$dm2_prebaseline, useNA = "ifany")

### 4) PREDIABETES BY HBA1C
master <- master %>%
  mutate(
    prediabetes_hba1c = as.integer(
      !is.na(HbA1c_mmol_mol) &
        HbA1c_mmol_mol >= 39 & HbA1c_mmol_mol <= 47 &
        dm2_prebaseline == 0L &
        on_diabetes_meds_any == 0L &
        dm1_icd_prebaseline == 0L
    )
  )

table(master$prediabetes_hba1c, useNA = "ifany")

master <- master %>%
  select(-any_of(c(
    "htn_icd_prebaseline",
    "htn_self_reported",
    "dyslipidemia_lab",
    "dyslip_icd_prebaseline",
    "dyslip_self_reported",
    "dm_hba1c",
    "dm2_prebaseline_raw",
    "dm2_icd_prebaseline",
    "dm1_icd_prebaseline",
    "dm2_self_reported"
  )))

master <- master %>%
  rename(
    hypertension_baseline = hypertension_prebaseline,
    dyslipidemia_baseline = dyslipidemia_prebaseline,
    dm2_baseline = dm2_prebaseline,
    prediabetes_baseline = prediabetes_hba1c
  )

# =========================================================
# CARDIOVASCULAR PROCEDURES (OPCS-4) — REVASCULARIZATION
# baseline anchor = baseline_date
# =========================================================

hesin_oper_raw <- read.csv(
  "C:/Users/nicol/Desktop/ukbb_obesity/data/hesin_oper_gp.csv",
  stringsAsFactors = FALSE,
  colClasses = "character",
  check.names = FALSE
)

hesin_oper_long <- hesin_oper_raw %>%
  mutate(
    eid = as.integer(eid),
    opdate_clean = na_if(trimws(as.character(opdate)), "NaT"),
    oper_date = as.Date(parse_date_time(opdate_clean, orders = c("ymd", "dmy", "mdy")))
  ) %>%
  select(eid, oper_date, oper3, oper4) %>%
  pivot_longer(
    cols = c(oper3, oper4),
    names_to = "oper_field",
    values_to = "opcs"
  ) %>%
  mutate(
    opcs = toupper(str_trim(opcs)),
    opcs = str_replace_all(opcs, "\\.", "")
  ) %>%
  filter(!is.na(eid), !is.na(opcs), opcs != "", !is.na(oper_date)) %>%
  distinct(eid, oper_date, opcs) %>%
  left_join(master %>% select(eid, baseline_date), by = "eid") %>%
  filter(!is.na(baseline_date)) %>%
  mutate(
    postbaseline = oper_date > baseline_date,
    prebaseline  = oper_date <= baseline_date
  )

opcs_regex <- "^(K40|K41|K42|K43|K44|K45|K46|K49|K50)"
revasc_eid <- hesin_oper_long %>%
  filter(grepl(opcs_regex, opcs)) %>%
  group_by(eid) %>%
  summarise(
    revasc_event = 1L,
    revasc_event_date = min(oper_date, na.rm = TRUE),
    revasc_event_prebaseline  = as.integer(any(prebaseline, na.rm = TRUE)),
    revasc_event_postbaseline = as.integer(any(postbaseline, na.rm = TRUE)),
    revasc_event_date_postbaseline = {
      d <- oper_date[postbaseline & !is.na(oper_date)]
      if (length(d) == 0) as.Date(NA) else min(d)
    },
    .groups = "drop"
  )

n0 <- nrow(master)

master <- master %>%
  select(-any_of(c(
    "revasc_event",
    "revasc_event_date",
    "revasc_event_prebaseline",
    "revasc_event_postbaseline",
    "revasc_event_date_postbaseline"
  ))) %>%
  left_join(revasc_eid, by = "eid")

stopifnot(nrow(master) == n0)
stopifnot(n_distinct(master$eid) == nrow(master))

master <- master %>%
  mutate(
    revasc_event = ifelse(is.na(revasc_event), 0L, revasc_event),
    revasc_event_prebaseline = ifelse(is.na(revasc_event_prebaseline), 0L, revasc_event_prebaseline),
    revasc_event_postbaseline = ifelse(is.na(revasc_event_postbaseline), 0L, revasc_event_postbaseline)
  )

table(master$revasc_event, useNA = "ifany")
table(master$revasc_event_prebaseline, useNA = "ifany")
table(master$revasc_event_postbaseline, useNA = "ifany")

summary(master$revasc_event_date)
summary(master$revasc_event_date_postbaseline)

sum(master$revasc_event == 0 & !is.na(master$revasc_event_date))
sum(master$revasc_event_postbaseline == 0 & !is.na(master$revasc_event_date_postbaseline))

##############################
###BUILDING ANALYSIS COHORT###
##############################
cohort <- master
nrow(cohort)

#ELIMINAZIONE SOGGETTI SENZA TUTTI I PARAMETRI ANTROPOMETRICI#
n0 <- nrow(cohort)
cohort <- cohort %>%
  filter(
    !is.na(bmi),
    !is.na(height),
    !is.na(weight),
    !is.na(waist_c),
    !is.na(hip_c)
  )

cat("Step 2 - complete anthropometry\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ELIMINAZIONE OUTLIER SU PARAMETRI ANTROPOMETRICI#
n0 <- nrow(cohort)

cohort <- cohort %>%
  filter(
    bmi >= 18.5 & bmi <= 300,
    height >= 100 & height <= 250,
    weight >= 30 & weight <= 300,
    waist_c >= 10 & waist_c <= 300,
    hip_c >= 50 & hip_c <= 200
  )

cat("Step 3 - anthropometric outliers removal\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ESCLUSIONE SOGGETTI CON EVENTI CV AL BASELINE#
n0 <- nrow(cohort)

cohort <- cohort %>%
  filter(
    ascvd_related_event_prebaseline == 0,
    revasc_event_prebaseline == 0
  )

cat("Step 4 - exclude prevalent ASCVD\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ESCLUSIONE SOGGETTI CON EVENTI LIVER AL BASELINE#
n0 <- nrow(cohort)

cohort <- cohort %>%
  filter(liver_related_event_prebaseline == 0)

cat("Step 5 - exclude prevalent liver disease\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ESCLUSIONE SOGGETTI CON CANCRO AL BASELINE#
n0 <- nrow(cohort)

cohort <- cohort %>%
  filter(cancer_pre_baseline == 0)

cat("Step 6 - exclude prevalent cancer\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ESCLUSIONE DONNE IN GRAVIDANZA#
n0 <- nrow(cohort)

cohort <- cohort %>%
  filter(is.na(pregnancy) | pregnancy == 0)

cat("Step - exclude pregnancy\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

#ESCLUSIONE DI SPECIFIC CONDITIONS (ICD-10) AT BASELINE#
exclusion_regex <- paste0(
  "^(E24|E881|E40|E41|E42|E43|E44|E45|E46|R64|E85|E230|",
  "K743|K745|K744|K754|K701|K709|K702|",
  "B180|B181|B182|B188|B189|B190|B191|B192|",
  "K753|K758|E830|K71)([0-9A-Z].*)?$"
)

exclusion_conditions <- hesin_long %>%
  mutate(diag = gsub("\\.", "", toupper(trimws(diag)))) %>%
  filter(
    grepl(exclusion_regex, diag),
    !is.na(diag_date),
    diag_date <= baseline_date
  ) %>%
  distinct(eid) %>%
  mutate(exclusion_condition = 1L)

n0 <- nrow(cohort)

cohort <- cohort %>%
  left_join(exclusion_conditions, by = "eid") %>%
  filter(is.na(exclusion_condition) | exclusion_condition == 0)

cat("Step - exclude specific ICD conditions\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")
cohort <- cohort %>% select(-exclusion_condition)

#ELIMINAZIONE SELF-REPORTED (BASELINE)#
selfrep_exclusion_codes <- c(
  1075,1081,1082,   # cardiovascular
  1604,1506,1141,1158,1024,1579,1580,1581  # liver disease
)

selfrep_exclusion <- df_20002_long_i0 %>%
  filter(illness_code %in% selfrep_exclusion_codes) %>%
  distinct(eid) %>%
  mutate(selfrep_exclusion = 1L)

n0 <- nrow(cohort)

cohort <- cohort %>%
  left_join(selfrep_exclusion, by = "eid") %>%
  filter(is.na(selfrep_exclusion) | selfrep_exclusion == 0)

cat("Step - exclude self-reported CV/liver conditions\n")
cat("Before:", n0, "\n")
cat("After :", nrow(cohort), "\n")
cat("Removed:", n0 - nrow(cohort), "\n")

cohort <- cohort %>% select(-selfrep_exclusion)

########################################
#######CLASSIFICAZIONI OBESITY##########
########################################
#CLASSIFICAZIONE BMI-BASED#
cohort <- cohort %>%
  mutate(
    bmi_category = case_when(
      bmi < 25 ~ "No obesity",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obesity",
      TRUE ~ NA_character_
    ),
    bmi_category = factor(
      bmi_category,
      levels = c("No obesity", "Overweight", "Obesity")
    ),
    obesity_bmi = as.integer(bmi >= 30)
  )

table(cohort$bmi_category, useNA = "ifany")
table(cohort$obesity_bmi, useNA = "ifany")

#DEFINIZIONE LANCET#
cohort <- cohort %>%
  mutate(
    waist_high = case_when(
      sex == "male" & waist_c >= 102 ~ 1L,
      sex == "female" & waist_c >= 88 ~ 1L,
      TRUE ~ 0L
    ),
    whr_high = case_when(
      sex == "male" & whr > 0.90 ~ 1L,
      sex == "female" & whr > 0.85 ~ 1L,
      TRUE ~ 0L
    ),
    whtr_high = as.integer(whtr > 0.50),
    n_anthro_high = waist_high + whr_high + whtr_high
  )

table(cohort$waist_high, useNA = "ifany")
table(cohort$whr_high, useNA = "ifany")
table(cohort$whtr_high, useNA = "ifany")
table(cohort$n_anthro_high, useNA = "ifany")

cohort <- cohort %>%
  mutate(
    obesity_lancet = case_when(
      bmi >= 40 ~ 1L,
      n_anthro_high >= 2 ~ 1L,
      bmi >= 30 & n_anthro_high >= 1 ~ 1L,
      TRUE ~ 0L
    )
  )

table(cohort$obesity_lancet, useNA = "ifany")

cohort <- cohort %>%
  mutate(
    lancet_category = case_when(
      obesity_lancet == 0 ~ "No obesity",
      obesity_lancet == 1 ~ "Obesity",
      TRUE ~ NA_character_
    ),
    lancet_category = factor(
      lancet_category,
      levels = c("No obesity", "Obesity")
    )
  )

table(cohort$lancet_category, useNA = "ifany")

#DEFINIZIONE CLINICAL OBESITY (LANCET)#
clinical_obesity_regex <- paste0(
  "^(G932|G473|E662|I50|I48|I27|I26|I82|I10|",
  "E11|E14|E78|K760|K74|N18|N394|R32|",
  "E282|N91|N97|E291|M16|M17|I890)([0-9A-Z].*)?$"
)

clinical_icd <- hesin_long_reg %>%
  mutate(
    diag = gsub("\\.", "", toupper(trimws(diag)))
  ) %>%
  filter(
    !is.na(baseline_date),
    grepl(clinical_obesity_regex, diag),
    !is.na(diag_date),
    diag_date <= baseline_date
  ) %>%
  distinct(eid) %>%
  mutate(clinical_obesity_icd = 1L)

n0 <- nrow(cohort)

cohort <- cohort %>%
  select(-any_of(c(
    "clinical_obesity_icd",
    "hyperglycaemia",
    "high_triglycerides",
    "low_hdl",
    "metabolic_cluster_n",
    "clinical_obesity_metabolic",
    "clinical_obesity_any",
    "lancet_obesity_stage",
    "obesity_preclinical",
    "obesity_clinical"
  ))) %>%
  left_join(clinical_icd, by = "eid")

stopifnot(nrow(cohort) == n0)

cohort <- cohort %>%
  mutate(
    clinical_obesity_icd = ifelse(is.na(clinical_obesity_icd), 0L, clinical_obesity_icd),
    
    tg_mmol_L = as.numeric(tg_mmol_L),
    HDL_mmol_L = as.numeric(HDL_mmol_L),
    
    hyperglycaemia = as.integer(dm2_baseline == 1L),
    
    high_triglycerides = as.integer(
      !is.na(tg_mmol_L) & tg_mmol_L >= 1.7
    ),
    
    low_hdl = as.integer(
      (!is.na(HDL_mmol_L) & sex == "male" & HDL_mmol_L < 1.0) |
        (!is.na(HDL_mmol_L) & sex == "female" & HDL_mmol_L < 1.3)
    ),
    
    metabolic_cluster_n = hyperglycaemia + high_triglycerides + low_hdl,
    
    clinical_obesity_metabolic = as.integer(metabolic_cluster_n >= 2),
    
    clinical_obesity_any = as.integer(
      clinical_obesity_icd == 1L |
        clinical_obesity_metabolic == 1L
    ),
    
    lancet_obesity_stage = case_when(
      obesity_lancet == 0L ~ "No obesity",
      obesity_lancet == 1L & clinical_obesity_any == 0L ~ "Preclinical obesity",
      obesity_lancet == 1L & clinical_obesity_any == 1L ~ "Clinical obesity",
      TRUE ~ NA_character_
    ),
    
    lancet_obesity_stage = factor(
      lancet_obesity_stage,
      levels = c("No obesity", "Preclinical obesity", "Clinical obesity")
    ),
    
    obesity_preclinical = as.integer(lancet_obesity_stage == "Preclinical obesity"),
    obesity_clinical = as.integer(lancet_obesity_stage == "Clinical obesity")
  )

#CHECK#
table(cohort$clinical_obesity_icd, useNA = "ifany")
table(cohort$hyperglycaemia, useNA = "ifany")
table(cohort$high_triglycerides, useNA = "ifany")
table(cohort$low_hdl, useNA = "ifany")
table(cohort$metabolic_cluster_n, useNA = "ifany")
table(cohort$clinical_obesity_metabolic, useNA = "ifany")
table(cohort$clinical_obesity_any, useNA = "ifany")
table(cohort$lancet_obesity_stage, useNA = "ifany")

sum(cohort$obesity_lancet == 0 & cohort$obesity_clinical == 1, na.rm = TRUE)
sum(cohort$obesity_lancet == 0 & cohort$obesity_preclinical == 1, na.rm = TRUE)
sum(cohort$obesity_clinical == 1 & cohort$obesity_preclinical == 1, na.rm = TRUE)

table(cohort$obesity_lancet, cohort$lancet_obesity_stage, useNA = "ifany")
