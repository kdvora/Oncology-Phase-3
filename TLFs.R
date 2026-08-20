# TLFs generated from a rendered HTML report for portfolio use.
# Source ADaM datasets and report outputs are intentionally excluded.

library(dplyr)
library(tidyr)
library(ggplot2)
library(survival)
library(nlme)
library(emmeans)
library(patchwork)
library(grid)

# Normalize study-specific treatment values to generic portfolio labels.
normalize_trt <- function(x) {
  x <- as.character(x)
  ifelse(grepl("placebo|control", x, ignore.case = TRUE), "Control",
         ifelse(is.na(x) | x == "", NA_character_, "Active"))
}

# Replace this lightweight display helper with a validated reporting function
# when producing submission-ready output.
fmt_table <- function(x) print(x)

# ===== TLF SECTION 1 =====

# Clean treatment/category variables
adsl <- adsl %>%
  mutate(
    TRT01P = normalize_trt(TRT01P),
    AGEGR1 = as.character(AGEGR1),
    ETHNIC = as.character(ETHNIC),
    RACE   = as.character(RACE),
    BECOG  = as.character(BECOG),
    REMISS = as.character(REMISS)
  )

denom <- adsl %>%
  group_by(TRT01P) %>%
  summarise(N = n(), .groups = "drop")

denom_total <- nrow(adsl)


# AGE
age_summary <- adsl %>%
  group_by(TRT01P) %>%
  summarise(
    n = sum(!is.na(AGE)),
    mean = mean(AGE, na.rm = TRUE),
    sd = sd(AGE, na.rm = TRUE),
    median = median(AGE, na.rm = TRUE),
    min = min(AGE, na.rm = TRUE),
    max = max(AGE, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

age_total <- adsl %>%
  summarise(
    TRT01P = "All Patients",
    n = sum(!is.na(AGE)),
    mean = mean(AGE, na.rm = TRUE),
    sd = sd(AGE, na.rm = TRUE),
    median = median(AGE, na.rm = TRUE),
    min = min(AGE, na.rm = TRUE),
    max = max(AGE, na.rm = TRUE)
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

age_tlf <- bind_rows(age_summary, age_total) %>%
  mutate(n = as.character(n)) %>%
  pivot_longer(
    cols = c(n, `Mean (SD)`, Median, Range),
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = TRT01P, values_from = Value) %>%
  mutate(
    Section = "Age (yr)",
    Statistic = factor(Statistic, levels = c("n", "Mean (SD)", "Median", "Range"))
  ) %>%
  arrange(Statistic) %>%
  mutate(Statistic = as.character(Statistic)) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

age_header <- tibble(
  Section = "Age (yr)",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

age_tlf <- bind_rows(age_header, age_tlf)


# AGE GROUP
age_group <- adsl %>%
  mutate(
    AGEGR1 = if_else(is.na(AGEGR1) | AGEGR1 == "", "Not Available", AGEGR1)
  ) %>%
  group_by(TRT01P, AGEGR1) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, AGEGR1, result)

age_group_total <- adsl %>%
  mutate(
    AGEGR1 = if_else(is.na(AGEGR1) | AGEGR1 == "", "Not Available", AGEGR1)
  ) %>%
  group_by(AGEGR1) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRT01P = "All Patients",
    result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)
  ) %>%
  select(TRT01P, AGEGR1, result)

age_group_tlf <- bind_rows(age_group, age_group_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "Age Group",
    Statistic = AGEGR1,
    order = case_when(
      Statistic %in% c("18-40", "18 – 40", "18 to 40") ~ 1,
      Statistic %in% c("41-64", "41 – 64", "41 to 64") ~ 2,
      Statistic %in% c(">=65", "≥65", "65+") ~ 3,
      Statistic == "Not Available" ~ 4,
      TRUE ~ 99
    )
  ) %>%
  arrange(order) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

age_group_header <- tibble(
  Section = "Age Group",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

age_group_tlf <- bind_rows(age_group_header, age_group_tlf)


# SEX - Female only
sex_summary <- adsl %>%
  group_by(TRT01P) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, result)

sex_total <- adsl %>%
  summarise(
    TRT01P = "All Patients",
    n = n()
  ) %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)) %>%
  select(TRT01P, result)

sex_tlf <- bind_rows(sex_summary, sex_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "Sex",
    Statistic = "Female"
  ) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

sex_header <- tibble(
  Section = "Sex",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

sex_tlf <- bind_rows(sex_header, sex_tlf)


# ETHNICITY
ethnicity_summary <- adsl %>%
  mutate(
    ETHNIC = if_else(is.na(ETHNIC) | ETHNIC == "", "Not Available", ETHNIC)
  ) %>%
  group_by(TRT01P, ETHNIC) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, ETHNIC, result)

ethnicity_total <- adsl %>%
  mutate(
    ETHNIC = if_else(is.na(ETHNIC) | ETHNIC == "", "Not Available", ETHNIC)
  ) %>%
  group_by(ETHNIC) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRT01P = "All Patients",
    result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)
  ) %>%
  select(TRT01P, ETHNIC, result)

ethnicity_tlf <- bind_rows(ethnicity_summary, ethnicity_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "Ethnicity",
    Statistic = ETHNIC,
    order = case_when(
      Statistic == "Hispanic or Latino" ~ 1,
      Statistic == "Not Hispanic or Latino" ~ 2,
      Statistic == "Not Available" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(order) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

ethnicity_header <- tibble(
  Section = "Ethnicity",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

ethnicity_tlf <- bind_rows(ethnicity_header, ethnicity_tlf)


# RACE
race_summary <- adsl %>%
  mutate(
    RACE = if_else(is.na(RACE) | RACE == "", "Not Available", RACE)
  ) %>%
  group_by(TRT01P, RACE) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, RACE, result)

race_total <- adsl %>%
  mutate(
    RACE = if_else(is.na(RACE) | RACE == "", "Not Available", RACE)
  ) %>%
  group_by(RACE) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRT01P = "All Patients",
    result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)
  ) %>%
  select(TRT01P, RACE, result)

race_tlf <- bind_rows(race_summary, race_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "Race",
    Statistic = RACE,
    order = case_when(
      Statistic == "American Indian or Alaska Native" ~ 1,
      Statistic == "Asian" ~ 2,
      Statistic == "Black or African American" ~ 3,
      Statistic == "Native Hawaiian or Other Pacific Islander" ~ 4,
      Statistic == "White" ~ 5,
      Statistic == "Not Available" ~ 6,
      TRUE ~ 99
    )
  ) %>%
  arrange(order) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

race_header <- tibble(
  Section = "Race",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

race_tlf <- bind_rows(race_header, race_tlf)


# WEIGHT
weight_summary <- adsl %>%
  group_by(TRT01P) %>%
  summarise(
    n = sum(!is.na(BWT)),
    mean = mean(BWT, na.rm = TRUE),
    sd = sd(BWT, na.rm = TRUE),
    median = median(BWT, na.rm = TRUE),
    min = min(BWT, na.rm = TRUE),
    max = max(BWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

weight_total <- adsl %>%
  summarise(
    TRT01P = "All Patients",
    n = sum(!is.na(BWT)),
    mean = mean(BWT, na.rm = TRUE),
    sd = sd(BWT, na.rm = TRUE),
    median = median(BWT, na.rm = TRUE),
    min = min(BWT, na.rm = TRUE),
    max = max(BWT, na.rm = TRUE)
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

weight_tlf <- bind_rows(weight_summary, weight_total) %>%
  mutate(n = as.character(n)) %>%
  pivot_longer(
    cols = c(n, `Mean (SD)`, Median, Range),
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = TRT01P, values_from = Value) %>%
  mutate(
    Section = "Weight (kg)",
    Statistic = factor(Statistic, levels = c("n", "Mean (SD)", "Median", "Range"))
  ) %>%
  arrange(Statistic) %>%
  mutate(Statistic = as.character(Statistic)) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

weight_header <- tibble(
  Section = "Weight (kg)",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

weight_tlf <- bind_rows(weight_header, weight_tlf)


# ECOG
ecog_summary <- adsl %>%
  mutate(
    BECOG = if_else(is.na(BECOG) | BECOG == "", "Not Available", BECOG)
  ) %>%
  group_by(TRT01P, BECOG) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, BECOG, result)

ecog_total <- adsl %>%
  mutate(
    BECOG = if_else(is.na(BECOG) | BECOG == "", "Not Available", BECOG)
  ) %>%
  group_by(BECOG) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRT01P = "All Patients",
    result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)
  ) %>%
  select(TRT01P, BECOG, result)

ecog_tlf <- bind_rows(ecog_summary, ecog_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "ECOG Score",
    Statistic = BECOG,
    order = case_when(
      Statistic == "0" ~ 1,
      Statistic == "1" ~ 2,
      Statistic == "Not Available" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(order) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

ecog_header <- tibble(
  Section = "ECOG Score",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

ecog_tlf <- bind_rows(ecog_header, ecog_tlf)


# CURRENT REMISSION STATUS
remission_summary <- adsl %>%
  mutate(
    REMISS = if_else(is.na(REMISS) | REMISS == "", "Not Available", REMISS)
  ) %>%
  group_by(TRT01P, REMISS) %>%
  summarise(n = n(), .groups = "drop") %>%
  left_join(denom, by = "TRT01P") %>%
  mutate(result = sprintf("%d (%.1f%%)", n, 100 * n / N)) %>%
  select(TRT01P, REMISS, result)

remission_total <- adsl %>%
  mutate(
    REMISS = if_else(is.na(REMISS) | REMISS == "", "Not Available", REMISS)
  ) %>%
  group_by(REMISS) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    TRT01P = "All Patients",
    result = sprintf("%d (%.1f%%)", n, 100 * n / denom_total)
  ) %>%
  select(TRT01P, REMISS, result)

remission_tlf <- bind_rows(remission_summary, remission_total) %>%
  pivot_wider(names_from = TRT01P, values_from = result) %>%
  mutate(
    Section = "Current Remission Status",
    Statistic = REMISS,
    order = case_when(
      Statistic == "2nd" ~ 1,
      Statistic == "3rd" ~ 2,
      Statistic == "Not Available" ~ 3,
      TRUE ~ 99
    )
  ) %>%
  arrange(order) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

remission_header <- tibble(
  Section = "Current Remission Status",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

remission_tlf <- bind_rows(remission_header, remission_tlf)


# WEEKS FROM LAST THERAPY
# Using PRTXDUR as proxy. Confirm against spec if required.
weeks_summary <- adsl %>%
  group_by(TRT01P) %>%
  summarise(
    n = sum(!is.na(PRTXDUR)),
    mean = mean(PRTXDUR, na.rm = TRUE),
    sd = sd(PRTXDUR, na.rm = TRUE),
    median = median(PRTXDUR, na.rm = TRUE),
    min = min(PRTXDUR, na.rm = TRUE),
    max = max(PRTXDUR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

weeks_total <- adsl %>%
  summarise(
    TRT01P = "All Patients",
    n = sum(!is.na(PRTXDUR)),
    mean = mean(PRTXDUR, na.rm = TRUE),
    sd = sd(PRTXDUR, na.rm = TRUE),
    median = median(PRTXDUR, na.rm = TRUE),
    min = min(PRTXDUR, na.rm = TRUE),
    max = max(PRTXDUR, na.rm = TRUE)
  ) %>%
  mutate(
    `Mean (SD)` = sprintf("%.1f (%.1f)", mean, sd),
    Median = sprintf("%.1f", median),
    Range = sprintf("%.1f - %.1f", min, max)
  ) %>%
  select(TRT01P, n, `Mean (SD)`, Median, Range)

weeks_tlf <- bind_rows(weeks_summary, weeks_total) %>%
  mutate(n = as.character(n)) %>%
  pivot_longer(
    cols = c(n, `Mean (SD)`, Median, Range),
    names_to = "Statistic",
    values_to = "Value"
  ) %>%
  pivot_wider(names_from = TRT01P, values_from = Value) %>%
  mutate(
    Section = "Weeks from Last Therapy",
    Statistic = factor(Statistic, levels = c("n", "Mean (SD)", "Median", "Range"))
  ) %>%
  arrange(Statistic) %>%
  mutate(Statistic = as.character(Statistic)) %>%
  select(Section, Statistic, Control, `Active`, `All Patients`)

weeks_header <- tibble(
  Section = "Weeks from Last Therapy",
  Statistic = "",
  Control = "",
  `Active` = "",
  `All Patients` = ""
)

weeks_tlf <- bind_rows(weeks_header, weeks_tlf)


# FINAL DEMOGRAPHIC TABLE
final_demog <- bind_rows(
  age_tlf,
  age_group_tlf,
  sex_tlf,
  ethnicity_tlf,
  race_tlf,
  weight_tlf,
  ecog_tlf,
  remission_tlf,
  weeks_tlf
)

fmt_table(final_demog)


# ===== TLF SECTION 2 =====

# Prepare ADAE
adae_saf <- adae %>%
  filter(SAFFL == "Y") %>%
  mutate(
    AEBODSYS = if_else(is.na(AEBODSYS) | AEBODSYS == "", "Not Available", AEBODSYS),
    AEDECOD  = if_else(is.na(AEDECOD)  | AEDECOD  == "", "Not Available", AEDECOD),
    AETOXGR  = if_else(is.na(AETOXGR)  | AETOXGR  == "", "Not graded", AETOXGR),
    AETOXGRN2 = case_when(
      AETOXGR == "5" ~ 5,
      AETOXGR == "4" ~ 4,
      AETOXGR == "3" ~ 3,
      AETOXGR == "2" ~ 2,
      AETOXGR == "1" ~ 1,
      AETOXGR == "Not graded" ~ 0,
      TRUE ~ suppressWarnings(as.numeric(AETOXGR))
    )
  )

# Denominator
denom_ae <- adae_saf %>%
  distinct(USUBJID, TRT01A) %>%
  group_by(TRT01A) %>%
  summarise(N = n(), .groups = "drop")

trt_levels <- denom_ae$TRT01A

grade_shell <- tibble(
  Grade = c("- All grades -", "5", "4", "3", "2", "1", "Not graded"),
  grade_order = 1:7
)

# Highest grade datasets
adae_any_max <- adae_saf %>%
  group_by(USUBJID, TRT01A) %>%
  arrange(desc(AETOXGRN2), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

adae_soc_max <- adae_saf %>%
  group_by(USUBJID, TRT01A, AEBODSYS) %>%
  arrange(desc(AETOXGRN2), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

adae_pt_max <- adae_saf %>%
  group_by(USUBJID, TRT01A, AEBODSYS, AEDECOD) %>%
  arrange(desc(AETOXGRN2), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# ---------------------------
# ANY AE BLOCK
# ---------------------------

any_all <- adae_any_max %>%
  distinct(TRT01A, USUBJID) %>%
  count(TRT01A, name = "n") %>%
  mutate(
    SOC = "Any adverse events",
    PT = "",
    Grade = "- All grades -"
  )

any_grade <- adae_any_max %>%
  count(TRT01A, AETOXGR, name = "n") %>%
  transmute(
    TRT01A,
    SOC = "Any adverse events",
    PT = "",
    Grade = AETOXGR,
    n
  )

any_block <- bind_rows(any_all, any_grade) %>%
  complete(
    TRT01A = trt_levels,
    SOC = "Any adverse events",
    PT = "",
    Grade = grade_shell$Grade,
    fill = list(n = 0)
  ) %>%
  left_join(denom_ae, by = "TRT01A") %>%
  left_join(grade_shell, by = "Grade") %>%
  mutate(
    result = sprintf("%d (%.1f%%)", n, 100 * n / N),
    block = 1,
    soc_order = 1,
    pt_order = 1
  ) %>%
  filter(Grade != "Not graded" | n > 0) %>%
  select(block, soc_order, pt_order, grade_order, SOC, PT, Grade, TRT01A, result)

# ---------------------------
# SOC ORDER
# ---------------------------

soc_order_tbl <- adae_soc_max %>%
  distinct(AEBODSYS, TRT01A, USUBJID) %>%
  count(AEBODSYS, name = "total_n") %>%
  arrange(desc(total_n), AEBODSYS) %>%
  mutate(
    SOC = AEBODSYS,
    soc_order = row_number() + 1
  ) %>%
  select(SOC, soc_order)

# ---------------------------
# SOC OVERALL BLOCK
# ---------------------------

soc_all <- adae_soc_max %>%
  distinct(TRT01A, AEBODSYS, USUBJID) %>%
  count(TRT01A, AEBODSYS, name = "n") %>%
  transmute(
    TRT01A,
    SOC = AEBODSYS,
    PT = "- Overall -",
    Grade = "- All grades -",
    n
  )

soc_grade <- adae_soc_max %>%
  count(TRT01A, AEBODSYS, AETOXGR, name = "n") %>%
  transmute(
    TRT01A,
    SOC = AEBODSYS,
    PT = "- Overall -",
    Grade = AETOXGR,
    n
  )

soc_shell <- expand_grid(
  TRT01A = trt_levels,
  SOC = unique(soc_order_tbl$SOC),
  PT = "- Overall -",
  Grade = grade_shell$Grade
)

soc_block <- bind_rows(soc_all, soc_grade) %>%
  right_join(soc_shell, by = c("TRT01A", "SOC", "PT", "Grade")) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(denom_ae, by = "TRT01A") %>%
  left_join(soc_order_tbl, by = "SOC") %>%
  left_join(grade_shell, by = "Grade") %>%
  mutate(
    result = sprintf("%d (%.1f%%)", n, 100 * n / N),
    block = 2,
    pt_order = 1
  ) %>%
  group_by(SOC, Grade) %>%
  filter(Grade != "Not graded" | sum(n) > 0) %>%
  ungroup() %>%
  select(block, soc_order, pt_order, grade_order, SOC, PT, Grade, TRT01A, result)

# ---------------------------
# PT ORDER
# ---------------------------

pt_order_tbl <- adae_pt_max %>%
  distinct(AEBODSYS, AEDECOD, TRT01A, USUBJID) %>%
  count(AEBODSYS, AEDECOD, name = "total_n") %>%
  left_join(soc_order_tbl, by = c("AEBODSYS" = "SOC")) %>%
  group_by(AEBODSYS) %>%
  arrange(desc(total_n), AEDECOD, .by_group = TRUE) %>%
  mutate(
    SOC = AEBODSYS,
    PT = AEDECOD,
    pt_order = row_number() + 1
  ) %>%
  ungroup() %>%
  select(SOC, PT, pt_order)

# ---------------------------
# PT BLOCK
# ---------------------------

pt_all <- adae_pt_max %>%
  distinct(TRT01A, AEBODSYS, AEDECOD, USUBJID) %>%
  count(TRT01A, AEBODSYS, AEDECOD, name = "n") %>%
  transmute(
    TRT01A,
    SOC = AEBODSYS,
    PT = AEDECOD,
    Grade = "- All grades -",
    n
  )

pt_grade <- adae_pt_max %>%
  count(TRT01A, AEBODSYS, AEDECOD, AETOXGR, name = "n") %>%
  transmute(
    TRT01A,
    SOC = AEBODSYS,
    PT = AEDECOD,
    Grade = AETOXGR,
    n
  )

pt_shell <- expand_grid(
  TRT01A = trt_levels,
  SOC = unique(pt_order_tbl$SOC),
  PT = unique(pt_order_tbl$PT),
  Grade = grade_shell$Grade
) %>%
  semi_join(pt_order_tbl, by = c("SOC", "PT"))

pt_block <- bind_rows(pt_all, pt_grade) %>%
  right_join(pt_shell, by = c("TRT01A", "SOC", "PT", "Grade")) %>%
  mutate(n = replace_na(n, 0)) %>%
  left_join(denom_ae, by = "TRT01A") %>%
  left_join(soc_order_tbl, by = "SOC") %>%
  left_join(pt_order_tbl, by = c("SOC", "PT")) %>%
  left_join(grade_shell, by = "Grade") %>%
  mutate(
    result = sprintf("%d (%.1f%%)", n, 100 * n / N),
    block = 2
  ) %>%
  group_by(SOC, PT, Grade) %>%
  filter(Grade != "Not graded" | sum(n) > 0) %>%
  ungroup() %>%
  select(block, soc_order, pt_order, grade_order, SOC, PT, Grade, TRT01A, result)

# ---------------------------
# FINAL AE TABLE
# ---------------------------

ae_tlf <- bind_rows(any_block, soc_block, pt_block) %>%
  pivot_wider(
    names_from = TRT01A,
    values_from = result,
    values_fill = "0 (0.0%)"
  ) %>%
  arrange(block, soc_order, pt_order, grade_order) %>%
  mutate(
    `MedDRA System Organ Class and Preferred Term` = case_when(
      SOC == "Any adverse events" & Grade == "- All grades -" ~ SOC,
      SOC == "Any adverse events" ~ "",
      PT == "- Overall -" & Grade == "- All grades -" ~ SOC,
      PT == "- Overall -" ~ "  - Overall -",
      Grade == "- All grades -" ~ paste0("  ", PT),
      TRUE ~ ""
    ),
    `NCI-CTCAE Grade` = Grade
  ) %>%
  select(
    `MedDRA System Organ Class and Preferred Term`,
    `NCI-CTCAE Grade`,
    Control,
    `Active`
  )

fmt_table(ae_tlf)


# ===== TLF SECTION 3 =====

#=========================================================
# Prepare PFS dataset
#=========================================================

pfs <- adtte %>%
  filter(PARAMCD == "TTPFS", ITTFL == "Y") %>%
  mutate(
    TRT01P = normalize_trt(TRT01P),
    TRT01P = factor(TRT01P, levels = c("Control", "Active")),
    
    REMISS_GRP = case_when(
      REMISS == "SECOND COMPLETE REMISSION" ~ "2nd remission",
      REMISS == "THIRD COMPLETE REMISSION"  ~ "3rd remission",
      TRUE ~ REMISS
    ),
    REMISS_GRP = factor(
      REMISS_GRP,
      levels = c("2nd remission", "3rd remission")
    ),
    
    EVENT = if_else(CNSR == 0, 1, 0),
    
    EVNTDESC2 = case_when(
      grepl("DEATH", EVNTDESC, ignore.case = TRUE) ~ "Death",
      grepl("PROGRESSION", EVNTDESC, ignore.case = TRUE) ~ "Disease progression",
      TRUE ~ EVNTDESC
    )
  )

# Add Overall group
pfs_all <- bind_rows(
  pfs,
  pfs %>%
    mutate(
      REMISS_GRP = factor(
        "Overall",
        levels = c("2nd remission", "3rd remission", "Overall")
      )
    )
)

#=========================================================
# Denominator
#=========================================================

denom <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop")

#=========================================================
# Subject/event rows
#=========================================================

subj_row <- denom %>%
  mutate(
    row_order = 1,
    row = "No. of Subjects",
    value = as.character(N)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

event_row <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(
    n = n_distinct(USUBJID[EVENT == 1]),
    .groups = "drop"
  ) %>%
  left_join(denom, by = c("REMISS_GRP", "TRT01P")) %>%
  mutate(
    row_order = 2,
    row = "No. of Subjects with an event (%)",
    value = sprintf("%d (%.1f%%)", n, 100 * n / N)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

prog_row <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(
    n = n_distinct(USUBJID[EVENT == 1 & EVNTDESC2 == "Disease progression"]),
    .groups = "drop"
  ) %>%
  mutate(
    row_order = 3,
    row = "  Disease progression",
    value = as.character(n)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

death_row <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(
    n = n_distinct(USUBJID[EVENT == 1 & EVNTDESC2 == "Death"]),
    .groups = "drop"
  ) %>%
  mutate(
    row_order = 4,
    row = "  Death",
    value = as.character(n)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

censor_row <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(
    n = n_distinct(USUBJID[EVENT == 0]),
    .groups = "drop"
  ) %>%
  left_join(denom, by = c("REMISS_GRP", "TRT01P")) %>%
  mutate(
    row_order = 5,
    row = "No. of Subjects without an event (%)",
    value = sprintf("%d (%.1f%%)", n, 100 * n / N)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

#=========================================================
# PFS header row
#=========================================================

pfs_header <- expand_grid(
  REMISS_GRP = factor(
    c("2nd remission", "3rd remission", "Overall"),
    levels = c("2nd remission", "3rd remission", "Overall")
  ),
  TRT01P = factor(c("Control", "Active"), levels = c("Control", "Active"))
) %>%
  mutate(
    row_order = 6,
    row = "PFS (months)",
    value = ""
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

#=========================================================
# KM Median and 95% CI
#=========================================================

km_fit <- survfit(Surv(AVAL, EVENT) ~ REMISS_GRP + TRT01P, data = pfs_all)

km_tbl <- as.data.frame(summary(km_fit)$table) %>%
  rownames_to_column("strata") %>%
  separate(strata, into = c("remiss_part", "trt_part"), sep = ", ") %>%
  mutate(
    REMISS_GRP = sub("REMISS_GRP=", "", remiss_part),
    TRT01P = sub("TRT01P=", "", trt_part),
    REMISS_GRP = factor(
      REMISS_GRP,
      levels = c("2nd remission", "3rd remission", "Overall")
    ),
    TRT01P = factor(TRT01P, levels = c("Control", "Active")),
    median_val = median,
    lcl = `0.95LCL`,
    ucl = `0.95UCL`
  ) %>%
  select(REMISS_GRP, TRT01P, median_val, lcl, ucl)

median_row <- km_tbl %>%
  mutate(
    row_order = 7,
    row = "Median",
    value = if_else(is.na(median_val), "NE", sprintf("%.1f", median_val))
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

ci_row <- km_tbl %>%
  mutate(
    row_order = 8,
    row = "  (95% CI)",
    value = case_when(
      is.na(lcl) & is.na(ucl) ~ "(NE, NE)",
      is.na(lcl) ~ sprintf("(NE, %.1f)", ucl),
      is.na(ucl) ~ sprintf("(%.1f, NE)", lcl),
      TRUE ~ sprintf("(%.1f, %.1f)", lcl, ucl)
    )
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

#=========================================================
# Percentiles and Range
#=========================================================

pfs_stats <- pfs_all %>%
  group_by(REMISS_GRP, TRT01P) %>%
  summarise(
    q1 = quantile(AVAL, 0.25, na.rm = TRUE),
    q3 = quantile(AVAL, 0.75, na.rm = TRUE),
    min = min(AVAL, na.rm = TRUE),
    max = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  )

q_row <- pfs_stats %>%
  mutate(
    row_order = 9,
    row = "25th-75th percentile",
    value = sprintf("%.1f-%.1f", q1, q3)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

range_row <- pfs_stats %>%
  mutate(
    row_order = 10,
    row = "Minimum-maximum",
    value = sprintf("%.1f-%.1f", min, max)
  ) %>%
  select(row_order, row, REMISS_GRP, TRT01P, value)

#=========================================================
# Main Table
#=========================================================

pfs_main <- bind_rows(
  subj_row,
  event_row,
  prog_row,
  death_row,
  censor_row,
  pfs_header,
  median_row,
  ci_row,
  q_row,
  range_row
)

#=========================================================
# Unstratified Analysis
#=========================================================

unstrat_results <- data.frame()

for (grp in c("2nd remission", "3rd remission", "Overall")) {
  
  dat <- pfs_all %>%
    filter(REMISS_GRP == grp) %>%
    mutate(TRT01P = relevel(factor(TRT01P), ref = "Control"))
  
  cox_fit <- coxph(Surv(AVAL, EVENT) ~ TRT01P, data = dat)
  cox_sum <- summary(cox_fit)
  
  hr  <- cox_sum$conf.int[1, "exp(coef)"]
  lcl <- cox_sum$conf.int[1, "lower .95"]
  ucl <- cox_sum$conf.int[1, "upper .95"]
  
  logrank <- survdiff(Surv(AVAL, EVENT) ~ TRT01P, data = dat, rho = 0)
  logrank_p <- 1 - pchisq(logrank$chisq, df = 1)
  
  wilcox <- survdiff(Surv(AVAL, EVENT) ~ TRT01P, data = dat, rho = 1)
  wilcox_p <- 1 - pchisq(wilcox$chisq, df = 1)
  
  temp <- data.frame(
    row_order = c(11, 12, 13, 14, 15, 16),
    row = c(
      "Unstratified analysis",
      "Hazard ratio (relative to placebo)",
      "  (95% CI)",
      "p-value (relative to placebo)",
      "  Log-rank",
      "  Wilcoxon"
    ),
    REMISS_GRP = grp,
    TRT01P = "Active",
    value = c(
      "",
      sprintf("%.3f", hr),
      sprintf("(%.3f, %.3f)", lcl, ucl),
      "",
      sprintf("%.4f", logrank_p),
      sprintf("%.4f", wilcox_p)
    )
  )
  
  unstrat_results <- bind_rows(unstrat_results, temp)
}

# Add blank placebo columns for HR rows
unstrat_blank <- expand_grid(
  row_order = c(11, 12, 13, 14, 15, 16),
  row = c(
    "Unstratified analysis",
    "Hazard ratio (relative to placebo)",
    "  (95% CI)",
    "p-value (relative to placebo)",
    "  Log-rank",
    "  Wilcoxon"
  ),
  REMISS_GRP = c("2nd remission", "3rd remission", "Overall"),
  TRT01P = "Control"
) %>%
  mutate(value = "")

unstrat_tlf <- bind_rows(unstrat_results, unstrat_blank) %>%
  mutate(
    REMISS_GRP = factor(
      REMISS_GRP,
      levels = c("2nd remission", "3rd remission", "Overall")
    ),
    TRT01P = factor(TRT01P, levels = c("Control", "Active"))
  )

#=========================================================
# Stratified Analysis - Overall only
#=========================================================

dat_strat <- pfs %>%
  mutate(TRT01P = relevel(factor(TRT01P), ref = "Control"))

cox_strat <- coxph(Surv(AVAL, EVENT) ~ TRT01P + strata(REMISS_GRP), data = dat_strat)
cox_strat_sum <- summary(cox_strat)

hr_s  <- cox_strat_sum$conf.int[1, "exp(coef)"]
lcl_s <- cox_strat_sum$conf.int[1, "lower .95"]
ucl_s <- cox_strat_sum$conf.int[1, "upper .95"]

logrank_s <- survdiff(Surv(AVAL, EVENT) ~ TRT01P + strata(REMISS_GRP), data = dat_strat, rho = 0)
logrank_s_p <- 1 - pchisq(logrank_s$chisq, df = 1)

wilcox_s <- survdiff(Surv(AVAL, EVENT) ~ TRT01P + strata(REMISS_GRP), data = dat_strat, rho = 1)
wilcox_s_p <- 1 - pchisq(wilcox_s$chisq, df = 1)

strat_tlf <- expand_grid(
  row_order = c(17, 18, 19, 20, 21, 22),
  row = c(
    "Stratified analysis",
    "Hazard ratio (relative to placebo)",
    "  (95% CI)",
    "p-value (relative to placebo)",
    "  Log-rank",
    "  Wilcoxon"
  ),
  REMISS_GRP = c("2nd remission", "3rd remission", "Overall"),
  TRT01P = c("Control", "Active")
) %>%
  mutate(
    value = case_when(
      REMISS_GRP == "Overall" & TRT01P == "Active" & row_order == 18 ~ sprintf("%.3f", hr_s),
      REMISS_GRP == "Overall" & TRT01P == "Active" & row_order == 19 ~ sprintf("(%.3f, %.3f)", lcl_s, ucl_s),
      REMISS_GRP == "Overall" & TRT01P == "Active" & row_order == 21 ~ sprintf("%.4f", logrank_s_p),
      REMISS_GRP == "Overall" & TRT01P == "Active" & row_order == 22 ~ sprintf("%.4f", wilcox_s_p),
      TRUE ~ ""
    ),
    REMISS_GRP = factor(
      REMISS_GRP,
      levels = c("2nd remission", "3rd remission", "Overall")
    ),
    TRT01P = factor(TRT01P, levels = c("Control", "Active"))
  )

#=========================================================
# Final PFS Table
#=========================================================

pfs_final <- bind_rows(
  pfs_main,
  unstrat_tlf,
  strat_tlf
) %>%
  mutate(
    REMISS_GRP = as.character(REMISS_GRP),
    TRT01P = as.character(TRT01P),
    REMISS_GRP = factor(
      REMISS_GRP,
      levels = c("2nd remission", "3rd remission", "Overall")
    ),
    TRT01P = factor(
      TRT01P,
      levels = c("Control", "Active")
    )
  ) %>%
  arrange(row_order) %>%
  pivot_wider(
    names_from = c(REMISS_GRP, TRT01P),
    values_from = value
  ) %>%
  select(
    row,
    `2nd remission_Control`,
    `2nd remission_Active`,
    `3rd remission_Control`,
    `3rd remission_Active`,
    `Overall_Control`,
    `Overall_Active`
  )

fmt_table(pfs_final)


# ===== TLF SECTION 4 =====

#=========================================================
# LAB SHIFT TABLE - NCI CTCAE GRADE SHIFT
# Baseline Grade vs Worst Post-Baseline Grade
# For Sodium, Potassium, Magnesium
#=========================================================


#=========================================================
# Prepare Data
#=========================================================

lb0 <- adlbsi %>%
  left_join(
    adsl %>%
      select(
        USUBJID,
        TRT01P_ADSL = TRT01P,
        SAFFL_ADSL  = SAFFL
      ),
    by = "USUBJID"
  ) %>%
  mutate(
    TRT01P = TRT01P_ADSL,
    SAFFL  = SAFFL_ADSL,
    
    PARAM_CLEAN = sub(".*\\|", "", PARAM),
    
    ATOXGRN = suppressWarnings(as.numeric(ATOXGR)),
    ATOXDIR = toupper(ATOXDIR)
  ) %>%
  filter(
    SAFFL == "Y",
    PARAM_CLEAN %in% c(
      "Magnesium (mmol/L)",
      "Potassium (mmol/L)",
      "Sodium (mmol/L)"
    )
  )


#=========================================================
# Baseline Grade
# Last baseline record per subject/parameter
#=========================================================

base <- lb0 %>%
  filter(
    ABLFL == "Y",
    !is.na(ATOXGRN)
  ) %>%
  group_by(TRT01P, USUBJID, PARAM_CLEAN) %>%
  arrange(ADT, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(
    TRT01P,
    USUBJID,
    PARAM = PARAM_CLEAN,
    BASEGR = ATOXGRN
  )


#=========================================================
#  Post-Baseline Records
#=========================================================

post <- lb0 %>%
  filter(
    is.na(ABLFL) | ABLFL != "Y",
    !is.na(ATOXGRN),
    !is.na(ATOXDIR)
  ) %>%
  transmute(
    TRT01P,
    USUBJID,
    PARAM = PARAM_CLEAN,
    POSTGRN = ATOXGRN,
    ATOXDIR
  )


#=========================================================
# Worst Post-Baseline Grade
#
# LOW grades go to 0,1,2,3,4 columns
# Any HIGH abnormality goes to Other
#=========================================================

post_final <- post %>%
  group_by(TRT01P, USUBJID, PARAM) %>%
  summarise(
    has_high = any(ATOXDIR == "H"),
    max_low = ifelse(
      any(ATOXDIR == "L"),
      max(POSTGRN[ATOXDIR == "L"], na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    POSTGR = case_when(
      has_high ~ "Other",
      !is.na(max_low) ~ as.character(max_low),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(POSTGR))


#=========================================================
# Combine Baseline + Post-Baseline
# Denominator includes subjects with baseline and post-baseline
#=========================================================

shift <- base %>%
  left_join(
    post_final,
    by = c("TRT01P", "USUBJID", "PARAM")
  ) %>%
  filter(!is.na(POSTGR))


#=========================================================
#  Denominator
# N = subjects with baseline + at least one post-baseline
#=========================================================

denom <- shift %>%
  distinct(TRT01P, USUBJID, PARAM) %>%
  group_by(TRT01P, PARAM) %>%
  summarise(
    N = n_distinct(USUBJID),
    .groups = "drop"
  )


#=========================================================
# LOW Counts
#=========================================================

cnt_low <- shift %>%
  mutate(
    LABEVENT = "Low",
    BASEGR_LABEL = as.character(BASEGR)
  ) %>%
  group_by(TRT01P, PARAM, LABEVENT, BASEGR_LABEL, POSTGR) %>%
  summarise(
    n = n_distinct(USUBJID),
    .groups = "drop"
  )


#=========================================================
# HIGH Counts
# Baseline grades pooled as 1-4
#=========================================================

cnt_high <- shift %>%
  mutate(
    LABEVENT = "High",
    BASEGR_LABEL = "1-4"
  ) %>%
  group_by(TRT01P, PARAM, LABEVENT, BASEGR_LABEL, POSTGR) %>%
  summarise(
    n = n_distinct(USUBJID),
    .groups = "drop"
  )


cnt_all <- bind_rows(cnt_low, cnt_high)


#=========================================================
# Shell Structure
#=========================================================

shell <- expand_grid(
  TRT01P = c("Active", "Control"),
  PARAM = c(
    "Magnesium (mmol/L)",
    "Potassium (mmol/L)",
    "Sodium (mmol/L)"
  ),
  LABEVENT = c("Low", "High"),
  BASEGR_LABEL = c("4", "3", "2", "1", "0", "1-4"),
  POSTGR = c("0", "1", "2", "3", "4", "Other")
) %>%
  filter(
    (LABEVENT == "Low"  & BASEGR_LABEL %in% c("4", "3", "2", "1", "0")) |
      (LABEVENT == "High" & BASEGR_LABEL == "1-4")
  )


#=========================================================
# Final Shift Table
#=========================================================

final_shift_table <- shell %>%
  left_join(
    denom,
    by = c("TRT01P", "PARAM")
  ) %>%
  left_join(
    cnt_all,
    by = c(
      "TRT01P",
      "PARAM",
      "LABEVENT",
      "BASEGR_LABEL",
      "POSTGR"
    )
  ) %>%
  mutate(
    N = ifelse(is.na(N), 0, N),
    n = ifelse(is.na(n), 0, n),
    pct = ifelse(N > 0, 100 * n / N, 0),
    
    value = case_when(
      N == 0 ~ "0",
      TRUE ~ sprintf("%d (%.1f%%)", n, pct)
    )
  ) %>%
  select(
    TRT01P,
    `Lab Parameter` = PARAM,
    `Lab Event` = LABEVENT,
    `Baseline Grade` = BASEGR_LABEL,
    N,
    POSTGR,
    value
  ) %>%
  pivot_wider(
    names_from = POSTGR,
    values_from = value,
    values_fill = "0"
  ) %>%
  arrange(
    TRT01P,
    factor(`Lab Parameter`, levels = c(
      "Magnesium (mmol/L)",
      "Potassium (mmol/L)",
      "Sodium (mmol/L)"
    )),
    factor(`Lab Event`, levels = c("Low", "High")),
    factor(`Baseline Grade`, levels = c("4", "3", "2", "1", "0", "1-4"))
  ) %>%
  select(
    TRT01P,
    `Lab Parameter`,
    `Lab Event`,
    `Baseline Grade`,
    N,
    `0`,
    `1`,
    `2`,
    `3`,
    `4`,
    Other
  )

fmt_table(final_shift_table)


# ===== TLF SECTION 5 =====

#=========================================================
# Figure 14.2.1/2
# Kaplan-Meier Curves for Progression Free Survival
#=========================================================

#-----------------------------
# Prepare PFS data
#-----------------------------

pfs <- adtte %>%
  filter(
    PARAMCD == "TTPFS",
    !is.na(AVAL),
    !is.na(CNSR),
    !is.na(TRT01P)
  ) %>%
  mutate(
    EVENT = ifelse(CNSR == 0, 1, 0),
    TRT01P = normalize_trt(TRT01P),
    TRT01P = factor(TRT01P, levels = c("Control", "Active"))
  )

#-----------------------------
# KM fit
#-----------------------------

km_fit <- survfit(
  Surv(AVAL, EVENT) ~ TRT01P,
  data = pfs
)

km_sum <- summary(km_fit)

km_df <- data.frame(
  time     = km_sum$time,
  surv     = km_sum$surv,
  strata   = km_sum$strata,
  n.risk   = km_sum$n.risk,
  n.event  = km_sum$n.event,
  n.censor = km_sum$n.censor
) %>%
  mutate(
    TRT01P = gsub("TRT01P=", "", strata),
    TRT01P = factor(TRT01P, levels = c("Control", "Active"))
  )

# Add time zero
km_df0 <- pfs %>%
  count(TRT01P) %>%
  transmute(
    time = 0,
    surv = 1,
    strata = paste0("TRT01P=", TRT01P),
    n.risk = n,
    n.event = 0,
    n.censor = 0,
    TRT01P
  )

km_df <- bind_rows(km_df0, km_df) %>%
  arrange(TRT01P, time)

#-----------------------------
# Censor points
#-----------------------------

censor_df <- km_df %>%
  filter(n.censor > 0)

#-----------------------------
# Log-rank p-value
#-----------------------------

logrank <- survdiff(
  Surv(AVAL, EVENT) ~ TRT01P,
  data = pfs
)

logrank_p <- 1 - pchisq(logrank$chisq, df = length(logrank$n) - 1)

logrank_p_fmt <- ifelse(
  logrank_p < 0.001,
  "<0.001",
  sprintf("%.3f", logrank_p)
)

#-----------------------------
# Cox model: HR and CI
#-----------------------------

cox_model <- coxph(
  Surv(AVAL, EVENT) ~ TRT01P,
  data = pfs
)

cox_sum <- summary(cox_model)

hr     <- cox_sum$conf.int[1, "exp(coef)"]
hr_lcl <- cox_sum$conf.int[1, "lower .95"]
hr_ucl <- cox_sum$conf.int[1, "upper .95"]

hr_fmt <- sprintf("%.3f", hr)
ci_fmt <- sprintf("(%.3f, %.3f)", hr_lcl, hr_ucl)

#-----------------------------
# Median time
#-----------------------------

median_tbl <- summary(km_fit)$table

med_df <- data.frame(
  TRT01P = rownames(median_tbl),
  median = median_tbl[, "median"]
) %>%
  mutate(
    TRT01P = gsub("TRT01P=", "", TRT01P),
    median_fmt = ifelse(is.na(median), "NE", sprintf("%.1f", median))
  )

placebo_med <- med_df %>%
  filter(TRT01P == "Control") %>%
  pull(median_fmt)

cmp_med <- med_df %>%
  filter(TRT01P == "Active") %>%
  pull(median_fmt)

#-----------------------------
# Legend labels with N
#-----------------------------

n_df <- pfs %>%
  count(TRT01P)

placebo_n <- n_df %>%
  filter(TRT01P == "Control") %>%
  pull(n)

cmp_n <- n_df %>%
  filter(TRT01P == "Active") %>%
  pull(n)

legend_labels <- c(
  paste0("Control (n=", placebo_n, ")"),
  paste0("Active (n=", cmp_n, ")")
)

#-----------------------------
#  Statistics block
#-----------------------------

stats_label <- paste0(
  "                         Control     Active\n",
  "Median Time (mo)     ", placebo_med, "          ", cmp_med, "\n",
  "Hazard Ratio                      ", hr_fmt, "\n",
  "(95% CI)                         ", ci_fmt, "\n",
  "Log-rank p-value                 ", logrank_p_fmt
)

x_max <- ceiling(max(pfs$AVAL, na.rm = TRUE) / 3) * 3

#-----------------------------
# KM plot like shell
#-----------------------------

km_plot <- ggplot(km_df, aes(x = time, y = surv, linetype = TRT01P)) +
  geom_step(linewidth = 0.7) +
  geom_point(
    data = censor_df,
    aes(x = time, y = surv),
    shape = 3,
    size = 2,
    show.legend = FALSE
  ) +
  annotate(
    "text",
    x = x_max * 0.58,
    y = 0.92,
    label = stats_label,
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  scale_linetype_manual(
    values = c("Control" = "solid", "Active" = "dashed"),
    labels = legend_labels
  ) +
  scale_x_continuous(
    limits = c(0, x_max),
    breaks = seq(0, x_max, by = 3),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  labs(
    title = paste0(
      "Figures 14.2.1/2\n",
      "Kaplan Meier Curves for Progression Free Survival by Treatment Arm in Second Remission\n",
      "Randomized Subjects with 2nd Remission"
    ),
    x = "Time to Progression (months)",
    y = "Progression-Free Rate",
    linetype = ""
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    axis.line = element_line(linewidth = 0.4),
    axis.ticks = element_line(linewidth = 0.4),
    legend.position = c(0.22, 0.18),
    legend.background = element_rect(color = "black", fill = "white", linewidth = 0.3),
    legend.key.width = unit(1.2, "cm"),
    legend.text = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
    plot.margin = margin(5, 15, 0, 15)
  )

#-----------------------------
# Number at Risk table
#-----------------------------

time_points <- seq(0, x_max, by = 3)

risk_sum <- summary(km_fit, times = time_points, extend = TRUE)

risk_df <- data.frame(
  time = risk_sum$time,
  n.risk = risk_sum$n.risk,
  strata = risk_sum$strata
) %>%
  mutate(
    TRT01P = gsub("TRT01P=", "", strata),
    TRT01P = factor(TRT01P, levels = c("Control", "Active"))
  )

risk_plot <- ggplot(risk_df, aes(x = time, y = TRT01P, label = n.risk)) +
  geom_text(size = 3) +
  scale_x_continuous(
    limits = c(0, x_max),
    breaks = time_points,
    expand = c(0, 0)
  ) +
  scale_y_discrete(limits = rev(levels(pfs$TRT01P))) +
  labs(
    title = "Number at Risk:",
    x = NULL,
    y = NULL
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0, size = 8),
    axis.text.y = element_text(size = 8, hjust = 1),
    plot.margin = margin(0, 15, 5, 15)
  )

#-----------------------------
#  Combine plot and risk table
#-----------------------------

final_km_figure <- km_plot / risk_plot +
  plot_layout(heights = c(4.6, 0.9))

final_km_figure


# ===== TLF SECTION 6 =====

#=========================================================
# Table 
# Best Overall Response per Investigator by Age Category
# ITT Subjects
#=========================================================
#=========================================================
#Prepare BOR Data
#=========================================================

#=========================================================
# BOR Table by Age Category using ADRS
# Source: ADRS where paramcd = BOR
#=========================================================

bor <- adrs %>%
  filter(
    paramcd == "BOR",
    ITTFL == "Y"
  ) %>%
  mutate(
    AGE_CAT = case_when(
      Age < 65 ~ "< 65",
      Age >= 65 ~ ">= 65",
      TRUE ~ NA_character_
    ),
    TRT01P = normalize_trt(Trt01p),
    TRT01P = factor(TRT01P, levels = c("Active", "Control")),
    
    RESPONSE = case_when(
      AVALC == "CR" ~ "COMPLETE RESPONSE (CR)",
      AVALC == "PR" ~ "PARTIAL RESPONSE (PR)",
      AVALC == "SD" ~ "STABLE DISEASE (SD)",
      AVALC == "PD" ~ "PROGRESSIVE DISEASE (PD)",
      AVALC == "NE" ~ "UNABLE TO DETERMINE (UD)",
      TRUE ~ "UNABLE TO DETERMINE (UD)"
    ),
    
    ORRFL = ifelse(AVALC %in% c("CR", "PR"), "Y", "N")
  ) %>%
  filter(!is.na(AGE_CAT), !is.na(TRT01P))


#=========================================================
# Denominators
#=========================================================

denom_trt <- bor %>%
  group_by(AGE_CAT, TRT01P) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = as.character(TRT01P))

denom_total <- bor %>%
  group_by(AGE_CAT) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = "Total")

denom_all <- bind_rows(denom_trt, denom_total)


#=========================================================
# Response counts
#=========================================================

resp_shell <- expand_grid(
  AGE_CAT = c("< 65", ">= 65"),
  TRT01P = c("Active", "Control", "Total"),
  RESPONSE = c(
    "COMPLETE RESPONSE (CR)",
    "PARTIAL RESPONSE (PR)",
    "STABLE DISEASE (SD)",
    "PROGRESSIVE DISEASE (PD)",
    "UNABLE TO DETERMINE (UD)"
  )
)

cnt_trt <- bor %>%
  group_by(AGE_CAT, TRT01P, RESPONSE) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = as.character(TRT01P))

cnt_total <- bor %>%
  group_by(AGE_CAT, RESPONSE) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = "Total")

cnt_all <- bind_rows(cnt_trt, cnt_total)

resp_final <- resp_shell %>%
  left_join(denom_all, by = c("AGE_CAT", "TRT01P")) %>%
  left_join(cnt_all, by = c("AGE_CAT", "TRT01P", "RESPONSE")) %>%
  mutate(
    N = ifelse(is.na(N), 0, N),
    n = ifelse(is.na(n), 0, n),
    pct = ifelse(N > 0, 100 * n / N, 0),
    value = ifelse(N > 0, sprintf("%d (%.1f%%)", n, pct), "0")
  )


#=========================================================
# NE / UD reason rows using NEREASN
#=========================================================

reason_shell <- expand_grid(
  AGE_CAT = c("< 65", ">= 65"),
  TRT01P = c("Active", "Control", "Total"),
  NEREASN = c(
    "Death Before Measurement",
    "Droped Study",
    "Withdrown Consent"
  )
)

reason_trt <- bor %>%
  filter(RESPONSE == "UNABLE TO DETERMINE (UD)") %>%
  mutate(NEREASN = ifelse(is.na(NEREASN), "Missing Reason", NEREASN)) %>%
  group_by(AGE_CAT, TRT01P, NEREASN) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = as.character(TRT01P))

reason_total <- bor %>%
  filter(RESPONSE == "UNABLE TO DETERMINE (UD)") %>%
  mutate(NEREASN = ifelse(is.na(NEREASN), "Missing Reason", NEREASN)) %>%
  group_by(AGE_CAT, NEREASN) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TRT01P = "Total")

reason_all <- bind_rows(reason_trt, reason_total)

reason_final <- reason_shell %>%
  left_join(denom_all, by = c("AGE_CAT", "TRT01P")) %>%
  left_join(reason_all, by = c("AGE_CAT", "TRT01P", "NEREASN")) %>%
  mutate(
    N = ifelse(is.na(N), 0, N),
    n = ifelse(is.na(n), 0, n),
    pct = ifelse(N > 0, 100 * n / N, 0),
    RESPONSE = paste0("  ", NEREASN),
    value = ifelse(N > 0, sprintf("%d (%.1f%%)", n, pct), "0")
  ) %>%
  select(AGE_CAT, TRT01P, RESPONSE, value)


#=========================================================
# ORR and Exact 95% CI
# ORR = CR + PR
#=========================================================

orr_trt <- bor %>%
  group_by(AGE_CAT, TRT01P) %>%
  summarise(
    responders = n_distinct(USUBJID[ORRFL == "Y"]),
    N = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  mutate(TRT01P = as.character(TRT01P))

orr_total <- bor %>%
  group_by(AGE_CAT) %>%
  summarise(
    responders = n_distinct(USUBJID[ORRFL == "Y"]),
    N = n_distinct(USUBJID),
    .groups = "drop"
  ) %>%
  mutate(TRT01P = "Total")

orr_all <- bind_rows(orr_trt, orr_total) %>%
  rowwise() %>%
  mutate(
    pct = ifelse(N > 0, 100 * responders / N, 0),
    ci_low = ifelse(N > 0, binom.test(responders, N)$conf.int[1] * 100, NA_real_),
    ci_high = ifelse(N > 0, binom.test(responders, N)$conf.int[2] * 100, NA_real_),
    ORR_VALUE = ifelse(N > 0, sprintf("%d/%d (%.1f%%)", responders, N, pct), "0"),
    CI_VALUE = ifelse(N > 0, sprintf("(%.1f, %.1f)", ci_low, ci_high), "")
  ) %>%
  ungroup()

orr_row <- orr_all %>%
  transmute(
    AGE_CAT,
    TRT01P,
    RESPONSE = "OBJECTIVE RESPONSE RATE (1)",
    value = ORR_VALUE
  )

ci_row <- orr_all %>%
  transmute(
    AGE_CAT,
    TRT01P,
    RESPONSE = "(95% CI)",
    value = CI_VALUE
  )


#=========================================================
# Combine all rows
#=========================================================

final_long <- bind_rows(
  resp_final %>% select(AGE_CAT, TRT01P, RESPONSE, value),
  reason_final,
  orr_row,
  ci_row
) %>%
  mutate(
    RESPONSE_ORDER = case_when(
      RESPONSE == "COMPLETE RESPONSE (CR)" ~ 1,
      RESPONSE == "PARTIAL RESPONSE (PR)" ~ 2,
      RESPONSE == "STABLE DISEASE (SD)" ~ 3,
      RESPONSE == "PROGRESSIVE DISEASE (PD)" ~ 4,
      RESPONSE == "UNABLE TO DETERMINE (UD)" ~ 5,
      RESPONSE == "  Death Before Measurement" ~ 6,
      RESPONSE == "  Droped Study" ~ 7,
      RESPONSE == "  Withdrown Consent" ~ 8,
      RESPONSE == "OBJECTIVE RESPONSE RATE (1)" ~ 9,
      RESPONSE == "(95% CI)" ~ 10,
      TRUE ~ 99
    )
  )


#=========================================================
# Final table
#=========================================================

final_bor_table <- final_long %>%
  pivot_wider(
    names_from = TRT01P,
    values_from = value,
    values_fill = "0"
  ) %>%
  arrange(
    factor(AGE_CAT, levels = c("< 65", ">= 65")),
    RESPONSE_ORDER
  ) %>%
  select(
    `Age Category` = AGE_CAT,
    `Best Overall Response` = RESPONSE,
    `Trt A` = `Active`,
    `Trt B` = Control,
    Total
  )


fmt_table(final_bor_table)


# ===== TLF SECTION 7 =====

#========================
# Common data
#========================

adlbsi2 <- adlbsi %>%
  filter(ITTFL == "Y") %>%
  mutate(
    AVISIT = case_when(
      ADY >= 1   & ADY <= 10  ~ "Week 1",
      ADY >= 22  & ADY <= 34  ~ "Week 4",
      ADY >= 50  & ADY <= 62  ~ "Week 8",
      ADY >= 78  & ADY <= 90  ~ "Week 12",
      ADY >= 106 & ADY <= 118 ~ "Week 16",
      TRUE ~ NA_character_
    ),
    Treatment = case_when(
      normalize_trt(TRT01P) == "Active" ~ "Active A",
      normalize_trt(TRT01P) == "Control" ~ "Active B",
      TRUE ~ TRT01P
    ),
    AVISIT = factor(
      AVISIT,
      levels = c("Week 1", "Week 4", "Week 8", "Week 12", "Week 16")
    ),
    TRTORD = case_when(
      Treatment == "Active A" ~ 1,
      Treatment == "Active B" ~ 2,
      TRUE ~ 99
    )
  ) %>%
  filter(!is.na(AVISIT), !is.na(AVAL))


#========================
# Table 11
# Summary of Viral Load over time
#========================

table11_trt <- adlbsi2 %>%
  group_by(AVISIT, TRTORD, Treatment) %>%
  summarise(
    N = n(),
    MEAN = mean(AVAL, na.rm = TRUE),
    SE = sd(AVAL, na.rm = TRUE) / sqrt(N),
    .groups = "drop"
  ) %>%
  mutate(`Unadjusted Mean (SE)` = sprintf("%.3f (%.3f)", MEAN, SE))

table11_final <- table11_trt %>%
  arrange(AVISIT, TRTORD) %>%
  group_by(AVISIT) %>%
  mutate(`Test Day` = if_else(row_number() == 1, as.character(AVISIT), "")) %>%
  ungroup() %>%
  select(`Test Day`, Treatment, N, `Unadjusted Mean (SE)`)

fmt_table(table11_final)


# ===== TLF SECTION 8 =====

#========================
# Table 12
# Adjusted Mean at Week 12 and Week 16
#========================

table12_data <- adlbsi2 %>%
  filter(AVISIT %in% c("Week 12", "Week 16")) %>%
  droplevels()

model <- lme(
  AVAL ~ AVISIT + Treatment,
  random = ~1 | USUBJID,
  data = table12_data,
  na.action = na.omit,
  control = lmeControl(returnObject = TRUE)
)

lsm <- emmeans(model, ~ Treatment | AVISIT)

n_data <- table12_data %>%
  group_by(AVISIT, Treatment) %>%
  summarise(N = n(), .groups = "drop")

table12_final <- as.data.frame(summary(lsm)) %>%
  mutate(
    `Adjusted Mean (SE)` = sprintf("%.3f (%.3f)", emmean, SE),
    TRTORD = case_when(
      Treatment == "Active A" ~ 1,
      Treatment == "Active B" ~ 2,
      TRUE ~ 99
    )
  ) %>%
  left_join(n_data, by = c("AVISIT", "Treatment")) %>%
  arrange(AVISIT, TRTORD) %>%
  group_by(AVISIT) %>%
  mutate(`Test Day` = if_else(row_number() == 1, as.character(AVISIT), "")) %>%
  ungroup() %>%
  select(`Test Day`, Treatment, N, `Adjusted Mean (SE)`)

fmt_table(table12_final)


