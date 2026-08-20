# Extracted from ADAM.Rmd for a public portfolio repository.
# Clinical source data and validation datasets are intentionally excluded.

SDTM_DATA_DIR <- Sys.getenv("SDTM_DATA_DIR", unset = file.path("data", "sdtm"))
ADAM_VALIDATION_DIR <- Sys.getenv("ADAM_VALIDATION_DIR", unset = file.path("data", "validation"))
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = file.path("output"))

library(haven)
library(dplyr)
library(tidyr)

# =============================================================================
# ADSL derivation from SDTM
# =============================================================================

sdtm_path <- SDTM_DATA_DIR

dm_sdtm <- read_xpt(file.path(sdtm_path, "DM.xpt"))        # Demographics
ex_sdtm <- read_xpt(file.path(sdtm_path, "EX.xpt"))        # Exposure
ds_sdtm <- read_xpt(file.path(sdtm_path, "DS.xpt"))        # Disposition
suppds_sdtm <- read_xpt(file.path(sdtm_path, "SUPPDS.xpt"))# Supplemental Disposition
vs_sdtm <- read_xpt(file.path(sdtm_path, "VS.xpt"))        # Vital Signs
zh_sdtm <- read_xpt(file.path(sdtm_path, "ZH.xpt"))        # Disease History
yp_sdtm <- read_xpt(file.path(sdtm_path, "YP.xpt"))        # Procedures
xr_sdtm <- read_xpt(file.path(sdtm_path, "XR.xpt"))        # Radiation Therapy
cm_sdtm <- read_xpt(file.path(sdtm_path, "CM.xpt"))        # Concomitant/Prior Medications
qs_sdtm <- read_xpt(file.path(sdtm_path, "QS.xpt"))        # Questionnaires
tu_sdtm <- read_xpt(file.path(sdtm_path, "TU.xpt"))        # Tumor Identification

# =============================================================================
# Treatment from EX
# =============================================================================

ex_trt <- ex_sdtm %>%
  mutate(
    EXTRT_UP = toupper(trimws(EXTRT)),              # Uppercase Exposure Treatment
    EXSTDT = as.Date(substr(EXSTDTC, 1, 10)),       # Exposure Start Date
    EXENDT = as.Date(substr(EXENDTC, 1, 10))        # Exposure End Date
  ) %>%
  filter(!is.na(EXSTDT)) %>%
  arrange(USUBJID, EXSTDT) %>%
  group_by(USUBJID) %>%
  summarise(
    TRT01A = case_when(                             # Actual Treatment for Period 1
      first(EXTRT_UP) == "CMP-135" ~ "CMP-135",
      first(EXTRT_UP) == "PLACEBO" ~ "Placebo",
      TRUE ~ NA_character_
    ),
    TRT01AN = case_when(                            # Actual Treatment for Period 1 Numeric
      first(EXTRT_UP) == "CMP-135" ~ 1,
      first(EXTRT_UP) == "PLACEBO" ~ 0,
      TRUE ~ NA_real_
    ),
    TRTSDT = min(EXSTDT, na.rm = TRUE),             # Treatment Start Date
    TRTEDT = if_else(                               # Treatment End Date
      any(!is.na(EXENDT)),
      max(EXENDT, na.rm = TRUE),
      as.Date(NA)
    ),
    .groups = "drop"
  ) %>%
  mutate(
    TRTSDTC = as.character(TRTSDT),                 # Treatment Start Date Character
    TRTEDTC = as.character(TRTEDT)                  # Treatment End Date Character
  )

# =============================================================================
# Clean DS once and keep both DS date variables
# =============================================================================

ds_clean <- ds_sdtm %>%
  mutate(
    DSCAT_UP = toupper(trimws(DSCAT)),              # Uppercase Disposition Category
    DSSCAT_UP = toupper(trimws(DSSCAT)),            # Uppercase Disposition Subcategory
    DSTERM_UP = toupper(trimws(DSTERM)),            # Uppercase Disposition Term
    DSDECOD_UP = toupper(trimws(DSDECOD)),          # Uppercase Disposition Decode
    DSEPOCH_UP = toupper(trimws(EPOCH)),            # Uppercase Disposition Epoch
    DSDTC_DT = as.Date(substr(DSDTC, 1, 10)),       # Disposition Date
    DSSTDTC_DT = as.Date(substr(DSSTDTC, 1, 10)),   # Disposition Start Date
    DTH_EVENT = (                                   # Death Event Flag
      DSCAT_UP == "OTHER EVENT" &
        DSSCAT_UP == "DEATH"
    ) | (
      DSCAT_UP == "DISPOSITION EVENT" &
        DSSCAT_UP == "FOLLOW-UP" &
        DSTERM_UP == "DEATH"
    )
  )

# =============================================================================
# Randomization Date
# =============================================================================

rand <- ds_clean %>%
  filter(
    DSCAT_UP == "PROTOCOL MILESTONE",
    DSEPOCH_UP == "SCREENING",
    DSTERM_UP == "RANDOMIZATION"
  ) %>%
  filter(!is.na(DSSTDTC_DT)) %>%
  group_by(USUBJID) %>%
  summarise(
    RANDDT = min(DSSTDTC_DT),                       # Randomization Date
    .groups = "drop"
  )

# =============================================================================
# Baseline ECOG from QS
# =============================================================================

becog_qs <- qs_sdtm %>%
  mutate(
    QSTESTCD_UP = toupper(trimws(QSTESTCD)),        # Uppercase Questionnaire Test Code
    QSDT = as.Date(substr(QSDTC, 1, 10)),           # Questionnaire Date
    QSORRESN = as.numeric(QSORRES)                  # Questionnaire Original Result Numeric
  ) %>%
  filter(QSTESTCD_UP == "ECOG", !is.na(QSORRESN)) %>%
  left_join(ex_trt %>% select(USUBJID, TRTSDT), by = "USUBJID") %>%
  filter(QSDT <= TRTSDT | is.na(TRTSDT)) %>%
  arrange(USUBJID, desc(QSDT)) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    USUBJID,
    BECOG = QSORRESN                                # Baseline ECOG Score
  )

# =============================================================================
# DS-derived ADSL variables
# =============================================================================

ds_adsl <- ds_clean %>%
  group_by(USUBJID) %>%
  summarise(
    TRTDCDT = {                                     # Treatment Discontinuation Date
      trtdcdt_vec <- DSDTC_DT[
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP %in% c("CMP-135", "PLACEBO") &
          DSEPOCH_UP == "STUDY PERIOD" &
          !is.na(DSTERM) &
          trimws(DSTERM) != ""
      ]
      
      if (length(trtdcdt_vec) > 0) {
        first(trtdcdt_vec)
      } else {
        as.Date(NA)
      }
    },
    
    TRTDCRS = if_else(                              # Treatment Discontinuation Reason
      any(
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP %in% c("CMP-135", "PLACEBO") &
          DSEPOCH_UP == "STUDY PERIOD"
      ),
      first(DSDECOD_UP[
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP %in% c("CMP-135", "PLACEBO") &
          DSEPOCH_UP == "STUDY PERIOD"
      ]),
      ""
    ),
    
    STDSDT = if_else(                               # Study Start Date
      any(
        DSCAT_UP == "PROTOCOL MILESTONE" &
          DSEPOCH_UP == "SCREENING" &
          DSTERM_UP == "RANDOMIZATION" &
          !is.na(DSSTDTC_DT)
      ),
      min(DSSTDTC_DT[
        DSCAT_UP == "PROTOCOL MILESTONE" &
          DSEPOCH_UP == "SCREENING" &
          DSTERM_UP == "RANDOMIZATION"
      ], na.rm = TRUE),
      as.Date(NA)
    ),
    
    STDEDT = if_else(                               # Study End Date
      any(
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "STUDY PERIOD" &
          DSEPOCH_UP == "STUDY PERIOD" &
          !is.na(DSSTDTC_DT)
      ),
      max(DSSTDTC_DT[
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "STUDY PERIOD" &
          DSEPOCH_UP == "STUDY PERIOD"
      ], na.rm = TRUE),
      as.Date(NA)
    ),
    
    DTHDT = if_else(                                # Death Date
      any(
        DSCAT_UP == "OTHER EVENT" &
          DSSCAT_UP == "DEATH" &
          !is.na(DSSTDTC_DT)
      ),
      min(DSSTDTC_DT[
        DSCAT_UP == "OTHER EVENT" &
          DSSCAT_UP == "DEATH"
      ], na.rm = TRUE),
      as.Date(NA)
    ),
    
    TRTDCFL = if_else(                              # Treatment Discontinuation Flag
      any(
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP %in% c("CMP-135", "PLACEBO")
      ),
      "Y",
      NA_character_
    ),
    
    COMPLFL = if_else(                              # Completion Flag
      any(
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "STUDY PERIOD" &
          DSDECOD_UP == "PROGRESSIVE DISEASE"
      ),
      "Y",
      "N"
    ),
    
    STDDCRS = if_else(                              # Study Discontinuation Reason
      any(DSCAT_UP == "DISPOSITION EVENT" & DSSCAT_UP == "STUDY PERIOD"),
      first(DSDECOD_UP[
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "STUDY PERIOD"
      ]),
      NA_character_
    ),
    
    SFUEDT = if_else(                               # Survival Follow-up End Date
      any(
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "FOLLOW-UP" &
          !is.na(DSSTDTC_DT)
      ),
      min(DSSTDTC_DT[
        DSCAT_UP == "DISPOSITION EVENT" &
          DSSCAT_UP == "FOLLOW-UP"
      ], na.rm = TRUE),
      as.Date(NA)
    ),
    
    DTHDCRS = if_else(                              # Death Discontinuation Reason
      any(DSCAT_UP == "OTHER EVENT" & DSSCAT_UP == "DEATH"),
      "DEATH DUE TO DISEASE PROGRESSION",
      ""
    ),
    
    STDDCFL = if_else(                              # Study Discontinuation Flag
      any(DSCAT_UP == "DISPOSITION EVENT" & DSSCAT_UP == "STUDY PERIOD"),
      "Y",
      "N"
    ),
    
    SFUDCFL = if_else(                              # Survival Follow-up Discontinuation Flag
      any(DSCAT_UP == "DISPOSITION EVENT" & DSSCAT_UP == "FOLLOW-UP"),
      "Y",
      "N"
    ),
    .groups = "drop"
  )

# =============================================================================
# Baseline Vital Signs
# =============================================================================

bwt_base <- vs_sdtm %>%
  mutate(
    VSTESTCD_UP = toupper(trimws(VSTESTCD)),        # Uppercase Vital Signs Test Code
    VISIT_UP = toupper(trimws(VISIT)),              # Uppercase Visit
    VSDT = as.Date(substr(VSDTC, 1, 10)),           # Vital Signs Date
    VSORRESN = as.numeric(VSORRES)                  # Vital Signs Original Result Numeric
  ) %>%
  filter(VSTESTCD_UP == "WEIGHT") %>%
  left_join(ex_trt %>% select(USUBJID, TRTSDT), by = "USUBJID") %>%
  mutate(
    BWT_PRIORITY = case_when(                       # Baseline Weight Selection Priority
      VISIT_UP == "DAY 1" ~ 1,
      VISIT_UP == "SCREENING" ~ 2,
      !is.na(VSDT) & !is.na(TRTSDT) & VSDT <= TRTSDT ~ 3,
      TRUE ~ 9
    )
  ) %>%
  filter(BWT_PRIORITY < 9) %>%
  arrange(USUBJID, BWT_PRIORITY, desc(VSDT)) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    USUBJID,
    BWT = VSORRESN                                  # Baseline Weight
  )

bht_base <- vs_sdtm %>%
  mutate(
    VSTESTCD_UP = toupper(trimws(VSTESTCD)),        # Uppercase Vital Signs Test Code
    VSDT = as.Date(substr(VSDTC, 1, 10)),           # Vital Signs Date
    VSORRESN = as.numeric(VSORRES)                  # Vital Signs Original Result Numeric
  ) %>%
  filter(VSTESTCD_UP == "HEIGHT") %>%
  arrange(USUBJID, desc(VSDT)) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    USUBJID,
    BHT = VSORRESN                                  # Baseline Height
  )

vs_base <- bwt_base %>%
  full_join(bht_base, by = "USUBJID")

# =============================================================================
# Follow-up Flag
# =============================================================================

spufl <- ds_clean %>%
  group_by(USUBJID) %>%
  summarise(
    SFUFL = if_else(                                # Survival Follow-up Flag
      any(DSCAT_UP == "DISPOSITION EVENT" & DSSCAT_UP == "FOLLOW-UP"),
      "Y",
      "N"
    ),
    .groups = "drop"
  )

# =============================================================================
# Prior Cancer Treatment and Disease History
# =============================================================================

prsurg <- yp_sdtm %>%
  mutate(
    YPCAT_UP = toupper(trimws(YPCAT))               # Uppercase Procedure Category
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    PRSURGFL = if_else(                             # Prior Surgery Flag
      any(YPCAT_UP == "PRIOR CANCER SURGERY", na.rm = TRUE),
      "N",
      "Y"
    ),
    .groups = "drop"
  )

prrad <- xr_sdtm %>%
  mutate(
    XROCCUR_UP = toupper(trimws(XROCCUR))           # Uppercase Radiation Occurrence
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    PRRADFL = if_else(                              # Prior Radiation Flag
      any(XROCCUR_UP == "Y", na.rm = TRUE),
      "Y",
      "N"
    ),
    .groups = "drop"
  )

prsys <- cm_sdtm %>%
  mutate(
    CMCAT_UP = toupper(trimws(CMCAT))               # Uppercase Medication Category
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    PRSYSFL = if_else(                              # Prior Systemic Therapy Flag
      any(CMCAT_UP == "PRIOR CANCER THERAPY", na.rm = TRUE),
      "Y",
      "N"
    ),
    .groups = "drop"
  )

prtx_yp <- yp_sdtm %>%
  mutate(
    YPCAT_UP = toupper(trimws(YPCAT)),              # Uppercase Procedure Category
    YPENDT = as.Date(substr(YPENDTC, 1, 10))        # Procedure End Date
  ) %>%
  filter(!is.na(YPENDT)) %>%
  group_by(USUBJID) %>%
  summarise(
    PRTXDT_YP = max(YPENDT, na.rm = TRUE),          # Prior Therapy Date from Procedures
    .groups = "drop"
  )

prtx_xr <- xr_sdtm %>%
  mutate(
    XRENDT = as.Date(substr(XRENDTC, 1, 10))        # Radiation End Date
  ) %>%
  filter(!is.na(XRENDT)) %>%
  group_by(USUBJID) %>%
  summarise(
    PRTXDT_XR = max(XRENDT, na.rm = TRUE),          # Prior Therapy Date from Radiation
    .groups = "drop"
  )

prtx_cm <- cm_sdtm %>%
  mutate(
    CMCAT_UP = toupper(trimws(CMCAT)),              # Uppercase Medication Category
    CMENDT = as.Date(substr(CMENDTC, 1, 10))        # Medication End Date
  ) %>%
  filter(CMCAT_UP == "PRIOR CANCER THERAPY", !is.na(CMENDT)) %>%
  left_join(rand, by = "USUBJID") %>%
  filter(CMENDT < RANDDT) %>%
  group_by(USUBJID) %>%
  summarise(
    PRTXDT_CM = max(CMENDT, na.rm = TRUE),          # Prior Therapy Date from Medication
    .groups = "drop"
  )

cancer_prior <- dm_sdtm %>%
  select(USUBJID) %>%
  left_join(prsurg, by = "USUBJID") %>%
  left_join(prrad, by = "USUBJID") %>%
  left_join(prsys, by = "USUBJID") %>%
  left_join(prtx_yp, by = "USUBJID") %>%
  left_join(prtx_xr, by = "USUBJID") %>%
  left_join(prtx_cm, by = "USUBJID") %>%
  mutate(
    PRSURGFL = coalesce(PRSURGFL, NA_character_),   # Prior Surgery Flag
    PRRADFL = coalesce(PRRADFL, "N"),               # Prior Radiation Flag
    PRSYSFL = coalesce(PRSYSFL, NA_character_),     # Prior Systemic Therapy Flag
    PRTXDT = pmax(PRTXDT_XR, PRTXDT_CM, na.rm = TRUE), # Prior Therapy Date
    PRTXDT = if_else(
      is.na(PRTXDT_XR) & is.na(PRTXDT_CM),
      as.Date(NA),
      PRTXDT
    )
  ) %>%
  select(
    USUBJID,
    PRTXDT,                                         # Prior Therapy Date
    PRSURGFL,                                       # Prior Surgery Flag
    PRRADFL,                                        # Prior Radiation Flag
    PRSYSFL                                         # Prior Systemic Therapy Flag
  )

zh_cancer <- zh_sdtm %>%
  mutate(
    ZHTESTCD_UP = toupper(trimws(ZHTESTCD)),        # Uppercase Disease History Test Code
    ZHORRES_UP = toupper(trimws(ZHORRES))           # Uppercase Disease History Result
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    REMISS = ZHORRES_UP[ZHTESTCD_UP == "DXRMS"][1], # Remission Status
    REMISSN = case_when(                            # Remission Status Numeric
      REMISS == "SECOND COMPLETE REMISSION" ~ 1,
      REMISS == "THIRD COMPLETE REMISSION" ~ 2,
      is.na(REMISS) ~ NA_real_,
      TRUE ~ NA_real_
    ),
    RSP125 = first(ZHORRES_UP[ZHTESTCD_UP == "RSP125YN"]), # CA-125 Response Indicator
    HPATHTYP = ZHORRES_UP[ZHTESTCD_UP == "HPATHTYP"][1],   # Histopathology Type
    HSUBTYP = if_else(                              # Histology Subtype
      any(ZHTESTCD_UP == "HSUBTYP"),
      first(ZHORRES_UP[ZHTESTCD_UP == "HSUBTYP"]),
      ""
    ),
    .groups = "drop"
  )

cancer_vars <- cancer_prior %>%
  left_join(zh_cancer, by = "USUBJID")

# =============================================================================
# Efficacy Evaluability from TU
# =============================================================================

tu_eff <- tu_sdtm %>%
  mutate(
    TUORRES_UP = toupper(trimws(TUORRES)),          # Uppercase Tumor Result
    TU_EVAL_REC = TUORRES_UP == "N"                 # Tumor Evaluable Record Flag
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    TU_EVALFL = if_else(                            # Tumor Evaluation Flag
      any(TU_EVAL_REC, na.rm = TRUE),
      "Y",
      "N"
    ),
    .groups = "drop"
  )

# =============================================================================
# Final ADSL
# =============================================================================

ADSL <- dm_sdtm %>%
  mutate(
    AGEGR1 = case_when(                             # Age Group 1
      AGE >= 18 & AGE < 41 ~ "18 - 40",
      AGE >= 41 & AGE < 65 ~ "41 - 64",
      AGE >= 65 ~ ">= 65"
    ),
    AGEGR1N = case_when(                            # Age Group 1 Numeric
      AGE >= 18 & AGE < 41 ~ 1,
      AGE >= 41 & AGE < 65 ~ 2,
      AGE >= 65 ~ 3
    ),
    TRT01P = case_when(                             # Planned Treatment for Period 1
      toupper(trimws(ARM)) == "CMP-135" ~ "CMP-135",
      toupper(trimws(ARM)) == "PLACEBO" ~ "Placebo"
    ),
    TRT01PN = case_when(                            # Planned Treatment for Period 1 Numeric
      toupper(trimws(ARM)) == "CMP-135" ~ 1,
      toupper(trimws(ARM)) == "PLACEBO" ~ 0
    )
  ) %>%
  left_join(ex_trt, by = "USUBJID") %>%
  left_join(rand, by = "USUBJID") %>%
  left_join(ds_adsl, by = "USUBJID") %>%
  left_join(spufl, by = "USUBJID") %>%
  left_join(vs_base, by = "USUBJID") %>%
  left_join(becog_qs, by = "USUBJID") %>%
  left_join(cancer_vars, by = "USUBJID") %>%
  left_join(tu_eff, by = "USUBJID") %>%
  mutate(
    ITTFL = if_else(!is.na(RANDDT), "Y", "N"),      # Intent-to-Treat Population Flag
    SAFFL = if_else(!is.na(TRTSDT), "Y", "N"),      # Safety Population Flag
    TRTDCFL = coalesce(TRTDCFL, "N"),               # Treatment Discontinuation Flag
    COMPLFL = coalesce(COMPLFL, "N"),               # Completion Flag
    STDDCFL = coalesce(STDDCFL, "N"),               # Study Discontinuation Flag
    SFUDCFL = coalesce(SFUDCFL, "N"),               # Survival Follow-up Discontinuation Flag
    SFUFL = coalesce(SFUFL, "N"),                   # Survival Follow-up Flag
    DTHFL = if_else(!is.na(DTHDT), "Y", ""),        # Death Flag
    TU_EVALFL = coalesce(TU_EVALFL, "N"),           # Tumor Evaluation Flag
    
    EFFL = if_else(                                 # Efficacy Population Flag
      SAFFL == "Y" &
        !is.na(REMISSN) &
        REMISSN > 0 &
        TU_EVALFL == "Y",
      "Y",
      "N"
    ),
    
    CA125FL = if_else(                              # CA-125 Responder Flag
      SAFFL == "Y" & RSP125 == "Y",
      "Y",
      "N"
    ),
    
    TRTDCDTC = as.character(TRTDCDT),               # Treatment Discontinuation Date Character
    STDSDTC = as.character(STDSDT),                 # Study Start Date Character
    STDEDTC = as.character(STDEDT),                 # Study End Date Character
    SFUEDTC = if_else(!is.na(SFUEDT), as.character(SFUEDT), ""), # Survival Follow-up End Date Character
    TRTSDTC = as.character(TRTSDT),                 # Treatment Start Date Character
    TRTEDTC = as.character(TRTEDT),                 # Treatment End Date Character
    
    FPDUR = case_when(                              # Follow-up Duration
      !is.na(SFUEDT) & !is.na(TRTSDT) ~
        as.numeric(SFUEDT - TRTSDT + 1),
      is.na(SFUEDT) & !is.na(STDEDT) & !is.na(TRTSDT) ~
        as.numeric(STDEDT - TRTSDT + 1),
      is.na(SFUEDT) & is.na(STDEDT) & !is.na(DTHDT) & !is.na(TRTSDT) ~
        as.numeric(DTHDT - TRTSDT + 1),
      TRUE ~ NA_real_
    ),
    
    DTHPER = if_else(                               # Death Period
      !is.na(DTHDT) & !is.na(TRTSDT),
      "SURVIVAL FOLLOW-UP PERIOD",
      ""
    ),
    
    TRTDUR = as.numeric(TRTEDT - TRTSDT + 1),       # Treatment Duration
    STDDUR = as.numeric(STDEDT - STDSDT + 1),       # Study Duration
    PRTXDUR = as.numeric((RANDDT - PRTXDT + 1) / 7) # Prior Therapy Duration in Weeks
  ) %>%
  select(
    STUDYID, USUBJID, SUBJID, SITEID, INVNAM, INVID,
    AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, RACE, ETHNIC, COUNTRY,
    ARM, ARMCD,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    ITTFL, SAFFL, EFFL, CA125FL,
    TRTDCFL, COMPLFL, STDDCFL, SFUDCFL, SFUFL, DTHFL,
    RANDDT,
    TRTSDTC, TRTSDT, TRTEDTC, TRTEDT, TRTDCDT,
    STDSDTC, STDSDT, STDEDTC, STDEDT,
    SFUEDTC, SFUEDT,
    DTHDT,
    TRTDUR, STDDUR, FPDUR, DTHPER,
    TRTDCRS, STDDCRS, DTHDCRS,
    PRTXDT, PRTXDUR,
    PRSURGFL, PRRADFL, PRSYSFL,
    BWT, BHT, BECOG,
    REMISS, REMISSN, RSP125,
    HPATHTYP, HSUBTYP
  )

print(ADSL)
adsl_orr <-  read_xpt(file.path(ADAM_VALIDATION_DIR, "ADSL.xpt"))
all.equal(adsl_orr,ADSL,check.attributes=FALSE)

# =============================================================================
# ADAE derivation from SDTM
# =============================================================================

# -----------------------------------------------------------------------------
# Read SDTM source domains
# -----------------------------------------------------------------------------

ae_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "AE.xpt"))          # AE = Adverse Events
suppae_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "SUPPAE.xpt")) # SUPPAE = Supplemental Adverse Events

# ADSL is derived in the ADSL section above.

# -----------------------------------------------------------------------------
# Transpose SUPPAE qualifiers
# -----------------------------------------------------------------------------

suppae_wide <- suppae_sdtm %>%
  filter(
    RDOMAIN == "AE",                         # RDOMAIN = Related Domain; keep AE supplemental records
    IDVAR == "AESEQ"                         # IDVAR = Identifying Variable; link SUPPAE by AE sequence
  ) %>%
  mutate(
    AESEQ = as.numeric(IDVARVAL)             # AESEQ = Adverse Event Sequence Number
  ) %>%
  select(
    STUDYID,
    RDOMAIN,
    USUBJID,
    AESEQ,
    QNAM,
    QVAL
  ) %>%
  pivot_wider(
    id_cols = c(STUDYID, RDOMAIN, USUBJID, AESEQ),
    names_from = QNAM,                       # QNAM = Qualifier Name; becomes the new column name
    values_from = QVAL                       # QVAL = Qualifier Value; becomes the new column value
  ) %>%
  rename(
    DOMAIN = RDOMAIN                         # DOMAIN = SDTM Domain
  )

# -----------------------------------------------------------------------------
# Merge AE, SUPPAE, and ADSL
# -----------------------------------------------------------------------------

ADAE <- ae_sdtm %>%
  mutate(
    AESEQ = as.numeric(AESEQ)                # AESEQ = Adverse Event Sequence Number
  ) %>%
  left_join(
    suppae_wide,
    by = c("STUDYID", "DOMAIN", "USUBJID", "AESEQ")
  ) %>%
  left_join(
    ADSL,
    by = c("STUDYID", "USUBJID"),
    suffix = c("", "_ADSL")
  ) %>%
  
  # ---------------------------------------------------------------------------
# Derive ADAE analysis variables
# ---------------------------------------------------------------------------

mutate(
  SRCDOM = DOMAIN,                         # SRCDOM = Source Domain
  SRCSEQ = AESEQ,                          # SRCSEQ = Source Sequence Number
  
  AESDT = if_else(                         # AESDT = Adverse Event Start Date
    nchar(substr(AESTDTC, 1, 10)) == 10,
    as.Date(substr(AESTDTC, 1, 10)),
    as.Date(NA)
  ),
  
  AEEDT = if_else(                         # AEEDT = Adverse Event End Date
    nchar(substr(AEENDTC, 1, 10)) == 10,
    as.Date(substr(AEENDTC, 1, 10)),
    as.Date(NA)
  ),
  
  AESDY = ifelse(                          # AESDY = Adverse Event Start Study Day
    as.Date(AESDT) >= as.Date(TRTSDT),
    as.numeric(AESDT - TRTSDT + 1),
    as.numeric(AESDT - TRTSDT)
  ),
  
  AERELOTH = if_else(                      # AERELOTH = Other Relationship to Study Treatment
    AERELNST != "MULTIPLE",
    AERELNST,
    paste(AERELNS1, AERELNS2, sep = "; ")
  ),
  
  AETOXGRN = as.numeric(AETOXGR),          # AETOXGRN = Numeric Toxicity Grade
  
  DTHAUTYN = "",                           # DTHAUTYN = Death Autopsy Performed Flag
  
  AETRTOTH = case_when(                    # AETRTOTH = Other Action Taken with Study Treatment
    AEACNOTH == "MULTIPLE" ~ NA_character_,
    AECONTRT == "Y" &
      (is.na(AEACNOTH) | AEACNOTH == "" | AEACNOTH == "NONE") ~ "MEDICATION",
    AECONTRT == "Y" ~ paste(AEACNOTH, "MEDICATION", sep = "; "),
    TRUE ~ AEACNOTH
  ),
  
  AEDTHDTC = ifelse(                       # AEDTHDTC = Adverse Event Death Date Character
    AESDTH == "Y",
    DTHDTC,
    ""
  ),
  
  AEHDTC = ifelse(                         # AEHDTC = Adverse Event Hospitalization Date Character
    !is.na(AEHDTC),
    AEHDTC,
    ""
  ),
  
  TRTSDT = as.Date(TRTSDT),                # TRTSDT = Treatment Start Date
  
  TRTEM = case_when(                       # TRTEM = Treatment Emergent Flag
    !is.na(AESTDTC) & nchar(AESTDTC) >= 10 &
      !is.na(TRTSDT) & as.Date(substr(AESTDTC, 1, 10)) < TRTSDT ~ "N",
    
    !is.na(AESTDTC) & nchar(AESTDTC) == 7 &
      !is.na(TRTSDT) &
      as.integer(substr(AESTDTC, 1, 4)) < as.integer(format(TRTSDT, "%Y")) ~ "N",
    
    !is.na(AESTDTC) & nchar(AESTDTC) == 7 &
      !is.na(TRTSDT) &
      as.integer(substr(AESTDTC, 1, 4)) == as.integer(format(TRTSDT, "%Y")) &
      as.integer(substr(AESTDTC, 6, 7)) < as.integer(format(TRTSDT, "%m")) ~ "N",
    
    !is.na(AESTDTC) & nchar(AESTDTC) == 4 &
      !is.na(TRTSDT) &
      as.integer(substr(AESTDTC, 1, 4)) < as.integer(format(TRTSDT, "%Y")) ~ "N",
    
    TRUE ~ "Y"
  )
) %>%
  
  # ---------------------------------------------------------------------------
# Keep final ADAE variable order
# ---------------------------------------------------------------------------

select(
  STUDYID, USUBJID, SUBJID, SITEID,
  TRT01P, TRT01PN, TRT01A, TRT01AN,
  REMISS, REMISSN, ITTFL, SAFFL, EFFL, CA125FL,
  TRTDCFL, COMPLFL, STDDCFL, SFUDCFL, SFUFL,
  SRCDOM, SRCSEQ,
  AETERM, AEMODIFY, AEDECOD, AEBODSYS,
  AEHLGT, AEHLT, AELLT, AELLTCD,
  TRTSDT, TRTSDTC, TRTEDT, TRTEDTC,
  AESTDTC, AESTRTPT, AEENDTC, AEENRTPT,
  AESDT, AEEDT, AESDY,
  AESER, AEREL, AERELOTH,
  AETOXGR, AETOXGRN,
  AEACN, AETRTOTH,
  AESDTH, AEDTHDTC, DTHAUTYN,
  AESLIFE, AESHOSP, AEHDTC,
  AESDISAB, AESCONG, AESMIE,
  TRTEM,
  INVNAM, INVID,
  AGE, AGEU, AGEGR1, AGEGR1N,
  SEX, RACE, ETHNIC, COUNTRY, RANDDT
)
print(ADAE)

######################### Validation ###########################################

adae_orr <-  read_xpt(file.path(ADAM_VALIDATION_DIR, "ADAE.xpt"))
all.equal(adae_orr,ADAE,check.attributes=FALSE)

library(haven)
library(dplyr)
library(tidyr)

# =============================================================================
# ADLBSI derivation from SDTM
# =============================================================================

lb_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "LB.xpt"))

# External local script omitted; required logic is included in this portfolio script.

ADLBSI <- lb_sdtm %>%
  filter(
    is.na(LBSTAT) | gsub(" ", "", toupper(LBSTAT)) == "",
    LBSTRESC != "ND - NOT DONE"
  ) %>%
  mutate(
    AVAL = LBSTRESN
  ) %>%
  left_join(
    ADSL,
    by = c("STUDYID", "USUBJID"),
    suffix = c("", "_ADSL")
  ) %>%
  mutate(
    TRTP = TRT01P,
    SRCDOM = DOMAIN,
    SRCSEQ = as.numeric(LBSEQ),
    SRCVAR = "LBSTRESN",
    
    ADTC = LBDTC,
    ADT = as.Date(substr(ADTC, 1, 10)),
    
    TRTSDT = as.Date(TRTSDT),
    TRTEDT = as.Date(TRTEDT),
    
    ADY = if_else(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT,
      as.numeric(ADT - TRTSDT + 1),
      as.numeric(ADT - TRTSDT)
    ),
    
    ONTRTFL = if_else(
      !is.na(ADT) & !is.na(TRTSDT) & !is.na(TRTEDT) &
        ADT >= TRTSDT & ADT <= TRTEDT,
      "Y",
      ""
    ),
    
    AVALC = LBSTRESC,
    
    ANL01FL = if_else(
      !is.na(AVAL) | (!is.na(AVALC) & AVALC != ""),
      "Y",
      ""
    ),
    
    PARAM = if_else(
      !is.na(LBSTRESU) & LBSTRESU != "",
      paste0(trimws(LBCAT), "|", trimws(LBTEST), " (", trimws(LBSTRESU), ")"),
      paste0(trimws(LBCAT), "|", trimws(LBTEST))
    ),
    
    PARAMCD = paste0(
      case_when(
        grepl("CHEM", LBCAT, ignore.case = TRUE) ~ "C",
        grepl("URIN", LBCAT, ignore.case = TRUE) ~ "U",
        grepl("HEMATOLOGY", LBCAT, ignore.case = TRUE) ~ "H",
        TRUE ~ "X"
      ),
      substr(trimws(LBTESTCD), 1, 6),
      case_when(
        is.na(LBSTRESU) | LBSTRESU == "" ~ "N",
        TRUE ~ "S"
      )
    ),
    
    ATOXGR = case_when(
      PARAMCD == "CBUNS" & !is.na(AVAL) & round(AVAL, 1) >= 12.9 ~ "2",
      
      PARAMCD == "CBUNS" &
        !is.na(AVAL) & !is.na(LBSTNRHI) &
        AVAL > LBSTNRHI & round(AVAL, 1) < 12.9 ~ "1",
      
      PARAMCD == "CBUNS" ~ "",
      
      is.na(LBTOXGR) | trimws(LBTOXGR) == "" ~ "",
      
      TRUE ~ gsub("[HL]", "", trimws(LBTOXGR))
    ),
    
    ATOXDIR = case_when(
      PARAMCD == "CBUNS" & !is.na(AVAL) & round(AVAL, 1) >= 12.9 ~ "H",
      
      PARAMCD == "CBUNS" &
        !is.na(AVAL) & !is.na(LBSTNRHI) &
        AVAL > LBSTNRHI & round(AVAL, 1) < 12.9 ~ "H",
      
      PARAMCD == "CBUNS" ~ "",
      
      is.na(LBTOXGR) | trimws(LBTOXGR) == "" ~ "",
      
      TRUE ~ gsub("[0-9]", "", trimws(LBTOXGR))
    ),
    
    ANRIND = LBNRIND,
    
    PARCAT1 = LBCAT,
    PARCAT2 = "SI",
    
    ANRLO = if_else(
      !is.na(LBSTNRLO) & grepl("e", as.character(LBSTNRLO), ignore.case = TRUE),
      trimws(format(LBSTNRLO, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)),
      if_else(!is.na(LBSTNRLO), trimws(as.character(LBSTNRLO)), "")
    ),
    
    ANRHI = if_else(
      !is.na(LBSTNRHI) & grepl("e", as.character(LBSTNRHI), ignore.case = TRUE),
      trimws(format(LBSTNRHI, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)),
      if_else(!is.na(LBSTNRHI), trimws(as.character(LBSTNRHI)), "")
    )
  ) %>%
  
  arrange(STUDYID, USUBJID, PARAMCD, ADT, SRCSEQ) %>%
  group_by(STUDYID, USUBJID, PARAMCD) %>%
  mutate(
    ABLFL = if_else(
      row_number() == case_when(
        any(!is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT &
              (!is.na(AVAL) | (!is.na(AVALC) & AVALC != ""))) ~
          max(which(!is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT &
                      (!is.na(AVAL) | (!is.na(AVALC) & AVALC != "")))),
        
        any(!is.na(ADT) & !is.na(TRTSDT) & ADT == TRTSDT &
              (!is.na(AVAL) | (!is.na(AVALC) & AVALC != ""))) ~
          max(which(!is.na(ADT) & !is.na(TRTSDT) & ADT == TRTSDT &
                      (!is.na(AVAL) | (!is.na(AVALC) & AVALC != "")))),
        
        TRUE ~ 0L
      ),
      "Y",
      ""
    ),
    
    BASE = if_else(
      any(ABLFL == "Y"),
      AVAL[ABLFL == "Y"][1],
      NA_real_
    ),
    
    BASEC = if_else(
      any(ABLFL == "Y"),
      AVALC[ABLFL == "Y"][1],
      ""
    ),
    
    BTOXGR = if_else(
      any(ABLFL == "Y"),
      ATOXGR[ABLFL == "Y"][1],
      ""
    ),
    
    BTOXDIR = if_else(
      any(ABLFL == "Y"),
      ATOXDIR[ABLFL == "Y"][1],
      ""
    ),
    
    BNRIND = if_else(
      any(ABLFL == "Y"),
      ANRIND[ABLFL == "Y"][1],
      ""
    )
  ) %>%
  ungroup() %>%
  
  mutate(
    CHG = if_else(
      !is.na(AVAL) & !is.na(BASE),
      AVAL - BASE,
      NA_real_
    ),
    
    PCHG = if_else(
      !is.na(CHG) & !is.na(BASE) & BASE != 0,
      (100 * CHG) / BASE,
      NA_real_
    )
  ) %>%
  
  select(
    SUBJID, AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, RACE, ETHNIC,
    ITTFL, SAFFL, EFFL, CA125FL,
    RANDDT,
    TRTSDTC, TRTSDT, TRTEDTC, TRTEDT,
    ADTC, ADT, ADY, VISIT, ONTRTFL,
    ANL01FL, ABLFL,
    SRCDOM, SRCSEQ, SRCVAR,
    PARAM, PARAMCD, PARCAT1, PARCAT2,
    BASE, BASEC, AVAL, AVALC,
    ANRLO, ANRHI,
    CHG, PCHG,
    BTOXGR, ATOXGR, BTOXDIR, ATOXDIR,
    BNRIND, ANRIND,
    STUDYID, USUBJID, SITEID,
    INVNAM, INVID, COUNTRY,
    ARM, ARMCD,
    TRT01P, TRT01PN, TRT01A, TRT01AN,
    TRTP
  )
print(ADLBSI)

######################### Validation ###########################################

adlbsi_orr <-  read_xpt(file.path(ADAM_VALIDATION_DIR, "ADLBSI.xpt"))
all.equal(adlbsi_orr,ADLBSI,check.attributes=FALSE)

# =============================================================================
# ADEX derivation from SDTM/Raw DA and ADSL
# =============================================================================

# -----------------------------------------------------------------------------
# Read source DA dataset and source ADSL
# -----------------------------------------------------------------------------

da_raw <- read_xpt(file.path(SDTM_DATA_DIR, "DA.xpt")) # DA = Drug Accountability

# ADSL is derived in the ADSL section above.

# -----------------------------------------------------------------------------
# Derive total number of capsules taken from DA
# -----------------------------------------------------------------------------

da_caps <- da_raw %>%
  distinct(
    USUBJID, DARFTDTC, DADTC, DATESTCD, DAORRES,
    .keep_all = TRUE
  ) %>%
  filter(
    DATESTCD == "TAKENAMT"                           # DATESTCD = Drug Accountability Test Code
  ) %>%
  group_by(USUBJID) %>%
  summarise(
    CUMCAP_AVAL = sum(DASTRESN, na.rm = TRUE),       # CUMCAP_AVAL = Total Number of 150mg Capsules Taken
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# Build one-row-per-subject ADEX base
# -----------------------------------------------------------------------------

adex_base <- ADSL %>%
  left_join(
    da_caps,
    by = "USUBJID"
  ) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT),                        # TRTSDT = Treatment Start Date
    TRTEDT = as.Date(TRTEDT),                        # TRTEDT = Treatment End Date
    CUMCAP_AVAL = if_else(is.na(CUMCAP_AVAL), 0, CUMCAP_AVAL), # Total capsules taken, set missing to 0
    TRTP = TRT01P,                                   # TRTP = Planned Treatment
    NTRTDAYS = as.numeric(TRTEDT - TRTSDT + 1)       # NTRTDAYS = Number of Treatment Days
  )

# -----------------------------------------------------------------------------
# Derive TXDUR: Duration of Treatment Received
# -----------------------------------------------------------------------------

txdur <- adex_base %>%
  mutate(
    PARAM = "Duration of Treatment Received (months)", # PARAM = Analysis Parameter
    PARAMCD = "TXDUR",                                 # PARAMCD = Analysis Parameter Code
    AVAL = if_else(                                    # AVAL = Analysis Value
      !is.na(TRTSDT) & !is.na(TRTEDT),
      as.numeric(TRTEDT - TRTSDT + 1) / 30.4375,
      NA_real_
    ),
    DTYPE = "DIFFERENCE"                               # DTYPE = Derivation Type
  )

# -----------------------------------------------------------------------------
# Derive CUMCAP: Total Number of 150mg Capsules Taken
# -----------------------------------------------------------------------------

cumcap <- adex_base %>%
  mutate(
    PARAM = "Total Number of 150mg Capsules Taken",   # PARAM = Analysis Parameter
    PARAMCD = "CUMCAP",                               # PARAMCD = Analysis Parameter Code
    AVAL = CUMCAP_AVAL,                               # AVAL = Analysis Value
    DTYPE = "SUM"                                     # DTYPE = Derivation Type
  )

# -----------------------------------------------------------------------------
# Derive CUMDOSE: Total Cumulative Dose
# -----------------------------------------------------------------------------

cumdose <- adex_base %>%
  mutate(
    PARAM = "Total Cumulative Dose (g)",              # PARAM = Analysis Parameter
    PARAMCD = "CUMDOSE",                              # PARAMCD = Analysis Parameter Code
    AVAL = (CUMCAP_AVAL * 150) / 1000,                # AVAL = Analysis Value in grams
    DTYPE = "SUM"                                     # DTYPE = Derivation Type
  )

# -----------------------------------------------------------------------------
# Derive INTENS: Dose Intensity
# -----------------------------------------------------------------------------

intens <- adex_base %>%
  mutate(
    PARAM = "Dose Intensity (%)",                     # PARAM = Analysis Parameter
    PARAMCD = "INTENS",                               # PARAMCD = Analysis Parameter Code
    AVAL = if_else(                                   # AVAL = Analysis Value
      !is.na(NTRTDAYS) & NTRTDAYS > 0,
      (CUMCAP_AVAL / NTRTDAYS) * 100,
      NA_real_
    ),
    DTYPE = "PERCENTAGE"                              # DTYPE = Derivation Type
  )

# -----------------------------------------------------------------------------
# Combine ADEX parameter records and keep final variable order
# -----------------------------------------------------------------------------

ADEX <- bind_rows(
  txdur,
  cumcap,
  cumdose,
  intens
) %>%
  select(
    SUBJID, AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, RACE, ETHNIC,
    RANDDT, REMISS, REMISSN,
    ITTFL, SAFFL, EFFL, CA125FL,
    TRTDCFL, COMPLFL, STDDCFL, SFUDCFL, SFUFL,
    TRTSDT, TRTEDT, FPDUR,
    PARAM, PARAMCD, AVAL, DTYPE,
    STUDYID, USUBJID, SITEID,
    INVNAM, INVID, COUNTRY,
    TRT01P, TRT01PN, TRT01A, TRT01AN, TRTP
  )%>% 
  arrange(STUDYID, USUBJID, PARAMCD)

print(ADEX)

######################### Validation ###########################################

adex_orr <-  read_xpt(file.path(ADAM_VALIDATION_DIR, "ADEX.xpt"))
all.equal(adex_orr,ADEX,check.attributes=FALSE)

# =============================================================================
# ADTTE derivation from SDTM
# =============================================================================

# -----------------------------------------------------------------------------
# Read SDTM source domains
# -----------------------------------------------------------------------------

tu_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "TU.xpt")) # TU = Tumor Identification
lb_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "LB.xpt")) # LB = Laboratory Test Results
ds_sdtm <- read_xpt(file.path(SDTM_DATA_DIR, "DS.xpt")) # DS = Disposition

# ADSL is derived in the ADSL section above.

# -----------------------------------------------------------------------------
# Define data cutoff date
# -----------------------------------------------------------------------------

cutdt <- as.Date("2010-05-15")                                          # Cutoff Date

# -----------------------------------------------------------------------------
# Derive tumor assessment dates from TU
# -----------------------------------------------------------------------------

tu_dates <- tu_sdtm %>%
  mutate(
    TUDT = as.Date(substr(TUDTC, 1, 10))                                # TUDT = Tumor Assessment Date
  ) %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    TUMLDT = max(                                                       # TUMLDT = Last Tumor Assessment Date
      TUDT[
        !is.na(TUDT) &
          !is.na(TUORRES) & TUORRES != "" &
          TUDT <= cutdt
      ],
      na.rm = TRUE
    ),
    
    FPDDT = min(                                                        # FPDDT = First Progressive Disease Date
      TUDT[
        !is.na(TUDT) &
          substr(trimws(TUORRES), 1, 2) %in% c("Y", "NL") &
          TUDT <= as.Date("2010-05-15")
      ],
      na.rm = TRUE
    )
  ) %>%
  ungroup() %>%
  mutate(
    TUMLDT = if_else(is.infinite(TUMLDT), as.Date(NA), TUMLDT),         # Set missing if no tumor assessment date
    FPDDT = if_else(is.infinite(FPDDT), as.Date(NA), FPDDT)             # Set missing if no progression date
  )

# -----------------------------------------------------------------------------
# Derive CA-125 lab dates from LB
# -----------------------------------------------------------------------------

lb_dates <- lb_sdtm %>%
  mutate(
    LBDT = as.Date(substr(LBDTC, 1, 10)),                               # LBDT = Laboratory Assessment Date
    LBORRESN = suppressWarnings(as.numeric(LBORRES)),                   # LBORRESN = Numeric Original Lab Result
    LBORNRHIN = suppressWarnings(as.numeric(LBORNRHI))                  # LBORNRHIN = Numeric Original Normal Range High
  ) %>%
  left_join(
    ADSL %>% select(STUDYID, USUBJID, TRTSDT, CA125FL),
    by = c("STUDYID", "USUBJID")
  ) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT)                                            # TRTSDT = Treatment Start Date
  ) %>%
  filter(LBTESTCD == "CA125") %>%
  group_by(STUDYID, USUBJID) %>%
  summarise(
    LCA125DT = max(                                                     # LCA125DT = Last CA-125 Lab Assessment Date
      LBDT[
        CA125FL == "Y" &
          !is.na(LBDT) &
          !is.na(TRTSDT) &
          LBDT >= TRTSDT &
          LBDT <= cutdt &
          !is.na(LBORRES) & LBORRES != ""
      ],
      na.rm = TRUE
    ),
    
    FCA125DT = {                                                        # FCA125DT = First CA-125 Progression Date
      ca_dates <- LBDT[
        CA125FL == "Y" &
          !is.na(LBDT) &
          !is.na(TRTSDT) &
          LBDT >= TRTSDT &
          LBDT <= cutdt &
          !is.na(LBORRESN) &
          !is.na(LBORNRHIN) &
          LBORRESN >= 2 * LBORNRHIN
      ]
      
      if (length(ca_dates) >= 2 && as.numeric(max(ca_dates) - min(ca_dates)) >= 7) {
        min(ca_dates)
      } else {
        as.Date(NA)
      }
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    LCA125DT = if_else(is.infinite(LCA125DT), as.Date(NA), LCA125DT)    # Set missing if no CA-125 lab date
  )

# -----------------------------------------------------------------------------
# Derive latest disposition date and reason from DS
# -----------------------------------------------------------------------------

ds_dates <- ds_sdtm %>%
  mutate(
    DSSTDT = as.Date(substr(DSSTDTC, 1, 10))                            # DSSTDT = Disposition Start Date
  ) %>%
  filter(
    DSCAT == "DISPOSITION EVENT",
    DSSCAT %in% c("STUDY PERIOD", "FOLLOW-UP"),
    DSDECOD != "DEATH",
    !is.na(DSSTDT)
  ) %>%
  group_by(STUDYID, USUBJID) %>%
  slice_max(order_by = DSSTDT, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    STUDYID,
    USUBJID,
    DSSTDT,                                                             # DSSTDT = Latest Disposition Start Date
    DSDECOD                                                             # DSDECOD = Disposition Decode
  )

# -----------------------------------------------------------------------------
# Build one-row-per-subject ADTTE base
# -----------------------------------------------------------------------------

adtte_base <- ADSL %>%
  mutate(
    RANDDT = as.Date(RANDDT),                                           # RANDDT = Randomization Date
    TRTSDT = as.Date(TRTSDT),                                           # TRTSDT = Treatment Start Date
    DTHDT = as.Date(DTHDT)                                              # DTHDT = Death Date
  ) %>%
  left_join(tu_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(lb_dates, by = c("STUDYID", "USUBJID")) %>%
  left_join(ds_dates, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    TRTP = TRT01P,                                                      # TRTP = Planned Treatment
    
    DTHDT_ = if_else(                                                   # DTHDT_ = Cutoff-adjusted Death Date
      !is.na(DTHDT) & DTHDT <= cutdt,
      DTHDT,
      as.Date(NA)
    ),
    
    FPD125DT = case_when(                                               # FPD125DT = First Progression Date including CA-125
      CA125FL == "Y" & !is.na(FPDDT) & !is.na(FCA125DT) ~ pmin(FPDDT, FCA125DT),
      CA125FL == "Y" & !is.na(FPDDT) & is.na(FCA125DT) ~ FPDDT,
      CA125FL == "Y" & is.na(FPDDT) & !is.na(FCA125DT) ~ FCA125DT,
      TRUE ~ as.Date(NA)
    )
  )

# -----------------------------------------------------------------------------
# Derive TTPFS: Time to Progression-Free Survival
# -----------------------------------------------------------------------------

ttpfs <- adtte_base %>%
  mutate(
    PARAM = "TIME TO PROGRESSION FREE SURVIVAL (month)",                # PARAM = Analysis Parameter
    PARAMCD = "TTPFS",                                                  # PARAMCD = Analysis Parameter Code
    STARTDT = RANDDT,                                                   # STARTDT = Analysis Start Date
    
    ADT = case_when(                                                    # ADT = Analysis Date
      !is.na(DTHDT_) & !is.na(FPDDT) ~ pmin(DTHDT_, FPDDT),
      is.na(DTHDT_) & !is.na(FPDDT) ~ FPDDT,
      !is.na(DTHDT_) & is.na(FPDDT) ~ DTHDT_,
      is.na(DTHDT_) & is.na(FPDDT) & !is.na(TUMLDT) ~ TUMLDT,
      TRUE ~ RANDDT
    ),
    
    EVNTDESC = case_when(                                               # EVNTDESC = Event Description
      !is.na(DTHDT_) & !is.na(FPDDT) & FPDDT <= DTHDT_ ~ "DISEASE PROGRESSION",
      !is.na(DTHDT_) & !is.na(FPDDT) & FPDDT > DTHDT_ ~ "DEATH",
      is.na(DTHDT_) & !is.na(FPDDT) ~ "DISEASE PROGRESSION",
      !is.na(DTHDT_) & is.na(FPDDT) ~ "DEATH",
      is.na(DTHDT_) & is.na(FPDDT) & !is.na(TUMLDT) ~ "CENSORED AS OF LAST TUMOR SCAN DATE",
      TRUE ~ "CENSORED AS OF RANDOMIZATION DATE"
    ),
    
    CNSR = case_when(                                                   # CNSR = Censoring Indicator
      EVNTDESC == "DISEASE PROGRESSION" ~ 0,
      EVNTDESC == "DEATH" ~ 0,
      EVNTDESC == "CENSORED AS OF LAST TUMOR SCAN DATE" ~ 1,
      EVNTDESC == "CENSORED AS OF RANDOMIZATION DATE" ~ 2,
      TRUE ~ NA_real_
    ),
    
    AVAL = as.numeric(ADT - STARTDT + 1) / 30.4375                      # AVAL = Analysis Value in Months
  )

# -----------------------------------------------------------------------------
# Derive TTPFS125: Time to Progression-Free Survival for CA-125 responders
# -----------------------------------------------------------------------------

ttpfs_ca125 <- adtte_base %>%
  filter(CA125FL == "Y") %>%
  rowwise() %>%
  mutate(
    PARAM = "TIME TO PROGRESSION FREE SURVIVAL CA-125 RESPONDER (month)", # PARAM = Analysis Parameter
    PARAMCD = "TTPFS125",                                                 # PARAMCD = Analysis Parameter Code
    STARTDT = RANDDT,                                                     # STARTDT = Analysis Start Date
    
    event_dt = min(c(DTHDT_, FPDDT, FCA125DT), na.rm = TRUE),             # Earliest event date
    censor_dt = max(c(TUMLDT, LCA125DT), na.rm = TRUE),                   # Latest censoring date
    
    ADT = case_when(                                                      # ADT = Analysis Date
      is.finite(as.numeric(event_dt)) ~ event_dt,
      is.finite(as.numeric(censor_dt)) ~ censor_dt,
      TRUE ~ RANDDT
    ),
    
    EVNTDESC = case_when(                                                 # EVNTDESC = Event Description
      !is.na(DTHDT_) & ADT == DTHDT_ ~ "DEATH",
      !is.na(FPDDT) & ADT == FPDDT ~ "DISEASE PROGRESSION",
      !is.na(FCA125DT) & ADT == FCA125DT ~ "CA-125 CRITERIA AS DISEASE PROGRESSION",
      is.na(DTHDT_) & is.na(FPDDT) & is.na(FCA125DT) & !is.na(TUMLDT) & ADT == TUMLDT ~
        "CENSORED AS OF LAST TUMOR SCAN DATE",
      is.na(DTHDT_) & is.na(FPDDT) & is.na(FCA125DT) & !is.na(LCA125DT) & ADT == LCA125DT ~
        "CENSORED AS OF LAST CA-125 LAB ASSESSMENT DATE",
      TRUE ~ "CENSORED AS OF RANDOMIZATION DATE"
    ),
    
    CNSR = case_when(                                                     # CNSR = Censoring Indicator
      EVNTDESC %in% c(
        "DEATH",
        "DISEASE PROGRESSION",
        "CA-125 CRITERIA AS DISEASE PROGRESSION"
      ) ~ 0,
      EVNTDESC == "CENSORED AS OF LAST TUMOR SCAN DATE" ~ 1,
      EVNTDESC == "CENSORED AS OF LAST CA-125 LAB ASSESSMENT DATE" ~ 2,
      EVNTDESC == "CENSORED AS OF RANDOMIZATION DATE" ~ 3,
      TRUE ~ NA_real_
    ),
    
    AVAL = as.numeric(ADT - STARTDT + 1) / 30.4375                        # AVAL = Analysis Value in Months
  ) %>%
  ungroup() %>%
  select(-event_dt, -censor_dt)

# -----------------------------------------------------------------------------
# Derive TTOS: Time to Overall Survival
# -----------------------------------------------------------------------------

ttos <- adtte_base %>%
  mutate(
    PARAM = "TIME TO OVERALL SURVIVAL (month)",                         # PARAM = Analysis Parameter
    PARAMCD = "TTOS",                                                   # PARAMCD = Analysis Parameter Code
    STARTDT = RANDDT,                                                   # STARTDT = Analysis Start Date
    
    ADT = case_when(                                                    # ADT = Analysis Date
      !is.na(DTHDT) ~ DTHDT,
      is.na(DTHDT) & !is.na(DSSTDT) ~ DSSTDT,
      TRUE ~ RANDDT
    ),
    
    CNSR = case_when(                                                   # CNSR = Censoring Indicator
      !is.na(DTHDT) ~ 0,
      DSDECOD == "STUDY TERMINATED BY SPONSOR" ~ 1,
      DSDECOD == "LOST TO FOLLOW-UP" ~ 2,
      DSDECOD == "WITHDRAWAL BY SUBJECT" ~ 3,
      DSDECOD %in% c("OTHER", "PHYSICIAN DECISION") ~ 4,
      DSDECOD == "PROGRESSIVE DISEASE" ~ 5,
      TRUE ~ NA_real_
    ),
    
    EVNTDESC = case_when(                                               # EVNTDESC = Event Description
      !is.na(DTHDT) ~ "EVENT: DEATH DUE TO ANY CAUSE",
      DSDECOD == "STUDY TERMINATED BY SPONSOR" ~
        "CENSORED AS OF DATE SPONSOR DECIDED TO TERMINATE THE STUDY",
      DSDECOD == "LOST TO FOLLOW-UP" ~
        "CENSORED AS OF DATE DUE TO LOST TO FOLLOW-UP",
      DSDECOD == "WITHDRAWAL BY SUBJECT" ~
        "CENSORED AS OF DATE SUBJECT DECIDED TO WITHDRAW",
      DSDECOD %in% c("OTHER", "PHYSICIAN DECISION") ~
        "CENSORED AS OF DATE OF WITHDRAWAL DUE TO OTHER REASONS",
      DSDECOD == "PROGRESSIVE DISEASE" ~
        "CENSORED AS OF DATE OF DISEASE PROGRESSION",
      TRUE ~ ""
    ),
    
    AVAL = as.numeric(ADT - STARTDT + 1) / 30.4375                      # AVAL = Analysis Value in Months
  )

# -----------------------------------------------------------------------------
# Combine ADTTE parameter records and keep final variable order
# -----------------------------------------------------------------------------

ADTTE <- bind_rows(ttpfs, ttpfs_ca125, ttos) %>%
  mutate(
    TUMLDT = if_else(PARAMCD == "TTOS", as.Date(NA), TUMLDT),           # Clear tumor last date for overall survival
    FPDDT = if_else(PARAMCD == "TTOS", as.Date(NA), FPDDT),             # Clear progression date for overall survival
    FPD125DT = if_else(PARAMCD == "TTOS", as.Date(NA), FPD125DT),       # Clear CA-125 progression date for overall survival
    FCA125DT = if_else(PARAMCD == "TTOS", as.Date(NA), FCA125DT),       # Clear first CA-125 progression date for overall survival
    LCA125DT = if_else(PARAMCD == "TTOS", as.Date(NA), LCA125DT)        # Clear last CA-125 lab date for overall survival
  ) %>%
  select(
    SUBJID, AGE, AGEU, AGEGR1, AGEGR1N,
    SEX, RACE, ETHNIC,
    ITTFL, SAFFL, EFFL, CA125FL,
    TRTSDT, TRTEDT, STDSDT, STDEDT,
    RANDDT, DTHDT, DTHPER,
    BECOG, REMISS, REMISSN, RSP125,
    HPATHTYP, HSUBTYP,
    TUMLDT, FPDDT, FPD125DT, FCA125DT, LCA125DT,
    PARAM, PARAMCD, STARTDT, ADT, CNSR, EVNTDESC, AVAL,
    STUDYID, USUBJID, SITEID,
    INVNAM, INVID, COUNTRY,
    TRT01P, TRT01PN, TRT01A, TRT01AN, TRTP
  )%>% 
  arrange(STUDYID, USUBJID, PARAMCD)

print(ADTTE)

######################### Validation ###########################################

adtte_orr <-  read_xpt(file.path(ADAM_VALIDATION_DIR, "ADTTE.xpt"))
all.equal(adtte_orr,ADTTE,check.attributes=FALSE)


