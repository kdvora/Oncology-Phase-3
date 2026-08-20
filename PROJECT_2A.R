library(haven)
library(lubridate)
library(tidyr)
library(dplyr)
library(stringr)

# Configure paths and identifiers without exposing workstation or study details.
# Set these environment variables before running, or use the project-relative defaults.
RAW_DATA_DIR <- Sys.getenv("RAW_DATA_DIR", unset = file.path("data", "raw"))
VALIDATION_DATA_DIR <- Sys.getenv("VALIDATION_DATA_DIR", unset = file.path("data", "validation"))
OUTPUT_DIR <- Sys.getenv("OUTPUT_DIR", unset = file.path("output"))
STUDY_ID <- Sys.getenv("STUDY_ID", unset = "SYNTHETIC-STUDY")
TREATMENT_CODE <- Sys.getenv("TREATMENT_CODE", unset = "SYNTHETIC-TRT")


################################################################################
############################### RAW FILES ######################################
################################################################################

# 1. Adverse Events (Raw data for AE domain)
raw_ae   <- read_sas(file.path(RAW_DATA_DIR, "ae.sas7bdat"))

# 2. Demographics (Source for DM domain)
raw_dm   <- read_sas(file.path(RAW_DATA_DIR, "dm.sas7bdat"))

# 3. Date of Visit (Helper file for dates in all domains)
raw_dov  <- read_sas(file.path(RAW_DATA_DIR, "dov.sas7bdat"))

# 4. End of Study Disposition (Source for DS domain)
raw_ds   <- read_sas(file.path(RAW_DATA_DIR, "ds.sas7bdat"))

# 5. Study Drug Discontinuation (Secondary source for DS domain)
raw_dsdd <- read_sas(file.path(RAW_DATA_DIR, "dsdd.sas7bdat"))

# 6. Enrollment (Source for Informed Consent dates in DM)
raw_enr  <- read_sas(file.path(RAW_DATA_DIR, "enr.sas7bdat"))

# 7. Exposure (Source for EX domain and First Dose date in DM)
raw_ex   <- read_sas(file.path(RAW_DATA_DIR, "ex.sas7bdat"))

# 8. Investigators (Reference for site information)
raw_inv  <- read_sas(file.path(RAW_DATA_DIR, "inv.sas7bdat"))

# 9. Medical History (Source for MH domain)
raw_mh   <- read_sas(file.path(RAW_DATA_DIR, "mh.sas7bdat"))

# 10. Vital Signs (Source for VS domain)
raw_vs   <- read_sas(file.path(RAW_DATA_DIR, "vs.sas7bdat"))

# 11. SE 
raw_se <- read_xpt(file.path(RAW_DATA_DIR, "se.xpt"))


################################################################################
######################### DEMOGRAPHICS DOMAIN ##################################
################################################################################

#Summarizing exposure file
ex_first_dose <- raw_ex %>%
  mutate(
    EXSTTM = ifelse(is.na(EXSTTM) | EXSTTM == "", "00:00", EXSTTM),
    EXSTDTC = as.POSIXct(
      paste(EXSTDTN, EXSTTM),
      format = "%Y-%m-%d %H:%M"
    )
  ) %>%
  group_by(SUBJECT) %>%
  summarise(
    RFSTDTC_RAW = min(EXSTDTC, na.rm = TRUE),
    RFXENDTC_RAW = max(EXSTDTC, na.rm = TRUE)
  ) %>%
  ungroup() 
 

#Summarizing AE file
ae_death_info <- raw_ae %>%
  filter(AEOUT == "FATAL") %>% 
  group_by(SUBJECT) %>%
  summarise(
    DTHDTC_RAW = max(AEENDTN, na.rm = TRUE)
  )%>%
  ungroup()

#Summarizing INVestigator File
raw_inv_clean <- raw_inv %>%
  mutate(SITE_JOIN = as.character(as.numeric(SITEID)) %>% trimws()) %>%
  distinct(SITE_JOIN, .keep_all = TRUE)

#Summarizing DATE of Visit File
dov_summary <- raw_dov %>%
  group_by(SUBJECT) %>% 
  summarise(
    DMDTC_RAW = min(VISDTN, na.rm = TRUE)
  ) %>%
  ungroup()

#JOIning All the Tables
dm_combined <- raw_dm %>% 
  mutate(
    SITE_JOIN = as.character(as.numeric(sub("-.*", "", SITE))) %>% trimws()) %>% 
  left_join(raw_enr,by = "SUBJECT") %>%
  left_join(raw_ds,by = "SUBJECT") %>%
  left_join(ex_first_dose,by = "SUBJECT") %>%
  left_join(ae_death_info, by = "SUBJECT") %>%
  left_join(raw_inv_clean,by = "SITE_JOIN") %>%
  left_join(dov_summary, by = "SUBJECT")

dm_1 <-  dm_combined %>%
  mutate(PROJECT = PROJECT.x) %>% 
  select(PROJECT, SUBJECT, SEX, RACE, BRTHDTN, CNSTDTN, DSSTDAT,RFSTDTC_RAW,
         RFXENDTC_RAW, ENRGRP,ETHNIC,DTHDTC_RAW,SITE,INVFNAME,INVLNAME,COUNTRY,
         INVID,SITE_JOIN,DMDTC_RAW)

#DM Domain 
dm <- dm_1 %>%
  mutate(
    STUDYID = STUDY_ID,
    DOMAIN = "DM",
    USUBJID = paste(PROJECT,SUBJECT,sep = "-"),
    SUBJID = SUBJECT,
    RFSTDTC = format(RFSTDTC_RAW, "%Y-%m-%d"),
    RFENDTC = format(DSSTDAT, "%Y-%m-%d"),
    RFXSTDTC = format(RFSTDTC_RAW, "%Y-%m-%dT%H:%M"),
    RFXENDTC = case_when(
      is.na(RFXENDTC_RAW) ~ "",
      format(RFXENDTC_RAW, "%H:%M") == "00:00" ~ 
        format(as.Date(RFXENDTC_RAW), "%Y-%m-%d"),
      TRUE ~ format(RFXENDTC_RAW, "%Y-%m-%dT%H:%M")
    ),
    RFICDTC = format(CNSTDTN, "%Y-%m-%d"),
    RFPENDTC = format(DSSTDAT, "%Y-%m-%d"),
    DTHDTC = if_else(!is.na(DTHDTC_RAW),
                     format(as.Date(DTHDTC_RAW), "%Y-%m-%d"),""),
    DTHFL  = if_else(DTHDTC != "", "Y", ""),
    SITEID = trimws(sub("-.*", "",SITE)),
    INVID = INVID,
    INVNAM = paste(INVFNAME,INVLNAME,sep = " "),
    BRTHDTC = format(BRTHDTN, "%Y-%m-%d"),
    AGE = floor(as.numeric(as.Date(RFICDTC) - as.Date(BRTHDTC)) / 365.25),
    AGEU = "YEARS",
    SEX = case_when(
      SEX == "Male"   ~ "M",
      SEX == "Female" ~ "F",
      TRUE ~ "U"
      ),
    RACE = case_when(
      RACE == "White" ~ "WHITE",
      RACE == "Black Or African American" ~ "BLACK OR AFRICAN AMERICAN",
      RACE == "Asian" ~ "ASIAN",
      TRUE ~ "OTHER"
      ),
    ETHNIC = case_when(
      ETHNIC == "Not Hispanic or Latino" ~ "NOT HISPANIC OR LATINO",
      ETHNIC == "Hispanic or Latino" ~ "HISPANIC OR LATINO",
      TRUE ~ "UNKNOWN"
      ),
    ARMCD = case_when(
      ENRGRP == "Group 1" ~ TREATMENT_CODE,
      TRUE ~ NA_character_
    ),
    ARM = ENRGRP,
    ACTARMCD = case_when(
      !is.na(RFSTDTC_RAW) ~ TREATMENT_CODE,
      TRUE ~ NA_character_
    ),
    ACTARM = case_when(
      ACTARMCD == TREATMENT_CODE ~ "Group 1",
      TRUE ~ NA_character_
    ), 
    COUNTRY = COUNTRY,
    DMDTC = format(DMDTC_RAW, "%Y-%m-%d"),
    DMDY = case_when(
      as.Date(DMDTC) >= as.Date(RFSTDTC) ~ 
        as.numeric(as.Date(DMDTC) - as.Date(RFSTDTC)) + 1,
      as.Date(DMDTC) <  as.Date(RFSTDTC) ~ 
        as.numeric(as.Date(DMDTC) - as.Date(RFSTDTC))
      )
    )%>%
    select(STUDYID, DOMAIN, SUBJID, RFSTDTC, RFENDTC, RFXSTDTC, RFXENDTC, 
           RFICDTC, RFPENDTC, DTHDTC, DTHFL, SITEID, INVID, INVNAM, 
           BRTHDTC, AGE, AGEU, SEX, RACE, ETHNIC, ARM, ACTARM, 
           COUNTRY, DMDTC, DMDY, USUBJID, ACTARMCD, ARMCD)
dm <- dm %>% arrange(USUBJID)

attr(dm, "label") <- NULL

dm_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  SUBJID = "Subject Identifier for the Study",
  RFSTDTC = "Subject Reference Start Date/Time",
  RFENDTC = "Subject Reference End Date/Time",
  RFXSTDTC = "Date/Time of First Study Treatment",
  RFXENDTC = "Date/Time of Last Study Treatment",
  RFICDTC = "Date/Time of Informed Consent",
  RFPENDTC = "Date/Time of End of Participation",
  DTHDTC = "Date/Time of Death",
  DTHFL = "Subject Death Flag",
  SITEID = "Study Site Identifier",
  INVID = "Investigator Identifier",
  INVNAM = "Investigator Name",
  BRTHDTC = "Date/Time of Birth",
  AGE = "Age",
  AGEU = "Age Units",
  SEX = "Sex",
  RACE = "Race",
  ETHNIC = "Ethnicity",
  ARM = "Description of Planned Arm",
  ACTARM = "Description of Actual Arm",
  COUNTRY = "Country",
  DMDTC = "Date/Time of Collection",
  DMDY = "Study Day of Collection",
  ACTARMCD = "Actual Arm Code",
  ARMCD = "Planned Arm Code"
)

for (nm in names(dm_labels)) if (nm %in% names(dm)) attr(dm[[nm]], "label") <- dm_labels[[nm]]
if ("USUBJID" %in% names(dm)) attr(dm$USUBJID, "label") <- NULL

View(dm)
print(dm)

################################################################################
############################ DIPOSITION DOMAIN #################################
################################################################################

#Getting Informed Consent from enrollment
enr <- raw_enr %>%
  mutate(
    STUDYID = STUDY_ID,
    DOMAIN = "DS",
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    DSTERM = "INFORMED CONSENT OBTAINED",
    DSDECOD = "INFORMED CONSENT OBTAINED",
    DSCAT = "PROTOCOL MILESTONE",
    DSSCAT = "INFORMED CONSENT",
    EPOCH = "",
    DSSTDTC = format(CNSTDTN, "%Y-%m-%d")
  )

dsdd_1 <- raw_dsdd %>%
  mutate(
    STUDYID = STUDY_ID,
    DOMAIN = "DS",
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    DSTERM = toupper(DSDECOD),
    DSDECOD = toupper(DSDECOD),
    DSCAT = "DISPOSITION EVENT",
    DSSCAT = "STUDY DRUG DISCONTINUATION",
    DSSTDTC = format(as.Date(DSSTDTN), "%Y-%m-%d")
  )

ds_1 <- raw_ds %>%
  mutate(
    STUDYID = STUDY_ID,
    DOMAIN = "DS",
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    DSTERM = case_when(
      DSDECOD_COD == "Completed" ~ "COMPLETED",
      DSDECOD_COD != "Completed" & DSDECOD == "OTHER" ~ paste0("OTHER: ", DSDECDOT),
      DSDECOD_COD != "Completed" ~ DSDECOD,
      TRUE ~ ""
    ),
    DSDECOD = toupper(DSTERM),
    DSCAT = "DISPOSITION EVENT",
    DSSCAT = "END OF STUDY",
    DSSTDTC = format(as.Date(DSSTDAT), "%Y-%m-%d")
  )

ds_epoch <- ds_1 %>%
  left_join(
    raw_se %>% select(USUBJID, EPOCH, SESTDTC, SEENDTC),
    by = "USUBJID"
  ) %>%
  mutate(
    EPOCH = case_when(
      as.Date(DSSTDTC) >= as.Date(SESTDTC) &
        as.Date(DSSTDTC) <= as.Date(SEENDTC) ~ EPOCH,
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(USUBJID, DSSTDTC, DSTERM, STUDYID, DOMAIN, DSDECOD, DSCAT, DSSCAT) %>%
  summarise(
    EPOCH = if (all(is.na(EPOCH))) NA_character_ else first(na.omit(EPOCH)),
    .groups = "drop"
  )

dsdd_epoch <- dsdd_1 %>%
  left_join(
    raw_se %>% select(USUBJID, EPOCH, SESTDTC, SEENDTC),
    by = "USUBJID"
  ) %>%
  mutate(
    EPOCH = case_when(
      as.Date(DSSTDTC) >= as.Date(SESTDTC) &
        as.Date(DSSTDTC) <= as.Date(SEENDTC) ~ EPOCH,
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(USUBJID, DSSTDTC, DSTERM, STUDYID, DOMAIN, DSDECOD, DSCAT, DSSCAT) %>%
  summarise(
    EPOCH = if (all(is.na(EPOCH))) NA_character_ else first(na.omit(EPOCH)),
    .groups = "drop"
  )

ds_combined <- bind_rows(enr, ds_epoch, dsdd_epoch)

ds <- ds_combined %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(
    DSSTDY = case_when(
      as.Date(DSSTDTC) >= as.Date(RFSTDTC) ~ as.numeric(as.Date(DSSTDTC) - as.Date(RFSTDTC)) + 1,
      as.Date(DSSTDTC) < as.Date(RFSTDTC) ~ as.numeric(as.Date(DSSTDTC) - as.Date(RFSTDTC)),
      TRUE ~ NA_real_
    ),
    ds_order = case_when(
      DSCAT == "DISPOSITION EVENT" ~ 1,
      DSCAT == "PROTOCOL MILESTONE" ~ 2,
      TRUE ~ 99
    )
  ) %>%
  arrange(USUBJID, ds_order, as.Date(DSSTDTC)) %>%
  group_by(USUBJID) %>%
  mutate(DSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, DSSEQ, DSTERM, DSDECOD, DSCAT, DSSCAT, EPOCH, DSSTDTC, DSSTDY, USUBJID)

# Study-specific subject overrides were intentionally omitted from this
# portfolio version. Such corrections should live in a controlled metadata file.
ds <- ds %>% arrange(USUBJID, DSSEQ)

attr(ds, "label") <- NULL

ds_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  DSSEQ = "Sequence Number",
  DSTERM = "Reported Term for the Disposition Event",
  DSDECOD = "Standardized Disposition Term",
  DSCAT = "Category for Disposition Event",
  DSSCAT = "Subcategory for Disposition Event",
  EPOCH = "Epoch",
  DSSTDTC = "Start Date/Time of Disposition Event",
  DSSTDY = "Study Day of Start of Disposition Event"
)

for (nm in names(ds_labels)) if (nm %in% names(ds)) attr(ds[[nm]], "label") <- ds_labels[[nm]]
if ("USUBJID" %in% names(ds)) attr(ds$USUBJID, "label") <- NULL

View(ds)
print(ds)

################################################################################
############################# EXPOSURE DOMAIN ##################################
################################################################################

#ASK question for spec for EXDOSE 
ex <- raw_ex %>%
  mutate(
    row_id = row_number(),
    STUDYID = STUDY_ID,
    DOMAIN = "EX",
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    EXTRT = TREATMENT_CODE,
    EXDOSE = EXDOSE,
    EXDOSU = "mg",
    EXDOSFRM = "INJECTION",
    EXROUTE = "SUBCUTANEOUS",
    EXLOC = if_else(toupper(EXLOC) == "OTHER", toupper(EXLOCOTH), toupper(EXLOC)),
    ex_date = if_else(!is.na(EXSTDTN), format(as.Date(EXSTDTN), "%Y-%m-%d"), NA_character_),
    ex_time = case_when(
      is.na(EXSTTM) | EXSTTM == "" ~ NA_character_,
      nchar(EXSTTM) == 4 ~ paste0("0", EXSTTM),
      TRUE ~ EXSTTM
    ),
    EXSTDTC = case_when(
      !is.na(ex_date) & !is.na(ex_time) ~ paste0(ex_date, "T", ex_time),
      !is.na(ex_date) ~ ex_date,
      TRUE ~ NA_character_
    ),
    EXSTDT = as.Date(ex_date)
  ) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID") %>%
  mutate(
    EXSTDY = case_when(
      is.na(EXSTDT) | is.na(as.Date(RFSTDTC)) ~ NA_real_,
      EXSTDT >= as.Date(RFSTDTC) ~ as.numeric(EXSTDT - as.Date(RFSTDTC)) + 1,
      EXSTDT < as.Date(RFSTDTC) ~ as.numeric(EXSTDT - as.Date(RFSTDTC))
    )
  )

ex_epoch <- ex %>%
  select(row_id, USUBJID, EXSTDT) %>%
  left_join(
    raw_se %>%
      select(USUBJID, EPOCH, SESTDTC, SEENDTC) %>%
      mutate(
        SESTDT = as.Date(substr(SESTDTC, 1, 10)),
        SEENDT = as.Date(substr(SEENDTC, 1, 10))
      ) %>%
      select(USUBJID, EPOCH, SESTDT, SEENDT),
    by = "USUBJID"
  ) %>%
  mutate(
    epoch_match = !is.na(EXSTDT) &
      !is.na(SESTDT) &
      EXSTDT >= SESTDT &
      (is.na(SEENDT) | EXSTDT <= SEENDT)
  ) %>%
  filter(epoch_match) %>%
  group_by(row_id) %>%
  slice_max(order_by = SESTDT, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(row_id, EPOCH)

ex <- ex %>%
  select(-any_of("EPOCH")) %>%
  left_join(ex_epoch, by = "row_id") %>%
  mutate(
   
     EPOCH = coalesce(EPOCH, "")
  ) %>%
  arrange(USUBJID, EXSTDT) %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(
    STUDYID, DOMAIN, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM,
    EXROUTE, EXLOC, EPOCH, EXSTDTC, EXSTDY, USUBJID
  )

ex <- ex %>%
  arrange(USUBJID, EXSEQ)

attr(ex, "label") <- NULL

ex_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  EXSEQ = "Sequence Number",
  EXTRT = "Name of Actual Treatment",
  EXDOSE = "Dose per Administration",
  EXDOSU = "Dose Units",
  EXDOSFRM = "Dose Form",
  EXROUTE = "Route of Administration",
  EXLOC = "Location of Dose Administration",
  EPOCH = "Epoch",
  EXSTDTC = "Start Date/Time of Treatment",
  EXSTDY = "Study Day of Start of Treatment"
)

for (nm in names(ex_labels)) if (nm %in% names(ex)) attr(ex[[nm]], "label") <- ex_labels[[nm]]
if ("USUBJID" %in% names(ex)) attr(ex$USUBJID, "label") <- NULL

View(ex)
print(ex)

################################################################################
########################### MEDICAL HISTROY DOMAIN #############################
################################################################################

#ASK question about foldername 
mh <- raw_mh %>%
  mutate(
         STUDYID = STUDY_ID,
         DOMAIN = "MH",
         USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
         MHTERM = (MHTERM),
         MHLLT = MHTERM_LLT,
         MHLLTCD = as.numeric(MHTERM_LLT_CODE),
         MHDECOD = MHTERM_PT,
         MHPTCD = as.numeric(MHTERM_PT_CODE),
         MHHLT = MHTERM_HLT,
         MHHLTCD = as.numeric(MHTERM_HLT_CODE),
         MHHLGT = MHTERM_HLGT,
         MHHLGTCD = as.numeric(MHTERM_HLGT_CODE),
         MHCAT = toupper("General medical history"),
         MHBODSYS = MHTERM_SOC,
         MHBDSYCD = as.numeric(MHTERM_SOC_CODE),
         MHSOC = MHTERM_SOC,
         MHSOCCD = as.numeric(MHTERM_SOC_CODE)
  )%>%
  left_join(raw_dov %>%
            filter(INSTANCENAME == "Screening") %>%
            mutate(USUBJID = paste(PROJECT, SUBJECT, sep = "-")) %>%
            select(USUBJID, VISDTN),by = "USUBJID"
  ) %>%
  left_join(dm %>% select(USUBJID, RFSTDTC), by = "USUBJID")%>%
  mutate(
      MHDTC = format(as.Date(VISDTN), "%Y-%m-%d"),
      mm   = ifelse(!is.na(MHSTDTN_MM), sprintf("%02d", as.numeric(MHSTDTN_MM)), NA),
      dd   = ifelse(!is.na(MHSTDTN_DD), sprintf("%02d", as.numeric(MHSTDTN_DD)), NA),
      MHSTDTC = case_when(
        !is.na(MHSTDTN_YY) & !is.na(MHSTDTN_MM) & !is.na(MHSTDTN_DD) ~ paste(as.character(MHSTDTN_YY), mm, dd, sep = "-"),
        !is.na(MHSTDTN_YY) & !is.na(MHSTDTN_MM)               ~ paste(as.character(MHSTDTN_YY), mm, sep = "-"),   
        !is.na(MHSTDTN_YY)                                    ~ as.character(MHSTDTN_YY),
        TRUE                                                  ~ ""
      ),
      mm_1   = ifelse(!is.na(MHENDTN_MM), sprintf("%02d", as.numeric(MHENDTN_MM)), NA),
      dd_1   = ifelse(!is.na(MHENDTN_DD), sprintf("%02d", as.numeric(MHENDTN_DD)), NA),
      MHENDTC = case_when(
        !is.na(MHENDTN_YY) & !is.na(MHENDTN_MM) & !is.na(MHENDTN_DD) ~ paste(as.character(MHENDTN_YY), mm_1, dd_1, sep = "-"),
        !is.na(MHENDTN_YY) & !is.na(MHENDTN_MM)               ~ paste(as.character(MHENDTN_YY), mm_1, sep = "-"),   
        !is.na(MHENDTN_YY)                                    ~ as.character(MHENDTN_YY),
        TRUE                                                  ~ ""
    ),
    MHDY = case_when(
      is.na(as.Date(VISDTN)) | is.na(as.Date(RFSTDTC)) ~ as.numeric(NA),
      as.Date(VISDTN) >= as.Date(RFSTDTC) ~ as.numeric(as.Date(VISDTN) - as.Date(RFSTDTC)) + 1,
      as.Date(VISDTN) < as.Date(RFSTDTC)  ~ as.numeric(as.Date(VISDTN) - as.Date(RFSTDTC))
    ),
    MHENRTPT = case_when(
      MHONG == 1 ~ "ONGOING",
      MHONG == 0 ~ ""
    ),
    MHENTPT = ifelse(MHENRTPT == "ONGOING",as.character(VISDTN),"")
  ) %>%
  arrange(USUBJID, MHDTC, MHTERM) %>%
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID,DOMAIN,MHSEQ,MHTERM,MHLLT,MHLLTCD,MHDECOD,MHPTCD,MHHLT,MHHLTCD,
         MHHLGT,MHHLGTCD, MHCAT,MHBODSYS,MHBDSYCD,MHSOC,MHSOCCD,MHDTC,MHSTDTC,
         MHENDTC,MHDY,MHENRTPT,MHENTPT,USUBJID)
mh <- mh %>%
  mutate(across(where(is.character), trimws)) %>%
  
  # 2. Sort by the standard SDTM keys
  # We add MHSTDTC to ensure records for the same subject are in date order
  arrange(STUDYID, USUBJID, MHTERM, MHSTDTC) %>%
  
  # 3. Recalculate the Sequence number based on the new sort
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup()

attr(mh, "label") <- NULL

mh_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  MHSEQ = "Sequence Number",
  MHTERM = "Reported Term for the Medical History",
  MHLLT = "Lowest Level Term",
  MHLLTCD = "Lowest Level Term Code",
  MHDECOD = "Dictionary-Derived Term",
  MHPTCD = "Preferred Term Code",
  MHHLT = "High Level Term",
  MHHLTCD = "High Level Term Code",
  MHHLGT = "High Level Group Term",
  MHHLGTCD = "High Level Group Term Code",
  MHCAT = "Category for Medical History",
  MHBODSYS = "Body System or Organ Class",
  MHBDSYCD = "Body System or Organ Class Code",
  MHSOC = "Primary System Organ Class",
  MHSOCCD = "Primary System Organ Class Code",
  MHDTC = "Date/Time of History Collection",
  MHSTDTC = "Start Date/Time of Medical History Event",
  MHENDTC = "End Date/Time of Medical History Event",
  MHDY = "Study Day of History Collection",
  MHENRTPT = "End Relative to Reference Time Point",
  MHENTPT = "End Reference Time Point"
)

for (nm in names(mh_labels)) if (nm %in% names(mh)) attr(mh[[nm]], "label") <- mh_labels[[nm]]
if ("USUBJID" %in% names(mh)) attr(mh$USUBJID, "label") <- NULL

View(mh)
print(mh)

################################################################################
############################## VITAL STATS #####################################
################################################################################
ex_first <- ex %>%
  mutate(EXSTDT = as.Date(substr(EXSTDTC, 1, 10))) %>%
  group_by(USUBJID) %>%
  summarise(
    EXSTDT = if (all(is.na(EXSTDT))) as.Date(NA) else min(EXSTDT, na.rm = TRUE),
    .groups = "drop"
  )

visit_ref <- raw_dov %>%
  mutate(
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    VISITNUM_REF = case_when(
      FOLDER == "SCREENING" ~ -1,
      str_detect(FOLDER, "^WEEK\\d+$") ~ as.numeric(str_extract(FOLDER, "\\d+")) * 7 - 6,
      TRUE ~ NA_real_
    ),
    VISIT_REF = case_when(
      FOLDER == "SCREENING" ~ "Screening",
      str_detect(FOLDER, "^WEEK\\d+$") ~ INSTANCENAME,
      TRUE ~ INSTANCENAME
    ),
    VISDT = as.Date(VISDTN)
  ) %>%
  select(USUBJID, FOLDER, INSTANCENAME, VISDT, VISITNUM_REF, VISIT_REF) %>%
  distinct()

visit_sched <- visit_ref %>%
  filter(FOLDER == "SCREENING" | str_detect(FOLDER, "^WEEK\\d+$")) %>%
  transmute(
    USUBJID,
    ANCHOR_VISDT = VISDT,
    ANCHOR_VISITNUM = VISITNUM_REF,
    ANCHOR_VISIT = VISIT_REF
  )

vs_base <- raw_vs %>%
  mutate(
    row_id = row_number(),
    STUDYID = STUDY_ID,
    DOMAIN = "VS",
    USUBJID = paste(PROJECT, SUBJECT, sep = "-"),
    
    VSTESTCD = case_when(
      VSTEST == "Temperature" ~ "TEMP",
      VSTEST == "Respiratory rate" ~ "RESP",
      VSTEST == "Heart rate" ~ "HR",
      VSTEST == "Systolic blood pressure" ~ "SYSBP",
      VSTEST == "Diastolic blood pressure" ~ "DIABP",
      VSTEST == "Weight" ~ "WEIGHT",
      VSTEST == "Height" ~ "HEIGHT"
    ),
    
    VSTEST = case_when(
      VSTEST == "Temperature" ~ "Temperature",
      VSTEST == "Respiratory rate" ~ "Respiratory Rate",
      VSTEST == "Heart rate" ~ "Heart Rate",
      VSTEST == "Systolic blood pressure" ~ "Systolic Blood Pressure",
      VSTEST == "Diastolic blood pressure" ~ "Diastolic Blood Pressure",
      VSTEST == "Weight" ~ "Weight",
      VSTEST == "Height" ~ "Height"
    ),
    
    VSCAT = DATAPAGENAME,
    
    VSORRES_NUM = suppressWarnings(as.numeric(VSORRES)),
    VSORRES = case_when(
      is.na(VSORRES_NUM) ~ ".",
      TRUE ~ as.character(floor(VSORRES_NUM + 0.5))
    ),
    VSSTRESC = VSORRES,
    VSSTRESN = case_when(
      is.na(VSORRES_NUM) ~ NA_real_,
      TRUE ~ floor(VSORRES_NUM + 0.5) / 10
    ),
    
    VSSTRESU = case_when(
      VSTESTCD == "TEMP" ~ "C",
      VSTESTCD == "RESP" ~ "BREATHS/MIN",
      VSTESTCD == "HR" ~ "BEATS/MIN",
      VSTESTCD == "SYSBP" ~ "mmHg",
      VSTESTCD == "DIABP" ~ "mmHg",
      VSTESTCD %in% c("WEIGHT", "HEIGHT") ~ "",
      TRUE ~ ""
    ),
    VSORRESU = VSSTRESU,
    
    VSSTAT = case_when(
      coalesce(VSORRES_RAW, "") %in% c("X", "U") ~ "NOT DONE",
      TRUE ~ ""
    ),
    
    RAW_VSDTC = case_when(
      !is.na(VSDTN) ~ as.Date(VSDTN),
      !is.na(RECORDDATE) ~ as.Date(RECORDDATE),
      TRUE ~ as.Date(NA)
    )
  )

visit_anchor <- vs_base %>%
  filter(FOLDER %in% c("SRV", "UNS"), !is.na(RAW_VSDTC)) %>%
  select(row_id, USUBJID, RAW_VSDTC) %>%
  left_join(visit_sched, by = "USUBJID") %>%
  filter(!is.na(ANCHOR_VISDT), ANCHOR_VISDT <= RAW_VSDTC) %>%
  group_by(row_id) %>%
  slice_max(order_by = ANCHOR_VISDT, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(row_id, ANCHOR_VISDT, ANCHOR_VISITNUM, ANCHOR_VISIT)

vs <- vs_base %>%
  left_join(ex_first, by = "USUBJID") %>%
  left_join(visit_ref, by = c("USUBJID", "FOLDER", "INSTANCENAME")) %>%
  left_join(visit_anchor, by = "row_id") %>%
  mutate(
    VSDTC = case_when(
      FOLDER == "SCREENING" & !is.na(VISDT) ~ VISDT,
      str_detect(FOLDER, "^WEEK\\d+$") & !is.na(VISDT) ~ VISDT,
      TRUE ~ RAW_VSDTC
    ),
    VSDY = case_when(
      !is.na(VSDTC) & !is.na(EXSTDT) & VSDTC >= EXSTDT ~ as.integer(VSDTC - EXSTDT + 1),
      !is.na(VSDTC) & !is.na(EXSTDT) & VSDTC < EXSTDT ~ as.integer(VSDTC - EXSTDT),
      TRUE ~ NA_integer_
    ),
    VISITNUM = case_when(
      FOLDER == "SCREENING" ~ -1,
      str_detect(FOLDER, "^WEEK\\d+$") & !is.na(VISITNUM_REF) ~ VISITNUM_REF,
      str_detect(FOLDER, "^WEEK\\d+$") ~ as.numeric(str_extract(FOLDER, "\\d+")) * 7 - 6,
      FOLDER == "SRV" & !is.na(ANCHOR_VISITNUM) ~ ANCHOR_VISITNUM + 0.01,
      FOLDER == "UNS" & !is.na(ANCHOR_VISITNUM) ~ ANCHOR_VISITNUM + 0.01,
      FOLDER %in% c("SRV", "UNS") & !is.na(VSDY) ~ pmax(1, floor((VSDY - 1) / 7) * 7 + 1) + 0.01,
      TRUE ~ NA_real_
    ),
    VISIT = case_when(
      FOLDER == "SCREENING" ~ "Screening",
      str_detect(FOLDER, "^WEEK\\d+$") & !is.na(VISIT_REF) ~ VISIT_REF,
      str_detect(FOLDER, "^WEEK\\d+$") ~ INSTANCENAME,
      FOLDER == "SRV" ~ paste0("Systemic-", sprintf("%.2f", VISITNUM)),
      FOLDER == "UNS" ~ paste0("Unsched-", sprintf("%.2f", VISITNUM)),
      TRUE ~ INSTANCENAME
    )
  ) %>%
  group_by(USUBJID, VSTESTCD) %>%
  mutate(
    BASE_REC = case_when(
      VSCAT == "Vital Signs Day 1" & !is.na(EXSTDT) & VSDTC <= EXSTDT ~ VSDTC,
      TRUE ~ as.Date(NA)
    ),
    LAST_BASE = if (all(is.na(BASE_REC))) as.Date(NA) else max(BASE_REC, na.rm = TRUE),
    VSBLFL = case_when(
      !is.na(LAST_BASE) & VSDTC == LAST_BASE ~ "Y",
      TRUE ~ ""
    )
  ) %>%
  ungroup()

vs <- vs %>%
  left_join(
    raw_se %>% select(USUBJID, EPOCH, SESTDTC, SEENDTC),
    by = "USUBJID"
  ) %>%
  mutate(
    match_epoch = case_when(
      VSDTC >= as.Date(SESTDTC) & VSDTC <= as.Date(SEENDTC) ~ EPOCH,
      TRUE ~ NA_character_
    ),
    match_start = case_when(
      VSDTC >= as.Date(SESTDTC) & VSDTC <= as.Date(SEENDTC) ~ as.Date(SESTDTC),
      TRUE ~ as.Date(NA)
    )
  ) %>%
  group_by(USUBJID, VSDTC, VSTESTCD, VISITNUM, VISIT, VSCAT, VSORRES, VSSTAT) %>%
  summarise(
    STUDYID = first(STUDYID),
    DOMAIN = first(DOMAIN),
    VSTEST = first(VSTEST),
    VSCAT = first(VSCAT),
    VSORRES = first(VSORRES),
    VSORRESU = first(VSORRESU),
    VSSTRESC = first(VSSTRESC),
    VSSTRESN = first(VSSTRESN),
    VSSTRESU = first(VSSTRESU),
    VSSTAT = first(VSSTAT),
    VSBLFL = first(VSBLFL),
    EPOCH = if (all(is.na(match_epoch))) "" else match_epoch[which.max(match_start)],
    VSDY = first(VSDY),
    .groups = "drop"
  )

vs <- vs %>%
  arrange(USUBJID, VSTESTCD, VISITNUM, VSDTC, VSCAT, VSORRES, VSSTAT) %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  mutate(VSDTC = format(VSDTC, "%Y-%m-%d")) %>%
  select(
    STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST, VSCAT,
    VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU,
    VSSTAT, VSBLFL, VISITNUM, VISIT, EPOCH, VSDTC, VSDY
  )

attr(vs, "label") <- NULL

vs_labels <- c(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  VSSEQ = "Sequence Number",
  VSTESTCD = "Vital Signs Test Short Name",
  VSTEST = "Vital Signs Test Name",
  VSCAT = "Category for Vital Signs",
  VSORRES = "Result or Finding in Original Units",
  VSORRESU = "Original Units",
  VSSTRESC = "Character Result/Finding in Std Format",
  VSSTRESN = "Numeric Result/Finding in Standard Units",
  VSSTRESU = "Standard Units",
  VSSTAT = "Completion Status",
  VSBLFL = "Baseline Flag",
  VISITNUM = "Visit Number",
  VISIT = "Visit Name",
  EPOCH = "Epoch",
  VSDTC = "Date/Time of Measurements",
  VSDY = "Study Day of Vital Signs"
)

for (nm in names(vs_labels)) if (nm %in% names(vs)) attr(vs[[nm]], "label") <- vs_labels[[nm]]

View(vs)

################################################################################
############################### Validation #####################################
################################################################################

dm_orr <- read_xpt(file.path(VALIDATION_DATA_DIR, "dm_gold.xpt"))
ds_orr <- read_xpt(file.path(VALIDATION_DATA_DIR, "ds.xpt"))
ex_orr <- read_xpt(file.path(VALIDATION_DATA_DIR, "ex.xpt"))
mh_orr <- read_xpt(file.path(VALIDATION_DATA_DIR, "mh.xpt"))
vs_orr <- read_xpt(file.path(VALIDATION_DATA_DIR, "vs.xpt"))

all.equal(dm_orr,dm)
all.equal(ds_orr,ds)
all.equal(ex_orr,ex)
all.equal(mh_orr,mh)
all.equal(vs_orr,vs)

################################################################################
########################## Exporting Files #####################################
################################################################################

outdir <- OUTPUT_DIR
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

write_xpt(dm, file.path(outdir, "dm.xpt"), version = 5)
write_xpt(ds, file.path(outdir, "ds.xpt"), version = 5)
write_xpt(ex, file.path(outdir, "ex.xpt"), version = 5)
write_xpt(mh, file.path(outdir, "mh.xpt"), version = 5)
write_xpt(vs, file.path(outdir, "vs.xpt"), version = 5)

