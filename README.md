# SDTM Domain Programming in R

Portfolio project demonstrating an R workflow that transforms synthetic clinical-style source data into SDTM-like DM, DS, EX, MH, and VS domains.

## Highlights

- Derives subject-level timing, treatment, disposition, medical-history, and vital-sign variables.
- Assigns study days, visits, epochs, sequence numbers, controlled terminology, and variable labels.
- Exports SAS Transport (`.xpt`) files and compares derived domains with validation references.
- Keeps clinical datasets, derived outputs, local paths, and study-specific identifiers out of version control.

## Requirements

R packages: `haven`, `lubridate`, `tidyr`, `dplyr`, and `stringr`.

## Running the project

Place authorized synthetic input files under `data/raw` and validation files under `data/validation`, or define `RAW_DATA_DIR`, `VALIDATION_DATA_DIR`, and `OUTPUT_DIR`. Optional `STUDY_ID` and `TREATMENT_CODE` environment variables control the identifiers used by the script.

Run `PROJECT_2A.R` from the repository root. Generated transport files are written to `output/`, which is excluded from Git.

## Data notice

No source data is included. Do not add protected health information, sponsor-confidential data, production clinical datasets, investigator details, or real subject identifiers to this repository.

## Disclaimer

This is an educational portfolio example and is not intended for regulatory submission or production use.

