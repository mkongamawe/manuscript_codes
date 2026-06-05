# 0. SECTIONS 
------------- 
1.Project

2.Dataset

3.Terms of Use/Data Access

4.Contents 

5.Method and Processing 

6.Related Dataset(s)

7.Related Publication(s)


# 1. PROJECT 
------------  

Title:  Improving Hypertension Control in Rural sub-Saharan Africa (IHCoR Africa)

Dates: 2023 - 2025

SSC/Protocol No:

Description of the related research project:

Funding organisation:  Wellcome (22713/Z/23/Z, 103951/Z/14/Z), Kenya Medical Research Institute (KEMRI/GRG/15/09), National Institute of Health and Care Research (NIHR 134544), Science for Africa Foundation DELTAS Africa programme (DEL-22-012).

Grant no.:  
 


# 2. DATASET 
------------  
Publication/Manuscript/Protocol Title: Prognostic Value of Clinic and Ambulatory Blood Pressure Measurements in a Rural African Population

Description: 
This retrospective population-based cohort study enrolled 1,884 adults (median age 45 years [IQR 24–59]; 56% women) from the Kilifi Health and Demographic Surveillance System (KHDSS) in rural coastal Kenya. Participants underwent both clinic and 24-hour ambulatory blood pressure monitoring (ABPM) between 2016–2018 and were followed for a median of 6.8 years (IQR 6.0–7.1) through 31 December 2023. During follow-up, 118 deaths (5.2%) and 34 cardiovascular events occurred.

The primary outcome was all-cause mortality; the secondary outcome was a composite of cardiovascular admission and cardiovascular death. 

Publication Year:

Authors(s)information: Name/Institution/ORCID/Email address <including data manager>

Contact information:
- Clement Mwagwabi: cmwagwabi@kemri-wellcome.org
- Anthony O. Etyang: aetyang@kemri-wellcome.org

Source(s): NA

Subject: Medicine

Keywords: Hypertension, Ambulatory blood pressure monitoring


# 3. TERMS OF USE/DATA ACCESS 
------------------------------  
The data is open access with access granted by the data governance committee of Kemri-Wellcome Trust upon reasonable request.

Files will be uploaded under Creative Commons CC By 4.0 License.


# 4. CONTENTS 
------------ 
```
manuscript_codes/
│
├── README.md                                    ← This file
│
├── data/
│   ├── manuscript_data.csv                      ← Analysis-ready dataset (N = 1,884)
│   └── manuscript_codebook.xlsx                 ← Variable codebook with definitions
├── results/                                     ← Generated tables and figures
│   ├── tables/
│   └── figures/
│
└── scripts/
    ├── 00_master.r                              ← Master script — runs full pipeline
    │
    │  ── DATA PREPARATION ──
    ├── 01_residual_calc.r                       ← Residualized BP + z-scores
    │
    │  ── DESCRIPTIVE ANALYSES ──
    ├── 02_table1.r                              ← Baseline characteristics (Table 1)
    ├── 03_dropped_participants.r                ← Excluded participants (Table S1a)
    ├── 04_correlation.r                         ← Correlation of BPs (Figure S2-S3)
    │
    │  ── BP CLASSIFICATION & SURVIVAL ──
    ├── 05_rates_nelson_aalen.r                  ← Nelson Aalen survival curves (Figure 1)
    ├── 06_prognostic_sensitivity_table_graph.r  ← Sensitivity/Specificity (Figure 2A)
    ├── 07_bp_kernel_density.r                   ← Blood Pressure Density (Figure 2B)
    ├── 08_mortality_proportions.R               ← Mortality proportions by BP category
    ├── 09_rates_forest_plot.r                   ← Rates by BP category (Figs S4–S5)
    │
    │  ── COX REGRESSION (PRIMARY) ──
    ├── 10_forest_plot_per_half_SD_increment.r   ← Cox per ½ SD increment (Figure 3, Fig S7)
    ├── 11_forest_plots_10_5.r                   ← Residualized per 10/5 mmHg (Fig S12)
    ├── 12_forest_plot_1year_sens.r              ← Excl. first-year deaths (Fig S11)
    │
    │  ── SUBGROUP & SENSITIVITY ──
    ├── 13_forest_plot_sens_age_sex_weight.r     ← Age/sex/BMI subgroups (Figs S8–S10)
    ├── 14_roc_curve.r"                          ← Blood pressure coefficients (Table S5)
    ├── 15_bp_measure_interaction.r              ← BP × measurement interaction (Table S3)
    │
    │  ── SUPPLEMENTARY ──
    ├── 16_poisson_regression.r                  ← Poisson IRR models
    └── 17_age_at_death.r                        ← Age-at-death distribution


``` 


# 5. METHOD and PROCESSING 
--------------------------  
## Title of manuscript
Prognostic Value of Clinic and Ambulatory Blood Pressure Measurements for All-cause Mortality in Rural Kenya: A Population based cohort study

## Authors:
- Nyiro C. Mwagwabi, B.Sc
- Ruth Lucinde, MD, MMed
- Juliet Otieno, MD
- Aurelia Brazeal, MD
- Elminah Saru, B.Sc
- Catherine Kalu, B.Sc
- Robinson Oyando, M.Sc
- Nadia Aliyan, MD
- Tim Clayton, M.Sc
- Modou Jobe, MD, PhD
- Rob Peck, MD, PhD
- Sam Kinyanjui, PhD
- Osman Abdullahi, PhD
- Antipa Sigilai, B.Sc
- Nelson Ouma, B.Sc
- David Walumbe, MPH
- Mark Otiende, M.Sc
- Amek Nyaguara, PhD
- J. Anthony G. Scott, FRCP, FMedSci
- Thomas N. Williams, FMEdSci
- Benjamin Tsofa, PhD
- Sophie Uyoga, PhD
- Edwine Barasa, PhD
- Elijah Ogola, MD5
- David Leon, PhD
- Alexander Perkins M.Sc
- Adrianna Murphy, PhD
- Emily Herrett, PhD
- Anoop Shah, MD, PhD
- Pablo Perel, MD, PhD
- Anthony O. Etyang, MD, PhD

## Status
In preparation

## Data Preprocessing Pipeline (Not Included)

The analysis-ready dataset was generated from multiple raw source files through scripts that are **not included** in this package because they require raw identifiable KHDSS data. For reference, the preprocessing pipeline was:

1. `shinda2_participants.r` — Loaded and cleaned Shinda 2 study baseline data (attended BP measurement)
2. `nahenda_part_manuscript.r` — Loaded and cleaned NAHENDA study baseline data (unattended BP measurement)
3. `project_master_manuscript.r` — Merged both cohorts, performed imputation, created hypertension phenotypes
4. `events_data_append_manuscript.r` — Linked follow-up events (hospitalisations, verbal autopsy, mortality)

The output of this pipeline is `manuscript_data.csv`.

## How to Run

### Prerequisites
To load the required environment that maintains the project:
```{R}
install.packages(renv)
renv::restore()
```

### Running the Full Pipeline

```{bash}
cd manuscript_codes/
Rscript scripts/00_manuscript_master.r
```

Or within an R session:

```{R}
setwd("path/to/manuscript_codes")
source("scripts/00_manuscript_master.r")
```
---
## Script-to-Manuscript Mapping

| Script | Manuscript Element | Description |
|--------|-------------------|-------------|
| `01_residual_calc.r` | — | Computes residualized BP (clinic on 24h, and vice versa) and z-scores for all BP indices; required by Model 2 analyses |
| `02_table1.r` | Table 1, Tables S1a, S1b | Baseline characteristics overall and stratified by vital status; excluded participant comparison |
| `03_dropped_participants.r` | Table S1a, Figure S1 | Characterisation of participants excluded from analysis; flow chart numbers |
| `04_correlation.r` | Figure S2, S3 | The correlation coefficients of various blood pressure measurements before (Figure S2) and after (Figure S3) residualization |
| `05_rates_nelson_aalen.r` | Figure 1 | Nelson-Aalen cumulative mortality by HTN category (ESC, ESH/ISH, ACC/AHA)|
| `06_prognostic_sensitivity_table_graph.r` | Figure 2 (Panel A), Table S7 | Prognostic sensitivity and specificity by BP index and guideline |
| `08_mortality_proportions.R` | Supports Figure 2 (Panel A) | Mortality proportions by BP category |
| `07_bp_kernel_density` | Figure 2 (Panel B) | Kernel Density plots of Clinic BP and Nighttime BP |
| `09_rates_forest_plot.r` | Figures S4, S5 | Crude and age-standardised mortality/CV event rates by BP category with forest plots |
| `10_forest_plot_per_half_SD_increment.r` | Figure 3, Figure S6, S7 | Primary Cox models: HR per ½ SD for all BP indices, Model 1 (confounder-adjusted) and Model 2 (residualized) |
| `11_forest_plots_10_5.r ` | Figure S12 | Residualized Cox models per 10 mmHg SBP / 5 mmHg DBP |
| `12_forest_plot_1year_sens.r ` | Figure S11 | Sensitivity analysis excluding first-year deaths |
| `13_forest_plot_sens_age_sex_weight.r` | Figures S8, S9, S10 | Stratified analyses by age (<45, 45–69, ≥70), sex, and BMI; interaction tests |
| `14_roc_curve.r` | Table S6 | ROC AUCs of the various BP measurements |
| `15_bp_measure_interaction.r` | Table S4 | BP × measurement method interaction test; clinic measurement method interaction |
| `16_bp_informativeness.r` | Table S3 | Relative informativeness of BP based on $\chi^2$ test |
---

## Variable Dictionary

The full codebook is provided in `data/manuscript_codebook.xlsx`. Key variables are summarised below.

### Identifiers and Demographics

| Variable | Description | Type |
|----------|-------------|------|
| `study_no` | Participant study number | character |
| `updated_time` | Date/time of enrollment | datetime |
| `dob` | Date of birth | date |
| `age` | Age at enrollment (years) | numeric |
| `agegrp` | Age group (5-year bands) | character |
| `agecat3` | Age category: 18–29, 30–59, 60+ | character |
| `agecat5` | Age category (5 groups) | character |
| `height` | Height (cm) | numeric |
| `weight` | Weight (kg) | integer |
| `bmi` | Body mass index (kg/m²) | numeric |

### Risk Factors

| Variable | Description | Type |
|----------|-------------|------|
| `on_med_htn` | On anti-hypertensive medication (Yes/No) | character |
| `current_smoke` | Current smoking status (Yes/No) | character |
| `prev_diag_htn` | Previous hypertension diagnosis | character |
| `a1c` | Glycosylated haemoglobin (HbA1c, %) | numeric |
| `a1c_imp` | Imputed HbA1c (age/sex/study regression slopes) | numeric |
| `diabetis_status` | Diabetes status based on HbA1c ≥ 6.5% | character |
| `sodium` | Urine sodium (mmol/L) | numeric |
| `potassium` | Urine potassium (mmol/L) | numeric |
| `sod_pot` | Urine sodium-to-potassium ratio | numeric |
| `sod_pot_imp` | Imputed Na/K ratio | numeric |

### Blood Pressure Measurements (mmHg)

| Variable | Description | Type |
|----------|-------------|------|
| `spot_bp_sys` | Clinic (office) systolic BP — average of 3 readings | numeric |
| `spot_bp_dys` | Clinic (office) diastolic BP — average of 3 readings | numeric |
| `idaco_day_sbpbr` | ABPM daytime systolic BP (1000–2000 hrs, time-weighted) | integer |
| `idaco_day_dia` | ABPM daytime diastolic BP | integer |
| `idaco_night_sbpbr` | ABPM nighttime systolic BP (0000–0600 hrs, time-weighted) | integer |
| `idaco_night_dia` | ABPM nighttime diastolic BP | integer |
| `idaco_sbpbr_avg` | 24-hour ABPM systolic BP (time-weighted average) | integer |
| `idaco_dia_avg` | 24-hour ABPM diastolic BP | integer |
| `idaco_good` | ABPM quality flag — meets IDACO criteria (≥10 daytime + ≥5 nighttime readings) | integer |

### Outcome Variables

| Variable | Description | Type |
|----------|-------------|------|
| `vital_status` | Status at end of follow-up: Alive / Dead / Outmigrated | character |
| `mortality_event` | All-cause mortality indicator (1 = died, 0 = censored) | integer |
| `time_to_event` | Follow-up time to death or censoring (days) | numeric |
| `cause1` | Cause of death (InterVA-4 verbal autopsy) | character |
| `icd10_va` | ICD-10 code for cause of death (verbal autopsy) | character |
| `death_date` | Date of death | date |
| `all_cardio_event` | Composite CVD event: CVD admission or CVD death (1/0) | integer |
| `time_to_all_cardio` | Follow-up to CVD event or censoring (days) | numeric |
| `cardio_admission` | Hospital admission for CVD (ICD-10 I00–I99) (1/0) | character |
| `cardio_death` | Death due to CVD cause (1/0) | character |
| `cardio_mortality_event` | CVD-specific mortality indicator (1/0) | integer |
| `admission_event` | Any hospital admission (1/0) | integer |
| `admission` | Whether participant was ever admitted (1=Yes, 2=No) | integer |
| `outmigration` | Whether participant outmigrated (1=Yes, NA=No) | integer |
| `date_obs` | Date of outmigration | date |
| `resp_event` | Mortality due to respiratory disease (1/0) | integer |
| `resp_death` | Date of death due to respiratory disease | date |
| `resp_admission` | Hospital admission date for respiratory illness (ICD-10 J00-J46) | date |
| `time_to_all_resp` | Follow-up to respiratory event or censoring (days) | numeric |
| `all_resp_event` | Composite respiratory event: respiratory admission or respiratory death (1/0) | integer |


### Hospital Admission Variables (up to 4 admissions)

| Variable pattern | Description |
|-----------------|-------------|
| `date_admn_{1-4}` | Date of admission |
| `disch_type_{1-4}` | Discharge type |
| `outcome_{1-4}` | Outcome of admission (alive/died) |
| `diagnosis_1_category_{1-4}` | Primary diagnosis category |
| `diagnosis_2_category_{1-4}` | Secondary diagnosis category |
| `icd10_{1-4}` | ICD-10 code for primary diagnosis at discharge |
| `icd10.2_{1-4}` | ICD-10 code for secondary diagnosis at discharge |
| `icd_category_{1-4}` | ICD category for primary diagnosis |
| `icd10_3characters_{1-4}` | 3-character ICD-10 code |

---

# 6. RELATED DATASET(S)
-------------------
NA


# 7. RELATED PUBLICATION(S)
-------------------------- 
In progress.


