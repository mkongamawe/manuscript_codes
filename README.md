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

Dates: [project start-end] 

SSC/Protocol No:

Description of the related research project:

Funding organisation:  Wellcome (22713/Z/23/Z, 103951/Z/14/Z), Kenya Medical Research Institute (KEMRI/GRG/15/09), National Institute of Health and Care Research (NIHR 134544), Science for Africa Foundation DELTAS Africa programme (DEL-22-012).

Grant no.:  

[Include in this section aknowledgements of all relevant funding sources, including e.g. 
public and charitable funders, industrial sponsors, and the institution]  


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

Source(s): [If applicable, provide citations/links to data derived from other sources] 

Subject: Medicine, Health and Life Sciences/Social Sciences/Other-specify

Keywords:


# 3. TERMS OF USE/DATA ACCESS 
------------------------------  
The data is open access with access granted by the data governance committee of Kemri-Wellcome Trust upon reasonable request.

[By default, files will be uploaded under Creative Commons CC By 4.0 License. Indicate if you would like to use an alternate license or custom data use agreement.]


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
│
├── scripts/
│   ├── 00_master.r                              ← Master script — runs full pipeline
│   │
│   │  ── DATA PREPARATION ──
│   ├── 01_residual_calc.r                       ← Residualized BP + z-scores
│   │
│   │  ── DESCRIPTIVE ANALYSES ──
│   ├── 02_table1.r                              ← Baseline characteristics (Table 1)
│   ├── 03_dropped_participants.r                ← Excluded participants (Table S1a)
│   │
│   │  ── BP CLASSIFICATION & SURVIVAL ──
│   ├── 04_rates_nelson_aalen.r                  ← Nelson Aalen survival curves (Figure 1)
│   ├── 05_prognostic_sensitivity_table_graph.r  ← Sensitivity/Specificity (Figure 2A)
│   ├── 06_bp_kernel_density.r                   ← Blood Pressure Density (Figure 2B)
│   ├── 06_prognostic_sensitivity.R              ← Sensitivity/specificity (Figure 2)
│   ├── 07_mortality_proportions.R               ← Mortality proportions by BP category
│   ├── 08_kaplan_meier_curves.r                 ← KM survival curves (Figure 1)
│   ├── 09_rates_forest_plot.r                   ← Rates by BP category (Figs S3–S4)
│   │
│   │  ── COX REGRESSION (PRIMARY) ──
│   ├── 10_cox_per_half_SD.r                     ← Cox per ½ SD increment (Figure 3)
│   ├── 11_cox_residualized.r                    ← Residualized per 10/5 mmHg (Fig S11)
│   ├── 12_sensitivity_exclude_year1.r           ← Excl. first-year deaths (Fig S10)
│   ├── 13_cox_per_1mmHg.r                       ← Per 1 mmHg increment (Fig S12)
│   │
│   │  ── SUBGROUP & SENSITIVITY ──
│   ├── 14_subgroup_age_sex_bmi.r                ← Age/sex/BMI subgroups (Figs S7–S9)
│   ├── 15_additional_covariates.r               ← Covariate coefficients (Table S5)
│   ├── 16_bp_age_interaction.r                  ← BP × age interaction (Table S3)
│   │
│   │  ── SUPPLEMENTARY ──
│   ├── 17_poisson_regression.r                  ← Poisson IRR models
│   └── 18_age_at_death.r                        ← Age-at-death distribution
│
└── output/                                      ← Generated tables and figures
    ├── tables/
    └── figures/
``` 


# 5. METHOD and PROCESSING 
--------------------------  
[This section should describe how content in the Dataset was generated. This should list 
equipment, hardware/software (including version), algorithms, formulae, experimental
procedures/protocols, how data have been altered or processed (e.g. normalised), etc. 

If this information is available in a separate document, it should be referenced, or 
stored together with the data. Associated articles or other publications should not be 
included in the Dataset, but a citation can be included here and direct links can be added 
to the Related resources and Associated  publications fields in the metadata record. 

If a publication is in preparation at the time of deposit, provide relevant details where 
known (authors, title, journal, etc.) and indicate status at time of deposit 
(In preparation, In press, published).]
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
# 6. RELATED DATASET(S)
-------------------
[If you used existing data for your analysis that are not readily available, you are encouraged to submit the data for archiving with the permission of the original data producer. For datasets that are a combination of primary and existing data, you are encouraged to submit the whole dataset. In situations where the access to the existing data is not permitted, provide clear descriptionof the data and indicate how and where users can find or request for the same.]


# 7. RELATED PUBLICATION(S)
-------------------------- 
[Include citations of publications that reported findings using these data]


