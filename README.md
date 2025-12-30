# Infrastructure Investment and Governance: Empirical Analysis

A comprehensive econometric analysis examining the determinants of private sector participation in infrastructure investment across developing countries (2005-2023).

## 📊 Project Overview

This research investigates three key hypotheses about infrastructure investment in developing economies:

1. **Governance Quality Effects**: The relationship between regulatory quality, rule of law, and private infrastructure investment
2. **Blended Finance Mechanisms**: The effectiveness of multilateral support (MLS) and budget support (BS) in mobilizing private capital, particularly in high-risk countries
3. **Sector Heterogeneity**: Comparing private sector participation across infrastructure sectors, with focus on renewable vs. conventional energy

### Research Questions

- How do governance indicators (regulatory quality and rule of law) affect private sector participation in infrastructure?
- Does blended finance increase private investment, especially in higher-risk countries?
- Are there systematic differences in private sector participation across infrastructure sectors?

## 🗂️ Repository Structure

```
.
├── H1_governance_private_investment_analysis_v2.r    # Hypothesis 1: Governance effects
├── H2_blended_finance_analysis_v2.r                  # Hypothesis 2: Blended finance
├── H3_sector_investment_analysis.r                   # Hypothesis 3: Sector analysis
├── Data_Analysis.ipynb                                # Data extraction and preparation
├── merge_files.py                                     # Dataset merging utility
└── README.md                                          # This file
```

## 📦 Data Sources

### Primary Datasets

1. **World Bank Private Participation in Infrastructure (PPI) Database**
   - Period: 2005-2023
   - Coverage: 33 developing countries
   - Project-level infrastructure investment data
   - Variables: Investment amounts, private share, sector, project type, blended finance indicators

2. **World Bank Development Indicators (WDI)**
   - Economic controls: GDP per capita, domestic credit, FDI inflows
   - 34 macroeconomic indicators

3. **Worldwide Governance Indicators (WGI)**
   - 6 governance dimensions:
     - Control of Corruption
     - Government Effectiveness
     - Political Stability and Absence of Violence
     - Regulatory Quality
     - Rule of Law
     - Voice and Accountability

## 🔧 Requirements

### R Packages

```r
# Core analysis
tidyverse      # Data manipulation and visualization
fixest         # Fast fixed effects estimation
modelsummary   # Publication-ready regression tables
kableExtra     # Enhanced table formatting

# Additional packages
ggplot2        # Advanced plotting
scales         # Axis formatting
plm            # Panel data modeling (for diagnostic tests)
openxlsx       # Excel export
rmarkdown      # Report generation
```

### Python Packages

```python
pandas         # Data manipulation
wbgapi         # World Bank API access
numpy          # Numerical operations
jupyter        # Notebook environment
```

## 🚀 Getting Started

### 1. Data Preparation

First, fetch and clean the World Bank data:

```bash
# Open Jupyter notebook
jupyter notebook Data_Analysis.ipynb

# Run all cells to:
# - Fetch WDI and WGI data via API
# - Process and merge datasets
# - Export cleaned CSV files
```

### 2. Merge Datasets

Combine PPI project data with World Bank indicators:

```bash
python merge_files.py
```

**Note**: Update file paths in `merge_files.py` to match your local directory structure.

### 3. Run Analysis Scripts

Execute hypothesis-specific R scripts:

```r
# Hypothesis 1: Governance and Private Investment
source("H1_governance_private_investment_analysis_v2.r")

# Hypothesis 2: Blended Finance
source("H2_blended_finance_analysis_v2.r")

# Hypothesis 3: Sector Analysis
source("H3_sector_investment_analysis.r")
```

**Important**: Update the `output_dir` variable in each R script to match your directory structure.

## 📈 Analysis Methods

### Econometric Approach

**Panel Regression with Fixed Effects**
- Country fixed effects (control for time-invariant country characteristics)
- Year fixed effects (control for global shocks and trends)
- Clustered standard errors at country level (account for within-country correlation)

**Progressive Model Specification**
Each hypothesis builds models progressively:
1. Baseline: Main variables + fixed effects
2. + Economic controls
3. + Sector controls
4. + Interaction terms
5. Full specification (preferred model)

### Key Variables

**Dependent Variables:**
- `private_share`: Private sector share of investment (0-1)
- `log_investment`: Log of total investment (2015 USD)
- `log_private_investment`: Log of private investment amount

**Independent Variables:**
- **Governance**: Regulatory Quality, Rule of Law (percentile ranks)
- **Blended Finance**: MLS indicator, Budget Support indicator
- **Risk**: Political Stability (percentile rank)
- **Economic**: Log GDP per capita, domestic credit (% GDP), FDI (% GDP)
- **Sector**: Energy, Transport, Water, ICT dummies
- **Project**: Greenfield indicator, renewable energy indicator

## 📋 Output Files

### Hypothesis 1: Governance

```
h1_governance_revised/
├── tables/
│   ├── table1_main_results.html                    # Progressive specifications
│   ├── table2_robustness.html                      # Alternative specifications
│   ├── table3_mechanisms.html                      # Interaction effects
│   ├── governance_correlation_matrix.csv           # Multicollinearity check
│   ├── summary_statistics.html                     # Descriptive stats
│   └── h1_wooldridge_autocorrelation_test.csv     # Serial correlation test
└── figures/
    ├── governance_effects_by_risk.png              # Heterogeneous effects
    ├── marginal_effects_rq_rol.png                 # Interaction visualization
    └── h1_coefficient_stability_*.png              # Lag structure tests
```

### Hypothesis 2: Blended Finance

```
h2_blended_finance_revised/
├── tables/
│   ├── table_h2a_main_results.html                 # Progressive specifications
│   ├── table_h2b_robustness.html                   # Alternative DVs
│   ├── table_h2c_country_year.html                 # Aggregate analysis
│   ├── table_h2d_nonlinearity.html                 # Quadratic effects
│   ├── descriptive_mls_by_risk.csv                 # MLS distribution by risk
│   └── summary_statistics.html                     # Descriptive stats
└── figures/
    └── blended_finance_effects.png                 # Risk heterogeneity plot
```

### Hypothesis 3: Sector Analysis

```
h3_alternative/
├── tables/
│   ├── approach1_within_energy.html                # Renewable vs conventional
│   ├── approach2_investment_amount.html            # Investment size analysis
│   └── approach3_selection_controls.html           # Project characteristics
└── figures/
    └── sector_comparison.png                       # Cross-sector effects
```

## 🔬 Key Findings Preview

### H1: Governance Effects
- Both regulatory quality and rule of law positively associated with private investment
- Effects are stronger in higher-risk countries (complementarity)
- Interaction effects suggest governance dimensions work synergistically

### H2: Blended Finance
- MLS significantly increases private investment in high-risk countries
- Risk-MLS interaction is positive and significant
- Nonlinear effects suggest threshold behavior at low governance levels

### H3: Sector Heterogeneity
- Renewable energy projects have systematically higher private participation
- Private share exhibits ceiling effect (97%+) across all sectors
- Investment size shows more variation than private share

## ⚙️ Diagnostic Tests

Each script includes comprehensive diagnostics:

### Multicollinearity
- Correlation matrices for governance indicators
- VIF analysis (not explicitly shown but can be added)

### Serial Correlation
- Wooldridge test for first-order autocorrelation
- Justifies country-level clustering strategy

### Robustness Checks
- Alternative dependent variables
- Different fixed effects specifications
- Subsample analyses by risk category
- Quadratic terms for nonlinearity

### Lag Structure
- Tests with 1, 2, and 3-year lags of governance variables
- Coefficient stability plots

## 📝 Citation

If you use this code or methodology, please cite:

```
Alexander Orta. (2025). Infrastructure Investment and Governance: 
Empirical Analysis of Developing Countries, 2005-2023. 
Boston University.
```

## 🤝 Contributing

This is a research project for EC561 Public Economics at Boston University. 

For questions or suggestions:
- Open an issue in this repository
- Contact: aorta1@bu.edu

## 📄 License

This project is licensed for academic use only.

## 🙏 Acknowledgments

- World Bank for PPI, WDI, and WGI data
- Boston University Economics Department
- EC561 Public Economics course instructors

## 🔍 Technical Notes

### File Paths
All scripts use absolute Windows paths. Update these variables before running:
- `output_dir` in R scripts
- File paths in `merge_files.py`

### Data Updates
To refresh data with latest World Bank releases:
1. Re-run `Data_Analysis.ipynb` to fetch updated data
2. Re-run `merge_files.py` to merge with PPI data
3. Re-run hypothesis scripts

### Performance
- R scripts use `fixest` for fast fixed effects estimation
- Large datasets may require 8GB+ RAM
- Full analysis suite takes approximately 15-30 minutes to run

### Reproducibility
- Set random seed for any stochastic processes
- All data transformations are documented in code comments
- Log files saved automatically to output directories

---

**Last Updated**: December 2025  
**Version**: 2.0  
**Contact**: Boston University, Questrom School of Business
