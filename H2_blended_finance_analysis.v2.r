################################################################################
# HYPOTHESIS 2: BLENDED FINANCE ANALYSIS - REVISED WITH NONLINEARITY TESTS
# Progressive Model Building for Publication
#
# Research Question: Does blended finance (MLS/BS) increase private investment,
#                    particularly in higher-risk countries?
#
# REVISION: Added nonlinearity tests to examine:
#   1. Nonlinear effects of governance on investment
#   2. Whether MLS effectiveness varies nonlinearly with risk/governance
#   3. Threshold effects for blended finance mechanisms
#
# Model Building Strategy:
#   Table H2-A (Main Results): Progressive specification
#     (1) Baseline: Blended finance × Risk + FEs
#     (2) + Governance controls
#     (3) + Economic controls (GDP, credit, FDI)
#     (4) + Sector FEs
#     (5) Full model (preferred specification)
#
#   Table H2-B (Robustness): Alternative specifications
#   Table H2-C (Mechanisms): Country-year analysis
#   Table H2-D (Nonlinearity): Governance and risk quadratic effects
################################################################################

# Load required packages
library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(ggplot2)
library(scales)

# Load openxlsx for XLSX export if available
if (!require("openxlsx", quietly = TRUE)) {
  message("Installing openxlsx for XLSX export...")
  install.packages("openxlsx", quiet = TRUE)
  library(openxlsx)
}

# Install pandoc if needed (for table formatting)
if (!require("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown")
  library(rmarkdown)
}

# Set output directory
output_dir <- "C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Analysis Output"

# Create output directories
dir.create(file.path(output_dir, "h2_blended_finance_revised"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h2_blended_finance_revised/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h2_blended_finance_revised/figures"), recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. DATA PREPARATION
################################################################################

cat("\n" , rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 2: BLENDED FINANCE ANALYSIS (REVISED)\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Loading data...\n")

# Load data
ppi <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/PPI_2005_2023.csv")
wb <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/WB_merged_2005_2023.csv")

# Compute 1-year lag of GDP per capita in the WB data if not already present
# This creates `lag1_gdp` used later to form `log_gdp_pc_lag1` in project and country-year data
wb <- wb %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(lag1_gdp = dplyr::lag(GDP_per_capita_constant_2015_USD, n = 1)) %>%
  ungroup()

# Create project-level dataset
cat("Preparing project-level data...\n")

df_projects <- ppi %>%
  filter(!is.na(investment_real), investment_real > 0) %>%
  mutate(
    # Blended finance indicators
    has_MLS = ifelse(MLS == "With MLS", 1, 0),
    has_BS = ifelse(BS %in% c("With BS", "With BLS"), 1, 0),
    has_blended = ifelse(has_MLS == 1 | has_BS == 1, 1, 0),
    
    # Dependent variable
    log_investment = log(investment_real),
    
    # Sector dummies
    sector_energy = ifelse(sector == "Energy", 1, 0),
    sector_transport = ifelse(sector == "Transport", 1, 0),
    sector_water = ifelse(sector == "Water and sewerage", 1, 0),
    sector_ict = ifelse(sector == "Information and communications technology", 1, 0)
  ) %>%
  
  # Merge with World Bank data
  left_join(wb, by = c("country_code" = "country", "year" = "year")) %>%
  
  # Create control variables
  mutate(
    # Economic variables
    log_gdp_pc = if_else(!is.na(GDP_per_capita_constant_2015_USD) & GDP_per_capita_constant_2015_USD > 0,
                         log(GDP_per_capita_constant_2015_USD), NA_real_),
    # 1-year lag of GDP per capita (from merged WB dataset; variable: `lag1_gdp`)
    # Use this lagged GDP because investment decisions are made based on prior-year GDP
    log_gdp_pc_lag1 = if_else(!is.na(lag1_gdp) & lag1_gdp > 0,
                  log(lag1_gdp), NA_real_),
    domestic_credit = Domestic_credit_to_private_sector_pct_GDP,
    fdi_gdp = FDI_Inflows_pct_GDP,
    
    # Risk measure: Political Stability percentile
    ps_percentile = Political_Stability_percentile,
    high_risk = ifelse(ps_percentile < 33, 1, 0),
    
    # Governance composite (average of 6 WGI indicators)
    governance = (Control_of_Corruption_percentile + 
                  Government_Effectiveness_percentile +
                  Political_Stability_percentile +
                  Rule_of_Law_percentile +
                  Regulatory_Quality_percentile +
                  Voice_and_Accountability_percentile) / 6,
    low_governance = ifelse(governance < 33, 1, 0),
    
    # FOR NONLINEARITY TESTS: Create squared terms and centered versions
    governance_sq = governance^2,
    ps_percentile_sq = ps_percentile^2,
    
    # Centered versions (for easier interpretation)
    governance_centered = governance - mean(governance, na.rm=T),
    governance_centered_sq = governance_centered^2,
    ps_centered = ps_percentile - mean(ps_percentile, na.rm=T),
    ps_centered_sq = ps_centered^2
  ) %>%
  
  # Keep complete cases for main analysis
  filter(!is.na(log_investment), !is.na(has_MLS), !is.na(high_risk))

cat(sprintf("\nProject-level sample: %d projects\n", nrow(df_projects)))
cat(sprintf("  - With MLS: %d (%.1f%%)\n", 
            sum(df_projects$has_MLS), 100*mean(df_projects$has_MLS)))
cat(sprintf("  - With BS: %d (%.1f%%)\n", 
            sum(df_projects$has_BS), 100*mean(df_projects$has_BS)))
cat(sprintf("  - High-risk countries: %d (%.1f%%)\n",
            sum(df_projects$high_risk, na.rm=T), 100*mean(df_projects$high_risk, na.rm=T)))
cat(sprintf("  - Mean governance: %.1f\n", mean(df_projects$governance, na.rm=T)))
cat(sprintf("  - Mean political stability: %.1f\n\n", mean(df_projects$ps_percentile, na.rm=T)))

################################################################################
# 2. DESCRIPTIVE STATISTICS
################################################################################

cat("Generating descriptive statistics...\n")

# Table: MLS/BS by Risk Category
risk_table <- df_projects %>%
  mutate(risk_category = case_when(
    ps_percentile < 33 ~ "High Risk",
    ps_percentile >= 33 & ps_percentile < 67 ~ "Medium Risk",
    ps_percentile >= 67 ~ "Low Risk"
  )) %>%
  filter(!is.na(risk_category)) %>%
  group_by(risk_category) %>%
  summarise(
    Total_Projects = n(),
    MLS_Projects = sum(has_MLS),
    MLS_Pct = 100 * mean(has_MLS),
    BS_Projects = sum(has_BS),
    BS_Pct = 100 * mean(has_BS),
    Avg_Investment_MLS = mean(investment_real[has_MLS==1], na.rm=T),
    Avg_Investment_No_MLS = mean(investment_real[has_MLS==0], na.rm=T),
    .groups = "drop"
  ) %>%
  arrange(desc(risk_category))

write_csv(risk_table, file.path(output_dir, "h2_blended_finance_revised/tables/descriptive_mls_by_risk.csv"))
print(risk_table)

# Summary statistics
summary_vars <- df_projects %>%
  select(investment_real, has_MLS, has_BS, high_risk, ps_percentile, 
         governance, log_gdp_pc, domestic_credit, fdi_gdp)

datasummary_skim(
  summary_vars,
  output = file.path(output_dir, "h2_blended_finance_revised/tables/summary_statistics.html"),
  title = "Summary Statistics: Key Variables"
)

################################################################################
# 3. TABLE 1: MAIN RESULTS - PROGRESSIVE SPECIFICATION
################################################################################

cat("\n" , rep("=", 80), "\n", sep = "")
cat("TABLE H2-A: MAIN RESULTS - PROGRESSIVE SPECIFICATION\n")
cat(rep("=", 80), "\n\n", sep = "")

# Prepare regression dataset
  reg_data <- df_projects %>%
  filter(
    !is.na(log_investment), !is.na(has_MLS), !is.na(has_BS), !is.na(high_risk),
    !is.na(governance), !is.na(log_gdp_pc_lag1), !is.na(domestic_credit),
    !is.na(fdi_gdp), !is.na(sector_energy), !is.na(sector_transport), !is.na(sector_water)
  )

cat(sprintf("Regression sample: %d projects\n", nrow(reg_data)))
cat(sprintf("  - Countries: %d\n", n_distinct(reg_data$country_code)))
cat(sprintf("  - Years: %d\n\n", n_distinct(reg_data$year)))

# Column (1): Baseline
main_1 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS | 
    country_code + year,
  data = reg_data,
  vcov = "hetero"
)

# Column (2): Add governance
main_2 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance | 
    country_code + year,
  data = reg_data,
  vcov = "hetero"
)

# Column (3): Add economic controls
main_3 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = reg_data,
  vcov = "hetero"
)

# Column (4): Add sector FEs
main_4 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = "hetero"
)

# Column (5): Clustered SEs (PREFERRED)
main_5 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# Extract key coefficients
mls_coef <- coef(main_5)["has_MLS"]
mls_risk_int <- coef(main_5)["has_MLS:high_risk"]
bs_coef <- coef(main_5)["has_BS"]

cat("Main results:\n")
cat(sprintf("  MLS effect (baseline):        %.3f\n", mls_coef))
cat(sprintf("  MLS × High Risk interaction:  %.3f\n", mls_risk_int))
cat(sprintf("  BS effect:                    %.3f\n", bs_coef))
cat(sprintf("  MLS effect in high-risk:      %.3f\n\n", mls_coef + mls_risk_int))

# Save Table H2-A
tryCatch({
  modelsummary(
    list(
      "(1) Baseline" = main_1,
      "(2) + Gov" = main_2,
      "(3) + Econ" = main_3,
      "(4) + Sector" = main_4,
      "(5) Preferred" = main_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2a_main_results.html"),
    title = "Table H2-A: Main Results - Blended Finance and Private Investment",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Log of total infrastructure investment.",
      "Progressive specification adds controls sequentially.",
      "Column (5) uses country-clustered standard errors (preferred).",
      "All specifications include country and year fixed effects."
    )
  )
  cat("✓ Table H2-A HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-A HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) Baseline" = main_1,
      "(2) + Gov" = main_2,
      "(3) + Econ" = main_3,
      "(4) + Sector" = main_4,
      "(5) Preferred" = main_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2a_main_results.docx"),
    title = "Table H2-A: Main Results - Blended Finance and Private Investment",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Log of total infrastructure investment.",
      "Progressive specification adds controls sequentially.",
      "Column (5) uses country-clustered standard errors (preferred).",
      "All specifications include country and year fixed effects."
    )
  )
  cat("✓ Table H2-A DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-A DOCX (", e$message, ")\n")
})

tryCatch({
  models_h2a <- list(
    "(1) Baseline" = main_1,
    "(2) + Gov" = main_2,
    "(3) + Econ" = main_3,
    "(4) + Sector" = main_4,
    "(5) Preferred" = main_5
  )

  msdf_h2a <- modelsummary::modelsummary(
    models_h2a,
    output = "data.frame",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )

  write.csv(
    msdf_h2a,
    file.path(output_dir, "h2_blended_finance_revised/tables/table_h2a_main_results.csv"),
    row.names = FALSE
  )

  if (require("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(
      msdf_h2a,
      file.path(output_dir, "h2_blended_finance_revised/tables/table_h2a_main_results.xlsx")
    )
  }

  cat("✓ Table H2-A CSV/XLSX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-A CSV/XLSX (", e$message, ")\n")
})

################################################################################
# 4. TABLE 2: ROBUSTNESS CHECKS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE H2-B: ROBUSTNESS CHECKS\n")
cat(rep("=", 80), "\n\n", sep = "")

# (1) Alternative risk measure: Low governance
robust_1 <- feols(
  log_investment ~ has_MLS * low_governance + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (2) Exclude mega-projects
robust_2 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data %>% filter(investment_real <= 1e9),
  vcov = ~country_code
)

# (3) Energy sector only
robust_3 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = reg_data %>% filter(sector == "Energy"),
  vcov = ~country_code
)

# (4) Transport sector only
robust_4 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = reg_data %>% filter(sector == "Transport"),
  vcov = ~country_code
)

# (5) Two-way clustering
robust_5 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code + year
)

cat("Robustness check results:\n")
cat(sprintf("  (1) Low governance measure:  MLS=%.3f, MLS×LowGov=%.3f\n", 
            coef(robust_1)["has_MLS"], coef(robust_1)["has_MLS:low_governance"]))
cat(sprintf("  (2) No mega-projects:        MLS=%.3f, MLS×Risk=%.3f\n", 
            coef(robust_2)["has_MLS"], coef(robust_2)["has_MLS:high_risk"]))
cat(sprintf("  (3) Energy only:             MLS=%.3f, MLS×Risk=%.3f\n", 
            coef(robust_3)["has_MLS"], coef(robust_3)["has_MLS:high_risk"]))
cat(sprintf("  (4) Transport only:          MLS=%.3f, MLS×Risk=%.3f\n", 
            coef(robust_4)["has_MLS"], coef(robust_4)["has_MLS:high_risk"]))
cat(sprintf("  (5) Two-way clustering:      MLS=%.3f, MLS×Risk=%.3f\n\n", 
            coef(robust_5)["has_MLS"], coef(robust_5)["has_MLS:high_risk"]))

# Save Table 2
tryCatch({
  modelsummary(
    list(
      "(1) Alt Risk" = robust_1,
      "(2) No Mega" = robust_2,
      "(3) Energy" = robust_3,
      "(4) Transport" = robust_4,
      "(5) 2-Way SE" = robust_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2b_robustness.html"),
    title = "Table H2-B: Robustness Checks",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "low_governance" = "Low Governance Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "has_MLS:low_governance" = "MLS × Low Governance",
      "governance" = "Governance Index",
        "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "(1) Uses governance < 33rd percentile as alternative risk measure.",
      "(2) Excludes projects > $1 billion to address outlier concerns.",
      "(3)-(4) Sector-specific subsamples.",
      "(5) Two-way clustering by country and year.",
      "All specifications include country and year fixed effects."
    )
  )
  cat("✓ Table 2 HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 2 HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) Alt Risk" = robust_1,
      "(2) No Mega" = robust_2,
      "(3) Energy" = robust_3,
      "(4) Transport" = robust_4,
      "(5) 2-Way SE" = robust_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2b_robustness.docx"),
    title = "Table H2-B: Robustness Checks",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "low_governance" = "Low Governance Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "has_MLS:low_governance" = "MLS × Low Governance",
      "governance" = "Governance Index",
        "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "(1) Uses governance < 33rd percentile as alternative risk measure.",
      "(2) Excludes projects > $1 billion to address outlier concerns.",
      "(3)-(4) Sector-specific subsamples.",
      "(5) Two-way clustering by country and year.",
      "All specifications include country and year fixed effects."
    )
  )
  cat("✓ Table 2 DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 2 DOCX (", e$message, ")\n")
})

tryCatch({
  models_h2b <- list(
    "(1) Robust 1" = robust_1,
    "(2) Robust 2" = robust_2,
    "(3) Robust 3" = robust_3,
    "(4) Robust 4" = robust_4,
    "(5) Robust 5" = robust_5
  )

  msdf_h2b <- modelsummary::modelsummary(
    models_h2b,
    output = "data.frame",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )

  write.csv(
    msdf_h2b,
    file.path(output_dir, "h2_blended_finance_revised/tables/table_h2b_robustness_results.csv"),
    row.names = FALSE
  )

  if (require("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(
      msdf_h2b,
      file.path(output_dir, "h2_blended_finance_revised/tables/table_h2b_robustness_results.xlsx")
    )
  }

  cat("✓ Table H2-B CSV/XLSX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-B CSV/XLSX (", e$message, ")\n")
})

################################################################################
# 5. TABLE H2-C: MECHANISMS - COUNTRY-YEAR ANALYSIS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE H2-C: MECHANISMS - COUNTRY-YEAR ANALYSIS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create country-year aggregated dataset
df_country_year <- df_projects %>%
  group_by(country_code, year) %>%
  summarise(
    total_investment = sum(investment_real, na.rm = TRUE),
    n_projects = n(),
    mls_share = mean(has_MLS, na.rm = TRUE),
    bs_share = mean(has_BS, na.rm = TRUE),
    governance = first(governance),
    ps_percentile = first(ps_percentile),
    high_risk = first(high_risk),
    gdp_pc = first(GDP_per_capita_constant_2015_USD),
    # 1-year lag of GDP per capita at country-year level
    gdp_pc_lag1 = first(lag1_gdp),
    population = first(Population_total),
    domestic_credit = first(domestic_credit),
    fdi_gdp = first(fdi_gdp),
    .groups = "drop"
  ) %>%
  mutate(
    log_investment_pc = log(total_investment / population),
    # Use lagged GDP per capita for country-year regressions
    log_gdp_pc_lag1 = if_else(!is.na(gdp_pc_lag1) & gdp_pc_lag1 > 0, log(gdp_pc_lag1), NA_real_),
    log_n_projects = log(n_projects)
  ) %>%
  filter(!is.na(log_investment_pc), !is.na(mls_share))

cat(sprintf("Country-year sample: %d observations\n", nrow(df_country_year)))
cat(sprintf("  - Countries: %d\n", n_distinct(df_country_year$country_code)))
cat(sprintf("  - Years: %d\n\n", n_distinct(df_country_year$year)))

# Country-year regressions
cy_data <- df_country_year %>%
  filter(
    !is.na(log_investment_pc), !is.na(mls_share), !is.na(governance),
    !is.na(log_gdp_pc_lag1), !is.na(domestic_credit), !is.na(fdi_gdp)
  )

# (1) MLS share
mech_1 <- feols(
  log_investment_pc ~ mls_share * high_risk + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = cy_data,
  vcov = ~country_code
)

# (2) BS share
mech_2 <- feols(
  log_investment_pc ~ bs_share * high_risk + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = cy_data,
  vcov = ~country_code
)

# (3) Both shares
mech_3 <- feols(
  log_investment_pc ~ mls_share * high_risk + bs_share * high_risk + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = cy_data,
  vcov = ~country_code
)

# (4) Number of projects
mech_4 <- feols(
  log_n_projects ~ mls_share * high_risk + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = cy_data,
  vcov = ~country_code
)

# (5) Investment per project
mech_5 <- feols(
  log_investment_pc ~ mls_share * high_risk + log_n_projects + governance + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp | 
    country_code + year,
  data = cy_data,
  vcov = ~country_code
)

cat("Country-year mechanism results:\n")
cat(sprintf("  (1) MLS share effect:       %.3f\n", coef(mech_1)["mls_share"]))
cat(sprintf("  (2) BS share effect:        %.3f\n", coef(mech_2)["bs_share"]))
cat(sprintf("  (3) MLS in high-risk:       %.3f\n", 
            coef(mech_3)["mls_share"] + coef(mech_3)["mls_share:high_risk"]))
cat(sprintf("  (4) BS in high-risk:        %.3f\n\n", 
            coef(mech_3)["bs_share"] + coef(mech_3)["bs_share:high_risk"]))

# Save Table 3
tryCatch({
  modelsummary(
    list(
      "(1) MLS Share" = mech_1,
      "(2) BS Share" = mech_2,
      "(3) Both" = mech_3,
      "(4) N Projects" = mech_4,
      "(5) Inv per Proj" = mech_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2c_mechanisms.html"),
    title = "Table H2-C: Mechanisms - Country-Year Analysis",
    coef_rename = c(
      "mls_share" = "MLS Share (0-1)",
      "bs_share" = "BS Share (0-1)",
      "mls_share:high_risk" = "MLS Share × High Risk",
      "bs_share:high_risk" = "BS Share × High Risk",
      "high_risk" = "High Risk Country",
      "log_n_projects" = "Log(Number of Projects)",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Country-year aggregated data.",
      "DV in (1)-(3), (5): Log investment per capita.",
      "DV in (4): Log number of projects.",
      "MLS/BS share = proportion of projects with support in country-year.",
      "All specifications include country and year fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table H2-C HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-C HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) MLS Share" = mech_1,
      "(2) BS Share" = mech_2,
      "(3) Both" = mech_3,
      "(4) N Projects" = mech_4,
      "(5) + N Proj" = mech_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2c_mechanisms.docx"),
    title = "Table H2-C: Mechanisms - Country-Year Analysis",
    coef_rename = c(
      "mls_share" = "MLS Share (0-1)",
      "bs_share" = "BS Share (0-1)",
      "mls_share:high_risk" = "MLS Share × High Risk",
      "bs_share:high_risk" = "BS Share × High Risk",
      "high_risk" = "High Risk Country",
      "log_n_projects" = "Log(Number of Projects)",
      "governance" = "Governance Index",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Country-year aggregated data.",
      "DV in (1)-(3), (5): Log investment per capita.",
      "DV in (4): Log number of projects.",
      "MLS/BS share = proportion of projects with support in country-year.",
      "All specifications include country and year fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table H2-C DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-C DOCX (", e$message, ")\n")
})

tryCatch({
  models_h2c <- list(
    "(1) Mech 1" = mech_1,
    "(2) Mech 2" = mech_2,
    "(3) Mech 3" = mech_3,
    "(4) Mech 4" = mech_4,
    "(5) Mech 5" = mech_5
  )

  msdf_h2c <- modelsummary::modelsummary(
    models_h2c,
    output = "data.frame",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )

  write.csv(
    msdf_h2c,
    file.path(output_dir, "h2_blended_finance_revised/tables/table_h2c_mechanisms_results.csv"),
    row.names = FALSE
  )

  if (require("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(
      msdf_h2c,
      file.path(output_dir, "h2_blended_finance_revised/tables/table_h2c_mechanisms_results.xlsx")
    )
  }

  cat("✓ Table H2-C CSV/XLSX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-C CSV/XLSX (", e$message, ")\n")
})

################################################################################
# 6. TABLE H2-D: NONLINEARITY TESTS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE H2-D: NONLINEARITY TESTS - QUADRATIC SPECIFICATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Testing for nonlinear relationships:\n")
cat("  H0: Linear relationship (β2 = 0)\n")
cat("  HA: Nonlinear relationship (β2 ≠ 0)\n\n")

# (1) Governance with quadratic term
nonlin_1 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + governance + governance_sq + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (2) Political stability with quadratic term
nonlin_2 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + ps_percentile + ps_percentile_sq + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (3) Both governance and PS with quadratic terms
nonlin_3 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + 
    governance + governance_sq + ps_percentile + ps_percentile_sq + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (4) Test whether MLS effectiveness varies nonlinearly with governance
# MLS × Governance + MLS × Governance²
nonlin_4 <- feols(
  log_investment ~ has_MLS * (governance + governance_sq) + has_MLS * high_risk + 
    has_BS + log_gdp_pc + domestic_credit + fdi_gdp + 
    has_BS + log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (5) Centered specification (for easier interpretation)
nonlin_5 <- feols(
  log_investment ~ has_MLS * high_risk + has_BS + 
    governance_centered + governance_centered_sq + 
    ps_centered + ps_centered_sq + 
    log_gdp_pc_lag1 + domestic_credit + fdi_gdp + 
    sector_energy + sector_transport + sector_water | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# Extract coefficients
gov_linear <- coef(nonlin_1)["governance"]
gov_sq <- coef(nonlin_1)["governance_sq"]
ps_linear <- coef(nonlin_2)["ps_percentile"]
ps_sq <- coef(nonlin_2)["ps_percentile_sq"]

cat("Nonlinearity test results:\n")
cat(sprintf("  Governance linear (β1):       %.6f\n", gov_linear))
cat(sprintf("  Governance squared (β2):      %.6f\n", gov_sq))
cat(sprintf("  PS linear (β3):               %.6f\n", ps_linear))
cat(sprintf("  PS squared (β4):              %.6f\n\n", ps_sq))

# Interpret nonlinearity
cat("INTERPRETATION:\n")

# Governance interpretation
if (!is.na(gov_sq)) {
  gov_pval <- summary(nonlin_1)$coeftable["governance_sq", "Pr(>|t|)"]
  cat(sprintf("  Governance (p=%.4f):\n", gov_pval))
  
  if (gov_pval < 0.05) {
    if (gov_sq > 0 & gov_linear < 0) {
      turning_point <- -gov_linear / (2 * gov_sq)
      cat(sprintf("    ✓ U-SHAPED (turning point: %.1f)\n", turning_point))
      cat("    → Governance must exceed threshold to increase investment\n")
    } else if (gov_sq < 0 & gov_linear > 0) {
      peak <- -gov_linear / (2 * gov_sq)
      cat(sprintf("    ✓ INVERTED U (peak: %.1f)\n", peak))
      cat("    → Diminishing returns to governance improvements\n")
    } else if (gov_sq > 0 & gov_linear > 0) {
      cat("    ✓ ACCELERATING POSITIVE\n")
      cat("    → Increasing returns to governance\n")
    } else {
      cat("    ✓ DECELERATING NEGATIVE\n")
    }
  } else {
    cat("    ✗ NO significant nonlinearity (relationship appears LINEAR)\n")
  }
}

cat("\n")

# Political stability interpretation
if (!is.na(ps_sq)) {
  ps_pval <- summary(nonlin_2)$coeftable["ps_percentile_sq", "Pr(>|t|)"]
  cat(sprintf("  Political Stability (p=%.4f):\n", ps_pval))
  
  if (ps_pval < 0.05) {
    if (ps_sq > 0 & ps_linear < 0) {
      turning_point <- -ps_linear / (2 * ps_sq)
      cat(sprintf("    ✓ U-SHAPED (turning point: %.1f)\n", turning_point))
      cat("    → Political stability must exceed threshold\n")
    } else if (ps_sq < 0 & ps_linear > 0) {
      peak <- -ps_linear / (2 * ps_sq)
      cat(sprintf("    ✓ INVERTED U (peak: %.1f)\n", peak))
      cat("    → Diminishing returns to political stability\n")
    } else if (ps_sq > 0 & ps_linear > 0) {
      cat("    ✓ ACCELERATING POSITIVE\n")
      cat("    → Increasing returns to stability\n")
    } else {
      cat("    ✓ DECELERATING NEGATIVE\n")
    }
  } else {
    cat("    ✗ NO significant nonlinearity (relationship appears LINEAR)\n")
  }
}

cat("\n")

# Joint test for nonlinearity
cat("JOINT TEST FOR NONLINEARITY:\n")
cat("  Testing H0: Both squared terms = 0\n")

tryCatch({
  wald_test <- wald(nonlin_3, c("governance_sq", "ps_percentile_sq"))
  cat(sprintf("  F-statistic: %.3f\n", wald_test$stat))
  cat(sprintf("  P-value:     %.4f\n", wald_test$p))
  
  if (wald_test$p < 0.05) {
    cat("  → REJECT null: Significant evidence of nonlinearity\n")
  } else {
    cat("  → FAIL TO REJECT null: Linear specification appears adequate\n")
  }
}, error = function(e) {
  cat("  (Wald test could not be computed)\n")
})

cat("\n")

# Model comparison
cat("MODEL COMPARISON (AIC):\n")
cat(sprintf("  Linear model (Table H2-A, Col 5):  %.1f\n", AIC(main_5)))
cat(sprintf("  Governance quadratic:           %.1f\n", AIC(nonlin_1)))
cat(sprintf("  PS quadratic:                   %.1f\n", AIC(nonlin_2)))
cat(sprintf("  Both quadratic:                 %.1f\n", AIC(nonlin_3)))
cat(sprintf("  MLS × Governance quadratic:     %.1f\n", AIC(nonlin_4)))
cat("\n  → Lower AIC = Better fit\n\n")

# Save Table H2-D
tryCatch({
  modelsummary(
    list(
      "(1) Gov²" = nonlin_1,
      "(2) PS²" = nonlin_2,
      "(3) Both²" = nonlin_3,
      "(4) MLS×Gov²" = nonlin_4,
      "(5) Centered" = nonlin_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2d_nonlinearity.html"),
    title = "Table H2-D: Nonlinearity Tests - Quadratic Specifications",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance (linear)",
      "governance_sq" = "Governance² (quadratic)",
      "ps_percentile" = "Political Stability (linear)",
      "ps_percentile_sq" = "PS² (quadratic)",
      "has_MLS:governance" = "MLS × Governance",
      "has_MLS:governance_sq" = "MLS × Governance²",
      "governance_centered" = "Governance (centered)",
      "governance_centered_sq" = "Governance² (centered)",
      "ps_centered" = "PS (centered)",
      "ps_centered_sq" = "PS² (centered)",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared", "aic"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Log of total infrastructure investment.",
      "Tests for nonlinear relationships in governance and political stability.",
      "Column (4) tests whether MLS effectiveness varies nonlinearly with governance.",
      "Column (5) uses centered variables for easier interpretation.",
      "All specifications include country, year, and sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table H2-D HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-D HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) Gov²" = nonlin_1,
      "(2) PS²" = nonlin_2,
      "(3) Both²" = nonlin_3,
      "(4) MLS×Gov²" = nonlin_4,
      "(5) Centered" = nonlin_5
    ),
    output = file.path(output_dir, "h2_blended_finance_revised/tables/table_h2d_nonlinearity.docx"),
    title = "Table H2-D: Nonlinearity Tests - Quadratic Specifications",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance (linear)",
      "governance_sq" = "Governance² (quadratic)",
      "ps_percentile" = "Political Stability (linear)",
      "ps_percentile_sq" = "PS² (quadratic)",
      "has_MLS:governance" = "MLS × Governance",
      "has_MLS:governance_sq" = "MLS × Governance²",
      "governance_centered" = "Governance (centered)",
      "governance_centered_sq" = "Governance² (centered)",
      "ps_centered" = "PS (centered)",
      "ps_centered_sq" = "PS² (centered)",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)"
    ),
    gof_map = c("nobs", "r.squared", "aic"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Log of total infrastructure investment.",
      "Tests for nonlinear relationships in governance and political stability.",
      "Column (4) tests whether MLS effectiveness varies nonlinearly with governance.",
      "Column (5) uses centered variables for easier interpretation.",
      "All specifications include country, year, and sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table H2-D DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-D DOCX (", e$message, ")\n")
})

tryCatch({
  models_h2d <- list(
    "(1) Nonlin 1" = nonlin_1,
    "(2) Nonlin 2" = nonlin_2,
    "(3) Nonlin 3" = nonlin_3,
    "(4) Nonlin 4" = nonlin_4,
    "(5) Nonlin 5" = nonlin_5
  )

  msdf_h2d <- modelsummary::modelsummary(
    models_h2d,
    output = "data.frame",
    coef_rename = c(
      "has_MLS" = "Multilateral Support (MLS)",
      "has_BS" = "Bilateral Support (BS)",
      "high_risk" = "High Risk Country",
      "has_MLS:high_risk" = "MLS × High Risk",
      "governance" = "Governance Index",
      "governance_sq" = "Governance²",
      "ps_percentile" = "Political Stability",
      "ps_percentile_sq" = "Political Stability²",
      "log_gdp_pc_lag1" = "Log GDP per capita (1-year lag)",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "sector_energy" = "Energy Sector",
      "sector_transport" = "Transport Sector",
      "sector_water" = "Water Sector"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01)
  )

  write.csv(
    msdf_h2d,
    file.path(output_dir, "h2_blended_finance_revised/tables/table_h2d_nonlinearity_results.csv"),
    row.names = FALSE
  )

  if (require("openxlsx", quietly = TRUE)) {
    openxlsx::write.xlsx(
      msdf_h2d,
      file.path(output_dir, "h2_blended_finance_revised/tables/table_h2d_nonlinearity_results.xlsx")
    )
  }

  cat("✓ Table H2-D CSV/XLSX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table H2-D CSV/XLSX (", e$message, ")\n")
})

################################################################################
# 7. VISUALIZATIONS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Figure 1: MLS effect by risk level
tryCatch({
  pred_data <- data.frame(
    has_MLS = c(0, 1, 0, 1),
    high_risk = c(0, 0, 1, 1),
    has_BS = 0,
    governance = mean(reg_data$governance, na.rm=T),
    # use lagged log GDP mean
    log_gdp_pc_lag1 = mean(reg_data$log_gdp_pc_lag1, na.rm=T),
    domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
    fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
    sector_energy = 1,
    sector_transport = 0,
    sector_water = 0
  ) %>%
    mutate(
      risk_cat = ifelse(high_risk == 1, "High Risk", "Low Risk"),
      mls_cat = ifelse(has_MLS == 1, "With MLS", "Without MLS")
    )
  
  pred_data$pred_log_inv <- predict(main_5, newdata = pred_data)
  pred_data$pred_inv_millions <- exp(pred_data$pred_log_inv) / 1e6
  
  fig1 <- ggplot(pred_data, aes(x = risk_cat, y = pred_inv_millions, fill = mls_cat)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("With MLS" = "#2ca02c", "Without MLS" = "#d62728"),
                     name = "") +
    labs(
      title = "Multilateral Support Effect on Investment by Country Risk",
      subtitle = "Predicted investment from full model (Table H2-A, Column 5)",
      x = "Country Risk Category",
      y = "Predicted Investment ($ millions)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "bottom")
  
  ggsave(file.path(output_dir, "h2_blended_finance_revised/figures/fig1_mls_by_risk.png"), 
         fig1, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 1 saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 1 (", e$message, ")\n")
})

# Figure 2: Governance quadratic relationship (if significant)
tryCatch({
  gov_pval <- summary(nonlin_1)$coeftable["governance_sq", "Pr(>|t|)"]
  
  if (gov_pval < 0.10) {
      pred_gov <- data.frame(
      governance = seq(0, 100, by = 1),
      has_MLS = 0,
      has_BS = 0,
      high_risk = 0,
        log_gdp_pc_lag1 = mean(reg_data$log_gdp_pc_lag1, na.rm=T),
      domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
      fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
      sector_energy = 1,
      sector_transport = 0,
      sector_water = 0
    ) %>%
      mutate(governance_sq = governance^2)
    
    pred_gov$pred_log_inv <- predict(nonlin_1, newdata = pred_gov)
    pred_gov$pred_inv_millions <- exp(pred_gov$pred_log_inv) / 1e6
    
    # Sample actual data
    sample_data <- reg_data %>%
      sample_n(min(2000, n())) %>%
      mutate(inv_millions = investment_real / 1e6)
    
    fig2 <- ggplot() +
      geom_point(data = sample_data, aes(x = governance, y = inv_millions),
                alpha = 0.2, size = 1, color = "gray50") +
      geom_line(data = pred_gov, aes(x = governance, y = pred_inv_millions),
               color = "#1f77b4", size = 1.5) +
      labs(
        title = "Nonlinear Relationship: Governance and Investment",
        subtitle = sprintf("Quadratic specification (β2 = %.6f, p = %.4f)", 
                          gov_sq, gov_pval),
        x = "Governance Index (0-100)",
        y = "Predicted Investment ($ millions)"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    
    # Add turning point if exists
    if ((gov_sq > 0 & gov_linear < 0) | (gov_sq < 0 & gov_linear > 0)) {
      turning_pt <- -gov_linear / (2 * gov_sq)
      if (turning_pt >= 0 & turning_pt <= 100) {
        fig2 <- fig2 + 
          geom_vline(xintercept = turning_pt, linetype = "dashed", color = "#d62728") +
          annotate("text", x = turning_pt, y = max(pred_gov$pred_inv_millions) * 0.95,
                  label = sprintf("Turning point: %.1f", turning_pt),
                  hjust = -0.1, color = "#d62728", size = 4)
      }
    }
    
    ggsave(file.path(output_dir, "h2_blended_finance_revised/figures/fig2_governance_quadratic.png"), 
           fig2, width = 10, height = 6, dpi = 300)
    cat("✓ Figure 2 (governance quadratic) saved successfully\n")
  } else {
    cat("⚠ Figure 2 (governance quadratic) skipped - no significant nonlinearity\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 2 (", e$message, ")\n")
})

# Figure 3: Political stability quadratic relationship (if significant)
tryCatch({
  ps_pval <- summary(nonlin_2)$coeftable["ps_percentile_sq", "Pr(>|t|)"]
  
  if (ps_pval < 0.10) {
    pred_ps <- data.frame(
      ps_percentile = seq(0, 100, by = 1),
      has_MLS = 0,
      has_BS = 0,
      high_risk = ifelse(seq(0, 100, by = 1) < 33, 1, 0),
      log_gdp_pc_lag1 = mean(reg_data$log_gdp_pc_lag1, na.rm=T),
      domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
      fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
      sector_energy = 1,
      sector_transport = 0,
      sector_water = 0
    ) %>%
      mutate(ps_percentile_sq = ps_percentile^2)
    
    pred_ps$pred_log_inv <- predict(nonlin_2, newdata = pred_ps)
    pred_ps$pred_inv_millions <- exp(pred_ps$pred_log_inv) / 1e6
    
    # Sample actual data
    sample_data <- reg_data %>%
      sample_n(min(2000, n())) %>%
      mutate(inv_millions = investment_real / 1e6)
    
    fig3 <- ggplot() +
      geom_point(data = sample_data, aes(x = ps_percentile, y = inv_millions),
                alpha = 0.2, size = 1, color = "gray50") +
      geom_line(data = pred_ps, aes(x = ps_percentile, y = pred_inv_millions),
               color = "#2ca02c", size = 1.5) +
      labs(
        title = "Nonlinear Relationship: Political Stability and Investment",
        subtitle = sprintf("Quadratic specification (β4 = %.6f, p = %.4f)", 
                          ps_sq, ps_pval),
        x = "Political Stability Percentile (0-100)",
        y = "Predicted Investment ($ millions)"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    
    # Add turning point if exists
    if ((ps_sq > 0 & ps_linear < 0) | (ps_sq < 0 & ps_linear > 0)) {
      turning_pt <- -ps_linear / (2 * ps_sq)
      if (turning_pt >= 0 & turning_pt <= 100) {
        fig3 <- fig3 + 
          geom_vline(xintercept = turning_pt, linetype = "dashed", color = "#d62728") +
          annotate("text", x = turning_pt, y = max(pred_ps$pred_inv_millions) * 0.95,
                  label = sprintf("Turning point: %.1f", turning_pt),
                  hjust = -0.1, color = "#d62728", size = 4)
      }
    }
    
    ggsave(file.path(output_dir, "h2_blended_finance_revised/figures/fig3_ps_quadratic.png"), 
           fig3, width = 10, height = 6, dpi = 300)
    cat("✓ Figure 3 (PS quadratic) saved successfully\n")
  } else {
    cat("⚠ Figure 3 (PS quadratic) skipped - no significant nonlinearity\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 3 (", e$message, ")\n")
})

# Figure 4: MLS effectiveness across governance levels (from nonlin_4)
tryCatch({
  # Check if MLS × Governance² is significant
  if ("has_MLS:governance_sq" %in% names(coef(nonlin_4))) {
    mls_gov_sq_pval <- summary(nonlin_4)$coeftable["has_MLS:governance_sq", "Pr(>|t|)"]
    
    if (mls_gov_sq_pval < 0.10) {
      pred_mls_gov <- expand.grid(
        governance = seq(0, 100, by = 5),
        has_MLS = c(0, 1),
        has_BS = 0,
        high_risk = 0,
        log_gdp_pc_lag1 = mean(reg_data$log_gdp_pc_lag1, na.rm=T),
        domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
        fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
        sector_energy = 1,
        sector_transport = 0,
        sector_water = 0
      ) %>%
        mutate(
          governance_sq = governance^2,
          mls_status = ifelse(has_MLS == 1, "With MLS", "Without MLS")
        )
      
      pred_mls_gov$pred_log_inv <- predict(nonlin_4, newdata = pred_mls_gov)
      pred_mls_gov$pred_inv_millions <- exp(pred_mls_gov$pred_log_inv) / 1e6
      
      fig4 <- ggplot(pred_mls_gov, aes(x = governance, y = pred_inv_millions, 
                                       color = mls_status, linetype = mls_status)) +
        geom_line(size = 1.2) +
        scale_color_manual(values = c("With MLS" = "#2ca02c", "Without MLS" = "#1f77b4"),
                          name = "") +
        scale_linetype_manual(values = c("With MLS" = "solid", "Without MLS" = "dashed"),
                             name = "") +
        labs(
          title = "MLS Effectiveness Varies Nonlinearly with Governance",
          subtitle = "How multilateral support impact changes across governance levels",
          x = "Governance Index (0-100)",
          y = "Predicted Investment ($ millions)"
        ) +
        theme_minimal(base_size = 12) +
        theme(plot.title = element_text(face = "bold"),
              legend.position = "bottom")
      
      ggsave(file.path(output_dir, "h2_blended_finance_revised/figures/fig4_mls_governance_interaction.png"), 
             fig4, width = 10, height = 6, dpi = 300)
      cat("✓ Figure 4 (MLS × Governance nonlinear) saved successfully\n")
    } else {
      cat("⚠ Figure 4 skipped - MLS × Governance² not significant\n")
    }
  } else {
    cat("⚠ Figure 4 skipped - MLS × Governance² term not in model\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 4 (", e$message, ")\n")
})

################################################################################
# 8. SUMMARY AND INTERPRETATION
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE - SUMMARY\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("REVISED ANALYSIS IMPROVEMENTS:\n")
cat("  1. Main results: MLS × Risk interaction\n")
cat("  2. Robustness checks across samples and specifications\n")
cat("  3. Country-year mechanism analysis\n")
cat("  4. Nonlinearity tests (governance and political stability)\n")
cat("  5. Test whether MLS effectiveness varies nonlinearly\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("  MLS effect (baseline):              %.3f\n", mls_coef))
cat(sprintf("  MLS × High Risk interaction:        %.3f\n", mls_risk_int))
cat(sprintf("  Total MLS effect in high-risk:      %.3f\n", mls_coef + mls_risk_int))
cat(sprintf("  BS effect:                          %.3f\n", bs_coef))

# Nonlinearity summary
if (exists("gov_sq") && !is.na(gov_sq)) {
  gov_pval <- summary(nonlin_1)$coeftable["governance_sq", "Pr(>|t|)"]
  cat(sprintf("  Governance squared (nonlinearity):  %.6f (p=%.4f)\n", gov_sq, gov_pval))
}
if (exists("ps_sq") && !is.na(ps_sq)) {
  ps_pval <- summary(nonlin_2)$coeftable["ps_percentile_sq", "Pr(>|t|)"]
  cat(sprintf("  PS squared (nonlinearity):          %.6f (p=%.4f)\n", ps_sq, ps_pval))
}

cat("\nHYPOTHESIS TESTING:\n")
cat("  Null Hypothesis: Blended finance has NO differential effect in high-risk countries\n")

mls_risk_pval <- summary(main_5)$coeftable["has_MLS:high_risk", "Pr(>|t|)"]
cat(sprintf("  MLS × High Risk p-value: %.4f ", mls_risk_pval))
if (mls_risk_pval < 0.05) {
  cat("*** SIGNIFICANT at 5%\n")
} else if (mls_risk_pval < 0.1) {
  cat("** SIGNIFICANT at 10%\n")
} else {
  cat("NOT SIGNIFICANT\n")
}

if (mls_risk_int != 0 & mls_risk_pval < 0.05) {
  if (mls_risk_int > 0) {
    cat("\n✓✓✓ HYPOTHESIS 2 SUPPORTED ✓✓✓\n")
    cat("Multilateral support has STRONGER positive effect in high-risk countries.\n")
    cat("We REJECT the null hypothesis at the 5% level.\n")
  } else {
    cat("\n✓ HYPOTHESIS 2 PARTIALLY SUPPORTED\n")
    cat("Multilateral support has WEAKER effect in high-risk countries.\n")
  }
} else {
  cat("\n✗ HYPOTHESIS 2 NOT SUPPORTED\n")
  cat("No significant differential effect of blended finance by country risk.\n")
}

cat("\nNONLINEARITY INTERPRETATION:\n")
if (exists("gov_pval") && gov_pval < 0.10) {
  if (gov_sq > 0 & gov_linear < 0) {
    cat("  → GOVERNANCE: U-shaped (threshold effect)\n")
    cat("     Governance must exceed minimum to catalyze investment\n")
  } else if (gov_sq < 0 & gov_linear > 0) {
    cat("  → GOVERNANCE: Inverted U (diminishing returns)\n")
    cat("     Additional governance improvements less effective at high levels\n")
  }
}

if (exists("ps_pval") && ps_pval < 0.10) {
  if (ps_sq > 0 & ps_linear < 0) {
    cat("  → POLITICAL STABILITY: U-shaped (threshold effect)\n")
    cat("     Stability must exceed minimum to attract investment\n")
  } else if (ps_sq < 0 & ps_linear > 0) {
    cat("  → POLITICAL STABILITY: Inverted U (diminishing returns)\n")
    cat("     Returns to stability plateau at high levels\n")
  }
}

cat("\nFILES SAVED TO:\n")
cat("  ", output_dir, "/h2_blended_finance_revised/\n", sep = "")

cat("\n", rep("=", 80), "\n", sep = "")
cat("INTERPRETATION NOTES:\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("1. MLS × RISK INTERACTION:\n")
cat("   - Positive interaction: MLS more effective in high-risk countries\n")
cat("   - Negative interaction: MLS less effective in high-risk countries\n")
cat("   - Tests whether blended finance catalyzes investment where most needed\n\n")

cat("2. NONLINEARITY IN GOVERNANCE:\n")
cat("   - U-shaped: Governance must exceed threshold for investment\n")
cat("   - Inverted U: Diminishing returns to governance improvements\n")
cat("   - Implications for targeting blended finance mechanisms\n\n")

cat("3. POLICY IMPLICATIONS:\n")
if (mls_coef + mls_risk_int > mls_coef) {
  cat("   - Multilateral support MOST effective in high-risk countries\n")
  cat("   - Priority: Scale MLS in politically unstable environments\n")
  cat("   - Mechanism: Risk-sharing and credibility signaling\n")
} else if (abs(mls_coef) > abs(bs_coef)) {
  cat("   - Multilateral support more effective than bilateral\n")
  cat("   - Priority: Multilateral institutions for infrastructure financing\n")
  cat("   - Potential: Coordination benefits and reduced political capture\n")
} else {
  cat("   - Bilateral support comparable to multilateral\n")
  cat("   - Both mechanisms play important catalytic roles\n")
}

cat("\n", rep("=", 80), "\n\n", sep = "")