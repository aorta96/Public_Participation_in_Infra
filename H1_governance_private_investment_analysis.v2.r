################################################################################
# HYPOTHESIS 1: GOVERNANCE AND PRIVATE INFRASTRUCTURE INVESTMENT - REVISED
# Progressive Model Building for Publication
#
# Research Question: Is private infrastructure investment positively and 
#                    significantly associated with higher scores in regulatory 
#                    quality and rule of law?
#
# Null Hypothesis: Improved governance has no impact on private sector 
#                  participation in infrastructure investment.
#
# Alternative Hypothesis: Higher governance rating is positively associated 
#                         with increases in private sector participation in 
#                         infrastructure investment.
#
# REVISION: This version properly decomposes governance into RQ and RoL
#           with interaction terms to identify individual and mixed effects.
#
# Model Building Strategy:
#   Table 1 (Main Results): Progressive specification with decomposed governance
#     (1) Baseline: RQ + RoL separately + FEs
#     (2) + Economic controls (GDP, credit, FDI)
#     (3) + Sector FEs
#     (4) + RQ × RoL interaction (test complementarity)
#     (5) + Risk interactions (RQ × Risk + RoL × Risk) [preferred specification]
#
#   Table 2 (Robustness): Alternative specifications
#     - Composite governance measure (for comparison)
#     - Alternative DVs and samples
#
#   Table 3 (Mechanisms): Deep dive into governance interactions
#     - RQ × RoL interaction
#     - Heterogeneous effects by risk, sector, project type
################################################################################

# Load required packages
library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(ggplot2)
library(scales)

# Install pandoc if needed (for table formatting)
if (!require("rmarkdown", quietly = TRUE)) {
  install.packages("rmarkdown")
  library(rmarkdown)
}

# Set output directory
output_dir <- "C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Analysis Output"

# Create output directories
dir.create(file.path(output_dir, "h1_governance_revised"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h1_governance_revised/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h1_governance_revised/figures"), recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1. DATA PREPARATION
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 1: GOVERNANCE AND PRIVATE INFRASTRUCTURE INVESTMENT (REVISED)\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("Loading data...\n")

# Load data
ppi <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/PPI_2005_2023.csv")
wb <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/WB_merged_2005_2023.csv")

# Create project-level dataset
cat("Preparing project-level data...\n")

df_projects <- ppi %>%
  filter(!is.na(investment_real), investment_real > 0) %>%
  mutate(
    # Dependent variables
    private_share = private / 100,  # Convert to proportion
    log_investment = log(investment_real),
    log_private_investment = log(investment_real * private_share),
    
    # Blended finance indicators (controls)
    has_MLS = ifelse(MLS == "With MLS", 1, 0),
    has_BS = ifelse(BS %in% c("With BS", "With BLS"), 1, 0),
    has_blended = ifelse(has_MLS == 1 | has_BS == 1, 1, 0),
    
    # Sector dummies
    sector_energy = ifelse(sector == "Energy", 1, 0),
    sector_transport = ifelse(sector == "Transport", 1, 0),
    sector_water = ifelse(sector == "Water and sewerage", 1, 0),
    sector_ict = ifelse(sector == "Information and communications technology", 1, 0),
    
    # Project characteristics
    is_greenfield = ifelse(type == "Greenfield project", 1, 0),
    is_renewable = ifelse(sector == "Energy" & Renewables == "Renewables", 1, 0)
  ) %>%
  
  # Merge with World Bank data
  left_join(wb, by = c("country_code" = "country", "year" = "year")) %>%
  
  # Create governance and control variables
  mutate(
    # PRIMARY GOVERNANCE VARIABLES (H1 focus) - KEEP SEPARATE
    regulatory_quality = Regulatory_Quality_percentile,
    rule_of_law = Rule_of_Law_percentile,
    
    # Create interaction term
    rq_rol_interaction = regulatory_quality * rule_of_law,
    
    # Governance composite (for robustness comparison only)
    governance_h1 = (regulatory_quality + rule_of_law) / 2,
    
    # Alternative: Full governance composite (for robustness)
    governance_full = (Control_of_Corruption_percentile + 
                       Government_Effectiveness_percentile +
                       Political_Stability_percentile +
                       Rule_of_Law_percentile +
                       Regulatory_Quality_percentile +
                       Voice_and_Accountability_percentile) / 6,
    
    # Governance categories
    high_governance = ifelse(governance_h1 >= 67, 1, 0),
    low_governance = ifelse(governance_h1 < 33, 1, 0),
    
    # Economic variables
    log_gdp_pc = if_else(!is.na(GDP_per_capita_constant_2015_USD) & 
                         GDP_per_capita_constant_2015_USD > 0,
                         log(GDP_per_capita_constant_2015_USD), NA_real_),
    domestic_credit = Domestic_credit_to_private_sector_pct_GDP,
    fdi_gdp = FDI_Inflows_pct_GDP,
    
    # Risk measure: Political Stability percentile
    ps_percentile = Political_Stability_percentile,
    high_risk = ifelse(ps_percentile < 33, 1, 0)
  ) %>%
  
  # Keep complete cases for main variables
  filter(!is.na(private_share), !is.na(regulatory_quality), !is.na(rule_of_law))

cat(sprintf("\nProject-level sample: %d projects\n", nrow(df_projects)))
cat(sprintf("  - Countries: %d\n", n_distinct(df_projects$country_code)))
cat(sprintf("  - Years: %d to %d\n", min(df_projects$year, na.rm=T), 
            max(df_projects$year, na.rm=T)))
cat(sprintf("  - Mean private share: %.1f%%\n", 100*mean(df_projects$private_share, na.rm=T)))
cat(sprintf("  - Mean Regulatory Quality: %.1f\n", 
            mean(df_projects$regulatory_quality, na.rm=T)))
cat(sprintf("  - Mean Rule of Law: %.1f\n", 
            mean(df_projects$rule_of_law, na.rm=T)))
cat(sprintf("  - Correlation (RQ, RoL): %.3f\n\n", 
            cor(df_projects$regulatory_quality, df_projects$rule_of_law, use="complete.obs")))

################################################################################
# 2. DESCRIPTIVE STATISTICS
################################################################################

cat("Generating descriptive statistics...\n")

# Correlation matrix for governance components
gov_cor <- df_projects %>%
  select(regulatory_quality, rule_of_law, 
         Control_of_Corruption_percentile, 
         Government_Effectiveness_percentile,
         Political_Stability_percentile,
         Voice_and_Accountability_percentile) %>%
  cor(use = "complete.obs")

write.csv(gov_cor, file.path(output_dir, "h1_governance_revised/tables/governance_correlation_matrix.csv"))

cat("\nGovernance Component Correlations:\n")
cat(sprintf("  RQ-RoL correlation: %.3f\n", gov_cor["regulatory_quality", "rule_of_law"]))
cat(sprintf("  (High correlation suggests potential multicollinearity)\n\n"))

# Summary statistics
summary_vars <- df_projects %>%
  select(
    private_share,
    investment_real,
    regulatory_quality,
    rule_of_law,
    log_gdp_pc,
    domestic_credit,
    fdi_gdp,
    has_blended
  ) %>%
  summarise(
    across(everything(), 
           list(mean = ~mean(., na.rm=T), 
                sd = ~sd(., na.rm=T),
                min = ~min(., na.rm=T),
                max = ~max(., na.rm=T)),
           .names = "{.col}_{.fn}")
  )

write_csv(summary_vars, file.path(output_dir, "h1_governance_revised/tables/summary_statistics.csv"))

################################################################################
# 3. TABLE 1: MAIN RESULTS - PROGRESSIVE SPECIFICATION WITH DECOMPOSED GOVERNANCE
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE 1: MAIN RESULTS - PROGRESSIVE SPECIFICATION\n")
cat(rep("=", 80), "\n\n", sep = "")

# Prepare regression data
reg_data <- df_projects %>%
  filter(!is.na(private_share), 
         !is.na(regulatory_quality), 
         !is.na(rule_of_law),
         !is.na(log_gdp_pc),
         !is.na(domestic_credit),
         !is.na(fdi_gdp),
         !is.na(log_investment))

cat(sprintf("Regression sample: %d projects\n\n", nrow(reg_data)))

# (1) Baseline: RQ + RoL separately with FEs
main_1 <- feols(
  private_share ~ regulatory_quality + rule_of_law | country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (2) Add economic controls
main_2 <- feols(
  private_share ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (3) Add sector FEs and project controls
main_3 <- feols(
  private_share ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (4) Add RQ × RoL interaction (TEST COMPLEMENTARITY)
main_4 <- feols(
  private_share ~ regulatory_quality * rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (5) Add risk interactions (PREFERRED SPECIFICATION)
main_5 <- feols(
  private_share ~ regulatory_quality * high_risk + rule_of_law * high_risk + 
    log_gdp_pc + domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# ============================================================================
# MULTICOLLINEARITY TEST: VARIANCE INFLATION FACTOR (VIF)
# Testing RQ and RoL correlation to justify separate coefficients
# ============================================================================

cat("\n", rep("=", 80), "\n", sep = "")
cat("MULTICOLLINEARITY DIAGNOSTICS: VIF TEST\n")
cat("Testing correlation between Regulatory Quality and Rule of Law\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create data for VIF calculation
vif_data <- reg_data %>%
  select(regulatory_quality, rule_of_law, log_gdp_pc, domestic_credit, fdi_gdp) %>%
  na.omit()

# Calculate VIF manually: VIF = 1 / (1 - R^2)
# VIF for regulatory_quality (predicted by other covariates)
vif_rq_model <- lm(regulatory_quality ~ rule_of_law + log_gdp_pc + domestic_credit + fdi_gdp, 
                    data = vif_data)
vif_rq <- 1 / (1 - summary(vif_rq_model)$r.squared)

# VIF for rule_of_law (predicted by other covariates)
vif_rol_model <- lm(rule_of_law ~ regulatory_quality + log_gdp_pc + domestic_credit + fdi_gdp, 
                     data = vif_data)
vif_rol <- 1 / (1 - summary(vif_rol_model)$r.squared)

# VIF for log_gdp_pc
vif_gdp_model <- lm(log_gdp_pc ~ regulatory_quality + rule_of_law + domestic_credit + fdi_gdp, 
                     data = vif_data)
vif_gdp <- 1 / (1 - summary(vif_gdp_model)$r.squared)

cat("Variance Inflation Factors (VIF):\n")
cat("VIF > 5 suggests potential multicollinearity concerns\n")
cat("VIF > 10 indicates serious multicollinearity\n\n")
cat(sprintf("  Regulatory Quality:  %.4f\n", vif_rq))
cat(sprintf("  Rule of Law:         %.4f\n", vif_rol))
cat(sprintf("  Log GDP per capita:  %.4f\n\n", vif_gdp))

# Correlation test between RQ and RoL
rq_rol_corr <- cor(vif_data$regulatory_quality, vif_data$rule_of_law)
rq_rol_test <- cor.test(vif_data$regulatory_quality, vif_data$rule_of_law)

cat(sprintf("Pearson Correlation between RQ and RoL: %.4f\n", rq_rol_corr))
cat(sprintf("Correlation t-statistic: %.4f\n", rq_rol_test$statistic))
cat(sprintf("Correlation p-value: %.6f\n", rq_rol_test$p.value))
cat(sprintf("Sample size: %d\n\n", nrow(vif_data)))

# Interpretation
if (vif_rq > 10 | vif_rol > 10) {
  cat("⚠ WARNING: Potential serious multicollinearity (VIF > 10)\n")
} else if (vif_rq > 5 | vif_rol > 5) {
  cat("⚠ Note: Moderate multicollinearity detected (VIF > 5)\n")
} else {
  cat("✓ Low multicollinearity: VIF values are acceptable\n")
}

# Extract key coefficients
rq_coef <- coef(main_3)["regulatory_quality"]
rol_coef <- coef(main_3)["rule_of_law"]
rq_rol_int <- coef(main_4)["regulatory_quality:rule_of_law"]

cat("\nMain specification results:\n")
cat(sprintf("  Regulatory Quality (Col 3):     %.5f\n", rq_coef))
cat(sprintf("  Rule of Law (Col 3):            %.5f\n", rol_coef))
cat(sprintf("  RQ × RoL Interaction (Col 4):   %.5f\n", rq_rol_int))
cat(sprintf("  RQ × High Risk (Col 5):         %.5f\n", 
            coef(main_5)["regulatory_quality:high_risk"]))
cat(sprintf("  RoL × High Risk (Col 5):        %.5f\n\n", 
            coef(main_5)["rule_of_law:high_risk"]))


# Save Table 1
tryCatch({
  modelsummary(
    list(
      "(1) Baseline" = main_1,
      "(2) + Economic" = main_2,
      "(3) + Sector FEs" = main_3,
      "(4) + RQ×RoL" = main_4,
      "(5) + Risk Int." = main_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table1_main_results.html"),
    title = "Table H1: Main Results - Governance Components and Private Investment",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "regulatory_quality:high_risk" = "RQ × High Risk",
      "rule_of_law:high_risk" = "RoL × High Risk",
      "high_risk" = "High Risk Country",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment (0-1).",
      "Progressive specification builds from baseline to full model.",
      "Column (4) tests complementarity between RQ and RoL.",
      "Column (5) tests heterogeneous effects by country risk level.",
      "All specifications include country and year fixed effects.",
      "Columns (3)-(5) include sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 1 HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 1 HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) Baseline" = main_1,
      "(2) + Economic" = main_2,
      "(3) + Sector FEs" = main_3,
      "(4) + RQ×RoL" = main_4,
      "(5) + Risk Int." = main_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table1_main_results.docx"),
    title = "Table H1: Main Results - Governance Components and Private Investment",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "regulatory_quality:high_risk" = "RQ × High Risk",
      "rule_of_law:high_risk" = "RoL × High Risk",
      "high_risk" = "High Risk Country",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment (0-1).",
      "Progressive specification builds from baseline to full model.",
      "Column (4) tests complementarity between RQ and RoL.",
      "Column (5) tests heterogeneous effects by country risk level.",
      "All specifications include country and year fixed effects.",
      "Columns (3)-(5) include sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 1 DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 1 DOCX (", e$message, ")\n")
})

################################################################################
# 4. TABLE 2: ROBUSTNESS CHECKS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE 2: ROBUSTNESS CHECKS\n")
cat(rep("=", 80), "\n\n", sep = "")

# (1) Use composite governance for comparison
robust_1 <- feols(
  private_share ~ governance_h1 * high_risk + log_gdp_pc + domestic_credit + 
    fdi_gdp + has_blended + log_investment | country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (2) Alternative DV: Log private investment amount
robust_2 <- feols(
  log_private_investment ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data %>% filter(private_share > 0),
  vcov = ~country_code
)

# (3) Exclude 100% private projects (address ceiling effect)
robust_3 <- feols(
  private_share ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data %>% filter(private_share < 1),
  vcov = ~country_code
)

# (4) Energy sector only (largest sector)
robust_4 <- feols(
  private_share ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year,
  data = reg_data %>% filter(sector == "Energy"),
  vcov = ~country_code
)

# (5) Two-way clustering (country + year)
robust_5 <- feols(
  private_share ~ regulatory_quality + rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code + year
)

cat("Robustness check results:\n")
cat(sprintf("  (1) Composite governance:    %.5f\n", coef(robust_1)["governance_h1"]))
cat(sprintf("  (2) Log private investment:  RQ=%.5f, RoL=%.5f\n", 
            coef(robust_2)["regulatory_quality"], coef(robust_2)["rule_of_law"]))
cat(sprintf("  (3) Exclude 100%% private:    RQ=%.5f, RoL=%.5f\n", 
            coef(robust_3)["regulatory_quality"], coef(robust_3)["rule_of_law"]))
cat(sprintf("  (4) Energy sector only:      RQ=%.5f, RoL=%.5f\n", 
            coef(robust_4)["regulatory_quality"], coef(robust_4)["rule_of_law"]))
cat(sprintf("  (5) Two-way clustering:      RQ=%.5f, RoL=%.5f\n\n", 
            coef(robust_5)["regulatory_quality"], coef(robust_5)["rule_of_law"]))

# Save Table 2
tryCatch({
  modelsummary(
    list(
      "(1) Composite" = robust_1,
      "(2) Log Priv Inv" = robust_2,
      "(3) Exclude 100%" = robust_3,
      "(4) Energy Only" = robust_4,
      "(5) 2-Way SE" = robust_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table2_robustness.html"),
    title = "Table H1-B: Robustness Checks",
    coef_rename = c(
      "governance_h1" = "Governance (RQ+RoL)/2",
      "governance_h1:high_risk" = "Governance × High Risk",
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "high_risk" = "High Risk Country",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Column (1): Uses composite governance measure for comparison with decomposed approach.",
      "Column (2): DV is log private investment amount instead of share.",
      "Column (3): Excludes projects with 100% private share to address ceiling effects.",
      "Column (4): Energy sector subsample only.",
      "Column (5): Two-way clustering (country + year) for standard errors."
    )
  )
  cat("✓ Table 2 HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 2 HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) Composite" = robust_1,
      "(2) Log Priv Inv" = robust_2,
      "(3) Exclude 100%" = robust_3,
      "(4) Energy Only" = robust_4,
      "(5) 2-Way SE" = robust_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table2_robustness.docx"),
    title = "Table H1-B: Robustness Checks",
    coef_rename = c(
      "governance_h1" = "Governance (RQ+RoL)/2",
      "governance_h1:high_risk" = "Governance × High Risk",
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "high_risk" = "High Risk Country",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Column (1): Uses composite governance measure for comparison with decomposed approach.",
      "Column (2): DV is log private investment amount instead of share.",
      "Column (3): Excludes projects with 100% private share to address ceiling effects.",
      "Column (4): Energy sector subsample only.",
      "Column (5): Two-way clustering (country + year) for standard errors."
    )
  )
  cat("✓ Table 2 DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 2 DOCX (", e$message, ")\n")
})

################################################################################
# 5. TABLE 3: MECHANISMS - DEEP DIVE INTO GOVERNANCE INTERACTIONS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE 3: MECHANISMS - GOVERNANCE INTERACTION EFFECTS\n")
cat(rep("=", 80), "\n\n", sep = "")

# (1) RQ × RoL interaction (CRITICAL: tests complementarity)
mech_1 <- feols(
  private_share ~ regulatory_quality * rule_of_law + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (2) Three-way interaction: RQ × RoL × High Risk
mech_2 <- feols(
  private_share ~ regulatory_quality * rule_of_law * high_risk + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (3) Sector heterogeneity: RQ × RoL × Energy
mech_3 <- feols(
  private_share ~ regulatory_quality * rule_of_law * sector_energy + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year,
  data = reg_data,
  vcov = ~country_code
)

# (4) Project type: RQ × RoL for Greenfield vs non-Greenfield
mech_4 <- feols(
  private_share ~ regulatory_quality * rule_of_law * is_greenfield + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (5) Renewable energy: RQ × RoL for renewable projects
mech_5 <- feols(
  private_share ~ regulatory_quality * rule_of_law * is_renewable + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year,
  data = reg_data %>% filter(sector == "Energy"),
  vcov = ~country_code
)

cat("Mechanism results:\n")
cat(sprintf("  RQ × RoL (baseline):                    %.6f\n", 
            coef(mech_1)["regulatory_quality:rule_of_law"]))
cat(sprintf("  RQ × RoL × High Risk (3-way):           %.6f\n", 
            coef(mech_2)["regulatory_quality:rule_of_law:high_risk"]))
cat(sprintf("  RQ × RoL × Energy (3-way):              %.6f\n", 
            coef(mech_3)["regulatory_quality:rule_of_law:sector_energy"]))
cat(sprintf("  RQ × RoL × Greenfield (3-way):          %.6f\n", 
            coef(mech_4)["regulatory_quality:rule_of_law:is_greenfield"]))
cat(sprintf("  RQ × RoL × Renewable (3-way, Energy):   %.6f\n\n", 
            coef(mech_5)["regulatory_quality:rule_of_law:is_renewable"]))

# Save Table 3
tryCatch({
  modelsummary(
    list(
      "(1) RQ×RoL" = mech_1,
      "(2) ×Risk" = mech_2,
      "(3) ×Energy" = mech_3,
      "(4) ×Greenfield" = mech_4,
      "(5) ×Renewable" = mech_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table3_mechanisms.html"),
    title = "Table H1-C: Mechanisms - Governance Interaction Effects",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "regulatory_quality:rule_of_law:high_risk" = "RQ × RoL × High Risk",
      "regulatory_quality:rule_of_law:sector_energy" = "RQ × RoL × Energy",
      "regulatory_quality:rule_of_law:is_greenfield" = "RQ × RoL × Greenfield",
      "regulatory_quality:rule_of_law:is_renewable" = "RQ × RoL × Renewable",
      "high_risk" = "High Risk Country",
      "sector_energy" = "Energy Sector",
      "is_greenfield" = "Greenfield Project",
      "is_renewable" = "Renewable Energy",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment.",
      "All specifications test whether RQ and RoL are complements or substitutes.",
      "Column (1): Base RQ × RoL interaction.",
      "Columns (2)-(5): Three-way interactions test heterogeneous complementarity.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 3 HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 3 HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) RQ×RoL" = mech_1,
      "(2) ×Risk" = mech_2,
      "(3) ×Energy" = mech_3,
      "(4) ×Greenfield" = mech_4,
      "(5) ×Renewable" = mech_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table3_mechanisms.docx"),
    title = "Table H1-C: Mechanisms - Governance Interaction Effects",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality",
      "rule_of_law" = "Rule of Law",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "regulatory_quality:rule_of_law:high_risk" = "RQ × RoL × High Risk",
      "regulatory_quality:rule_of_law:sector_energy" = "RQ × RoL × Energy",
      "regulatory_quality:rule_of_law:is_greenfield" = "RQ × RoL × Greenfield",
      "regulatory_quality:rule_of_law:is_renewable" = "RQ × RoL × Renewable",
      "high_risk" = "High Risk Country",
      "sector_energy" = "Energy Sector",
      "is_greenfield" = "Greenfield Project",
      "is_renewable" = "Renewable Energy",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment.",
      "All specifications test whether RQ and RoL are complements or substitutes.",
      "Column (1): Base RQ × RoL interaction.",
      "Columns (2)-(5): Three-way interactions test heterogeneous complementarity.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 3 DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 3 DOCX (", e$message, ")\n")
})

################################################################################
# 6. NONLINEARITY TESTS - SQUARED TERMS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("TABLE 4: NONLINEARITY TESTS - QUADRATIC SPECIFICATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Create squared terms
reg_data <- reg_data %>%
  mutate(
    rq_squared = regulatory_quality^2,
    rol_squared = rule_of_law^2,
    # Centered versions (for interpretation)
    rq_centered = regulatory_quality - mean(regulatory_quality, na.rm=T),
    rol_centered = rule_of_law - mean(rule_of_law, na.rm=T),
    rq_centered_sq = rq_centered^2,
    rol_centered_sq = rol_centered^2
  )

cat("Testing for nonlinear relationships:\n")
cat("  H0: Linear relationship (β2 = 0)\n")
cat("  HA: Nonlinear relationship (β2 ≠ 0)\n\n")

# (1) RQ with quadratic term only
nonlin_1 <- feols(
  private_share ~ regulatory_quality + rq_squared + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (2) RoL with quadratic term only
nonlin_2 <- feols(
  private_share ~ rule_of_law + rol_squared + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (3) Both with quadratic terms
nonlin_3 <- feols(
  private_share ~ regulatory_quality + rq_squared + rule_of_law + rol_squared +
    log_gdp_pc + domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (4) Quadratic + interaction (full nonlinear specification)
nonlin_4 <- feols(
  private_share ~ regulatory_quality + rq_squared + rule_of_law + rol_squared +
    regulatory_quality * rule_of_law + log_gdp_pc + domestic_credit + 
    fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# (5) Centered versions (for easier interpretation)
nonlin_5 <- feols(
  private_share ~ rq_centered + rq_centered_sq + rol_centered + rol_centered_sq +
    log_gdp_pc + domestic_credit + fdi_gdp + has_blended + log_investment | 
    country_code + year + sector,
  data = reg_data,
  vcov = ~country_code
)

# Extract coefficients
rq_linear <- coef(nonlin_1)["regulatory_quality"]
rq_squared_coef <- coef(nonlin_1)["rq_squared"]
rol_linear <- coef(nonlin_2)["rule_of_law"]
rol_squared_coef <- coef(nonlin_2)["rol_squared"]

cat("Nonlinearity test results:\n")
cat(sprintf("  RQ linear term (β1):          %.6f\n", rq_linear))
cat(sprintf("  RQ squared term (β2):         %.6f\n", rq_squared_coef))
cat(sprintf("  RoL linear term (β3):         %.6f\n", rol_linear))
cat(sprintf("  RoL squared term (β4):        %.6f\n\n", rol_squared_coef))

# Interpret nonlinearity
cat("INTERPRETATION:\n")

# RQ interpretation
if (!is.na(rq_squared_coef)) {
  rq_pval <- summary(nonlin_1)$coeftable["rq_squared", "Pr(>|t|)"]
  cat(sprintf("  Regulatory Quality (p=%.4f):\n", rq_pval))
  
  if (rq_pval < 0.05) {
    if (rq_squared_coef > 0 & rq_linear < 0) {
      # Calculate turning point
      turning_point <- -rq_linear / (2 * rq_squared_coef)
      cat(sprintf("    ✓ U-SHAPED relationship (turning point: %.1f)\n", turning_point))
      cat("    → Private investment DECREASES at low RQ, then INCREASES\n")
      cat("    → Suggests threshold effect: RQ must exceed minimum to attract investment\n")
    } else if (rq_squared_coef < 0 & rq_linear > 0) {
      turning_point <- -rq_linear / (2 * rq_squared_coef)
      cat(sprintf("    ✓ INVERTED U-SHAPED (diminishing returns, peak: %.1f)\n", turning_point))
      cat("    → Private investment INCREASES then DECREASES\n")
      cat("    → Diminishing or even negative returns at very high RQ levels\n")
    } else if (rq_squared_coef < 0 & rq_linear < 0) {
      cat("    ✓ NEGATIVE with DIMINISHING negative effect\n")
      cat("    → Always negative but becomes less negative at higher levels\n")
    } else {
      cat("    ✓ POSITIVE with ACCELERATING effect\n")
      cat("    → Increasing returns: each additional unit of RQ has greater impact\n")
    }
  } else {
    cat("    ✗ NO significant nonlinearity (relationship appears LINEAR)\n")
  }
}

cat("\n")

# ============================================================================
# EXPORT: Write all model coefficient tables to CSV/XLSX
# This scans the workspace for models (fixest, feglm, lm) and saves tidy outputs
# ============================================================================
cat("\nExporting model coefficients to CSV/XLSX...\n")
all_objs <- ls()
model_names <- Filter(function(n) {
  obj <- get(n)
  inherits(obj, c("fixest", "feglm", "lm"))
}, all_objs)
out_dir_tables <- file.path(output_dir, "h1_governance_revised/tables")
dir.create(out_dir_tables, recursive = TRUE, showWarnings = FALSE)
if(length(model_names) > 0){
  for(nm in model_names){
    m <- get(nm)
    df_t <- NULL
    # Primary: use broom::tidy if available
    if(requireNamespace("broom", quietly = TRUE)){
      tidy_ok <- try({
        df_t <- broom::tidy(m, conf.int = TRUE)
      }, silent = TRUE)
      if(inherits(tidy_ok, "try-error")) df_t <- NULL
    }
    # Fallback: construct tidy table from coefficients and vcov
    if(is.null(df_t)){
      coefs <- tryCatch(coef(m), error = function(e) NULL)
      if(!is.null(coefs)){
        # ensure vector/named
        coefs_vec <- as.numeric(coefs)
        names_vec <- names(coefs)
        se_vec <- tryCatch({
          vc <- vcov(m)
          se <- sqrt(diag(vc))
          as.numeric(se)
        }, error = function(e) rep(NA_real_, length(coefs_vec)))
        df_t <- data.frame(
          term = names_vec,
          estimate = coefs_vec,
          std.error = se_vec,
          conf.low = ifelse(!is.na(se_vec), coefs_vec - 1.96 * se_vec, NA_real_),
          conf.high = ifelse(!is.na(se_vec), coefs_vec + 1.96 * se_vec, NA_real_),
          stringsAsFactors = FALSE
        )
      }
    }
    if(!is.null(df_t)){
      df_t$.model <- nm
      write_csv(df_t, file.path(out_dir_tables, paste0(nm, "_coefficients.csv")))
      if(requireNamespace("openxlsx", quietly = TRUE)){
        openxlsx::write.xlsx(df_t, file.path(out_dir_tables, paste0(nm, "_coefficients.xlsx")))
      }
    } else {
      # Nothing to write for this model
      cat(sprintf("Could not extract tidy coefficients for model: %s\n", nm))
    }
  }
  cat(sprintf("Saved %d model coefficient files to %s\n", length(model_names), out_dir_tables))
} else {
  cat("No models found to export.\n")
}


# RoL interpretation
if (!is.na(rol_squared_coef)) {
  rol_pval <- summary(nonlin_2)$coeftable["rol_squared", "Pr(>|t|)"]
  cat(sprintf("  Rule of Law (p=%.4f):\n", rol_pval))
  
  if (rol_pval < 0.05) {
    if (rol_squared_coef > 0 & rol_linear < 0) {
      turning_point <- -rol_linear / (2 * rol_squared_coef)
      cat(sprintf("    ✓ U-SHAPED relationship (turning point: %.1f)\n", turning_point))
      cat("    → Private investment DECREASES at low RoL, then INCREASES\n")
      cat("    → Suggests threshold effect: RoL must exceed minimum to attract investment\n")
    } else if (rol_squared_coef < 0 & rol_linear > 0) {
      turning_point <- -rol_linear / (2 * rol_squared_coef)
      cat(sprintf("    ✓ INVERTED U-SHAPED (diminishing returns, peak: %.1f)\n", turning_point))
      cat("    → Private investment INCREASES then DECREASES\n")
      cat("    → Diminishing or even negative returns at very high RoL levels\n")
    } else if (rol_squared_coef < 0 & rol_linear < 0) {
      cat("    ✓ NEGATIVE with DIMINISHING negative effect\n")
      cat("    → Always negative but becomes less negative at higher levels\n")
    } else {
      cat("    ✓ POSITIVE with ACCELERATING effect\n")
      cat("    → Increasing returns: each additional unit of RoL has greater impact\n")
    }
  } else {
    cat("    ✗ NO significant nonlinearity (relationship appears LINEAR)\n")
  }
}

cat("\n")

# Joint test for nonlinearity
cat("JOINT TEST FOR NONLINEARITY:\n")
cat("  Testing H0: Both squared terms = 0\n")

# Wald test for joint significance
tryCatch({
  wald_test <- wald(nonlin_3, c("rq_squared", "rol_squared"))
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
cat(sprintf("  Linear model (Table 1, Col 3):  %.1f\n", AIC(main_3)))
cat(sprintf("  Quadratic RQ only:              %.1f\n", AIC(nonlin_1)))
cat(sprintf("  Quadratic RoL only:             %.1f\n", AIC(nonlin_2)))
cat(sprintf("  Both quadratic:                 %.1f\n", AIC(nonlin_3)))
cat(sprintf("  Quadratic + interaction:        %.1f\n", AIC(nonlin_4)))
cat("\n  → Lower AIC = Better fit\n\n")

# Save Table 4
tryCatch({
  modelsummary(
    list(
      "(1) RQ²" = nonlin_1,
      "(2) RoL²" = nonlin_2,
      "(3) Both²" = nonlin_3,
      "(4) + Interact" = nonlin_4,
      "(5) Centered" = nonlin_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table4_nonlinearity.html"),
    title = "Table H1-D: Nonlinearity Tests - Quadratic Specifications",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality (linear)",
      "rq_squared" = "RQ² (quadratic)",
      "rule_of_law" = "Rule of Law (linear)",
      "rol_squared" = "RoL² (quadratic)",
      "rq_centered" = "RQ (centered)",
      "rq_centered_sq" = "RQ² (centered)",
      "rol_centered" = "RoL (centered)",
      "rol_centered_sq" = "RoL² (centered)",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared", "aic"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment.",
      "Tests for nonlinear (quadratic) relationships between governance and private investment.",
      "Positive β2 with negative β1 → U-shaped (threshold effect).",
      "Negative β2 with positive β1 → Inverted U-shaped (diminishing returns).",
      "Column (5) uses centered variables for easier interpretation around mean.",
      "All specifications include country, year, and sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 4 HTML saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 4 HTML (", e$message, ")\n")
})

tryCatch({
  modelsummary(
    list(
      "(1) RQ²" = nonlin_1,
      "(2) RoL²" = nonlin_2,
      "(3) Both²" = nonlin_3,
      "(4) + Interact" = nonlin_4,
      "(5) Centered" = nonlin_5
    ),
    output = file.path(output_dir, "h1_governance_revised/tables/table4_nonlinearity.docx"),
    title = "Table H1-D: Nonlinearity Tests - Quadratic Specifications",
    coef_rename = c(
      "regulatory_quality" = "Regulatory Quality (linear)",
      "rq_squared" = "RQ² (quadratic)",
      "rule_of_law" = "Rule of Law (linear)",
      "rol_squared" = "RoL² (quadratic)",
      "rq_centered" = "RQ (centered)",
      "rq_centered_sq" = "RQ² (centered)",
      "rol_centered" = "RoL (centered)",
      "rol_centered_sq" = "RoL² (centered)",
      "regulatory_quality:rule_of_law" = "RQ × RoL",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI Inflows (% GDP)",
      "has_blended" = "Blended Finance",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared", "aic"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Dependent variable: Private share of infrastructure investment.",
      "Tests for nonlinear (quadratic) relationships between governance and private investment.",
      "Positive β2 with negative β1 → U-shaped (threshold effect).",
      "Negative β2 with positive β1 → Inverted U-shaped (diminishing returns).",
      "Column (5) uses centered variables for easier interpretation around mean.",
      "All specifications include country, year, and sector fixed effects.",
      "Standard errors clustered by country."
    )
  )
  cat("✓ Table 4 DOCX saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Table 4 DOCX (", e$message, ")\n")
})

# ============================================================================
# EXPORT: Save full regression tables (Tables H1, H1-B, H1-C, H1-D) as XLSX/CSV
# ============================================================================
cat("\nSaving full regression tables to XLSX/CSV...\n")
tables_dir <- file.path(output_dir, "h1_governance_revised/tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

safe_save_table <- function(models_list, file_base){
  tryCatch({
    # Base table produced by modelsummary (includes GOF rows)
    df_table <- modelsummary(models_list, output = "data.frame")

    # Attempt to produce coefficient strings with stars using broom if available
    coef_strings <- list()
    if(requireNamespace("broom", quietly = TRUE)){
      for(i in seq_along(models_list)){
        name_i <- names(models_list)[i]
        if(is.null(name_i) || name_i == "") name_i <- paste0("model", i)
        m <- models_list[[i]]
        tidy_i <- tryCatch(broom::tidy(m), error = function(e) NULL)
        if(!is.null(tidy_i) && all(c("term","estimate","std.error","p.value") %in% names(tidy_i))){
          tidy_i <- tidy_i %>% mutate(
            star = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              TRUE ~ ""
            ),
            est_fmt = sprintf("%0.3f%s", estimate, star),
            se_fmt = sprintf("(%0.3f)", std.error),
            coef_cell = paste0(est_fmt, "\n", se_fmt)
          )
          coef_strings[[name_i]] <- tidy_i %>% select(term, coef_cell)
        }
      }
    }

    out_xlsx <- file.path(tables_dir, paste0(file_base, ".xlsx"))
    out_csv <- file.path(tables_dir, paste0(file_base, ".csv"))

    # If we have coefficient strings, try to inject them into df_table
    if(length(coef_strings) > 0){
      # Determine the term column (first column) name
      term_col <- names(df_table)[1]
      for(model_name in names(coef_strings)){
        col_index <- which(names(df_table) == model_name)
        if(length(col_index) == 0) next
        cs <- coef_strings[[model_name]]
        for(j in seq_len(nrow(cs))){
          term_j <- cs$term[j]
          cell_val <- cs$coef_cell[j]
          # exact match first
          row_match <- which(as.character(df_table[[term_col]]) == term_j)
          if(length(row_match) == 0){
            # fallback to grepl
            row_match <- which(grepl(term_j, as.character(df_table[[term_col]]), fixed = TRUE))
          }
          if(length(row_match) > 0){
            df_table[row_match, col_index] <- cell_val
          }
        }
      }
    }

    # Write styled XLSX if openxlsx is available
    if(requireNamespace("openxlsx", quietly = TRUE)){
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "Table")
      # Write table starting at A1
      openxlsx::writeData(wb, sheet = "Table", df_table, startCol = 1, startRow = 1, rowNames = FALSE)
      # Styling: header bold, wrap text, freeze top row
      headerStyle <- openxlsx::createStyle(textDecoration = "bold", halign = "center")
      wrapStyle <- openxlsx::createStyle(wrapText = TRUE)
      openxlsx::addStyle(wb, sheet = "Table", headerStyle, rows = 1, cols = 1:ncol(df_table), gridExpand = TRUE)
      openxlsx::addStyle(wb, sheet = "Table", wrapStyle, rows = 2:(nrow(df_table)+1), cols = 1:ncol(df_table), gridExpand = TRUE)
      openxlsx::freezePane(wb, "Table", firstRow = TRUE)
      # Adjust column widths
      openxlsx::setColWidths(wb, "Table", cols = 1:ncol(df_table), widths = "auto")
      openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)
      cat(sprintf("Saved styled XLSX %s\n", out_xlsx))
    } else {
      # Fallback: write CSV. This will include coef cells with embedded newlines where available.
      write_csv(df_table, out_csv)
      cat(sprintf("openxlsx not installed; saved %s instead\n", out_csv))
    }
  }, error = function(e){
    cat(sprintf("Could not save table %s: %s\n", file_base, e$message))
  })
}

safe_save_table(list("(1) Baseline"=main_1, "(2) + Economic"=main_2, "(3) + Sector FEs"=main_3, "(4) + RQ×RoL"=main_4, "(5) + Risk Int."=main_5), "H1_main_results")
safe_save_table(list("(1) Composite"=robust_1, "(2) Log Priv Inv"=robust_2, "(3) Exclude 100%"=robust_3, "(4) Energy Only"=robust_4, "(5) 2-Way SE"=robust_5), "H1_robustness_checks")
safe_save_table(list("(1) RQ×RoL"=mech_1, "(2) ×Risk"=mech_2, "(3) ×Energy"=mech_3, "(4) ×Greenfield"=mech_4, "(5) ×Renewable"=mech_5), "H1_mechanisms")
safe_save_table(list("(1) RQ²"=nonlin_1, "(2) RoL²"=nonlin_2, "(3) Both²"=nonlin_3, "(4) + Interact"=nonlin_4, "(5) Centered"=nonlin_5), "H1_nonlinearity")


################################################################################
# 7. VISUALIZATIONS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Figure 1: Coefficient comparison - RQ vs RoL
tryCatch({
  coef_data <- data.frame(
    Component = c("Regulatory Quality", "Rule of Law"),
    Estimate = c(coef(main_3)["regulatory_quality"],
                 coef(main_3)["rule_of_law"]),
    SE = c(summary(main_3)$se["regulatory_quality"],
           summary(main_3)$se["rule_of_law"])
  ) %>%
    mutate(
      CI_lower = Estimate - 1.96 * SE,
      CI_upper = Estimate + 1.96 * SE
    )
  
  fig1 <- ggplot(coef_data, aes(x = Estimate, y = Component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 4, color = "#1f77b4") +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, color = "#1f77b4") +
    labs(
      title = "Governance Components and Private Investment",
      subtitle = "Individual effects from Table 1, Column 3",
      x = "Effect on Private Share (percentage points per unit)",
      y = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "h1_governance_revised/figures/fig1_component_comparison.png"), 
         fig1, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 1 saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 1 (", e$message, ")\n")
})

# Figure 2: Interaction plot - RQ × RoL
tryCatch({
  # Create prediction grid
  pred_data <- expand.grid(
    regulatory_quality = seq(0, 100, by = 10),
    rule_of_law = c(25, 50, 75),  # Low, Medium, High
    log_gdp_pc = mean(reg_data$log_gdp_pc, na.rm=T),
    domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
    fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
    has_blended = 0,
    log_investment = mean(reg_data$log_investment, na.rm=T)
  ) %>%
    mutate(
      rol_level = factor(rule_of_law, 
                        levels = c(25, 50, 75),
                        labels = c("Low RoL (25th pct)", "Medium RoL (50th pct)", "High RoL (75th pct)"))
    )
  
  # Manual prediction from mech_1 coefficients
  pred_data$pred_private <- with(pred_data,
    coef(mech_1)["regulatory_quality"] * regulatory_quality +
    coef(mech_1)["rule_of_law"] * rule_of_law +
    coef(mech_1)["regulatory_quality:rule_of_law"] * regulatory_quality * rule_of_law +
    coef(mech_1)["log_gdp_pc"] * log_gdp_pc +
    coef(mech_1)["domestic_credit"] * domestic_credit +
    coef(mech_1)["fdi_gdp"] * fdi_gdp +
    coef(mech_1)["log_investment"] * log_investment
  )
  
  # Adjust to realistic range
  baseline_adjust <- mean(reg_data$private_share) - mean(pred_data$pred_private)
  pred_data$pred_private <- pred_data$pred_private + baseline_adjust
  
  fig2 <- ggplot(pred_data, aes(x = regulatory_quality, y = pred_private, 
                                 color = rol_level, linetype = rol_level)) +
    geom_line(size = 1.2) +
    scale_color_manual(values = c("#d62728", "#ff7f0e", "#2ca02c"), name = "") +
    scale_linetype_manual(values = c("dashed", "dotted", "solid"), name = "") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Complementarity Between Regulatory Quality and Rule of Law",
      subtitle = "Effect of RQ on private investment at different levels of RoL",
      x = "Regulatory Quality (percentile)",
      y = "Predicted Private Share"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "h1_governance_revised/figures/fig2_rq_rol_interaction.png"), 
         fig2, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 2 saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 2 (", e$message, ")\n")
})

# Figure 3: Scatter plot with both governance dimensions
tryCatch({
  # Sample for visualization
  plot_data <- reg_data %>%
    sample_n(min(3000, n())) %>%
    mutate(gov_category = case_when(
      regulatory_quality >= 67 & rule_of_law >= 67 ~ "High RQ & RoL",
      regulatory_quality < 33 | rule_of_law < 33 ~ "Low RQ or RoL",
      TRUE ~ "Medium"
    ))
  
  fig3 <- ggplot(plot_data, aes(x = regulatory_quality, y = rule_of_law, 
                                 color = private_share, size = private_share)) +
    geom_point(alpha = 0.6) +
    scale_color_gradient2(low = "#d62728", mid = "#ff7f0e", high = "#2ca02c",
                         midpoint = 0.5, name = "Private\nShare",
                         labels = percent_format()) +
    scale_size_continuous(range = c(0.5, 3), guide = "none") +
    labs(
      title = "Private Investment Across Governance Dimensions",
      subtitle = "Relationship between regulatory quality, rule of law, and private participation",
      x = "Regulatory Quality (percentile)",
      y = "Rule of Law (percentile)"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          legend.position = "right")
  
  ggsave(file.path(output_dir, "h1_governance_revised/figures/fig3_governance_space.png"), 
         fig3, width = 10, height = 7, dpi = 300)
  cat("✓ Figure 3 saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 3 (", e$message, ")\n")
})

# Figure 4: Marginal effects of RQ at different RoL levels
tryCatch({
  # Calculate marginal effect of RQ at different RoL values
  rol_values <- seq(0, 100, by = 5)
  marginal_effects <- data.frame(
    rule_of_law = rol_values,
    marginal_effect = coef(mech_1)["regulatory_quality"] + 
                     coef(mech_1)["regulatory_quality:rule_of_law"] * rol_values,
    # Approximate SE using delta method (simplified)
    se = sqrt(summary(mech_1)$se["regulatory_quality"]^2 + 
             (rol_values * summary(mech_1)$se["regulatory_quality:rule_of_law"])^2)
  ) %>%
    mutate(
      ci_lower = marginal_effect - 1.96 * se,
      ci_upper = marginal_effect + 1.96 * se
    )
  
  fig4 <- ggplot(marginal_effects, aes(x = rule_of_law, y = marginal_effect)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, fill = "#1f77b4") +
    geom_line(color = "#1f77b4", size = 1.2) +
    labs(
      title = "Marginal Effect of Regulatory Quality by Rule of Law Level",
      subtitle = "How the impact of RQ on private investment depends on RoL",
      x = "Rule of Law (percentile)",
      y = "Marginal Effect of RQ on Private Share"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "h1_governance_revised/figures/fig4_marginal_effects.png"), 
         fig4, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 4 saved successfully\n")
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 4 (", e$message, ")\n")
})

# Figure 5: Quadratic relationship for RQ
tryCatch({
  # Check if quadratic term is significant
  rq_quad_pval <- summary(nonlin_1)$coeftable["rq_squared", "Pr(>|t|)"]
  
  if (rq_quad_pval < 0.10) {  # Show if at least marginally significant
    # Create prediction grid
    pred_rq <- data.frame(
      regulatory_quality = seq(0, 100, by = 1),
      log_gdp_pc = mean(reg_data$log_gdp_pc, na.rm=T),
      domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
      fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
      has_blended = 0,
      log_investment = mean(reg_data$log_investment, na.rm=T)
    ) %>%
      mutate(rq_squared = regulatory_quality^2)
    
    # Predict
    pred_rq$pred_private <- predict(nonlin_1, newdata = pred_rq, type = "response")
    
    # Add actual data points (sample)
    sample_data <- reg_data %>%
      sample_n(min(2000, n())) %>%
      select(regulatory_quality, private_share)
    
    fig5 <- ggplot() +
      geom_point(data = sample_data, 
                aes(x = regulatory_quality, y = private_share),
                alpha = 0.2, size = 1, color = "gray50") +
      geom_line(data = pred_rq, 
               aes(x = regulatory_quality, y = pred_private),
               color = "#1f77b4", size = 1.5) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Nonlinear Relationship: Regulatory Quality and Private Investment",
        subtitle = sprintf("Quadratic specification (β2 = %.6f, p = %.4f)", 
                          rq_squared_coef, rq_quad_pval),
        x = "Regulatory Quality (percentile)",
        y = "Predicted Private Share"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    
    # Add turning point if U-shaped or inverted U
    if ((rq_squared_coef > 0 & rq_linear < 0) | (rq_squared_coef < 0 & rq_linear > 0)) {
      turning_pt <- -rq_linear / (2 * rq_squared_coef)
      if (turning_pt >= 0 & turning_pt <= 100) {
        fig5 <- fig5 + 
          geom_vline(xintercept = turning_pt, linetype = "dashed", color = "#d62728") +
          annotate("text", x = turning_pt, y = max(pred_rq$pred_private) * 0.95,
                  label = sprintf("Turning point: %.1f", turning_pt),
                  hjust = -0.1, color = "#d62728", size = 4)
      }
    }
    
    ggsave(file.path(output_dir, "h1_governance_revised/figures/fig5_rq_quadratic.png"), 
           fig5, width = 10, height = 6, dpi = 300)
    cat("✓ Figure 5 (RQ quadratic) saved successfully\n")
  } else {
    cat("⚠ Figure 5 (RQ quadratic) skipped - no significant nonlinearity\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 5 (", e$message, ")\n")
})

# Figure 6: Quadratic relationship for RoL
tryCatch({
  rol_quad_pval <- summary(nonlin_2)$coeftable["rol_squared", "Pr(>|t|)"]
  
  if (rol_quad_pval < 0.10) {
    # Create prediction grid
    pred_rol <- data.frame(
      rule_of_law = seq(0, 100, by = 1),
      log_gdp_pc = mean(reg_data$log_gdp_pc, na.rm=T),
      domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
      fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
      has_blended = 0,
      log_investment = mean(reg_data$log_investment, na.rm=T)
    ) %>%
      mutate(rol_squared = rule_of_law^2)
    
    # Predict
    pred_rol$pred_private <- predict(nonlin_2, newdata = pred_rol, type = "response")
    
    # Add actual data points (sample)
    sample_data <- reg_data %>%
      sample_n(min(2000, n())) %>%
      select(rule_of_law, private_share)
    
    fig6 <- ggplot() +
      geom_point(data = sample_data, 
                aes(x = rule_of_law, y = private_share),
                alpha = 0.2, size = 1, color = "gray50") +
      geom_line(data = pred_rol, 
               aes(x = rule_of_law, y = pred_private),
               color = "#2ca02c", size = 1.5) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Nonlinear Relationship: Rule of Law and Private Investment",
        subtitle = sprintf("Quadratic specification (β4 = %.6f, p = %.4f)", 
                          rol_squared_coef, rol_quad_pval),
        x = "Rule of Law (percentile)",
        y = "Predicted Private Share"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    
    # Add turning point if U-shaped or inverted U
    if ((rol_squared_coef > 0 & rol_linear < 0) | (rol_squared_coef < 0 & rol_linear > 0)) {
      turning_pt <- -rol_linear / (2 * rol_squared_coef)
      if (turning_pt >= 0 & turning_pt <= 100) {
        fig6 <- fig6 + 
          geom_vline(xintercept = turning_pt, linetype = "dashed", color = "#d62728") +
          annotate("text", x = turning_pt, y = max(pred_rol$pred_private) * 0.95,
                  label = sprintf("Turning point: %.1f", turning_pt),
                  hjust = -0.1, color = "#d62728", size = 4)
      }
    }
    
    ggsave(file.path(output_dir, "h1_governance_revised/figures/fig6_rol_quadratic.png"), 
           fig6, width = 10, height = 6, dpi = 300)
    cat("✓ Figure 6 (RoL quadratic) saved successfully\n")
  } else {
    cat("⚠ Figure 6 (RoL quadratic) skipped - no significant nonlinearity\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 6 (", e$message, ")\n")
})

# Figure 7: 3D surface plot for quadratic + interaction
tryCatch({
  # Only create if both quadratic terms or interaction are significant
  both_quad_significant <- (summary(nonlin_3)$coeftable["rq_squared", "Pr(>|t|)"] < 0.10 |
                           summary(nonlin_3)$coeftable["rol_squared", "Pr(>|t|)"] < 0.10 |
                           summary(nonlin_4)$coeftable["regulatory_quality:rule_of_law", "Pr(>|t|)"] < 0.10)
  
  if (both_quad_significant) {
    # Create grid
    rq_seq <- seq(0, 100, by = 10)
    rol_seq <- seq(0, 100, by = 10)
    pred_grid <- expand.grid(
      regulatory_quality = rq_seq,
      rule_of_law = rol_seq,
      log_gdp_pc = mean(reg_data$log_gdp_pc, na.rm=T),
      domestic_credit = mean(reg_data$domestic_credit, na.rm=T),
      fdi_gdp = mean(reg_data$fdi_gdp, na.rm=T),
      has_blended = 0,
      log_investment = mean(reg_data$log_investment, na.rm=T)
    ) %>%
      mutate(
        rq_squared = regulatory_quality^2,
        rol_squared = rule_of_law^2
      )
    
    # Predict from full quadratic + interaction model
    pred_grid$pred_private <- predict(nonlin_4, newdata = pred_grid, type = "response")
    
    fig7 <- ggplot(pred_grid, aes(x = regulatory_quality, y = rule_of_law, 
                                   fill = pred_private)) +
      geom_tile() +
      scale_fill_gradient2(low = "#d62728", mid = "#ff7f0e", high = "#2ca02c",
                          midpoint = median(pred_grid$pred_private),
                          name = "Predicted\nPrivate Share",
                          labels = percent_format()) +
      labs(
        title = "Nonlinear Governance Effects on Private Investment",
        subtitle = "Full quadratic specification with interaction (Table 4, Column 4)",
        x = "Regulatory Quality (percentile)",
        y = "Rule of Law (percentile)"
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title = element_text(face = "bold"))
    
    ggsave(file.path(output_dir, "h1_governance_revised/figures/fig7_quadratic_surface.png"), 
           fig7, width = 10, height = 8, dpi = 300)
    cat("✓ Figure 7 (quadratic surface) saved successfully\n")
  } else {
    cat("⚠ Figure 7 (quadratic surface) skipped - no significant nonlinearity\n")
  }
}, error = function(e) {
  cat("⚠ Warning: Could not create Figure 7 (", e$message, ")\n")
})

################################################################################
# 8. SUMMARY AND INTERPRETATION
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("ANALYSIS COMPLETE - SUMMARY\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("REVISED ANALYSIS IMPROVEMENTS:\n")
cat("  1. Decomposed governance into RQ and RoL (separate effects)\n")
cat("  2. Tested RQ × RoL interaction (complementarity vs substitutability)\n")
cat("  3. Examined heterogeneous effects by risk, sector, project type\n")
cat("  4. Compared decomposed vs composite governance approaches\n")
cat("  5. Added nonlinearity tests (quadratic specifications)\n\n")

cat("KEY FINDINGS:\n")
cat(sprintf("  Regulatory Quality effect:              %.5f\n", rq_coef))
cat(sprintf("  Rule of Law effect:                     %.5f\n", rol_coef))
cat(sprintf("  RQ × RoL interaction:                   %.6f\n", rq_rol_int))

# Nonlinearity summary
if (exists("rq_squared_coef") && !is.na(rq_squared_coef)) {
  rq_quad_pval <- summary(nonlin_1)$coeftable["rq_squared", "Pr(>|t|)"]
  cat(sprintf("  RQ squared term (nonlinearity):         %.6f (p=%.4f)\n", 
              rq_squared_coef, rq_quad_pval))
}
if (exists("rol_squared_coef") && !is.na(rol_squared_coef)) {
  rol_quad_pval <- summary(nonlin_2)$coeftable["rol_squared", "Pr(>|t|)"]
  cat(sprintf("  RoL squared term (nonlinearity):        %.6f (p=%.4f)\n", 
              rol_squared_coef, rol_quad_pval))
}

# Interpret interaction
if (rq_rol_int > 0) {
  cat("\n  → COMPLEMENTARITY: RQ and RoL reinforce each other\n")
  cat("     (Both governance dimensions are more effective together)\n")
} else if (rq_rol_int < 0) {
  cat("\n  → SUBSTITUTABILITY: RQ and RoL can partially replace each other\n")
  cat("     (One governance dimension can compensate for weakness in the other)\n")
} else {
  cat("\n  → ADDITIVE: RQ and RoL have independent effects\n")
}

# Interpret nonlinearity
if (exists("rq_squared_coef") && !is.na(rq_squared_coef) && rq_quad_pval < 0.10) {
  if (rq_squared_coef > 0 & rq_coef < 0) {
    cat("\n  → RQ NONLINEARITY: U-shaped (threshold effect)\n")
    cat("     (RQ must exceed minimum level to positively affect investment)\n")
  } else if (rq_squared_coef < 0 & rq_coef > 0) {
    cat("\n  → RQ NONLINEARITY: Inverted U-shaped (diminishing returns)\n")
    cat("     (Positive returns to RQ eventually diminish or reverse)\n")
  }
}

if (exists("rol_squared_coef") && !is.na(rol_squared_coef) && rol_quad_pval < 0.10) {
  if (rol_squared_coef > 0 & rol_coef < 0) {
    cat("\n  → RoL NONLINEARITY: U-shaped (threshold effect)\n")
    cat("     (RoL must exceed minimum level to positively affect investment)\n")
  } else if (rol_squared_coef < 0 & rol_coef > 0) {
    cat("\n  → RoL NONLINEARITY: Inverted U-shaped (diminishing returns)\n")
    cat("     (Positive returns to RoL eventually diminish or reverse)\n")
  }
}

cat("\nHYPOTHESIS TESTING:\n")
cat("  Null Hypothesis: Governance has NO effect on private investment\n")

# Test RQ
rq_pval <- summary(main_3)$coeftable["regulatory_quality", "Pr(>|t|)"]
cat(sprintf("  Regulatory Quality p-value: %.4f ", rq_pval))
if (rq_pval < 0.05) {
  cat("*** SIGNIFICANT at 5%\n")
} else if (rq_pval < 0.1) {
  cat("** SIGNIFICANT at 10%\n")
} else {
  cat("NOT SIGNIFICANT\n")
}

# Test RoL
rol_pval <- summary(main_3)$coeftable["rule_of_law", "Pr(>|t|)"]
cat(sprintf("  Rule of Law p-value:        %.4f ", rol_pval))
if (rol_pval < 0.05) {
  cat("*** SIGNIFICANT at 5%\n")
} else if (rol_pval < 0.1) {
  cat("** SIGNIFICANT at 10%\n")
} else {
  cat("NOT SIGNIFICANT\n")
}

# Overall conclusion
if ((rq_coef > 0 & rq_pval < 0.05) | (rol_coef > 0 & rol_pval < 0.05)) {
  cat("\n✓✓✓ HYPOTHESIS 1 SUPPORTED ✓✓✓\n")
  cat("At least one governance component is positively and significantly associated\n")
  cat("with higher private sector participation in infrastructure investment.\n")
  cat("We REJECT the null hypothesis.\n")
} else {
  cat("\n✗ HYPOTHESIS 1 NOT SUPPORTED\n")
  cat("Neither governance component shows significant positive effect.\n")
}

cat("\nCOMPARISON: DECOMPOSED vs COMPOSITE:\n")
composite_coef <- coef(robust_1)["governance_h1"]
cat(sprintf("  Composite governance (RQ+RoL)/2:       %.5f\n", composite_coef))
cat(sprintf("  Sum of separate effects (RQ + RoL):    %.5f\n", rq_coef + rol_coef))
cat("\n  The decomposed approach provides:\n")
cat("    - Individual governance dimension effects\n")
cat("    - Tests of complementarity (RQ × RoL)\n")
cat("    - Better policy targeting\n")
cat("    - More nuanced interpretation\n")

cat("\nFILES SAVED TO:\n")
cat("  ", output_dir, "/h1_governance_revised/\n", sep = "")

cat("\n", rep("=", 80), "\n", sep = "")
cat("INTERPRETATION NOTES:\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("1. INDIVIDUAL EFFECTS:\n")
cat("   - RQ effect: Impact of regulatory quality holding rule of law constant\n")
cat("   - RoL effect: Impact of rule of law holding regulatory quality constant\n")
cat("   - Both control for economic conditions, sector, and project characteristics\n\n")

cat("2. INTERACTION EFFECT (RQ × RoL):\n")
cat("   - Positive: Governance dimensions complement each other\n")
cat("   - Negative: Governance dimensions substitute for each other\n")
cat("   - Near zero: Governance dimensions work independently\n\n")

cat("3. NONLINEARITY TESTS (QUADRATIC TERMS):\n")
cat("   A. U-SHAPED (β1 < 0, β2 > 0):\n")
cat("      - Interpretation: Threshold effect\n")
cat("      - Implication: Governance must exceed minimum to attract investment\n")
cat("      - Policy: Focus on bringing low-governance countries above threshold\n\n")
cat("   B. INVERTED U-SHAPED (β1 > 0, β2 < 0):\n")
cat("      - Interpretation: Diminishing returns\n")
cat("      - Implication: Additional governance improvements less effective at high levels\n")
cat("      - Policy: Prioritize low/medium governance countries for reforms\n\n")
cat("   C. ACCELERATING POSITIVE (β1 > 0, β2 > 0):\n")
cat("      - Interpretation: Increasing returns\n")
cat("      - Implication: Governance improvements more valuable at higher levels\n")
cat("      - Policy: Support countries already making progress\n\n")
cat("   D. LINEAR (β2 ≈ 0):\n")
cat("      - Interpretation: Constant marginal effects\n")
cat("      - Implication: Each governance unit has same impact regardless of level\n")
cat("      - Policy: Governance reforms equally valuable everywhere\n\n")

cat("4. POLICY IMPLICATIONS:\n")
if (abs(rq_coef) > abs(rol_coef)) {
  cat("   - Regulatory quality appears more important than rule of law\n")
  cat("   - Focus on: Streamlining regulations, reducing red tape, improving\n")
  cat("     business environment for infrastructure investment\n")
} else if (abs(rol_coef) > abs(rq_coef)) {
  cat("   - Rule of law appears more important than regulatory quality\n")
  cat("   - Focus on: Contract enforcement, property rights, judicial system,\n")
  cat("     reducing corruption in infrastructure projects\n")
} else {
  cat("   - Both dimensions contribute roughly equally\n")
  cat("   - Focus on: Comprehensive governance reforms addressing both\n")
  cat("     regulatory environment and legal institutions\n")
}

cat("\n", rep("=", 80), "\n\n", sep = "")

# ==============================================================================
# 1. DATA PREPARATION: GENERATING LAGS (Fixing Simultaneity)
# ==============================================================================

# Ensure data is sorted by country-year before creating lags
wb <- wb %>%
  arrange(country, year) %>%
  group_by(country) %>%
  mutate(
    # 1-Year Lags (t-1)
    lag1_rq  = lag(Regulatory_Quality_percentile, 1),
    lag1_rol = lag(Rule_of_Law_percentile, 1),
    lag1_gdp = lag(GDP_constant_2015_USD, 1),
    
    # 2-Year Lags (t-2)
    lag2_rq  = lag(Regulatory_Quality_percentile, 2),
    lag2_rol = lag(Rule_of_Law_percentile, 2),
    lag2_gdp = lag(GDP_constant_2015_USD, 2)
  ) %>%
  ungroup()

# Merge with Project Data (Ensure you are using the lagged WB dataset)
# Note: join on `year` (consistent with earlier merges in this script)
df_projects <- left_join(ppi, wb, by = c("country_code" = "country", "year" = "year")) %>%
  filter(investment_real > 0) %>%  # Ensure valid investment data
  mutate(private_share = private / 100)  # Create private_share for lag regressions

# Save lagged datasets so you can inspect the created lag variables
tryCatch({
  write_csv(wb %>% select(country, year, lag1_rq, lag1_rol, lag1_gdp, lag2_rq, lag2_rol, lag2_gdp),
            file.path(output_dir, "h1_governance_revised/tables/wb_lagged.csv"))
  write_csv(df_projects,
            file.path(output_dir, "h1_governance_revised/tables/df_projects_with_lags.csv"))
  cat("✓ Lagged datasets saved to h1_governance_revised/tables/\n")
}, error = function(e) {
  cat("⚠ Warning: Could not save lagged datasets (", e$message, ")\n")
})

# ==============================================================================
# 2. ROBUSTNESS CHECK: PROGRESSIVE LAG ANALYSIS
# ==============================================================================
# Run both log(investment_real) and private_share as DVs

# ============ DV 1: LOG(INVESTMENT_REAL) ============

# Model 1: Contemporaneous (Original Specification)
# Note: Susceptible to simultaneity bias
m1_contemp_inv <- feols(log(investment_real) ~ Regulatory_Quality_percentile + 
                          Rule_of_Law_percentile + log(GDP_constant_2015_USD) | 
                          country_code + sector, 
                        data = df_projects, 
                        vcov = ~country_code)

# Model 2: 1-Year Lag (Standard Infrastructure Planning Horizon)
# Note: Governance at t-1 affects Investment at t
m2_lag1_inv <- feols(log(investment_real) ~ lag1_rq + lag1_rol + log(lag1_gdp) | 
                       country_code + sector, 
                     data = df_projects, 
                     vcov = ~country_code)

# Model 3: 2-Year Lag (Long-term Institutional Stability)
# Note: Governance at t-2 affects Investment at t
m3_lag2_inv <- feols(log(investment_real) ~ lag2_rq + lag2_rol + log(lag2_gdp) | 
                       country_code + sector, 
                     data = df_projects, 
                     vcov = ~country_code)

# ============ DV 2: PRIVATE_SHARE ============

# Model 4: Contemporaneous with private_share
m4_contemp_share <- feols(private_share ~ Regulatory_Quality_percentile + 
                            Rule_of_Law_percentile + log(GDP_constant_2015_USD) | 
                            country_code + sector, 
                          data = df_projects, 
                          vcov = ~country_code)

# Model 5: 1-Year Lag with private_share
m5_lag1_share <- feols(private_share ~ lag1_rq + lag1_rol + log(lag1_gdp) | 
                         country_code + sector, 
                       data = df_projects, 
                       vcov = ~country_code)

# Model 6: 2-Year Lag with private_share
m6_lag2_share <- feols(private_share ~ lag2_rq + lag2_rol + log(lag2_gdp) | 
                         country_code + sector, 
                       data = df_projects, 
                       vcov = ~country_code)

# ==============================================================================
# 3. OUTPUT: TABLES FOR BOTH DVs
# ==============================================================================

out_tables_dir <- file.path(output_dir, "h1_governance_revised/tables")
if (!dir.exists(out_tables_dir)) dir.create(out_tables_dir, recursive = TRUE)

cat("\n", rep("=", 80), "\n", sep = "")
cat("LAG ROBUSTNESS ANALYSIS - DEPENDENT VARIABLE: LOG(INVESTMENT_REAL)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Table for Log Investment
models_lag_inv <- list(
  "(1) Contemporaneous" = m1_contemp_inv,
  "(2) 1-Year Lag" = m2_lag1_inv,
  "(3) 2-Year Lag" = m3_lag2_inv
)

rows_inv <- tribble(
  ~term,          ~"(1) Contemporaneous",  ~"(2) 1-Year Lag",  ~"(3) 2-Year Lag",
  "Fixed Effects", "Country, Sector", "Country, Sector", "Country, Sector",
  "Cluster SE",    "Country",         "Country",         "Country"
)

# Save log investment table
tryCatch({
  modelsummary(models_lag_inv,
               coef_map = c(
                 "Rule_of_Law_percentile" = "Rule of Law",
                 "lag1_rol" = "Rule of Law",
                 "lag2_rol" = "Rule of Law",
                 "Regulatory_Quality_percentile" = "Regulatory Quality",
                 "lag1_rq" = "Regulatory Quality",
                 "lag2_rq" = "Regulatory Quality",
                 "log(GDP_constant_2015_USD)" = "Log GDP",
                 "log(lag1_gdp)" = "Log GDP",
                 "log(lag2_gdp)" = "Log GDP"
               ),
               stars = c('*' = .1, '**' = .05, '***' = .01),
               gof_map = c("nobs", "r.squared"),
               add_rows = rows_inv,
               title = "Table: Effect of Governance on Log(Investment) - Robustness to Time Lags",
               output = file.path(out_tables_dir, "h1_lag_robustness_log_investment.html")
  )
  cat("✓ Log Investment table saved: h1_lag_robustness_log_investment.html\n")
}, error = function(e) {
  cat("⚠ Log Investment table save failed: ", e$message, "\n")
})

# Save log investment coefficients CSV
tryCatch({
  tidy_lag_coefs_inv <- bind_rows(
    broom::tidy(m1_contemp_inv, conf.int = TRUE) %>% mutate(model = "Contemporaneous", dv = "log(investment)"),
    broom::tidy(m2_lag1_inv, conf.int = TRUE) %>% mutate(model = "1-Year Lag", dv = "log(investment)"),
    broom::tidy(m3_lag2_inv, conf.int = TRUE) %>% mutate(model = "2-Year Lag", dv = "log(investment)")
  ) %>% select(dv, model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
  
  write_csv(tidy_lag_coefs_inv, file.path(out_tables_dir, "h1_lag_coefficients_log_investment.csv"))
  cat("✓ Log Investment coefficients saved: h1_lag_coefficients_log_investment.csv\n")
}, error = function(e) {
  cat("⚠ Log Investment coefficients save failed: ", e$message, "\n")
})

cat("\n", rep("=", 80), "\n", sep = "")
cat("LAG ROBUSTNESS ANALYSIS - DEPENDENT VARIABLE: PRIVATE_SHARE\n")
cat(rep("=", 80), "\n\n", sep = "")

# Table for Private Share
models_lag_share <- list(
  "(1) Contemporaneous" = m4_contemp_share,
  "(2) 1-Year Lag" = m5_lag1_share,
  "(3) 2-Year Lag" = m6_lag2_share
)

rows_share <- tribble(
  ~term,          ~"(1) Contemporaneous",  ~"(2) 1-Year Lag",  ~"(3) 2-Year Lag",
  "Fixed Effects", "Country, Sector", "Country, Sector", "Country, Sector",
  "Cluster SE",    "Country",         "Country",         "Country"
)

# Save private share table
tryCatch({
  modelsummary(models_lag_share,
               coef_map = c(
                 "Rule_of_Law_percentile" = "Rule of Law",
                 "lag1_rol" = "Rule of Law",
                 "lag2_rol" = "Rule of Law",
                 "Regulatory_Quality_percentile" = "Regulatory Quality",
                 "lag1_rq" = "Regulatory Quality",
                 "lag2_rq" = "Regulatory Quality",
                 "log(GDP_constant_2015_USD)" = "Log GDP",
                 "log(lag1_gdp)" = "Log GDP",
                 "log(lag2_gdp)" = "Log GDP"
               ),
               stars = c('*' = .1, '**' = .05, '***' = .01),
               gof_map = c("nobs", "r.squared"),
               add_rows = rows_share,
               title = "Table: Effect of Governance on Private Share - Robustness to Time Lags",
               output = file.path(out_tables_dir, "h1_lag_robustness_private_share.html")
  )
  cat("✓ Private Share table saved: h1_lag_robustness_private_share.html\n")
}, error = function(e) {
  cat("⚠ Private Share table save failed: ", e$message, "\n")
})

# Save private share coefficients CSV
tryCatch({
  tidy_lag_coefs_share <- bind_rows(
    broom::tidy(m4_contemp_share, conf.int = TRUE) %>% mutate(model = "Contemporaneous", dv = "private_share"),
    broom::tidy(m5_lag1_share, conf.int = TRUE) %>% mutate(model = "1-Year Lag", dv = "private_share"),
    broom::tidy(m6_lag2_share, conf.int = TRUE) %>% mutate(model = "2-Year Lag", dv = "private_share")
  ) %>% select(dv, model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)
  
  write_csv(tidy_lag_coefs_share, file.path(out_tables_dir, "h1_lag_coefficients_private_share.csv"))
  cat("✓ Private Share coefficients saved: h1_lag_coefficients_private_share.csv\n")
}, error = function(e) {
  cat("⚠ Private Share coefficients save failed: ", e$message, "\n")
})

cat("\n")

# ==============================================================================
# 4. OUTPUT: COEFFICIENT STABILITY PLOTS (BOTH DVs)
# ==============================================================================

# Helper function to extract coefficients
extract_coefs <- function(model, name, dv_name) {
  res <- broom::tidy(model, conf.int = TRUE) %>%
    filter(term %in% c("Rule_of_Law_percentile", "lag1_rol", "lag2_rol",
                       "Regulatory_Quality_percentile", "lag1_rq", "lag2_rq")) %>%
    mutate(
      model_name = name,
      dv = dv_name,
      variable = ifelse(grepl("Rule", term) | grepl("rol", term), "Rule of Law", "Regulatory Quality")
    )
  return(res)
}

# Combine data for both DVs
plot_data_both <- bind_rows(
  # Log Investment DV
  extract_coefs(m1_contemp_inv, "Contemporaneous", "Log(Investment)"),
  extract_coefs(m2_lag1_inv, "1-Year Lag", "Log(Investment)"),
  extract_coefs(m3_lag2_inv, "2-Year Lag", "Log(Investment)"),
  # Private Share DV
  extract_coefs(m4_contemp_share, "Contemporaneous", "Private Share"),
  extract_coefs(m5_lag1_share, "1-Year Lag", "Private Share"),
  extract_coefs(m6_lag2_share, "2-Year Lag", "Private Share")
)

# Plot 1: Log Investment
p_lags_inv <- ggplot(
  plot_data_both %>% filter(dv == "Log(Investment)"),
  aes(x = model_name, y = estimate, color = variable, group = variable)
) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.3), width = 0.2, size = 1) +
  geom_line(position = position_dodge(width = 0.3), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(
    title = "Coefficient Stability: Log(Investment) Across Time Horizons",
    subtitle = "DV = Log of real infrastructure investment (USD)",
    y = "Coefficient Estimate (95% CI)",
    x = "Model Specification",
    color = "Governance Indicator"
  ) +
  scale_color_manual(values = c("Regulatory Quality" = "#E69F00", "Rule of Law" = "#0072B2")) +
  theme_minimal() +
  theme(legend.position = "bottom")

tryCatch({
  ggsave(file.path(out_tables_dir, "h1_coefficient_stability_log_investment.png"), 
         p_lags_inv, width = 8, height = 6, dpi = 300)
  cat("✓ Log Investment plot saved: h1_coefficient_stability_log_investment.png\n")
}, error = function(e) {
  cat("⚠ Log Investment plot save failed: ", e$message, "\n")
})

# Plot 2: Private Share
p_lags_share <- ggplot(
  plot_data_both %>% filter(dv == "Private Share"),
  aes(x = model_name, y = estimate, color = variable, group = variable)
) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.3), width = 0.2, size = 1) +
  geom_line(position = position_dodge(width = 0.3), linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
  labs(
    title = "Coefficient Stability: Private Share Across Time Horizons",
    subtitle = "DV = Private sector share of project investment (0-1)",
    y = "Coefficient Estimate (95% CI)",
    x = "Model Specification",
    color = "Governance Indicator"
  ) +
  scale_color_manual(values = c("Regulatory Quality" = "#E69F00", "Rule of Law" = "#0072B2")) +
  theme_minimal() +
  theme(legend.position = "bottom")

tryCatch({
  ggsave(file.path(out_tables_dir, "h1_coefficient_stability_private_share.png"), 
         p_lags_share, width = 8, height = 6, dpi = 300)
  cat("✓ Private Share plot saved: h1_coefficient_stability_private_share.png\n")
}, error = function(e) {
  cat("⚠ Private Share plot save failed: ", e$message, "\n")
})

print(p_lags_inv)
print(p_lags_share)

# ==============================================================================
# 5. WOOLDRIDGE AUTOCORRELATION TEST
# ==============================================================================
# Test for serial correlation to justify country-level clustering
# H0: No first-order autocorrelation
# H1: First-order autocorrelation exists

cat("\n", rep("=", 80), "\n", sep = "")
cat("WOOLDRIDGE TEST FOR AUTOCORRELATION (Justify Clustering Strategy)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Load plm package for panel data testing
if (!require("plm", quietly = TRUE)) {
  install.packages("plm", repos = "https://cran.r-project.org")
  library(plm)
}

# Prepare panel data structure
# Create a clean dataset with no missing values for the test
# Use Regulatory_Quality_percentile and Rule_of_Law_percentile from WB data
test_data <- df_projects %>%
  select(country_code, year, investment_real, GDP_constant_2015_USD, private_share, 
         Regulatory_Quality_percentile, Rule_of_Law_percentile) %>%
  filter(complete.cases(.)) %>%
  arrange(country_code, year) %>%
  mutate(
    log_investment = log(investment_real),
    log_gdp = log(GDP_constant_2015_USD)
  ) %>%
  select(country_code, year, log_investment, log_gdp, private_share, 
         Regulatory_Quality_percentile, Rule_of_Law_percentile)

# Convert to panel data frame
pdata <- plm::pdata.frame(test_data, index = c("country_code", "year"))

cat(sprintf("Panel structure: %d countries, %d time periods (years)\n", 
            length(unique(test_data$country_code)),
            length(unique(test_data$year))))
cat(sprintf("Observations: %d\n\n", nrow(test_data)))

# ============ TEST 1: Log Investment ============
cat("TEST 1: Wooldridge AR(1) Test - DV: Log(Investment)\n")
cat("Formula: log_investment ~ Regulatory_Quality_percentile + Rule_of_Law_percentile\n\n")

tryCatch({
  test1 <- plm::pwartest(log_investment ~ Regulatory_Quality_percentile + Rule_of_Law_percentile, 
                         data = pdata)
  
  cat("Test Results:\n")
  cat(sprintf("  F-statistic:  %.4f\n", test1$statistic))
  cat(sprintf("  P-value:      %.6f\n", test1$p.value))
  cat(sprintf("  Degrees of freedom: %d, %d\n\n", test1$parameter[1], test1$parameter[2]))
  
  if (test1$p.value < 0.05) {
    cat("  ★ RESULT: SIGNIFICANT autocorrelation detected (p < 0.05)\n")
    cat("  → Justifies clustering standard errors by country\n")
    cat("  → Assumptions: observations within countries are not independent over time\n")
  } else {
    cat("  ★ RESULT: No significant autocorrelation (p >= 0.05)\n")
    cat("  → Linear independence assumption may hold\n")
    cat("  → Clustering may not be necessary, but conservative to cluster anyway\n")
  }
  
  # Save test results
  wooldridge_results <- data.frame(
    test_name = "Log(Investment)",
    formula = "log_investment ~ regulatory_quality + rule_of_law",
    F_statistic = test1$statistic,
    p_value = test1$p.value,
    df1 = test1$parameter[1],
    df2 = test1$parameter[2],
    interpretation = ifelse(test1$p.value < 0.05, 
                           "Autocorrelation detected", 
                           "No autocorrelation")
  )
  
}, error = function(e) {
  cat("⚠ Test 1 failed: ", e$message, "\n\n")
  wooldridge_results <<- data.frame()
})

# ============ TEST 2: Private Share ============
cat("\n", rep("-", 80), "\n\n", sep = "")
cat("TEST 2: Wooldridge AR(1) Test - DV: Private Share\n")
cat("Formula: private_share ~ Regulatory_Quality_percentile + Rule_of_Law_percentile\n\n")

tryCatch({
  test2 <- plm::pwartest(private_share ~ Regulatory_Quality_percentile + Rule_of_Law_percentile, 
                         data = pdata)
  
  cat("Test Results:\n")
  cat(sprintf("  F-statistic:  %.4f\n", test2$statistic))
  cat(sprintf("  P-value:      %.6f\n", test2$p.value))
  cat(sprintf("  Degrees of freedom: %d, %d\n\n", test2$parameter[1], test2$parameter[2]))
  
  if (test2$p.value < 0.05) {
    cat("  ★ RESULT: SIGNIFICANT autocorrelation detected (p < 0.05)\n")
    cat("  → Justifies clustering standard errors by country\n")
    cat("  → Assumptions: observations within countries are not independent over time\n")
  } else {
    cat("  ★ RESULT: No significant autocorrelation (p >= 0.05)\n")
    cat("  → Linear independence assumption may hold\n")
    cat("  → Clustering may not be necessary, but conservative to cluster anyway\n")
  }
  
  # Combine results
  test2_results <- data.frame(
    test_name = "Private Share",
    formula = "private_share ~ regulatory_quality + rule_of_law",
    F_statistic = test2$statistic,
    p_value = test2$p.value,
    df1 = test2$parameter[1],
    df2 = test2$parameter[2],
    interpretation = ifelse(test2$p.value < 0.05, 
                           "Autocorrelation detected", 
                           "No autocorrelation")
  )
  
  if (nrow(wooldridge_results) > 0) {
    wooldridge_results <<- bind_rows(wooldridge_results, test2_results)
  } else {
    wooldridge_results <<- test2_results
  }
  
}, error = function(e) {
  cat("⚠ Test 2 failed: ", e$message, "\n\n")
})

# ============ SAVE RESULTS ============
cat("\n", rep("=", 80), "\n\n", sep = "")

if (nrow(wooldridge_results) > 0) {
  tryCatch({
    write_csv(wooldridge_results, 
              file.path(out_tables_dir, "h1_wooldridge_autocorrelation_test.csv"))
    cat("✓ Wooldridge test results saved: h1_wooldridge_autocorrelation_test.csv\n\n")
  }, error = function(e) {
    cat("⚠ Could not save Wooldridge results: ", e$message, "\n\n")
  })
}

# ============ INTERPRETATION & JUSTIFICATION ============
cat("IMPLICATIONS FOR YOUR ANALYSIS:\n")
cat("──────────────────────────────────\n\n")

cat("Wooldridge (2002) test for AR(1) in unbalanced panels:\n")
cat("  • Null (H0): No first-order serial correlation\n")
cat("  • Alternative (H1): Correlation between consecutive errors\n")
cat("  • Test statistic: F-distributed under H0\n\n")

cat("WHY CLUSTER BY COUNTRY?\n")
cat("  1. If Wooldridge test is significant (p < 0.05):\n")
cat("     → Serial correlation violates standard OLS assumptions\n")
cat("     → Observations in same country across years are dependent\n")
cat("     → Standard errors underestimated without adjustment\n")
cat("     → Country-level clustering corrects for intra-group correlation\n\n")

cat("  2. If Wooldridge test is NOT significant (p >= 0.05):\n")
cat("     → No detectable first-order serial correlation\n")
cat("     → Standard clustering still valid (conservative approach)\n")
cat("     → No harm in clustering even if not strictly needed\n\n")

cat("RECOMMENDATION:\n")
cat("  Regardless of Wooldridge result, country clustering is appropriate because:\n")
cat("    • Infrastructure investments are at the project level (nested within countries)\n")
cat("    • Governance variables vary at country level (systematic within-country variation)\n")
cat("    • Unobserved country characteristics (political/institutional) affect investment\n")
cat("    • Correlated random effects: country shocks affect all projects in that year\n\n")

cat(rep("=", 80), "\n\n", sep = "")
