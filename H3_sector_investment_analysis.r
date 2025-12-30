################################################################################
# HYPOTHESIS 3: ALTERNATIVE APPROACHES FOR IMBALANCED DATA
# Addressing the renewable energy dominance problem (479/739 projects)
#
# PROBLEM: 
#   - 64% of projects are renewable energy
#   - Private share is 97%+ across ALL sectors (ceiling effect)
#   - Imbalanced comparison makes causal inference difficult
#
# SOLUTIONS:
#   Approach 1: Within-Energy comparison (Renewable vs Conventional)
#   Approach 2: Investment amount (not just private share)
#   Approach 3: Project characteristics & selection model
#
# THIS SCRIPT IMPLEMENTS ALL THREE APPROACHES
################################################################################

library(tidyverse)
library(fixest)
library(modelsummary)
library(ggplot2)
library(scales)

output_dir <- "C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Analysis Output"
dir.create(file.path(output_dir, "h3_alternative"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h3_alternative/tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "h3_alternative/figures"), recursive = TRUE, showWarnings = FALSE)

################################################################################
# DATA PREPARATION
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("HYPOTHESIS 3: ALTERNATIVE APPROACHES\n")
cat(rep("=", 80), "\n\n", sep = "")

# Load data
ppi <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/PPI_2005_2023.csv")
wb <- read_csv("C:/Users/aorta/OneDrive/Documents/Boston University/3. Fall 2025/EC561 Public Economics 1/Research Paper/Cleaned Data/WB_merged_2005_2023.csv")

df_projects <- ppi %>%
  filter(!is.na(investment_real), investment_real > 0) %>%
  mutate(
    # Sector classification
    is_renewable = ifelse(sector == "Energy" & Renewables == "Renewables", 1, 0),
    is_conventional_energy = ifelse(sector == "Energy" & Renewables != "Renewables", 1, 0),
    is_energy = ifelse(sector == "Energy", 1, 0),
    is_transport = ifelse(sector == "Transport", 1, 0),
    is_water = ifelse(sector == "Water and sewerage", 1, 0),
    is_ict = ifelse(sector == "Information and communications technology", 1, 0),
    
    sector_group = case_when(
      is_renewable == 1 ~ "Renewable Energy",
      is_conventional_energy == 1 ~ "Conventional Energy",
      is_transport == 1 ~ "Transport",
      is_water == 1 ~ "Water",
      is_ict == 1 ~ "ICT",
      TRUE ~ "Other"
    ),
    
    # Multiple dependent variables
    private_share = private / 100,
    log_investment = log(investment_real),
    log_private_investment = log(investment_real * private_share),
    
    # Project characteristics
    has_MLS = ifelse(MLS == "With MLS", 1, 0),
    has_BS = ifelse(BS %in% c("With BS", "With BLS"), 1, 0),
    is_greenfield = ifelse(type == "Greenfield project", 1, 0),
  ) %>%
  left_join(wb, by = c("country_code" = "country", "year" = "year")) %>%
  mutate(
    log_gdp_pc = if_else(!is.na(GDP_per_capita_constant_2015_USD) & GDP_per_capita_constant_2015_USD > 0,
                         log(GDP_per_capita_constant_2015_USD), NA_real_),
    domestic_credit = Domestic_credit_to_private_sector_pct_GDP,
    fdi_gdp = FDI_Inflows_pct_GDP,
    ps_percentile = Political_Stability_percentile,
    high_risk = ifelse(ps_percentile < 33, 1, 0),
    governance = (Control_of_Corruption_percentile + Government_Effectiveness_percentile +
                  Political_Stability_percentile + Rule_of_Law_percentile +
                  Regulatory_Quality_percentile + Voice_and_Accountability_percentile) / 6
  )

# Show the imbalance problem
cat("DATASET COMPOSITION:\n")
cat("====================\n")
sector_counts <- table(df_projects$sector_group)
print(sector_counts)
cat("\n")
cat(sprintf("Renewable Energy: %d (%.1f%% of total)\n", 
            sum(df_projects$is_renewable), 100*mean(df_projects$is_renewable)))
cat(sprintf("This creates an IMBALANCED comparison problem.\n\n"))

################################################################################
# APPROACH 1: WITHIN-ENERGY COMPARISON
# Compare renewable vs conventional WITHIN energy sector only
# This creates a more balanced comparison within similar project types
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("APPROACH 1: WITHIN-ENERGY COMPARISON\n")
cat("Renewable vs Conventional Energy (controls for sector effects)\n")
cat(rep("=", 80), "\n\n", sep = "")

# Filter to energy projects only
energy_data <- df_projects %>%
  filter(is_energy == 1, !is.na(private_share),
         !is.na(governance), !is.na(log_gdp_pc),
         !is.na(domestic_credit), !is.na(fdi_gdp))

cat(sprintf("Energy sector sample: %d projects\n", nrow(energy_data)))
cat(sprintf("  - Renewable: %d (%.1f%%)\n", 
            sum(energy_data$is_renewable), 100*mean(energy_data$is_renewable)))
cat(sprintf("  - Conventional: %d (%.1f%%)\n\n", 
            sum(energy_data$is_conventional_energy), 
            100*mean(energy_data$is_conventional_energy)))

# Progressive specifications for energy sector
app1_1 <- feols(
  private_share ~ is_renewable | country_code + year,
  data = energy_data, vcov = "hetero"
)

app1_2 <- feols(
  private_share ~ is_renewable + governance | country_code + year,
  data = energy_data, vcov = "hetero"
)

app1_3 <- feols(
  private_share ~ is_renewable + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = energy_data, vcov = "hetero"
)

app1_4 <- feols(
  private_share ~ is_renewable * high_risk + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = energy_data, vcov = "hetero"
)

app1_5 <- feols(
  private_share ~ is_renewable * high_risk + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp + has_MLS + log_investment | country_code + year,
  data = energy_data, vcov = ~country_code
)

cat("Within-Energy Results:\n")
cat(sprintf("  Renewable effect (Col 5): %.4f (%.2f pp)\n", 
            coef(app1_5)["is_renewable"], coef(app1_5)["is_renewable"] * 100))

# Save table
tryCatch({
  modelsummary(
    list("(1)" = app1_1, "(2)" = app1_2, "(3)" = app1_3, 
         "(4)" = app1_4, "(5)" = app1_5),
    output = file.path(output_dir, "h3_alternative/tables/approach1_within_energy.html"),
    title = "Table H3-A: Within-Energy Sector Comparison",
    coef_rename = c(
      "is_renewable" = "Renewable Energy",
      "is_renewable:high_risk" = "Renewable × High Risk",
      "high_risk" = "High Risk Country",
      "governance" = "Governance",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI (% GDP)",
      "has_MLS" = "Multilateral Support",
      "log_investment" = "Log Investment Size"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = "Sample: Energy sector projects only (renewable vs conventional)."
  )
  cat("✓ Approach 1 table saved\n")
}, error = function(e) cat("⚠ Table creation failed\n"))

################################################################################
# APPROACH 2: INVESTMENT AMOUNT (not just share)
# Since private share has ceiling effect (97%+), analyze TOTAL investment
# Question: Do renewable projects attract LARGER investments?
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("APPROACH 2: INVESTMENT AMOUNT ANALYSIS\n")
cat("Focus on investment SIZE, not just private share\n")
cat(rep("=", 80), "\n\n", sep = "")

# All sectors comparison
all_sectors_data <- df_projects %>%
  filter(sector_group %in% c("Renewable Energy", "Conventional Energy", 
                             "Transport", "Water"),
         !is.na(log_investment), !is.na(governance), 
         !is.na(log_gdp_pc), !is.na(domestic_credit), !is.na(fdi_gdp))

cat(sprintf("Full sample: %d projects\n", nrow(all_sectors_data)))

# DV: Log total investment
app2_1 <- feols(
  log_investment ~ is_renewable | country_code + year,
  data = all_sectors_data, vcov = "hetero"
)

app2_2 <- feols(
  log_investment ~ is_renewable + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = all_sectors_data, vcov = "hetero"
)

app2_3 <- feols(
  log_investment ~ is_renewable * private_share + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = all_sectors_data, vcov = "hetero"
)

# DV: Log private investment (size × share)
app2_4 <- feols(
  log_private_investment ~ is_renewable + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = all_sectors_data %>% filter(private_share > 0),
  vcov = "hetero"
)

# Sector dummies (base = Transport)
app2_5 <- feols(
  log_investment ~ i(sector_group, ref = "Transport") + governance + 
    log_gdp_pc + domestic_credit + fdi_gdp | country_code + year,
  data = all_sectors_data,
  vcov = ~country_code
)

cat("Investment Amount Results:\n")
cat(sprintf("  Renewable effect on log investment (Col 2): %.4f (%.1f%% larger)\n",
            coef(app2_2)["is_renewable"], 100*(exp(coef(app2_2)["is_renewable"])-1)))

# Save table
tryCatch({
  modelsummary(
    list("(1)" = app2_1, "(2)" = app2_2, "(3)" = app2_3, 
         "(4)" = app2_4, "(5)" = app2_5),
    output = file.path(output_dir, "h3_alternative/tables/approach2_investment_amount.html"),
    title = "Table H3-B: Investment Amount Analysis",
    coef_rename = c(
      "is_renewable" = "Renewable Energy",
      "is_renewable:private_share" = "Renewable × Private Share",
      "private_share" = "Private Share",
      "governance" = "Governance",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI (% GDP)"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "DV in columns 1-3: Log total investment.",
      "DV in column 4: Log private investment (investment × private share).",
      "Column 5: Sector fixed effects (base = Transport)."
    )
  )
  cat("✓ Approach 2 table saved\n")
}, error = function(e) cat("⚠ Table creation failed\n"))

################################################################################
# APPROACH 3: SELECTION & PROJECT CHARACTERISTICS
# Address: Why are so many projects renewable?
# Test: Do renewable projects differ in characteristics?
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("APPROACH 3: PROJECT SELECTION ANALYSIS\n")
cat("Understanding WHY renewable projects dominate\n")
cat(rep("=", 80), "\n\n", sep = "")

# Logit: What predicts renewable vs traditional?
selection_data <- df_projects %>%
  filter(sector_group %in% c("Renewable Energy", "Transport", "Water")) %>%
  mutate(.row = row_number())

# Identify the exact rows used by the logit (drop rows with NA in model RHS)
used_rows <- selection_data %>%
  filter(!is.na(governance), !is.na(log_gdp_pc),
         !is.na(domestic_credit), !is.na(fdi_gdp), !is.na(high_risk))

# Fit the selection (logit) model on the used rows only
app3_1 <- feglm(
  is_renewable ~ governance + log_gdp_pc + domestic_credit + fdi_gdp + 
    high_risk | country_code + year,
  data = used_rows,
  family = binomial(link = "logit")
)

# Predict only for the rows actually used in estimation, then merge back
preds <- predict(app3_1, newdata = used_rows, type = "response")
used_rows <- used_rows %>% mutate(pred_renewable = preds)

selection_data <- selection_data %>%
  left_join(used_rows %>% select(.row, pred_renewable), by = ".row") %>%
  select(-.row)

# Stage 2: Private share controlling for selection
app3_2 <- feols(
  private_share ~ is_renewable + pred_renewable + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = selection_data,
  vcov = "hetero"
)

# Heckman-style: Include inverse Mills ratio approximation
selection_data <- selection_data %>%
  mutate(
    mills_ratio = ifelse(is_renewable == 1, 
                         dnorm(qnorm(pred_renewable)) / pred_renewable,
                         -dnorm(qnorm(1-pred_renewable)) / (1-pred_renewable))
  )

app3_3 <- feols(
  private_share ~ is_renewable + mills_ratio + governance + log_gdp_pc + 
    domestic_credit + fdi_gdp | country_code + year,
  data = selection_data %>% filter(!is.na(mills_ratio), !is.infinite(mills_ratio)),
  vcov = "hetero"
)

cat("Selection Analysis Results:\n")
cat("Factors predicting renewable projects:\n")
cat(sprintf("  - Governance: %.4f\n", coef(app3_1)["governance"]))
cat(sprintf("  - High risk: %.4f\n", coef(app3_1)["high_risk"]))
cat("\nPrivate share after controlling for selection:\n")
cat(sprintf("  - Renewable effect: %.4f (%.2f pp)\n\n",
            coef(app3_3)["is_renewable"], coef(app3_3)["is_renewable"] * 100))

# Save table
tryCatch({
  modelsummary(
    list("Selection Model" = app3_1, 
         "Private Share (1)" = app3_2,
         "Private Share (2)" = app3_3),
    output = file.path(output_dir, "h3_alternative/tables/approach3_selection.html"),
    title = "Table H3-C: Selection & Two-Stage Analysis",
    coef_rename = c(
      "is_renewable" = "Renewable Energy",
      "pred_renewable" = "Predicted Renewable (from Stage 1)",
      "mills_ratio" = "Inverse Mills Ratio",
      "governance" = "Governance",
      "log_gdp_pc" = "Log GDP per capita",
      "domestic_credit" = "Domestic Credit (% GDP)",
      "fdi_gdp" = "FDI (% GDP)",
      "high_risk" = "High Risk Country"
    ),
    gof_map = c("nobs", "r.squared"),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    notes = list(
      "Column 1: Logit model predicting renewable vs traditional.",
      "Columns 2-3: Private share models controlling for selection.",
      "Mills ratio approximates Heckman selection correction."
    )
  )
  cat("✓ Approach 3 table saved\n")
}, error = function(e) cat("⚠ Table creation failed\n"))

################################################################################
# VISUALIZATION: Comparing Approaches
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("CREATING VISUALIZATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

# Figure 1: Effect sizes across approaches
tryCatch({
  effect_sizes <- data.frame(
    Approach = c("Within Energy\n(Approach 1)", 
                 "Investment Amount\n(Approach 2)",
                 "Selection Model\n(Approach 3)"),
    Coefficient = c(coef(app1_5)["is_renewable"],
                   coef(app2_2)["is_renewable"],
                   coef(app3_3)["is_renewable"]),
    SE = c(summary(app1_5)$se["is_renewable"],
           summary(app2_2)$se["is_renewable"],
           summary(app3_3)$se["is_renewable"]),
    DV = c("Private Share", "Log Investment", "Private Share")
  ) %>%
    mutate(
      CI_lower = Coefficient - 1.96 * SE,
      CI_upper = Coefficient + 1.96 * SE
    )
  
  fig1 <- ggplot(effect_sizes, aes(x = Coefficient, y = Approach)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
    labs(
      title = "Renewable Energy Effect Across Three Approaches",
      subtitle = "Coefficient estimates with 95% confidence intervals",
      x = "Coefficient Estimate",
      y = ""
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "h3_alternative/figures/fig1_effect_comparison.png"),
         fig1, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 1 saved\n")
}, error = function(e) cat("⚠ Figure creation failed\n"))

# Figure 2: Distribution comparison
tryCatch({
  fig2_data <- df_projects %>%
    filter(sector_group %in% c("Renewable Energy", "Conventional Energy", 
                               "Transport", "Water")) %>%
    mutate(sector_type = case_when(
      is_renewable == 1 ~ "Renewable",
      is_conventional_energy == 1 ~ "Conventional Energy",
      TRUE ~ "Traditional (Transport/Water)"
    ))
  
  fig2 <- ggplot(fig2_data, aes(x = log_investment, fill = sector_type)) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("#2ca02c", "#ff7f0e", "#d62728"),
                     name = "Sector Type") +
    labs(
      title = "Investment Size Distribution by Sector",
      subtitle = "Addressing the question: Are renewable projects systematically different?",
      x = "Log Investment (Real 2020 USD)",
      y = "Density"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", plot.title = element_text(face = "bold"))
  
  ggsave(file.path(output_dir, "h3_alternative/figures/fig2_investment_distribution.png"),
         fig2, width = 10, height = 6, dpi = 300)
  cat("✓ Figure 2 saved\n")
}, error = function(e) cat("⚠ Figure creation failed\n"))

################################################################################
# SUMMARY & RECOMMENDATIONS
################################################################################

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY & RECOMMENDATIONS\n")
cat(rep("=", 80), "\n\n", sep = "")

cat("PROBLEM DIAGNOSED:\n")
cat("  - 64% of projects are renewable (479/739)\n")
cat("  - Private share is 97%+ across all sectors (ceiling effect)\n")
cat("  - This makes simple comparison problematic\n\n")

cat("THREE ALTERNATIVE APPROACHES:\n\n")

cat("1. WITHIN-ENERGY COMPARISON (Recommended for H3)\n")
cat("   Pros: Controls for sector-specific effects\n")
cat("         More balanced comparison (renewable vs conventional)\n")
cat("   Cons: Smaller sample (energy sector only)\n")
cat(sprintf("   Result: Renewable effect = %.4f (%.2f pp)\n\n",
            coef(app1_5)["is_renewable"], coef(app1_5)["is_renewable"] * 100))

cat("2. INVESTMENT AMOUNT ANALYSIS\n")
cat("   Pros: Avoids ceiling effect problem\n")
cat("         Uses full sample\n")
cat("   Cons: Changes the question (size vs share)\n")
cat(sprintf("   Result: Renewable projects %.1f%% larger\n\n",
            100*(exp(coef(app2_2)["is_renewable"])-1)))

cat("3. SELECTION MODEL\n")
cat("   Pros: Addresses 'why so many renewable?' question\n")
cat("         Controls for non-random sector selection\n")
cat("   Cons: Complex interpretation\n")
cat(sprintf("   Result: After selection correction = %.4f (%.2f pp)\n\n",
            coef(app3_3)["is_renewable"], coef(app3_3)["is_renewable"] * 100))

cat("RECOMMENDATION FOR YOUR PAPER:\n")
cat("  - Use Approach 1 (Within-Energy) as MAIN specification\n")
cat("  - Use Approach 2 (Investment Amount) as ROBUSTNESS\n")
cat("  - Use Approach 3 (Selection) to address reviewer concerns\n")
cat("  - In discussion: Acknowledge data limitations clearly\n\n")

cat("FILES SAVED TO:\n")
cat("  ", output_dir, "/h3_alternative/\n\n", sep = "")

cat(rep("=", 80), "\n\n", sep = "")

################################################################################
# ROBUSTNESS CHECKS FOR H3
# ==============================================================================
# ROBUSTNESS CHECK 1: PHYSICAL CAPACITY (The "Efficiency vs. Scale" Test)
# ==============================================================================
# We verify if renewables are smaller in Megawatts (MW), not just Dollars.

# Filter for Energy projects that have valid Capacity data (column `pcapacity`)
df_energy_phys <- df_projects %>%
  filter(sector == "Energy") %>%
  mutate(
    pcapacity_num = as.numeric(pcapacity),
    is_renewable = ifelse(Renewables == "Renewables", 1, 0)
  ) %>%
  filter(!is.na(pcapacity_num), pcapacity_num > 0)

# Run the model on Capacity (MW) instead of Investment ($)
# Use the numeric `pcapacity_num` and log-transform safely
h3_robust_capacity <- feols(log(pcapacity_num) ~ is_renewable + 
                              log(GDP_constant_2015_USD) + 
                              Rule_of_Law_percentile | 
                              country_code, 
                            data = df_energy_phys, 
                            vcov = ~country_code)

# ==============================================================================
# ROBUSTNESS CHECK 2: TEMPORAL STABILITY (Pre vs. Post Paris Agreement)
# ==============================================================================
# We test if the "Small Scale" trap is an old phenomenon or if it persists 
# in the modern era (Post-2015).

df_energy <- df_projects %>%
  filter(sector == "Energy", investment_real > 0) %>%
  mutate(
    is_renewable = ifelse(Renewables == "Renewables", 1, 0),
    period_split = ifelse(year >= 2016, "Post-Paris (2016-2023)", "Pre-Paris (2005-2015)")
  )

# Interaction model: Does the renewable penalty get smaller (less negative) over time?
h3_robust_time <- feols(log(investment_real) ~ is_renewable * period_split + 
                          log(GDP_constant_2015_USD) | 
                          country_code, 
                        data = df_energy, 
                        vcov = ~country_code)

# ==============================================================================
# OUTPUT: COMBINED ROBUSTNESS TABLE
# ==============================================================================

rows_h3 <- tribble(
  ~term,          ~"Model 1 (Physical)", ~"Model 2 (Temporal)",
  "Dep. Variable", "Log Capacity (MW)",   "Log Investment ($)",
  "Sample",        "Energy Only",         "Energy Only"
)

tryCatch({
  modelsummary(
    list(
      "(1) Physical Scale" = h3_robust_capacity,
      "(2) Time Trends" = h3_robust_time
    ),
    coef_map = c(
      "is_renewable" = "Renewable Dummy",
      "is_renewable:period_splitPost-Paris (2016-2023)" = "Renewable × Post-Paris",
      "period_splitPost-Paris (2016-2023)" = "Post-Paris Dummy"
    ),
    stars = c('*' = .1, '**' = .05, '***' = .01),
    add_rows = rows_h3,
    title = "Table H3-D: Robustness Checks (Physical Scale & Time Stability)",
    output = file.path(output_dir, "h3_alternative/tables/h3_robustness_checks.docx")
  )
  cat("✓ H3 robustness table saved: h3_robustness_checks.docx\n")
}, error = function(e) {
  # Fallback to HTML if DOCX fails (e.g., pandoc missing)
  tryCatch({
    modelsummary(
      list(
        "(1) Physical Scale" = h3_robust_capacity,
        "(2) Time Trends" = h3_robust_time
      ),
      coef_map = c(
        "is_renewable" = "Renewable Dummy",
        "is_renewable:period_splitPost-Paris (2016-2023)" = "Renewable × Post-Paris",
        "period_splitPost-Paris (2016-2023)" = "Post-Paris Dummy"
      ),
      stars = c('*' = .1, '**' = .05, '***' = .01),
      add_rows = rows_h3,
      title = "Table H3-D: Robustness Checks (Physical Scale & Time Stability)",
      output = file.path(output_dir, "h3_alternative/tables/h3_robustness_checks.html")
    )
    cat("✓ H3 robustness table saved (HTML fallback): h3_robustness_checks.html\n")
  }, error = function(e2) {
    cat("⚠ Could not save H3 robustness table: ", e$message, " / ", e2$message, "\n")
  })
})
