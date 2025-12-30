
import pandas as pd

# Load the datasets
ppi_df = pd.read_csv(r"C:\Users\aorta\OneDrive\Documents\Boston University\3. Fall 2025\EC561 Public Economics 1\Research Paper\Analysis Output\h1_governance_revised\tables\df_projects_with_lags.csv")
wb_df = pd.read_csv(r"C:\Users\aorta\OneDrive\Documents\Boston University\3. Fall 2025\EC561 Public Economics 1\Research Paper\Analysis Output\h1_governance_revised\tables\wb_lagged.csv")

# Display the first few rows and info for PPI dataframe
print("PPI Data Head:")
print(ppi_df.head())
print("\nPPI Data Info:")
print(ppi_df.info())

# Display the first few rows and info for WB dataframe
print("\nWB Data Head:")
print(wb_df.head())
print("\nWB Data Info:")
print(wb_df.info())

# Merge the datasets
# Left join to keep all PPI projects and add WB data for the corresponding country and year
merged_df = pd.merge(
    ppi_df,
    wb_df,
    how='left',
    left_on=['country_code', 'IY'],
    right_on=['country', 'year']
)

# Check the shape of the merged dataframe
print("Merged Data Shape:", merged_df.shape)

# Inspect columns to see if duplicate columns were created (suffixes)
print("Merged Data Columns:", merged_df.columns.tolist())

# Save the merged dataframe to a CSV file
output_filename = r"C:\Users\aorta\OneDrive\Documents\Boston University\3. Fall 2025\EC561 Public Economics 1\Research Paper\Final Report\Final\merged_PPI_WB_2005_2023.csv"
merged_df.to_csv(output_filename, index=False)

print(f"Merged data saved to {output_filename}")