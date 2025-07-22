"""
Script Name: extract_state_gas_data.py
Author: Spencer Zhang
Created: July 4, 2025

Description:
    This script processes annual state-level natural gas data (consumption and dry production)
    from EIA Excel files. It reads data from the second sheet of each file, filters for the
    year 2022, extracts state names and values from column headers, and outputs clean CSV files.

    Output CSV files:
        - state_consumption_2022.csv
        - state_production_2022.csv

Dependencies:
    - pandas

Data Source:
    U.S. Energy Information Administration (EIA)
    Accessed: July 4, 2025
    Consumption: https://www.eia.gov/dnav/ng/ng_cons_sum_a_epg0_vc0_mmcf_a.htm
    Production:  https://www.eia.gov/dnav/ng/ng_sum_snd_a_epg0_fpd_mmcf_a.htm

Assumptions:
    - Input files are Excel workbooks with data in the second sheet.
    - Data starts on the third row (i.e., skiprows=2).
    - Column names follow the structure "<State> Natural Gas Consumption [...]" or
      "<State> Dry Natural Gas Production [...]".
    - The 'Date' column is in datetime format.
    - Only data from the year 2022 is processed.
"""

import pandas as pd
import os

def extract_state_data(
    file_path: str,
    output_path: str,
    year: int,
    keyword: str,
    value_col_name: str
) -> pd.DataFrame:
    """
    Extracts state-level data from an EIA Excel file.

    Args:
        file_path (str): Path to the input Excel file.
        output_path (str): Path to save the output CSV.
        year (int): Year of interest to filter.
        keyword (str): Keyword to split column names on (e.g., 'Natural', 'Dry').
        value_col_name (str): Name for the output value column.

    Returns:
        pd.DataFrame: Cleaned DataFrame with 'state' and value columns.
    """
    # Read the Excel file, second sheet, skip top two rows
    df = pd.read_excel(file_path, sheet_name=1, skiprows=2)

    # Parse date and filter for target year
    df["Date"] = pd.to_datetime(df["Date"], errors="coerce")
    df_yr = df[df["Date"].dt.year == year].dropna(axis=1, how='all')

    # Extract state and value pairs
    records = []
    for col in df_yr.columns:
        if col != "Date" and keyword in col:
            state = col.split(keyword)[0].strip()
            value = df_yr[col].values[0]
            records.append({"state": state, value_col_name: value})

    result_df = pd.DataFrame(records)
    result_df.to_csv(output_path, index=False)
    print(f"Saved: {output_path}")
    return result_df


# ───────────────────────────────────────────────
# Process state-level consumption/production data
# ───────────────────────────────────────────────
year = 2022

# Process natural gas consumption
state_consumption = extract_state_data(
    file_path="/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/raw/NG_CONS_SUM_A_EPG0_VC0_MMCF_A.xls",
    output_path="/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/state_consumption_2022.csv",
    year=year,
    keyword="Natural",
    value_col_name="natural_gas_consumption_value_MMcf"
)

# Process dry gas production
state_production = extract_state_data(
    file_path="/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/raw/NG_SUM_SND_A_EPG0_FPD_MMCF_A.xls",
    output_path="/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/state_production_2022.csv",
    year=year,
    keyword="Dry",
    value_col_name="natural_gas_dry_production_MMcf"
)

# ───────────────────────────────────────────────
# Merge
# ───────────────────────────────────────────────

path_save = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/"

df = state_consumption.merge(
    state_production,
    left_on= 'state',
    right_on='state',
    how = 'outer'
)

# Fill NaN with zeros
df["natural_gas_dry_production_MMcf"] = df["natural_gas_dry_production_MMcf"].fillna(0)
df["natural_gas_consumption_value_MMcf"] = df["natural_gas_consumption_value_MMcf"].fillna(0)

# Calculate net surplus = production - consumption
df["net_surplus_MMcf"] = (
    df["natural_gas_dry_production_MMcf"] - df["natural_gas_consumption_value_MMcf"]
)

df.to_csv(path_save + "state_prod_consump.csv")


# ───────────────────────────────────────────────
# Process state-level consumption data by sector
# ───────────────────────────────────────────────

import os 
import pandas as pd
import re

def extract_state_data_by_sector(
    file_path: str,
    output_path: str,
    year: int,
    keyword: str,
    value_col_name: str
) -> pd.DataFrame:
    """
    Extracts state-level data from an EIA Excel file.

    Args:
        file_path (str): Path to the input Excel file.
        output_path (str): Path to save the output CSV.
        year (int): Year of interest to filter.
        keyword (str): Keyword to split column names on (e.g., 'Natural', 'Dry').
        value_col_name (str): Name for the output value column.

    Returns:
        pd.DataFrame: Cleaned DataFrame with 'state' and value columns.
    """
    # Read the Excel file, second sheet, skip top two rows
    df = pd.read_excel(file_path, sheet_name=1, skiprows=2)

    # Parse date and filter for target year
    df["Date"] = pd.to_datetime(df["Date"], errors="coerce")
    df_yr = df[df["Date"].dt.year == year].dropna(axis=1, how='all')

    # Extract state and value pairs
    records = []
    for col in df_yr.columns:
        if col != "Date" and keyword in col:
            state = col.split(keyword)[0].strip()
            # If it's missing or invalid, try alternative extraction
            if not state or state.lower() in ["na", "nan"]:
                # Try to extract the string between 'in' and '('
                match = re.search(r'in\s+(.*?)\s*\(', col, flags=re.IGNORECASE)
                if match:
                    state = match.group(1).strip()
                else:
                    state = "Unknown"
            
            sector = col.split("Natural Gas")[1].strip()
            # Keep only the first 4 words
            sector = " ".join(sector.split()[:4])
            value = df_yr[col].values[0]
            records.append({"state": state, value_col_name: value, "sector": sector})

    result_df = pd.DataFrame(records)
    result_df.to_csv(output_path, index=False)
    print(f"Saved: {output_path}")
    return result_df

# Define the folder path
folder_path = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/EIA state-level gas consumption by sector/raw/'
save_path = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/EIA state-level gas consumption by sector/'

# List all files (excluding directories)
file_names = [f for f in os.listdir(folder_path) if os.path.isfile(os.path.join(folder_path, f))]
year = 2022

all_dfs = []

# Print the file names
for file in file_names:
    print(file)

    # # Process natural gas consumption
    t = extract_state_data_by_sector(
        file_path= folder_path + file,
        output_path=save_path + "cleaned.csv",
        year=year,
        keyword="Natural",
        value_col_name="natural_gas_consumption_value_MMcf"
    )
    # Append result
    all_dfs.append(t)

# Concatenate all extracted DataFrames
df_combined = pd.concat(all_dfs, ignore_index=True)

# Save or inspect
df_combined.to_csv(save_path + "all_state_sector_consumption_2022.csv", index=False)
print(f"[DONE] Combined data saved to: {save_path}all_state_sector_consumption_2022.csv")

# Load the dataset
df = pd.read_csv(save_path + "all_state_sector_consumption_2022.csv")

df_wide = df_combined.pivot_table(
    index="state",                             # or "State" if column is capitalized
    columns="sector",                          # change this to actual column name for sector
    values="natural_gas_consumption_value_MMcf",
    aggfunc="sum",                             # in case of duplicates
    fill_value=0
)

print(df_wide.columns)


# Rename columns
# df_wide = df_wide.rename(columns={
#     df_wide.columns[0]: "commerical",
#     df_wide.columns[1]: "electric_power",
#     df_wide.columns[2]: "industrial", 
#     df_wide.columns[3]: "residential",
#     df_wide.columns[3]: "total",
#     df_wide.columns[3]: "vehicle"
# })

# Reset index if needed
df_wide = df_wide.reset_index()

# Save to CSV
df_wide.to_csv(save_path + "state_sector_consumption_wide.csv", index=False)
print("Saved pivoted table to: state_sector_consumption_wide.csv")

df_cleaned = pd.read_csv(save_path + "state_consumption_by_sector_cleaned.csv")

df_cleaned['ind'] = df_cleaned['Industrial Consumption (MMcf)']
df_cleaned['pow'] = df_cleaned['Deliveries to Electric Power']
df_cleaned['others'] = df_cleaned['Total Consumption (MMcf)'] - df_cleaned['ind'] - df_cleaned['pow']

df_cleaned[['state', 'ind', 'pow', 'others']].to_csv(save_path + 'state_consumption_weights.csv')

