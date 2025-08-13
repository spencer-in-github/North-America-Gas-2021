"""
Script Name: extract_interstate_flows.py
Author: Spencer Zhang
Description:
    This script extracts annual interstate natural gas flow data from multiple EIA Excel files.
    Raw data downloaded from https://www.eia.gov/dnav/ng/ng_move_ist_a2dcu_nus_a.htm accessed
    July 03 2025.
    It reads each file in a specified directory, filters for the year 2022, parses directional
    flows between states from column names, and appends all flows into a single clean DataFrame.

    Output: interstate_flow.csv

Dependencies:
    - pandas
    - os
    - glob

Data Source:
    U.S. Energy Information Administration (EIA) - International & Interstate Movements of Natural Gas by State

Assumptions:
    - All files are named with the pattern "ng_move_ist_a2dcu_*.xls"
    - Column names follow the structure: "<To State> Natural Gas Net Receipts From <From State> (MMcf)"
    - Year is extracted from the "Date" column formatted as full dates (e.g., "2022-06-30")
"""

import pandas as pd
import os
import glob
import numpy as np

# ─────────────────────────────────────────────────────────────────────────────
# CONFIGURATION
# ─────────────────────────────────────────────────────────────────────────────

raw_path = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/interstate flows/raw/"
path_save = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/interstate flows/"
file_pattern = os.path.join(raw_path, "ng_move_ist_a2dcu_*.xls")
year = 2023  # target year

# ─────────────────────────────────────────────────────────────────────────────
# INITIALIZATION
# ─────────────────────────────────────────────────────────────────────────────

file_list = glob.glob(file_pattern)
all_dfs = []

# ─────────────────────────────────────────────────────────────────────────────
# PROCESS EACH FILE
# ─────────────────────────────────────────────────────────────────────────────

for file_path in file_list:
    try:
        # Read from "Data 3" sheet, skipping metadata rows
        df_raw = pd.read_excel(file_path, sheet_name="Data 3", skiprows=2)

        # Parse "Date" column and filter to rows from the target year
        df_raw["Date"] = pd.to_datetime(df_raw["Date"], errors="coerce")
        df_2022 = df_raw[df_raw["Date"].dt.year == year]

        if df_2022.empty:
            print(f"[SKIPPED] No data for year {year} in {os.path.basename(file_path)}")
            continue

        # Take first row corresponding to the target year
        row_2022 = df_2022.iloc[0]

        # Initialize list of records for this file
        result_rows = []

        # Loop through data columns (excluding "Date")
        for col in df_2022.columns[1:]:
            value = row_2022[col]

            # Only process non-null values and columns containing "from"/"From"
            if pd.notna(value) and "from" in col.lower():
                try:
                    # Extract "to_state" from the beginning of the column name (before "Natural")
                    to_state = col.split("Natural")[0].strip()

                    # Extract "from_state" from the string after "from" (case-insensitive) and before "("
                    from_index = col.lower().index("from")
                    from_state_raw = col[from_index + 4:]  # skip "from"
                    from_state = from_state_raw.split("(")[0].strip()

                    result_rows.append({
                        "year": year,
                        "from_state": from_state,
                        "to_state": to_state,
                        "flow_value_mmcf": value,
                        "source_file": os.path.basename(file_path)
                    })
                except Exception as e:
                    print(f"[WARNING] Could not parse column '{col}' in {os.path.basename(file_path)}: {e}")

        # Create DataFrame for this file and store
        flow_df = pd.DataFrame(result_rows)
        all_dfs.append(flow_df)

    except Exception as e:
        print(f"[ERROR] Failed to process {os.path.basename(file_path)}: {e}")

# ─────────────────────────────────────────────────────────────────────────────
# FINALIZE AND EXPORT
# ─────────────────────────────────────────────────────────────────────────────

# Concatenate all DataFrames into one
combined_df = pd.concat(all_dfs, ignore_index=True)

# Preview output
print(combined_df.head())

# Save to CSV
output_path = os.path.join(path_save, "interstate_flow.csv")
combined_df.to_csv(output_path, index=False)
print(f"[SUCCESS] Saved combined data to {output_path}")

# ─────────────────────────────────────────────────────────────────────────────
# Turn into flow matrix
# ─────────────────────────────────────────────────────────────────────────────

# # Load the flow data
# flow_df = combined_df

# # Pivot the DataFrame to create the state-to-state flow matrix
# flow_matrix = flow_df.pivot_table(
#     index="from_state",
#     columns="to_state",
#     values="flow_value_mmcf",
#     aggfunc="sum",    # Sum flows if there are duplicates
#     fill_value=0      # Replace NaN with 0
# )

# # Display a preview
# print(flow_matrix.head())

# # Optionally save to CSV
# flow_matrix.to_csv(path_save + "state_to_state_flow_matrix.csv")
# print("Flow matrix saved to state_to_state_flow_matrix.csv")

# # Identify rows not in US_states → treat them as "International"
# non_us_rows = [state for state in flow_matrix.index if state not in US_states]

# # Sum the non-US rows into one
# international_row = flow_matrix.loc[non_us_rows].sum()

# # Drop non-US rows from matrix
# flow_matrix_cleaned = flow_matrix.drop(index=non_us_rows)

# # Append 'International' row
# flow_matrix_cleaned.loc['International'] = international_row

# # Reorder if you want 'International' on top (optional)
# flow_matrix_cleaned = flow_matrix_cleaned.reset_index().set_index('from_state')
# flow_matrix_cleaned = flow_matrix_cleaned.reindex(['International'] + [s for s in flow_matrix_cleaned.index if s != 'International']).clip(lower=0)

# # Define full list of valid U.S. states + offshore + 'International'
# valid_states = [
#     'Alabama', 'Alaska', 'Arizona', 'Arkansas','California', 'Colorado',
#     'Connecticut', 'Delaware', 'District of Columbia', 'Florida', 'Georgia',
#     'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana',
#     'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi',
#     'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey',
#     'New Mexico', 'New York', 'North Carolina', 'North Dakota', 'Ohio',
#     'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina',
#     'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia',
#     'Washington', 'West Virginia', 'Wisconsin', 'Wyoming',
#     'Federal Offshore--Gulf of America', 'International'
# ]

# # Reindex rows and columns to enforce square shape
# flow_matrix_square = flow_matrix_cleaned.reindex(
#     index=valid_states,
#     columns=valid_states,
#     fill_value=0
# )

# # Sanity check
# print("Final square matrix shape:", flow_matrix_square.shape)

# # Load state production and consumption data
# prod_cons_df = pd.read_csv('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/state_prod_consump.csv')

# # Fill NA with 0 for safe minimum computation
# prod_cons_df = prod_cons_df.fillna(0)

# # Compute the minimum of production and consumption
# # prod_cons_df["diag_value"] = prod_cons_df[[
# #     "natural_gas_dry_production_MMcf",
# #     "natural_gas_consumption_value_MMcf"
# # ]].min(axis=1)

# prod_cons_df["diag_value"] = prod_cons_df["natural_gas_dry_production_MMcf"]

# # Set diagonal values in flow matrix
# for state, diag_val in zip(prod_cons_df["state"], prod_cons_df["diag_value"]):
#     if state in flow_matrix_square.index and state in flow_matrix_square.columns:
#         flow_matrix_square.loc[state, state] = diag_val

# # Save updated matrix
# updated_path = path_save + "state_to_state_flow_matrix_with_diag.csv"
# flow_matrix_square.to_csv(updated_path)
# print(f"Saved matrix with diagonal values to: {updated_path}")
