import pandas as pd
import numpy as np

# File path
folder_path = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/leontief/"
matrix_path = folder_path + "matrix_block_producers_to_consumers.csv"

# Read the matrix
matrix_df = pd.read_csv(matrix_path, index_col=0)

# Compute row and column sums
row_sums = matrix_df.sum(axis=1).replace(0, np.nan)  # avoid division by zero
col_sums = matrix_df.sum(axis=0).replace(0, np.nan)

# Compute contribution percentages
row_contrib = matrix_df.div(row_sums, axis=0) * 100  # row-wise
col_contrib = matrix_df.div(col_sums, axis=1) * 100  # column-wise

# Handle NaN values in row contributions
# For rows with NaN sum, put 100 in diagonal cell and 0 elsewhere
for idx in row_contrib.index:
    if pd.isna(row_sums[idx]):
        # Set diagonal cell to 100 (where row name equals column name)
        if idx in row_contrib.columns:
            row_contrib.loc[idx, idx] = 100
        # Set all other cells in this row to 0
        row_contrib.loc[idx, :] = 0
        if idx in row_contrib.columns:
            row_contrib.loc[idx, idx] = 100

# Handle NaN values in column contributions
# For columns with NaN sum, put 100 in diagonal cell and 0 elsewhere
for col in col_contrib.columns:
    if pd.isna(col_sums[col]):
        # Set diagonal cell to 100 (where column name equals row name)
        if col in col_contrib.index:
            col_contrib.loc[col, col] = 100
        # Set all other cells in this column to 0
        col_contrib.loc[:, col] = 0
        if col in col_contrib.index:
            col_contrib.loc[col, col] = 100

# Pivot to long format for row contributions
row_contrib_long = row_contrib.reset_index().melt(
    id_vars='index', 
    var_name='destination', 
    value_name='contribution_percentage'
).rename(columns={'index': 'source'})

# Pivot to long format for column contributions
col_contrib_long = col_contrib.reset_index().melt(
    id_vars='index', 
    var_name='destination', 
    value_name='contribution_percentage'
).rename(columns={'index': 'source'})

# Optional: Save to CSV
row_contrib.to_csv(folder_path+ "row_contribution_percentage_production.csv")
col_contrib.to_csv(folder_path +"column_contribution_percentage_receiving.csv")

# Save long format versions
row_contrib_long.to_csv(folder_path+ "row_contribution_percentage_production_long.csv", index=False)
col_contrib_long.to_csv(folder_path +"column_contribution_percentage_receiving_long.csv", index=False)

# Display the results (if in a notebook or script with UI)
print("Row Contribution Percentage (first 5 rows):")
print(row_contrib.head())

print("\nColumn Contribution Percentage (first 5 columns):")
print(col_contrib.iloc[:, :5].head())

print("\nRow Contribution Percentage (long format, first 10 rows):")
print(row_contrib_long.head(10))

print("\nColumn Contribution Percentage (long format, first 10 rows):")
print(col_contrib_long.head(10))
