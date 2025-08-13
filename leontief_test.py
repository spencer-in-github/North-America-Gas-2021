
import pandas as pd
import numpy as np

path_save = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/aerial loss rate/'

# require county FIPS, basin name, and similarity match results (up/midstream loss rates)
oil_input_df = pd.read_excel('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG Latest Input Sheet/US_INPUT.xlsx', sheet_name="OIL")
gas_input_df = pd.read_excel('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG Latest Input Sheet/US_INPUT.xlsx', sheet_name="GAS")

combined_df = pd.concat([oil_input_df[['GEOID','AAPG.Basin.x']], gas_input_df[['GEOID','AAPG.Basin.x']]], ignore_index=True)

opgee_counties = combined_df.groupby('GEOID', as_index=False).first().rename(columns = {"GEOID":"FIPS","AAPG.Basin.x": "AAPG.Basin"}).merge(
    merged_df,
    left_on="AAPG.Basin",
    right_on="AAPG_BASIN_NAME",
    how = "left"
).drop(
    columns = "AAPG_BASIN_NAME"
)

opgee_counties['AIR_WELL_LOSS_RATE'] = opgee_counties['AIR_WELL_LOSS_RATE'] * 0.01
opgee_counties['AIR_MID_LOSS_RATE'] = opgee_counties['AIR_MID_LOSS_RATE'] * 0.01
 
opgee_counties.to_csv(path_save + "equipment_match_aerial_rate_lookup_table.csv")





# Read matrix from Excel sheet
file_path = "/Users/spencerzhang/Documents/PhD/Research/2023MAY NA Gas/Manuscript/Major revision 2025June/Farah Loentief/Spencer Leontief matrices 2021.xlsx"  # <-- replace with your file path
sheet_name = "(I - A_norm)"

# Read the matrix (assuming it's numeric and has no headers)
matrix_df = pd.read_excel(file_path, sheet_name=sheet_name, header=None)

# Convert to NumPy array
matrix = matrix_df.to_numpy()

# Check if the matrix is square
rows, cols = matrix.shape
if rows != cols:
    print("Matrix is not square, cannot be inverted.")
else:
    # Check if matrix is invertible (i.e., determinant is not zero)
    det = np.linalg.det(matrix)
    print(f"Determinant: {det}")

    if np.isclose(det, 0):
        print("Matrix is singular and cannot be inverted.")
    else:
        # Compute the inverse
        inverse_matrix = np.linalg.inv(matrix)
        print("Matrix successfully inverted.")
        # Optionally, print or save inverse
        # print(inverse_matrix)
