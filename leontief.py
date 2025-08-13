import pandas as pd
import numpy as np

# === File paths ===
path_interstate = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/interstate flows/'
path_production = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/'
path_leontief = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/leontief/"
prod_file = path_production + "state_production_2022.csv"
cons_file = path_production + "state_consumption_2022.csv"
flow_file = path_interstate + "interstate_flow.csv"

# === Load raw data ===
prod_df = pd.read_csv(prod_file)
cons_df = pd.read_csv(cons_file)
flow_df = pd.read_csv(flow_file)

# === Define US states ===
US_states = ['Alabama', 'Alaska', 'Arizona', 'Arkansas','California', 'Colorado', 'Connecticut', 'Delaware',
       'District of Columbia', 
       'Federal Offshore--Gulf of America', 'Florida', 'Georgia',
     'Idaho', 'Illinois', 'Indiana', 
       'Iowa', 'Kansas', 'Kentucky',  'Louisiana', 'Maine', 'Maryland',
       'Massachusetts',  'Michigan', 'Minnesota', 'Mississippi',
       'Missouri', 'Montana', 'Nebraska',  'Nevada',
       'New Hampshire', 'New Jersey', 'New Mexico', 'New York',
       'North Carolina', 'North Dakota', 'Ohio', 'Oklahoma', 'Oregon',
       'Pennsylvania', 
       'Rhode Island', 'South Carolina', 'South Dakota',
     'Tennessee', 'Texas', 'Utah', 'Vermont',
       'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming', 'Hawaii']
print(f"Number of US states: {len(US_states)}")

# === Count non-zero producing states ===
non_zero_producers = prod_df[prod_df['natural_gas_dry_production_MMcf'] > 0]['state'].nunique()

# ... existing code ...
unique_nonzero_producers = sorted(prod_df[(prod_df['natural_gas_dry_production_MMcf'] > 0) & (prod_df['state'].isin(US_states))]['state'].unique())
print(f"Number of unique non-zero gas producing US states: {len(unique_nonzero_producers)}")
print("Names:", unique_nonzero_producers)

# Assign unique IDs to non-zero producers
producer_id_map = {state: idx + 1 for idx, state in enumerate(unique_nonzero_producers)}

# Add unique producer ID to production data (only for non-zero producers)
prod_df['producer_id'] = prod_df.apply(lambda row: producer_id_map[row['state']] if (row['state'] in producer_id_map) else np.nan, axis=1)

# Save the updated production data with IDs to a new CSV file
prod_df.to_csv(path_production + 'state_production_2023_with_ids.csv', index=False)
# ... existing code ...

# === Count unique (from_state, to_state) combinations ===

# Print unique from_state values
unique_from_states = set(flow_df['from_state'])
# print("Unique from_state values in flow_df:")
# print(sorted(unique_from_states))

# Print from_states not in US_states
not_in_us_states = [state for state in unique_from_states if state not in US_states]
if not_in_us_states:
    print("\nThe following from_state values are NOT in US_states:")
    print(sorted(not_in_us_states))
else:
    print("\nAll from_state values are in US_states.")

# Print unique to_state values
unique_to_states = set(flow_df['to_state'])
# print("Unique to_state values in flow_df:")
# print(sorted(unique_to_states))

# Print to_states not in US_states
not_in_us_states_to = [state for state in unique_to_states if state not in US_states]
if not_in_us_states_to:
    print("\nThe following to_state values are NOT in US_states:")
    print(sorted(not_in_us_states_to))
else:
    print("\nAll to_state values are in US_states.")

# Group non-US from_state and to_state as 'International'
flow_df['from_state'] = flow_df['from_state'].apply(lambda x: x if x in US_states else 'International')
flow_df['to_state'] = flow_df['to_state'].apply(lambda x: x if x in US_states else 'International')

# If flow_value_mmcf is negative, make it positive and swap from_state and to_state
if 'flow_value_mmcf' in flow_df.columns:
    mask_negative = flow_df['flow_value_mmcf'] < 0
    # Store original values for swapping
    temp_from = flow_df.loc[mask_negative, 'from_state'].copy()
    temp_to = flow_df.loc[mask_negative, 'to_state'].copy()
    # Swap from_state and to_state where flow_value_mmcf is negative
    flow_df.loc[mask_negative, 'from_state'] = temp_to.values
    flow_df.loc[mask_negative, 'to_state'] = temp_from.values
    # Make flow_value_mmcf positive
    flow_df.loc[mask_negative, 'flow_value_mmcf'] = flow_df.loc[mask_negative, 'flow_value_mmcf'].abs()
    # Drop duplicate rows
    flow_df = flow_df.drop_duplicates()

# Aggregate so each (from_state, to_state) pair is unique, summing flow_value_mmcf
if 'flow_value_mmcf' in flow_df.columns:
    flow_df = flow_df.groupby(['from_state', 'to_state'], as_index=False)['flow_value_mmcf'].sum()

# Assign unique IDs to each unique (from_state, to_state) pair
flow_df['interstate_id'] = range(1, len(flow_df) + 1)

flow_df.to_csv(path_interstate + 'interstate_flow_2023_flipped.csv', index=False)

# === Count gas consuming states (correct column name from file) ===
consuming_states = cons_df[cons_df['natural_gas_consumption_value_MMcf'] > 0]['state'].nunique()

# Get unique gas consuming regions (states with nonzero consumption)
unique_gas_consuming_regions = sorted(cons_df[(cons_df['natural_gas_consumption_value_MMcf'] > 0) & (cons_df['state'].isin(US_states))]['state'])

# Assign unique IDs to gas consuming states
consumer_id_map = {state: idx + 1 for idx, state in enumerate(unique_gas_consuming_regions)}
consumer_id_map['International'] = 53

# Add unique consumer ID to consumption data (only for non-zero consumers)
cons_df['consumer_id'] = cons_df.apply(lambda row: consumer_id_map[row['state']] if (row['state'] in consumer_id_map) else np.nan, axis=1)

# Save the updated consumption data with IDs to a new CSV file
cons_df.to_csv(path_production + 'state_consumption_2023_with_ids.csv', index=False)

# Join consumer_id to flow_df for both from_state and to_state
# Prepare mapping from state to consumer_id
from_state_consumer_id = cons_df[['state', 'consumer_id']].drop_duplicates().rename(columns={'state': 'from_state', 'consumer_id': 'from_consumer_id'})
to_state_consumer_id = cons_df[['state', 'consumer_id']].drop_duplicates().rename(columns={'state': 'to_state', 'consumer_id': 'to_consumer_id'})

# Add International row if not present
if not (from_state_consumer_id['from_state'] == 'International').any():
    from_state_consumer_id = pd.concat([
        from_state_consumer_id,
        pd.DataFrame({'from_state': ['International'], 'from_consumer_id': [53]})
    ], ignore_index=True)
if not (to_state_consumer_id['to_state'] == 'International').any():
    to_state_consumer_id = pd.concat([
        to_state_consumer_id, 
        pd.DataFrame({'to_state': ['International'], 'to_consumer_id': [53]})
    ], ignore_index=True)

# Merge to get from_consumer_id
flow_df = flow_df.merge(from_state_consumer_id, on='from_state', how='left')
# Merge to get to_consumer_id
flow_df = flow_df.merge(to_state_consumer_id, on='to_state', how='left')

# Save the updated flow data with consumer IDs to a new CSV file
flow_df.to_csv(path_interstate + 'interstate_flow_2023_flipped_with_consumer_ids.csv', index=False)

# === add consuming state ids to production state data as well ===
# Add consumer_id from cons_df to prod_df by matching on state
prod_df = prod_df.merge(cons_df[['state', 'consumer_id']], on='state', how='left')

# Save the updated production data with consumer IDs to a new CSV file
prod_df.to_csv(path_leontief + 'state_production_2023_with_consumer_ids.csv', index=False)





# === CONSTRUCT LEONTIEF ===

import pandas as pd
import numpy as np

# Load datasets
path_leontief = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/leontief/"
flows_df = pd.read_csv(path_leontief+ "interstate_flow_2023_flipped_with_consumer_ids.csv")
prod_df = pd.read_csv(path_leontief+ "state_production_2023_with_consumer_ids.csv")
prod_df = prod_df[prod_df['state'] != 'U.S.']
cons_df = pd.read_csv(path_leontief+ "state_consumption_2023_with_ids.csv")
cons_df = cons_df[cons_df['state'] != 'U.S.']


# === A MATRIX ===

# Define counts
n_producers = len(prod_df)              # 35
n_flows = len(flows_df)                 # 136
n_consumers = len(cons_df) + 1          # 53 plus international
N = n_producers + n_flows + n_consumers # 224

# Index ranges
prod_start, prod_end = 0, n_producers
flow_start, flow_end = prod_end, prod_end + n_flows
cons_start, cons_end = flow_end, flow_end + n_consumers

# Initialize square matrix
matrix = np.zeros((N, N))

# Create ID-to-index mappings
prod_id_to_idx = {pid: idx for idx, pid in enumerate(prod_df["producer_id"], start=prod_start)}
flow_id_to_idx = {fid: idx for idx, fid in enumerate(flows_df["interstate_id"], start=flow_start)}
cons_id_to_idx = {cid: idx for idx, cid in enumerate(cons_df["consumer_id"], start=cons_start)}
cons_id_to_idx[n_consumers] = N-1

# ---- Block 2: [consumer, flow] ----
for _, row in flows_df.iterrows():
    from_id = row.get("from_consumer_id")
    flow_id = row.get("interstate_id")
    value = row.get("flow_value_mmcf", 0)
    
    if from_id in cons_id_to_idx and flow_id in flow_id_to_idx:
        row_idx = cons_id_to_idx[from_id]
        col_idx = flow_id_to_idx[flow_id]
        matrix[row_idx, col_idx] = value

# ---- Block 3: [flow, consumer] ----
for _, row in flows_df.iterrows():
    to_id = row.get("to_consumer_id")
    flow_id = row.get("interstate_id")
    value = row.get("flow_value_mmcf", 0)
    
    if flow_id in flow_id_to_idx and to_id in cons_id_to_idx:
        row_idx = flow_id_to_idx[flow_id]
        col_idx = cons_id_to_idx[to_id]
        matrix[row_idx, col_idx] = value

# ---- Block 4: [producer, consumer_diag] ----
for i, row in prod_df.iterrows():
    pid = row["producer_id"]
    value = row.get("natural_gas_dry_production_MMcf", 0)
    consumer_id = row.get("consumer_id", None)
    
    if pid in prod_id_to_idx and pd.notnull(consumer_id):
        row_idx = prod_id_to_idx[pid]
        col_idx = cons_id_to_idx[consumer_id]
        if col_idx < N:
            matrix[row_idx, col_idx] = value

# Save to CSV without column names (no header row)
matrix_df = pd.DataFrame(matrix)
matrix_df.to_csv(path_leontief + "gas_flow_matrix_2023.csv", index=False, header=False)

print("Matrix constructed successfully. Shape:", matrix_df.shape)

# === DEMAND VECTOR ===
# Step 1: Compute row sums as the base demand vector
demand_vector = matrix.sum(axis=1)

# Step 2: Use cons_id_to_idx mapping for consumer_id to index
for _, row in cons_df.iterrows():
    try:
        consumer_id = int(row["consumer_id"])
        consumer_index = cons_id_to_idx.get(consumer_id, None)
        if consumer_index is not None and 0 <= consumer_index < len(demand_vector):
            demand_vector[consumer_index] += row["natural_gas_consumption_value_MMcf"]
    except (ValueError, TypeError, KeyError):
        continue  # Skip invalid or malformed rows

# Optional: Convert to DataFrame and save or inspect
demand_vector_df = pd.DataFrame(demand_vector, columns=["demand"])
demand_vector_df.to_csv(path_leontief + "demand_vector.csv", index=False, header= False)

# Print first few rows for verification
print(demand_vector_df.head())


# === NORMALIZE A MATRIX ===

# Avoid division by zero by replacing zeros in demand_vector with np.nan or a small value
safe_demand_vector = demand_vector.copy()
safe_demand_vector[safe_demand_vector == 0] = 1e-12  # or use a small value like 1e-12 if you prefer

# Normalize each column j by D(j)
normalized_matrix = matrix / safe_demand_vector[np.newaxis, :]

# Check for NaN values in the normalized matrix
norm_nan_count = np.isnan(normalized_matrix).sum()
norm_total_elements = normalized_matrix.size
print(f"NaN values in normalized matrix: {norm_nan_count} out of {norm_total_elements} total elements")
print(f"Percentage of NaN values in normalized matrix: {(norm_nan_count/norm_total_elements)*100:.4f}%")

if norm_nan_count > 0:
    print("WARNING: NaN values detected in the normalized matrix!")
    # Find positions of NaN values
    norm_nan_positions = np.where(np.isnan(normalized_matrix))
    print(f"NaN positions in normalized matrix (row, col): {list(zip(norm_nan_positions[0], norm_nan_positions[1]))}")
else:
    print("No NaN values found in the normalized matrix.")

# Optional: convert to DataFrame and save
normalized_matrix_df = pd.DataFrame(normalized_matrix)
normalized_matrix_df.to_csv(path_leontief + "gas_flow_matrix_2023_normalized.csv", index=False, header=False)

print("Normalized matrix saved. Shape:", normalized_matrix_df.shape)

# === (I - A_norm)^-1 ===

# Compute (I - normalized_matrix)
I = np.eye(normalized_matrix.shape[0])
I_minus_A = I - normalized_matrix

pd.DataFrame(I_minus_A).to_csv(path_leontief + "I_minus_A.csv", index=False, header=False)

# Check if I_minus_A is singular
rank = np.linalg.matrix_rank(I_minus_A)
if rank < I_minus_A.shape[0]:
    print(f"I_minus_A is singular. Rank: {rank} < {I_minus_A.shape[0]}")
    # Check for linearly dependent columns
    from itertools import combinations
    tol = 1e-8
    n = I_minus_A.shape[0]
    # Check columns
    for i, j in combinations(range(n), 2):
        col_i = I_minus_A[:, i]
        col_j = I_minus_A[:, j]
        # Avoid division by zero
        if np.all(np.abs(col_j) < tol):
            continue
        ratio = col_i / (col_j + (col_j == 0) * tol)
        if np.all(np.abs(ratio - ratio[0]) < tol):
            print(f"Columns {i} and {j} are linearly dependent (parallel).")
    # Check rows
    for i, j in combinations(range(n), 2):
        row_i = I_minus_A[i, :]
        row_j = I_minus_A[j, :]
        if np.all(np.abs(row_j) < tol):
            continue
        ratio = row_i / (row_j + (row_j == 0) * tol)
        if np.all(np.abs(ratio - ratio[0]) < tol):
            print(f"Rows {i} and {j} are linearly dependent (parallel).")
else:
    print("I_minus_A is not singular.")

# === COMPUTE INVERSE ===
try:
    # Compute the inverse of (I - A_norm)
    I_minus_A_inv = np.linalg.inv(I_minus_A)
    print("Inverse computed successfully!")
    
    # Check for NaN values in the inverse
    inv_nan_count = np.isnan(I_minus_A_inv).sum()
    inv_total_elements = I_minus_A_inv.size
    print(f"NaN values in inverse matrix: {inv_nan_count} out of {inv_total_elements} total elements")
    
    if inv_nan_count > 0:
        print("WARNING: NaN values detected in the inverse matrix!")
        inv_nan_positions = np.where(np.isnan(I_minus_A_inv))
        print(f"NaN positions in inverse matrix (row, col): {list(zip(inv_nan_positions[0], inv_nan_positions[1]))}")
    else:
        print("No NaN values found in the inverse matrix.")
    
    # Zero out values less than 1e-4
    threshold = 1e-4
    I_minus_A_inv_filtered = I_minus_A_inv.copy()
    I_minus_A_inv_filtered[np.abs(I_minus_A_inv_filtered) < threshold] = 0
    
    # Count how many values were zeroed out
    original_nonzero = np.count_nonzero(I_minus_A_inv)
    filtered_nonzero = np.count_nonzero(I_minus_A_inv_filtered)
    zeroed_count = original_nonzero - filtered_nonzero
    print(f"Zeroed out {zeroed_count} values less than {threshold}")
    print(f"Remaining non-zero values: {filtered_nonzero}")
    
    # Save filtered inverse matrix to CSV
    inv_matrix_df = pd.DataFrame(I_minus_A_inv_filtered)
    inv_matrix_df.to_csv(path_leontief + "I_minus_A_inverse.csv", index=False, header=False)
    print(f"Filtered inverse matrix saved to: {path_leontief + 'I_minus_A_inverse.csv'}")
    print(f"Inverse matrix shape: {I_minus_A_inv_filtered.shape}")
    
    # Optional: Save as numpy array for easier loading later
    np.save(path_leontief + "I_minus_A_inverse.npy", I_minus_A_inv_filtered)
    print(f"Filtered inverse matrix also saved as numpy array: {path_leontief + 'I_minus_A_inverse.npy'}")
    
    # === SEGMENT MATRIX BLOCK ===
    # Extract block: row [0:n_producers] and column [n_prod + n_flows: end]
    block_start_row = 0
    block_end_row = n_producers
    block_start_col = n_producers + n_flows
    block_end_col = N
    
    print(f"Extracting block: rows [{block_start_row}:{block_end_row}], columns [{block_start_col}:{block_end_col}]")
    
    # Extract the specified block from the filtered inverse matrix
    matrix_block = I_minus_A_inv_filtered[block_start_row:block_end_row, block_start_col:block_end_col]
    
    print(f"Extracted block shape: {matrix_block.shape}")
    print(f"Block contains {np.count_nonzero(matrix_block)} non-zero elements")
    
    # Save the extracted block to CSV
    block_df = pd.DataFrame(matrix_block)
    block_df.to_csv(path_leontief + "matrix_block_producers_to_consumers.csv", index=False, header=False)
    print(f"Matrix block saved to: {path_leontief + 'matrix_block_producers_to_consumers.csv'}")
    
    # Save as numpy array for easier loading later
    np.save(path_leontief + "matrix_block_producers_to_consumers.npy", matrix_block)
    print(f"Matrix block also saved as numpy array: {path_leontief + 'matrix_block_producers_to_consumers.npy'}")
    
except np.linalg.LinAlgError as e:
    print(f"Error computing inverse: {e}")
    print("The matrix (I - A_norm) is singular and cannot be inverted.")
except Exception as e:
    print(f"Unexpected error computing inverse: {e}")


