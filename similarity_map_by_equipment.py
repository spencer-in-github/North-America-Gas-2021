"""
Script: basin_equipment_similarity.py
Purpose: Analyze and match basin equipment profiles using cosine similarity.
Author: Spencer Zhang
Project: North America Gas GHGRP Equipment Revision Analysis

Steps:
1. Read basin and equipment data
2. Aggregate equipment counts by basin
3. Pivot data to wide format
4. Calculate cosine similarity between basins
5. Match each basin to most similar reference basin
6. Merge back to include basin IDs
7. Match Enverus basins to these results
"""

import pandas as pd
from sklearn.metrics.pairwise import cosine_similarity

# ───────────────────────────────────────────────
# File paths
# ───────────────────────────────────────────────
path_raw = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/GHGRP equipment data/processed_basin_equipment/'
path_save = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/GHGRP equipment data/'

basin_file = path_raw + "basin_id.csv"
equip_file = path_raw + "Equip_2022.csv"

# ───────────────────────────────────────────────
# Step 1: Read input data
# ───────────────────────────────────────────────
basin_df = pd.read_csv(basin_file)
equip_df = pd.read_csv(equip_file)

# Ensure consistent ID types
basin_df["BASIN_CODE"] = basin_df["BASIN_CODE"].astype(str)
equip_df["Basin_ID"] = equip_df["Basin_ID"].astype(str)

# ───────────────────────────────────────────────
# Step 2: Aggregate equipment counts per basin
# ───────────────────────────────────────────────
grouped_df = (
    equip_df.groupby(["equipment_type", "Basin_ID"], as_index=False)
    .agg({"equip_count_total": "sum"})
)

# Add basin names
merged_df = grouped_df.merge(
    basin_df, left_on="Basin_ID", right_on="BASIN_CODE", how="left"
).drop(columns=["BASIN_CODE"]).dropna(subset="BASIN_NAME")

# Save merged long-format data
merged_df.to_csv(path_save + 'basin_level_equipment_counts.csv', index=False)

print(merged_df["equipment_type"].unique())

upstream_equipment = ['Dehydrators',
                      'Header',
                      'Heater-treater',
                      'In-line heaters',
                      'Separators',
                      'Wellhead']

midstream_equipment = ['Compressors',
                       'Meters/piping']

# ───────────────────────────────────────────────
# Step 3: Pivot to wide format (basins as rows, equipment types as columns)
# ───────────────────────────────────────────────
wide_df = merged_df.pivot(
    index=["Basin_ID", "BASIN_NAME"],
    columns="equipment_type",
    values="equip_count_total"
).reset_index().fillna(0)

wide_df.to_csv(path_save + 'wide_equipment.csv', index=False)

# ───────────────────────────────────────────────
# Step 4: Compute cosine similarity between basin equipment vectors
# ───────────────────────────────────────────────
basins_to_match = [
    'Appalachian Basin',
    'San Joaquin Basin',
    'Denver Basin',
    'Permian Basin',
    'Uinta Basin',
    'Fort Worth Syncline'
]

equipment_vectors = wide_df.select_dtypes(include=["number"])
similarity_matrix = cosine_similarity(equipment_vectors)

similarity_df = pd.DataFrame(
    similarity_matrix,
    index=wide_df["BASIN_NAME"],
    columns=wide_df["BASIN_NAME"]
)
similarity_df.to_csv(path_save + 'basin_similarity_value.csv')

upstream_equipment_vector = wide_df[upstream_equipment]
similarity_matrix = cosine_similarity(upstream_equipment_vector)

similarity_df = pd.DataFrame(
    similarity_matrix,
    index=wide_df["BASIN_NAME"],
    columns=wide_df["BASIN_NAME"]
)
similarity_df.to_csv(path_save + 'basin_upstream_similarity_value.csv')
# Keep only similarity to reference basins
filtered_similarity_df = similarity_df[basins_to_match].copy()

# Find the best match for each basin
filtered_similarity_df["match_basin"] = filtered_similarity_df.idxmax(axis=1)
filtered_similarity_df.to_csv(path_save + "basin_upstream_similarity_matches.csv")


midstream_equipment_vector = wide_df[midstream_equipment]
similarity_matrix = cosine_similarity(midstream_equipment_vector)

similarity_df = pd.DataFrame(
    similarity_matrix,
    index=wide_df["BASIN_NAME"],
    columns=wide_df["BASIN_NAME"]
)
similarity_df.to_csv(path_save + 'basin_midstream_similarity_value.csv')
# Keep only similarity to reference basins
filtered_similarity_df = similarity_df[basins_to_match].copy()

# Find the best match for each basin
filtered_similarity_df["match_basin"] = filtered_similarity_df.idxmax(axis=1)
filtered_similarity_df.to_csv(path_save + "basin_midstream_similarity_matches.csv")

upstream_match = pd.read_csv(path_save + "basin_upstream_similarity_matches.csv")
midstream_match = pd.read_csv(path_save + "basin_midstream_similarity_matches.csv")

basin_matches = upstream_match[['BASIN_NAME', 'match_basin']].rename(
    columns={"match_basin":"upstream_match"}
    ).merge(
        midstream_match[['BASIN_NAME', 'match_basin']].rename(columns={"match_basin":"midstream_match"}),
        left_on="BASIN_NAME",
        right_on="BASIN_NAME",
        how = "left")

basin_matches.to_csv(path_save + 'basin_matches_by_part.csv')

# ───────────────────────────────────────────────
# Step 6: Add original and match basin IDs
# ───────────────────────────────────────────────
id_df = pd.read_csv(path_save + "basin_id.csv")

# Merge to add original BASIN_ID
matches_with_id = basin_matches.merge(id_df, on="BASIN_NAME", how="left")
matches_with_id.rename(columns={"BASIN_CODE": "BASIN_ID"}, inplace=True)

print(matches_with_id.columns)

# Merge to add MATCH_BASIN_ID
matches_with_id = matches_with_id.merge(
    id_df.rename(columns={"BASIN_NAME": "upstream_basin","BASIN_CODE": "UPSTREAM_BASIN_ID"}),
    left_on="upstream_match",
    right_on = "upstream_basin",
    how="left"
).merge(
    id_df.rename(columns={"BASIN_NAME": "midstream_basin","BASIN_CODE": "MIDSTREAM_BASIN_ID"}),
    left_on="midstream_match",
    right_on = "midstream_basin",
    how="left"
)

matches_with_id = matches_with_id[['BASIN_NAME', 'BASIN_ID',
                                   'upstream_match', 'UPSTREAM_BASIN_ID',
                                   'midstream_match', 'MIDSTREAM_BASIN_ID']]

matches_with_id.to_csv(path_save + "matched_basins_with_ids.csv", index=False)

# ───────────────────────────────────────────────
# Step 7: Join with Enverus basin mapping
# ───────────────────────────────────────────────
enverus_basins = pd.read_csv(path_save + "AAPG_Basin_ID_Mapping.csv")
enverus_basins["BASIN_CODE"] = enverus_basins["BASIN_CODE"].astype(int).astype(str)

matches_with_id["BASIN_ID"] = matches_with_id["BASIN_ID"].astype(str)

matched_enverus_basins = enverus_basins.merge(
    matches_with_id,
    left_on= "BASIN_CODE",
    right_on="BASIN_ID",
    how="left"
)

matched_enverus_basins.to_csv(path_save + "matched_enverus_basins.csv", index=False)

# ───────────────────────────────────────────────
# Step 8: Join with Sherwin basin loss rates
# ───────────────────────────────────────────────

sherwin_df = pd.read_csv(path_save + "sherwin_basin_loss_rate.csv")

# Ensure consistent data types for merge keys
matched_enverus_basins["UPSTREAM_BASIN_ID"] = matched_enverus_basins["UPSTREAM_BASIN_ID"].astype("Int64").astype(str)
matched_enverus_basins["MIDSTREAM_BASIN_ID"] = matched_enverus_basins["MIDSTREAM_BASIN_ID"].astype("Int64").astype(str)
sherwin_df["BASIN_ID"] = sherwin_df["BASIN_ID"].astype("Int64").astype(str)

# Merge on UPSTREAM_BASIN_ID with Sherwin's BASIN_ID
merged_df = matched_enverus_basins.merge(
    sherwin_df[["BASIN_ID", "AIR_WELL_LOSS_RATE"]],
    left_on="UPSTREAM_BASIN_ID",
    right_on="BASIN_ID",
    how="left"
).merge(
    sherwin_df[["BASIN_ID", "AIR_MID_LOSS_RATE"]],
    left_on="MIDSTREAM_BASIN_ID",
    right_on="BASIN_ID",
    how="left"
)

# Get the U.S. mean loss rate from the 'U.S. mean' row
us_mean_loss_up = sherwin_df.loc[sherwin_df["MATCH_NAME"] == "U.S. MEAN", "AIR_WELL_LOSS_RATE"].values[0]
us_mean_loss_mid = sherwin_df.loc[sherwin_df["MATCH_NAME"] == "U.S. MEAN", "AIR_MID_LOSS_RATE"].values[0]

# Fill missing with U.S. mean value
merged_df["AIR_WELL_LOSS_RATE"] = merged_df["AIR_WELL_LOSS_RATE"].fillna(us_mean_loss_up)
merged_df["AIR_MID_LOSS_RATE"] = merged_df["AIR_MID_LOSS_RATE"].fillna(us_mean_loss_mid)

# Optional: Drop redundant column if not needed
# merged_df = merged_df.drop(columns=["BASIN_ID_y"])

# Save to file (optional)
merged_df = merged_df[["BASIN_CODE","AAPG_BASIN_NAME", 
           "UPSTREAM_BASIN_ID", "upstream_match","AIR_WELL_LOSS_RATE",
            "MIDSTREAM_BASIN_ID", "midstream_match","AIR_MID_LOSS_RATE"]]

merged_df.to_csv(path_save + "basin_matched_loss_rates.csv", index=False)

# ───────────────────────────────────────────────
# Step 9: create OPGEE lookup table
# ───────────────────────────────────────────────

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

# ───────────────────────────────────────────────
# Done
# ───────────────────────────────────────────────
print("✅ Processing complete. Outputs saved to:", path_save)
