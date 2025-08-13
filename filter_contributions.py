# Filter out rows where contribution_percentage equals 0
prod_contribution_long = prod_contribution_long[prod_contribution_long['contribution_percentage'] != 0]
rnd_contribution_long = rnd_contribution_long[rnd_contribution_long['contribution_percentage'] != 0]

print(f"Filtered prod_contribution_long shape: {prod_contribution_long.shape}")
print(f"Filtered rnd_contribution_long shape: {rnd_contribution_long.shape}")

# Show first few rows of filtered data
print("\nFirst 5 rows of filtered prod_contribution_long:")
print(prod_contribution_long.head())

print("\nFirst 5 rows of filtered rnd_contribution_long:")
print(rnd_contribution_long.head()) 