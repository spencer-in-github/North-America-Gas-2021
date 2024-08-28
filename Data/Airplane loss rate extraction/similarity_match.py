import pandas as pd
import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

# Since the user has uploaded a CSV file, we can now directly read the data and calculate the similarity matrix.

# Load the CSV data into a DataFrame
csv_path = 'BASIN_CHARACTERISTICS.csv'
df_basins = pd.read_csv(csv_path)

# We need to select the columns from 'annual_gas' to 'well_age' to form the vectors
# We also need to handle 'NA' values. We will assume 'NA' means missing data and will replace it with zeros.

# Select the relevant columns
vector_columns = df_basins.columns[df_basins.columns.get_loc('annual_gas'):df_basins.columns.get_loc('well_age')+1]
df_vectors = df_basins[vector_columns].fillna(0)

# Now calculate the cosine similarity matrix for these vectors
cosine_sim = cosine_similarity(df_vectors)

# Create a DataFrame from the similarity matrix, with rows and columns as basin names
df_similarity = pd.DataFrame(cosine_sim, index=df_basins['AAPG.Basin.x'], columns=df_basins['AAPG.Basin.x'])

# For each basin, find the most similar other basin
# We will first set the diagonal to NaN to avoid self-matching
np.fill_diagonal(df_similarity.values, np.nan)
most_similar_basins = df_similarity.idxmax(axis=1)

# Create a DataFrame for the most similar basin pairs
most_similar_pairs = most_similar_basins.reset_index()
most_similar_pairs.columns = ['Basin', 'Most Similar Basin']

most_similar_pairs
