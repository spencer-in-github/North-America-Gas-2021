import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the data from the uploaded CSV file
file_path = 'viz.csv'
data = pd.read_csv(file_path)

# Display the first few rows of the dataframe to understand its structure
data.head()

x_values = data['cum_perc']
y_values_diff = data['CI_gCO2_MJ_diff']
y_values_base = data['CI_gCO2_MJ_Base']

# Calculate the start and end points for each bar
start_points = np.insert(x_values[:-1].values, 0, 0)
end_points = x_values.values

# Calculate bar widths
bar_widths = end_points - start_points

# To differentiate each adjacent bar, we will assign a distinct color to each

# Generate a color palette with as many colors as there are bars
colors = plt.cm.viridis(np.linspace(0, 1, len(data)))

# Create the plot with distinct colors
plt.figure(figsize=(12, 6))

# Loop through each row to plot each bar with a different color
for i in range(len(data)):
    plt.bar(start_points[i], y_values_diff[i], width=bar_widths[i], color=colors[i], align='edge')
    plt.bar(start_points[i], y_values_base[i], width=bar_widths[i], color=colors[i], align='edge', bottom=y_values_diff[i])

# Adding labels and title
plt.xlabel('Cumulative gas production (0-1)')
plt.ylabel('Upstream Carbon Intensity (gCO23/MJ)')
plt.title('U.S. basin level upstream natural gas carbon intensity curve by cumulative production.')

# Show the plot
plt.show()
