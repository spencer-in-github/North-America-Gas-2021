import pandas as pd

# Read in the data
path_pervious = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/GHGRP equipment data/'
path_save = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/aerial loss rate/'
equipment_match = pd.read_csv(path_save + 'equipment_match_original.csv')
basin_loss = pd.read_csv(path_save +'sherwin_basin_loss_rate.csv')

import pandas as pd

def update_loss_rates(basin_path, sherwin_path, col_upstream, col_midstream):
    # Read in the data
    basin_df = pd.read_csv(basin_path)
    sherwin_df = pd.read_csv(sherwin_path)

    # Create lookup dictionaries for basin ID to loss rate
    upstream_lookup = sherwin_df.set_index('BASIN_ID')[col_upstream].to_dict()
    midstream_lookup = sherwin_df.set_index('BASIN_ID')[col_midstream].to_dict()

    # Get fallback values from 'U.S. mean' row
    us_mean_row = sherwin_df[sherwin_df['BASIN_ID'] == 1].iloc[0] 
    fallback_upstream = us_mean_row[col_upstream]
    fallback_midstream = us_mean_row[col_midstream]

    # Replace AIR_WELL_LOSS_RATE
    basin_df['AIR_WELL_LOSS_RATE'] = basin_df['UPSTREAM_BASIN_ID'].apply(
        lambda x: upstream_lookup.get(x, fallback_upstream)*0.01
    )

    # Replace AIR_MID_LOSS_RATE
    basin_df['AIR_MID_LOSS_RATE'] = basin_df['MIDSTREAM_BASIN_ID'].apply(
        lambda x: midstream_lookup.get(x, fallback_midstream)*0.01
    )

    return basin_df


# Example usage
if __name__ == "__main__":
    path = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/aerial loss rate/'
    equipment_path = path + "equipment_match_original.csv"
    sherwin_path = path + "sherwin_basin_loss_rate.csv"
    updated_df = update_loss_rates(equipment_path, sherwin_path, 
                                    col_upstream="wellsite_high", 
                                    col_midstream="midstream_high")

    # Save to CSV
    file = "basin_loss_rate_high.csv"
    updated_df.to_csv(path + file , index=False)
    print(f"Updated file saved to {file}")
