import os
import glob
import pandas as pd

process_annual = False
unique_well_check = False
process_total_production = False
summarize_total_well_count = False
summarize_basins = False
concat_annual_files = True  # Set to True to concatenate all _annual.csv files

if process_annual:
    data_dir = "revision_data/enverus 2023"
    csv_files = glob.glob(os.path.join(data_dir, "*.csv"))

    for file in csv_files:
        print(f"Processing {file}...")
        df = pd.read_csv(file)
        
        # Identify the API column
        api_col = None
        for col in df.columns:
            if 'api' in col.lower():
                api_col = col
                break
        if api_col is None:
            raise ValueError("No API column found. Please check your CSV format.")
        
        # Columns to sum: 
        sum_cols = ['Prod_MCFE', 'LiquidsProd_BBL', 'GasProd_MCF', 'WaterProd_BBL']
        # All other columns (excluding API and sum_cols and producing month)
        other_cols = [col for col in df.columns if col not in sum_cols + [api_col, 'ProducingMonth']]
        
        # If you want to keep the producing year, extract it from 'producing month'
        if 'ProducingMonth' in df.columns:
            df['producing year'] = pd.to_datetime(df['ProducingMonth'], errors='coerce').dt.year
            # If all years are 2023, you can keep this column, or drop it if not needed
            keep_cols = [api_col] + sum_cols + other_cols + (['producing year'] if df['producing year'].nunique() == 1 else [])
        else:
            keep_cols = [api_col] + sum_cols + other_cols

        # Add row count per API
        df['row count'] = df.groupby(api_col)[api_col].transform('count')

        # Group by API, sum monthly columns, keep first value for others, and take first row count (all are the same per group)
        grouped = df.groupby(api_col, as_index=False).agg(
            {**{col: 'sum' for col in sum_cols},
            **{col: 'first' for col in other_cols},
            'row count': 'first'}
        )

        # If you want to keep producing year, add it back (should be 2023 for all)
        if 'producing year' in df.columns and df['producing year'].nunique() == 1:
            grouped['producing year'] = 2023

        # Save to new CSV
        out_file = file.replace('.csv', '_annual.csv')
        grouped.to_csv(out_file, index=False)
        print(f"Saved annualized data to {out_file}")

    print("All files processed.")

if unique_well_check:
    data_dir = "revision_data/enverus 2023"
    annual_files = glob.glob(os.path.join(data_dir, "*_annual.csv"))

    for file in annual_files:
        print(f"\nChecking file: {file}")
        df = pd.read_csv(file)

        # Identify the API column
        api_col = None
        for col in df.columns:
            if 'api' in col.lower():
                api_col = col
                break
        if api_col is None:
            print("No API column found. Skipping this file.")
            continue

        # Count rows per unique API
        api_counts = df[api_col].value_counts()

        # Get the unique counts of row counts
        unique_count_of_row_counts = api_counts.value_counts().sort_index()
        print("Number of APIs with a given number of rows:")
        print(unique_count_of_row_counts)


if process_total_production:
    import os
    import glob
    import pandas as pd

    data_dir = "revision_data/enverus 2023"
    annual_files = glob.glob(os.path.join(data_dir, "*_annual.csv"))

    all_basins = set()
    total_gas = 0
    total_oil = 0
    total_water = 0

    # For basin-level summary
    basin_summary = {}

    for file in annual_files:
        print(f"Processing file: {file}")
        df = pd.read_csv(file)
        
        # Find basin and API columns
        basin_col = next((col for col in df.columns if 'basin' in col.lower()), None)
        api_col = next((col for col in df.columns if 'api' in col.lower()), None)
        if basin_col is None or api_col is None:
            print(f"Skipping {file}: missing basin or API column.")
            continue
        all_basins.update(df[basin_col].dropna().unique())
        
        # Find production columns
        gas_cols = [col for col in df.columns if 'gas' in col.lower()]
        oil_cols = [col for col in df.columns if 'oil' in col.lower() or 'liquid' in col.lower()]
        water_cols = [col for col in df.columns if 'water' in col.lower()]
        
        # Group by basin
        for basin, group in df.groupby(basin_col):
            if pd.isna(basin):
                continue
            if basin not in basin_summary:
                basin_summary[basin] = {'gas': 0, 'liquids': 0, 'water': 0, 'well_count': set()}
            basin_summary[basin]['gas'] += group[gas_cols].sum(numeric_only=True).sum() if gas_cols else 0
            basin_summary[basin]['liquids'] += group[oil_cols].sum(numeric_only=True).sum() if oil_cols else 0
            basin_summary[basin]['water'] += group[water_cols].sum(numeric_only=True).sum() if water_cols else 0
            basin_summary[basin]['well_count'].update(group[api_col].dropna().unique())

    # Prepare DataFrame for output
    summary_rows = []
    for basin, vals in basin_summary.items():
        summary_rows.append({
            'Basin': basin,
            'Total Gas (Mcf)': vals['gas'],
            'Total Liquids (bbl)': vals['liquids'],
            'Total Water (bbl)': vals['water'],
            'Well Count': len(vals['well_count'])
        })
    summary_df = pd.DataFrame(summary_rows)
    summary_df.to_csv(os.path.join(data_dir, "basin_production_summary.csv"), index=False)
    print("\nSaved basin production summary to basin_production_summary.csv")

    # Calculate total production across all basins
    total_gas = sum(vals['gas'] for vals in basin_summary.values())
    total_oil = sum(vals['liquids'] for vals in basin_summary.values())
    total_water = sum(vals['water'] for vals in basin_summary.values())

    # Format total_gas as billions with two decimal places
    total_gas_billion = round(total_gas / 1e9, 2)

    print(f"\nTotal annual gas production: {total_gas_billion:,.2f} Bcf")
    print(f"Total annual oil production: {total_oil:,.2f}")
    print(f"Total annual water production: {total_water:,.2f}")

    # Save total production to CSV with units
    total_prod_df = pd.DataFrame([
        {"Product": "Gas", "Total (Bcf)": total_gas_billion, "Unit": "Bcf"},
        {"Product": "Oil", "Total": total_oil, "Unit": "bbl"},
        {"Product": "Water", "Total": total_water, "Unit": "bbl"}
    ])
    total_prod_df.to_csv(os.path.join(data_dir, "total_annual_production_summary.csv"), index=False)
    print("\nSaved total annual production summary to total_annual_production_summary.csv")


if summarize_total_well_count:
    data_dir = "revision_data/enverus 2023"
    annual_files = glob.glob(os.path.join(data_dir, "*_annual.csv"))

    all_apis = set()

    for file in annual_files:
        df = pd.read_csv(file)
        # Identify the API column
        api_col = None
        for col in df.columns:
            if 'api' in col.lower():
                api_col = col
                break
        if api_col is None:
            print(f"No API column found in {file}. Skipping.")
            continue
        all_apis.update(df[api_col].dropna().unique())

    print(f"Total unique well APIs across all annual files: {len(all_apis)}")


if summarize_basins:

    data_dir = "revision_data/enverus 2023"
    annual_files = glob.glob(os.path.join(data_dir, "*_annual.csv"))

    all_basins = set()
    total_gas = 0
    total_oil = 0
    total_water = 0

    # For basin-level summary
    basin_summary = {}

    for file in annual_files:
        print(f"Processing file: {file}")
        df = pd.read_csv(file)
        
        # Find basin and API columns
        basin_col = next((col for col in df.columns if 'basin' in col.lower()), None)
        api_col = next((col for col in df.columns if 'api' in col.lower()), None)
        if basin_col is None or api_col is None:
            print(f"Skipping {file}: missing basin or API column.")
            continue
        all_basins.update(df[basin_col].dropna().unique())
        
        # Find production columns
        gas_cols = [col for col in df.columns if 'gas' in col.lower()]
        oil_cols = [col for col in df.columns if 'oil' in col.lower() or 'liquid' in col.lower()]
        water_cols = [col for col in df.columns if 'water' in col.lower()]
        
        # Group by basin
        for basin, group in df.groupby(basin_col):
            if pd.isna(basin):
                continue
            if basin not in basin_summary:
                basin_summary[basin] = {'gas': 0, 'liquids': 0, 'water': 0, 'well_count': set()}
            basin_summary[basin]['gas'] += group[gas_cols].sum(numeric_only=True).sum() if gas_cols else 0
            basin_summary[basin]['liquids'] += group[oil_cols].sum(numeric_only=True).sum() if oil_cols else 0
            basin_summary[basin]['water'] += group[water_cols].sum(numeric_only=True).sum() if water_cols else 0
            basin_summary[basin]['well_count'].update(group[api_col].dropna().unique())

        

        # Prepare DataFrame for output
        summary_rows = []
        for basin, vals in basin_summary.items():
            summary_rows.append({
                'Basin': basin,
                'Total Gas (Mcf)': vals['gas'],
                'Total Liquids (bbl)': vals['liquids'],
                'Total Water (bbl)': vals['water'],
                'Well Count': len(vals['well_count'])
            })
        summary_df = pd.DataFrame(summary_rows)
        summary_df.to_csv(data_dir + "basin_production_summary.csv", index=False)
        print("\nSaved basin production summary to basin_production_summary.csv")

        print("\nUnique basins across all files:")
        print(sorted(all_basins))

        print(f"\nTotal annual gas production: {total_gas:,.2f}")
        print(f"Total annual oil production: {total_oil:,.2f}")
        print(f"Total annual water production: {total_water:,.2f}")

        total_gas = sum(vals['gas'] for vals in basin_summary.values())
        total_oil = sum(vals['liquids'] for vals in basin_summary.values())
        total_water = sum(vals['water'] for vals in basin_summary.values())

       
        # Save total production to CSV with units
        total_prod_df = pd.DataFrame([
            {"Product": "Gas", "Total": round(total_gas/1e9, 2), "Unit": "Tcf"},
            {"Product": "Oil", "Total": round(total_oil/1e9, 2), "Unit": "Bbbl"},
            {"Product": "Water", "Total": round(total_water/1e9, 2), "Unit": "Bbbl"}
        ])
        total_prod_df.to_csv(data_dir +"total_annual_production_summary.csv", index=False)
        print("\nSaved total annual production summary to total_annual_production_summary.csv")


if concat_annual_files:
    data_dir = "revision_data/enverus 2023"
    annual_files = glob.glob(os.path.join(data_dir, "*_annual.csv"))
    
    if not annual_files:
        print("No _annual.csv files found to concatenate.")
    else:
        print(f"Concatenating {len(annual_files)} files...")
        df_list = []
        for file in annual_files:
            print(f"Reading {file}")
            df = pd.read_csv(file)
            df_list.append(df)
        big_df = pd.concat(df_list, ignore_index=True)
        out_file = os.path.join(data_dir, "annual_well_production_2023.csv")
        big_df.to_csv(out_file, index=False)
        print(f"Saved concatenated file to {out_file}")
