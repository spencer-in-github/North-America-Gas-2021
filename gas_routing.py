import pandas as pd

# Load data
prod_consump_df = pd.read_csv('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/state production consumption/state_prod_consump.csv')
flows_df = pd.read_csv('/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/interstate flows/interstate_flow.csv')
path_save = '/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/revision_data/interstate flows/'

# Fill missing with 0
prod_consump_df.fillna(0, inplace=True)

# Filter only exporting states
exporting_states = prod_consump_df[prod_consump_df["net_surplus_MMcf"] > 0].copy()

# Merge net export info into flows_df
flows_with_export = flows_df.merge(
    exporting_states[["state", "net_surplus_MMcf"]],
    left_on="from_state", right_on="state", how="left"
)

# Drop rows where the from_state is not a net exporter
flows_with_export = flows_with_export.dropna(subset=["net_surplus_MMcf"])
flows_with_export.to_csv(path_save + "flows_with_export.csv")

# For each exporting state, calculate total outgoing flow
total_outflow = flows_with_export.groupby("from_state")["flow_value_mmcf"].transform("sum")

# Allocate exported gas proportionally to each receiving state
flows_with_export["allocated_production_flow_MMcf"] = (
    flows_with_export["flow_value_mmcf"] / total_outflow
) * flows_with_export["net_surplus_MMcf"]

# Select final columns of interest
allocated_flows = flows_with_export[[
    "from_state", "to_state", "allocated_production_flow_MMcf"
]]

# Save result
allocated_flows.to_csv(path_save+ "allocated_production_flows.csv", index=False)

print(allocated_flows.head())
