# A notebook for dumpy result inspection and plotting
# Not for repeatable use

# Author:   Spencer Zhang
# Date:     September 26, 2023

import pandas as pd
import numpy as np
import plotly.express as px


PATH_RESULT_SHEET             = "/Users/spencerzhang/GitHub/PhD/North-America-Gas-2021/NAG run 20231128/NAG_CI_20231128_with_input_no_sf.xlsx"

raw_result = pd.read_excel(PATH_RESULT_SHEET)

fig = px.scatter(raw_result, x="Annual_Gas", y="CI_gCO2_MJ", color = "Check")
fig.show()