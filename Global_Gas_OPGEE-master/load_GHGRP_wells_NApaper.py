import datetime
import math
import numpy as np
import pandas as pd
import os
from tqdm import tqdm
import time
import matplotlib.pyplot as plt
import re

cwd = os.getcwd()

# Important: Make sure to save API row as text using this approach: https://stackoverflow.com/questions/13732077/how-to-import-long-number-from-csv-to-excel-without-converting-to-scientific-not
wells_path = os.path.join(cwd, 'EF_W_ONSHORE_WELLS_2019.csv')
wells_data = pd.read_csv(wells_path, skiprows=1, usecols=[1, 6],
                          names=['FACILITY_ID', 'WELL_ID_NUMBER'],
                          index_col=None,
                          encoding_errors='ignore')


Enverus_path = os.path.join(cwd, 'annualDF_2019_AllUSA_22717_Join.csv')
Enverus_data = pd.read_csv(Enverus_path, skiprows=1, usecols=[5, 6,8,20],
                          names=['Annual Oil [bbl/year]', 'Annual Gas [mscf/year]','API/UWI','Prov_Cod_1'],
                          index_col=None,
                          encoding_errors='ignore')

Enverus_data['API/UWI'] = Enverus_data['API/UWI'].astype('int64')
Enverus_data['API/UWI'] = Enverus_data['API/UWI'].astype('string')

# Drop row with "NA"
wells_data.drop(485820, axis = 0, inplace = True)

wells_data.WELL_ID_NUMBER = wells_data.WELL_ID_NUMBER.astype('string')
wells_data.WELL_ID_NUMBER = wells_data.WELL_ID_NUMBER.str.replace(r'\D+', '')
# Convert to integer to remove leading zeros
wells_data.WELL_ID_NUMBER = wells_data.WELL_ID_NUMBER.astype('int64')
# Convert back to string
wells_data.WELL_ID_NUMBER = wells_data.WELL_ID_NUMBER.astype('string')

for idx, row in tqdm(wells_data.iterrows()):
    #time.sleep(0.5)
    try:
        wells_data.loc[idx,'WELL_ID_NUMBER'].lstrip('0')
        if (len(wells_data.loc[idx,'WELL_ID_NUMBER']) == 12):
            wells_data.loc[idx, 'WELL_ID_NUMBER'] = wells_data.loc[idx, 'WELL_ID_NUMBER'].ljust(14, '0')
        if (len(wells_data.loc[idx,'WELL_ID_NUMBER']) == 11):
            wells_data.loc[idx, 'WELL_ID_NUMBER'] = wells_data.loc[idx, 'WELL_ID_NUMBER'].ljust(13, '0')
        if (len(wells_data.loc[idx,'WELL_ID_NUMBER']) == 10):
            wells_data.loc[idx, 'WELL_ID_NUMBER'] = wells_data.loc[idx, 'WELL_ID_NUMBER'].ljust(14, '0')
        if (len(wells_data.loc[idx,'WELL_ID_NUMBER']) == 9):
            wells_data.loc[idx, 'WELL_ID_NUMBER'] = wells_data.loc[idx, 'WELL_ID_NUMBER'].ljust(13, '0')
    except (IndexError, TypeError) as error:
        print('ERROR at index {}: {!r}'.format(idx))

    matchedDF = pd.DataFrame()  # makae empty df to store results
    matchedDF = Enverus_data.merge(wells_data, left_on = ['API/UWI'], right_on= ['WELL_ID_NUMBER'])

    #matchedDF_left = Enverus_data.merge(wells_data, left_on=['API/UWI'], right_on=['WELL_ID_NUMBER'], how = 'left')

    csvPath = os.path.join(cwd, 'API_Facility_correspondence_2019.csv')
    matchedDF.to_csv(csvPath)