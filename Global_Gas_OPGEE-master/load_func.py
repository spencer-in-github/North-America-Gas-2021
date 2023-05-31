import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import time
import math
import statistics
from math import sin, cos, sqrt, atan2, radians
from scipy.optimize import linprog 
# HOW TO INSTALL geopandas and relevant packages, watch this video: https://youtu.be/LNPETGKAe0c
import geopandas as gpd
from shapely.geometry import Point, LineString
import os, os.path
import win32com.client
from win32com.client import Dispatch,constants

def loadCountyData(df, filepath):
    """Load County level data."""
    cwd = os.getcwd()

    dfraw = pd.read_csv(filepath, parse_dates=True)

    #df['Country']        = 'United States'
    #df["Name"] = dfraw['County']
    df["FIPS"] = dfraw['FIPS']
    #df["Basin"] = dfraw['Basin']
    df["Basin"] = dfraw['Prov_Cod_1']
    df['Producing_Wells'] = dfraw['Join_Count']

    #df["Oil_B_Day"] = dfraw['Oil [bbl/day]']
    df["Oil_B_Day"] = dfraw['Annual_Oil'] / 365
    df["Water_B_Day"] = dfraw['Annual_Wat'] / 365
    #df["Gas_Mscf_Day"] = dfraw['Gas [MMscf/day]']*1000
    df["Gas_Mscf_Day"] = dfraw['Annual_Gas'] / 365
    #df["FOR_SCF_B"] = dfraw["FOR_mscf_b"]*1000
    #df["GOR_SCF_B"] = (df['Gas_Mscf_Day']*1000)/df["Oil_B_Day"]

    for idx, row in dfraw.iterrows():
        if (dfraw.loc[idx,'age_since_'] > 0)       : df.loc[idx,'Field_age'] = dfraw.loc[idx,'age_since_'] / dfraw.loc[idx,'Age_count']
        if (dfraw.loc[idx,'True_Verti'] > 0)       : df.loc[idx,'Field_depth'] = dfraw.loc[idx,'True_Verti'] / dfraw.loc[idx,'Depth_coun']
        if (dfraw.loc[idx,'API_Double'] > 4)       :
            df.loc[idx,'deg_API'] = dfraw.loc[idx,'API_Double'] / dfraw.loc[idx,'API_count']
        elif (dfraw.loc[idx,'API_count'] > 0):
            if ((dfraw.loc[idx,'API_Double'] / dfraw.loc[idx,'API_count']) > 0) and ((dfraw.loc[idx,'API_Double'] / dfraw.loc[idx,'API_count']) < 4):
                df.loc[idx, 'deg_API'] = 4

    for idx, row in df.iterrows():
        if row['Oil_B_Day'] > 0 and row['Gas_Mscf_Day'] > 0:
            df.loc[idx, 'GOR_SCF_B'] = (row['Gas_Mscf_Day']*1000) / row['Oil_B_Day']
        elif row['Oil_B_Day'] == 0 and row['Gas_Mscf_Day'] > 0:
            df.loc[idx, 'Oil_B_Day'] = 0.01*df.loc[idx, 'Producing_Wells']
            df.loc[idx, 'GOR_SCF_B'] = (row['Gas_Mscf_Day']*1000) / df.loc[idx, 'Oil_B_Day']

    for idx, row in df.iterrows():
        if row['Oil_B_Day'] > 0 and row['Water_B_Day'] > 0:
            df.loc[idx, 'WOR_BBL_B'] = row['Water_B_Day'] / row['Oil_B_Day']

    column_names = [
        "FIPS",
        "BCM_2019"]

    data_FOR = pd.DataFrame(columns=column_names)

    DataPath = os.path.join(cwd, 'Input Data\\FOR')

    FOR_path = os.path.join(DataPath, 'UScounty_withflaring2019.csv')
    data_FOR = loadFORData(data_FOR, FOR_path)

    df = df.merge(data_FOR, left_on = ['FIPS'], right_on = ['FIPS'])

    for idx, row in df.iterrows():
        if row['Oil_B_Day'] > 0 and row['BCM_2019'] > 0:
            df.loc[idx, 'FOR_SCF_B'] = (row['BCM_2019']*35.147*1000000000) / (row['Oil_B_Day']*365.25)
        else:
            df.loc[idx, 'FOR_SCF_B'] = 0

    return df

def loadActivityData(df, Activity_path, LU_path, correspondence_path):
    """Load County level data."""
    
    df_activity = pd.read_csv(Activity_path, parse_dates=True)
    df_correspondence = pd.read_csv(correspondence_path, parse_dates=True)
    df_LU = pd.read_csv(LU_path, parse_dates=True)

    df_activity = df_activity.merge(df_LU, left_on=['Basin_ID'], right_on=['Basin_ID'])
    dfraw = df_correspondence.merge(df_activity, left_on=['Prov_Cod_1'], right_on=['Basin_ID'], how='left')

    df['FIPS']        = dfraw['FIPS']
    df["Headers per well"] = dfraw['Headers per well']
    df['Heaters per well'] = dfraw['Heaters per well']
    #df["Heater-treater per well"] = dfraw['Heater-treater per well']
    df["Separators per well"] = dfraw['Separators per well']
    df["Meters per well"] = dfraw['Meters per well']
    df["Tanks per well"] = dfraw['Tanks per well']
    df["Compressors per well"] = dfraw['Compressors per well']
    df["Dehydrators per well"] = dfraw['Dehydrators per well']
    df["Pneumatic Pumps"] = dfraw['Pneumatic Pumps']
    df["Pneumatics - All"] = dfraw['Pneumatics - All']
    df["Frac wells with LU (plunger)"] = dfraw['LU_plunger']
    df['Frac wells with LU (no plunger)'] = dfraw['LU_noplunger']
    df['CH4 fraction'] = dfraw['CH4_fraction']
    
    for idx, row in dfraw.iterrows():
        if pd.isna(row['LU_plunger']) | pd.isna(row['LU_noplunger']):
              df["Frac wells no LU"] = 1 - row['LU_plunger'] - row['LU_noplunger']
    
    #df["Frac wells no LU"] = dfraw['Frac wells no LU']
    df["Frac flash control (throughput)"] = dfraw['Thru - tank controls']
    
    return df

def loadFORData(df, filepath):
    """Load County level data."""

    dfraw = pd.read_csv(filepath, parse_dates=True)

    df['FIPS'] = dfraw['FIPS']
    df["BCM_2019"] = dfraw['BCM_2019']

    return df