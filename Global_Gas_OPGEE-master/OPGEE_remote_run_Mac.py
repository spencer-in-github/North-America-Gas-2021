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
from load_func import loadCountyData, loadActivityData, loadFORData


name_OPGEE          = "OPGEE_3.0b_BETA_Allocation_221130.xlsm"
Fugitives_model     = 'Component'

MODE_RecordInputs      = "ON"
cwd = os.getcwd()  
path_Local          = '\\Users\\spencerzhang\\GitHub\\PhD\\North-America-Gas-2021\\Global_Gas_OPGEE-master\\'
path_Macro          = "OPGEE_3.0b_BETA_Allocation_221130.xlsm!UltraBulk_assessment"


print("Opening OPGEE...")
xl = win32com.client.gencache.EnsureDispatch('Excel.Application')
wb = xl.Workbooks.Open(os.path.abspath(name_OPGEE))
#wb.Worksheets('Inputs').Range("H9:SM115").ClearContents()
#wb.Worksheets('Inputs').Range("H6:SM6").Value = 'NA'
wb.Worksheets('Inputs').Cells(2,2).Value = 1
wb.Worksheets('Inputs').Cells(2,3).Value = 1
wb.Worksheets('Inputs').Cells(2,6).Value = 1
wb.Worksheets('Inputs').Cells(2,9).Value = 'Gas'
wb.Worksheets('Inputs').Cells(2,13).Value = 'LHV'
wb.Worksheets('Inputs').Cells(2,17).Value = 'Field'
wb.Worksheets('Inputs').Cells(2,21).Value = 'Field'
wb.Worksheets('Inputs').Cells(2,25).Value = 'Allocation'
#wb.Worksheets('Inputs').Cells(2,29).Value = Fugitives_model
wb.Save()


column_names = [
    "FIPS",
    "Field_age",
    "Field_depth",
    "Oil_B_Day",
    "deg_API",
    "Gas_Mscf_Day",
    "Producing_Wells",
    "GOR_SCF_B",
    "FOR_SCF_B",
    "Basin"]

data_fields = pd.DataFrame(columns = column_names)

DataPath = os.path.join(cwd, 'Input Data')
#County_path = os.path.join(DataPath, 'file_inputs_main.csv')
#County_path = os.path.join(DataPath, 'COUNTIES_SpatialJoin_22720_flarev3.csv')
County_path = os.path.join(DataPath, 'US_INPUT_2022.csv')
data_fields      = loadCountyData(data_fields, County_path)

column_names = [
    "FIPS",
    "Headers per well",
    "Heaters per well",
    "Separators per well",
    "Meters per well",
    "Tanks per well",
    "Compressors per well",
    "Dehydrators per well",
    "Pneumatic Pumps",
    "Pneumatics - All",
    "Frac wells with LU (plunger)",
    "Frac wells with LU (no plunger)",
    "Frac wells no LU",
    "Frac flash control (throughput)",
    "CH4 fraction"]

data_activity = pd.DataFrame(columns = column_names)

Activity_path = os.path.join(DataPath, 'Equip_AF_exp_22831.csv')
LU_path = os.path.join(DataPath, 'LU_exp_22831.csv')
# Manually replaced 160A with 160
correspondence_path = os.path.join(DataPath, 'FIPS_Basin_Correspondence.csv')
data_activity = loadActivityData(data_activity, Activity_path, LU_path, correspondence_path)



#data_fields = data_fields.loc[(data_fields.index > 650) & (data_fields.index < 655)]    
#data_fields = data_fields.loc[data_fields['Basin'] == 'APPALACHIAN']   

data_fields_all = data_fields.merge(data_activity, left_on = "FIPS", right_on = "FIPS")
#data_fields_all = data_fields_all.merge(data_FOR, left_on = "Basin", right_on = "Basin")

data_fields_all = data_fields_all[~np.isnan(data_fields_all['Producing_Wells'])]

# finding cell row index of all input items 
Ind_LCGHG_MJ                          = int(xl.WorksheetFunction.Match('Lifecycle GHG emissions'           , wb.Worksheets('Results').Range("A1:A1000"),0)) # return the position of the matched item

Ind_Name                              = int(xl.WorksheetFunction.Match('Field name'                        , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Reinj                             = int(xl.WorksheetFunction.Match('Natural gas reinjection'           , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Oil_B_Day                         = int(xl.WorksheetFunction.Match('Oil production volume'             , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Producing_Wells                   = int(xl.WorksheetFunction.Match('Number of producing wells'         , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Age                               = int(xl.WorksheetFunction.Match('Field age'                         , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Country                           = int(xl.WorksheetFunction.Match('Field location (Country)'          , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_API_Gravity                       = int(xl.WorksheetFunction.Match('API gravity (oil at standard pressure and temperature, or "dead oil")', wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_Depth_Feet                        = int(xl.WorksheetFunction.Match('Field depth'                       , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_GOR                               = int(xl.WorksheetFunction.Match('Gas-to-oil ratio (GOR)'            , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_FOR                               = int(xl.WorksheetFunction.Match('Flaring-to-oil ratio'              , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_WOR                               = int(xl.WorksheetFunction.Match('Water-to-oil ratio (WOR)'          , wb.Worksheets('Inputs').Range("B1:B115"),0))
Ind_C1                               = int(xl.WorksheetFunction.Match('C1'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
#new inputs regarding gas composition from phi's work - added Nov 9 2023
#TODO: modify below chunks so that we actually read in these new inputs
Ind_N2                               = int(xl.WorksheetFunction.Match('N2'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
Ind_CO2                               = int(xl.WorksheetFunction.Match('CO2'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
Ind_C2                               = int(xl.WorksheetFunction.Match('C2'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
Ind_C3                               = int(xl.WorksheetFunction.Match('C3'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
Ind_C4plus                               = int(xl.WorksheetFunction.Match('C4+'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))
Ind_H2S                               = int(xl.WorksheetFunction.Match('H2S'                                , wb.Worksheets('Inputs').Range("C1:C115"),0))

Ind_Frac_Reinj                        = int(xl.WorksheetFunction.Match('Fraction of remaining natural gas reinjected'                         , wb.Worksheets('Inputs').Range("B1:B115"),0))

Ind_Results_Oil_B_Day                 = int(xl.WorksheetFunction.Match('Oil production volume'             , wb.Worksheets('Results').Range("B1:B115"),0))
Ind_Results_Producing_Wells           = int(xl.WorksheetFunction.Match('Number of producing wells'         , wb.Worksheets('Results').Range("B1:B115"),0))
Ind_Results_GOR                       = int(xl.WorksheetFunction.Match('Gas-to-oil ratio (GOR)'            , wb.Worksheets('Results').Range("B1:B115"),0))

Ind_Denominator_for_computing_CI      = int(xl.WorksheetFunction.Match('Denominator for computing CI'      , wb.Worksheets('Energy Summary').Range("B1:B1000"),0))
#Ind_Diluent_adjustment_factor         = int(xl.WorksheetFunction.Match('Diluent adjustment factor'         , wb.Worksheets('GHG Summary').Range("B1:B1000"),0)) + 2

print("Recording inputs and results...")
if MODE_RecordInputs == "ON":
    if os.path.exists(path_Local + "RESULT_Data_Fields_recordinputs_22122_alloc3.xlsx"): os.remove(path_Local + "RESULT_Data_Fields_recordinputs_22122_alloc3.xlsx")
    wb_recordinputs = xl.Workbooks.Add()
    wb.Worksheets('Results').Range('A1:F196').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('A1:F196').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('Results').Range('G1:AA4').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('G1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('G1:AA4').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('Secondary inputs').Range('A176:F180').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('A198:F202').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('Secondary inputs').Range('A799:F824').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('A204:F229').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('VFF Summary').Range('F85:F134').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F231:F280').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('VFF Summary').Range('F85:F134').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F282:F331').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('VFF Summary').Range('B9:B31').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F333:F355').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B66').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F358:F399').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B66').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F401:F442').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B66').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F444:F485').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B66').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F487:F528').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)

    wb.Worksheets('Energy Summary').Range('B128').Copy()
    wb_recordinputs.Worksheets('Sheet1').Range('A1').Select()
    wb_recordinputs.Worksheets('Sheet1').Range('F474').PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)

    wb_recordinputs.SaveAs(path_Local + 'RESULT_Data_Fields_recordinputs_22122_alloc3.xlsx')
    wb_recordinputs.Close(True)

if MODE_RecordInputs == "ON":
    if os.path.exists(path_Local + "RESULT_Index_Exceptions_22122_alloc3.xlsx"): os.remove(
        path_Local + "RESULT_Index_Exceptions_22122_alloc3.xlsx")
    wb_recordexceptions = xl.Workbooks.Add()

    # result headers
    wb.Worksheets('Results').Range('A1:F196').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1:F196').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                       Operation=constants.xlNone)
    # global setting headers                                                                   
    wb.Worksheets('Results').Range('G1:AA4').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('G1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('G1:AA4').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                      Operation=constants.xlNone)
    # crude oil storage                                                                  
    wb.Worksheets('Secondary inputs').Range('A176:F180').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('A198:F202').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                         Operation=constants.xlNone)
    # fugitive - component level method                                             
    wb.Worksheets('Secondary inputs').Range('A799:F824').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('A204:F229').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                         Operation=constants.xlNone)
    # Process level CH4 emission (ton/day) - venting                                                                     
    wb.Worksheets('VFF Summary').Range('F85:F134').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F231:F280').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                         Operation=constants.xlNone)
    # Process level CH4 emission (ton/day) - fugitives                                                                                
    wb.Worksheets('VFF Summary').Range('F85:F134').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F282:F331').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                         Operation=constants.xlNone)
    # total CH4 emission (ton/day) process stage level (higher granularity)                                                                     
    wb.Worksheets('VFF Summary').Range('B9:B31').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F333:F355').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme,
                                                                         Operation=constants.xlNone)
    # GHG emission (ton CO2eq/day)                                                                     
    wb.Worksheets('GHG Summary').Range('B25:B61').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F358:F399').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme, Operation=constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B61').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F401:F442').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme, Operation=constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B61').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F444:F485').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme, Operation=constants.xlNone)
    wb.Worksheets('GHG Summary').Range('B25:B61').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F487:F528').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme, Operation=constants.xlNone)
    # CI denominator (MJ/day)
    wb.Worksheets('Energy Summary').Range('B128').Copy()
    wb_recordexceptions.Worksheets('Sheet1').Range('A1').Select()
    wb_recordexceptions.Worksheets('Sheet1').Range('F474').PasteSpecial(Paste=constants.xlPasteAllUsingSourceTheme, Operation=constants.xlNone)
    wb_recordexceptions.SaveAs(path_Local + 'RESULT_Index_Exceptions_22122_alloc3.xlsx')
    wb_recordexceptions.Close(True)


# i_start = 0
# wb.Worksheets('Inputs').Range("H9:H115").ClearContents()

# time_now = time.time()
# print("ITERATION STARTED AT:", time.asctime(time.localtime(time_now)))

print("Run macro for all fields")
data_fields_all = data_fields_all.reset_index(drop=True)
#for i in range(381, data_fields_all.shape[0]):
for i in range(data_fields_all.shape[0]):
    Flag_exception = 0
    wb.Worksheets('Inputs').Range("H9:H115").ClearContents()

    # Production characteristics 
    wb.Worksheets('Inputs').Cells(Ind_Name,8).Value       = data_fields_all.at[i,'FIPS']
    wb.Worksheets('Inputs').Cells(Ind_Country,8).Value       = 'United States' 
    if not math.isnan(data_fields_all.at[i,'Oil_B_Day'])       : wb.Worksheets('Inputs').Cells(Ind_Oil_B_Day,8).Value       = data_fields_all.at[i,'Oil_B_Day']
    if not math.isnan(data_fields_all.at[i,'Producing_Wells']) : 
        if data_fields_all.at[i,'Producing_Wells'] != 0        : wb.Worksheets('Inputs').Cells(Ind_Producing_Wells,8).Value = data_fields_all.at[i,'Producing_Wells']
        if data_fields_all.at[i,'Producing_Wells'] == 0        : continue
    if not math.isnan(data_fields_all.at[i,'deg_API'])     : wb.Worksheets('Inputs').Cells(Ind_API_Gravity,8).Value     = data_fields_all.at[i,'deg_API']
    if not math.isnan(data_fields_all.at[i,'Field_depth'])      : wb.Worksheets('Inputs').Cells(Ind_Depth_Feet,8).Value      = data_fields_all.at[i,'Field_depth']
    if not math.isnan(data_fields_all.at[i,'Field_age'])      : wb.Worksheets('Inputs').Cells(Ind_Age,8).Value      = data_fields_all.at[i,'Field_age']
    if not math.isnan(data_fields_all.at[i,'GOR_SCF_B'])       : wb.Worksheets('Inputs').Cells(Ind_GOR,8).Value             = data_fields_all.at[i,'GOR_SCF_B']
    if not math.isnan(data_fields_all.at[i,'WOR_BBL_B'])       : wb.Worksheets('Inputs').Cells(Ind_WOR,8).Value             = data_fields_all.at[i,'WOR_BBL_B']
    if not math.isnan(data_fields_all.at[i,'CH4 fraction'])       : wb.Worksheets('Inputs').Cells(Ind_C1,8).Value             = data_fields_all.at[i,'CH4 fraction']*100
    if not math.isnan(data_fields_all.at[i,'FOR_SCF_B'])       : wb.Worksheets('Inputs').Cells(Ind_FOR, 8).Value = data_fields_all.at[i, 'FOR_SCF_B']

    if data_fields_all.at[i, 'GOR_SCF_B'] > 10000:
        wb.Worksheets('Inputs').Cells(Ind_Reinj, 8).Value = 0
    else:
        wb.Worksheets('Inputs').Cells(Ind_Frac_Reinj, 8).Value = 0.075

    # Activity characteristics
    
    wb.Worksheets('Secondary inputs').Cells(811,13).Value = 0
    
    if data_fields_all.at[i,'GOR_SCF_B'] > 100000:
        wb.Worksheets('Secondary inputs').Cells(815,13).Value = data_fields_all.at[i,'Headers per well'] if not math.isnan(data_fields_all.at[i,'Headers per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(816,13).Value = data_fields_all.at[i,'Heaters per well'] if not math.isnan(data_fields_all.at[i,'Heaters per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(817,13).Value = data_fields_all.at[i,'Separators per well'] if not math.isnan(data_fields_all.at[i,'Separators per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(818,13).Value = data_fields_all.at[i,'Meters per well'] if not math.isnan(data_fields_all.at[i,'Meters per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(819,13).Value = data_fields_all.at[i,'Tanks per well'] if not math.isnan(data_fields_all.at[i,'Tanks per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(820,13).Value = data_fields_all.at[i,'Compressors per well'] if not math.isnan(data_fields_all.at[i,'Compressors per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(821,13).Value = data_fields_all.at[i,'Dehydrators per well'] if not math.isnan(data_fields_all.at[i,'Dehydrators per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(822,13).Value = data_fields_all.at[i,'Pneumatic Pumps'] if not math.isnan(data_fields_all.at[i,'Pneumatic Pumps']) else 0
        wb.Worksheets('Secondary inputs').Cells(823,13).Value = data_fields_all.at[i,'Pneumatics - All'] if not math.isnan(data_fields_all.at[i,'Pneumatics - All']) else 0
        if data_fields_all.iloc[i,11:20].sum() == 0:
            wb.Worksheets('Secondary inputs').Cells(811,13).Value = 1
    else:
        wb.Worksheets('Secondary inputs').Cells(815,13).Value = data_fields_all.at[i,'Headers per well'] if not math.isnan(data_fields_all.at[i,'Headers per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(816,13).Value = data_fields_all.at[i,'Heaters per well'] if not math.isnan(data_fields_all.at[i,'Heaters per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(817,13).Value = data_fields_all.at[i,'Separators per well'] if not math.isnan(data_fields_all.at[i,'Separators per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(818,13).Value = data_fields_all.at[i,'Meters per well'] if not math.isnan(data_fields_all.at[i,'Meters per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(819,13).Value = data_fields_all.at[i,'Tanks per well'] if not math.isnan(data_fields_all.at[i,'Tanks per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(820,13).Value = data_fields_all.at[i,'Compressors per well'] if not math.isnan(data_fields_all.at[i,'Compressors per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(821,13).Value = data_fields_all.at[i,'Dehydrators per well'] if not math.isnan(data_fields_all.at[i,'Dehydrators per well']) else 0
        wb.Worksheets('Secondary inputs').Cells(822,13).Value = data_fields_all.at[i,'Pneumatic Pumps'] if not math.isnan(data_fields_all.at[i,'Pneumatic Pumps']) else 0
        wb.Worksheets('Secondary inputs').Cells(823,13).Value = data_fields_all.at[i,'Pneumatics - All'] if not math.isnan(data_fields_all.at[i,'Pneumatics - All']) else 0
        if data_fields_all.iloc[i,11:20].sum() == 0:
            wb.Worksheets('Secondary inputs').Cells(811,13).Value = 1
            

    # Liquids unloadings and tank emission controls characteristics
    if not math.isnan(data_fields_all.at[i,'Frac wells with LU (plunger)']):
        wb.Worksheets('Secondary inputs').Cells(807,13).Value = data_fields_all.at[i,'Frac wells with LU (plunger)']
    else:
        wb.Worksheets('Secondary inputs').Cells(807,13).Value = wb.Worksheets('Secondary inputs').Cells(807,14).Value
    if not math.isnan(data_fields_all.at[i,'Frac wells with LU (no plunger)']):
        wb.Worksheets('Secondary inputs').Cells(808,13).Value = data_fields_all.at[i,'Frac wells with LU (no plunger)']
    else:
        wb.Worksheets('Secondary inputs').Cells(808,13).Value = wb.Worksheets('Secondary inputs').Cells(808,14).Value
    if not math.isnan(data_fields_all.at[i,'Frac flash control (throughput)']):
        wb.Worksheets('Secondary inputs').Cells(179,13).Value = data_fields_all.at[i,'Frac flash control (throughput)']
    else:
        wb.Worksheets('Secondary inputs').Cells(179,13).Value = wb.Worksheets('Secondary inputs').Cells(179,13).Value


    #print("Running macro...")
    print('Start: Index = ', i)
    xl.Application.Run(path_Macro)
    Index_Exceptions = []
    
    #if wb.Worksheets('Results').Cells(Ind_LCGHG_MJ,8).Value is None:
    if wb.Worksheets('Results').Cells(Ind_LCGHG_MJ,8).Value < -1000:
         Flag_exception = 1
         Index_Exceptions.append(i)
         print("NOTE: An exception occured. Index recorded.")
    print('Flag Exception = ', Flag_exception)
    print('LCGHG_MJ = ', wb.Worksheets('Results').Cells(Ind_LCGHG_MJ,8).Value)    
    
    #     data_fields.at[i,'GHG_upstream']                  = wb.Worksheets('Results').Cells(Ind_LCGHG_MJ                         ,8).Value * 0.000001 * 6120
    #     data_fields_withOPGEEdefaults.loc[[i]] = data_fields.loc[[i]]
        
    #     data_fields_withOPGEEdefaults.at[i,'Shore'           ]     = wb.Worksheets('Results').Cells(Ind_Results_Shore,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Water_flooding'  ]     = wb.Worksheets('Results').Cells(Ind_Results_Water_flooding,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Steam_flooding'  ]     = wb.Worksheets('Results').Cells(Ind_Results_Steam_flooding,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Oil_B_Day'       ]     = wb.Worksheets('Results').Cells(Ind_Results_Oil_B_Day,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Production_Wells']     = wb.Worksheets('Results').Cells(Ind_Results_Producing_Wells,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Inj_Wells'       ]     = wb.Worksheets('Results').Cells(Ind_Results_Inj_Wells,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'API_Gravity'     ]     = wb.Worksheets('Results').Cells(Ind_Results_API_Gravity,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Depth_Feet'      ]     = wb.Worksheets('Results').Cells(Ind_Results_Depth_Feet,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'WOR_B_B'         ]     = wb.Worksheets('Results').Cells(Ind_Results_WOR,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'SOR_B_B'         ]     = wb.Worksheets('Results').Cells(Ind_Results_SOR,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'FOR_SCF_B'       ]     = wb.Worksheets('Results').Cells(Ind_Results_FOR,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'GOR_SCF_B'       ]     = wb.Worksheets('Results').Cells(Ind_Results_GOR,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'WIR_B_B'         ]     = wb.Worksheets('Results').Cells(Ind_Results_WIR,8).Value
    #     data_fields_withOPGEEdefaults.at[i,'Asset_Age_Year'  ]     = wb.Worksheets('Results').Cells(Ind_Results_Field_Age,8).Value
        
    print('End: Index = ', i)
        
    if (MODE_RecordInputs == "ON") & (Flag_exception == 0):
        wb_recordinputs = xl.Workbooks.Open(os.path.abspath(path_Local + "RESULT_Data_Fields_recordinputs_22122_alloc3.xlsx"))
        #wb_recordinputs.Worksheets('Sheet1').Cells(8,i+8).Value = current_mined_basin + current_mined_sector
        wb.Worksheets('Results').Range("H9:H196").Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(9,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(9,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('Secondary inputs').Range('M176:M180').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(198,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(198,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('Secondary inputs').Range('M799:M824').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(204,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(204,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('L85:L134').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(231,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(231,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('M85:M134').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(282,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(282,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('D9:D31').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(333,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(333,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('P25:P66').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(358,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(358,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('Q25:Q66').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(401,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(401,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('R25:R66').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(444,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(444,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('S25:S66').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(487,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(487,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Energy Summary').Range('F128').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(530,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(530,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AG62').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(532,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(532,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AY62').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(533,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(533,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AX62').Copy()
        wb_recordinputs.Worksheets('Sheet1').Cells(534,i+8).Select()
        wb_recordinputs.Worksheets('Sheet1').Cells(534,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb_recordinputs.Save()
        wb_recordinputs.Close(True)

    if (MODE_RecordInputs == "ON") & (Flag_exception == 1):
        wb_recordexceptions = xl.Workbooks.Open(os.path.abspath(path_Local + "RESULT_Index_Exceptions_22122_alloc3.xlsx"))
        #wb_recordinputs.Worksheets('Sheet1').Cells(8,i+8).Value = current_mined_basin + current_mined_sector
        wb.Worksheets('Results').Range("H9:H196").Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(9,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(9,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('Secondary inputs').Range('M176:M180').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(198,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(198,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('Secondary inputs').Range('M799:M824').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(204,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(204,i+8).PasteSpecial(Paste = constants.xlPasteAllUsingSourceTheme, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('L85:L134').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(231,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(231,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('M85:M134').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(282,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(282,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('VFF Summary').Range('D9:D31').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(333,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(333,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('P25:P66').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(358,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(358,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('Q25:Q66').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(401,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(401,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('R25:R66').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(444,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(444,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb.Worksheets('GHG Summary').Range('S25:S66').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(487,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(487,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AG62').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(532,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(532,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AY62').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(533,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(533,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)

        wb.Worksheets('Flow Sheet').Range('AX62').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(534,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(534,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)


        wb.Worksheets('Energy Summary').Range('F128').Copy()
        wb_recordexceptions.Worksheets('Sheet1').Cells(530,i+8).Select()
        wb_recordexceptions.Worksheets('Sheet1').Cells(530,i+8).PasteSpecial(Paste = constants.xlPasteValues, Operation = constants.xlNone)
        wb_recordexceptions.Save()
        wb_recordexceptions.Close(True)

    # time_last = time_now
    # time_now  = time.time()
    # time_iter = time_now - time_last
    # time_left = time_iter * (len(data_fields)-(data_fields.index.tolist().index(i)+1)) /60/60      
    
    # print('Finished: ', data_fields.index.tolist().index(i)+1, "/", len(data_fields), ";", round(time_iter,0),"seconds;", round(time_left,2), "hours left.", "Fugitives:", Fugitives_model, "; MODE_RecordInputs:", MODE_RecordInputs, "OPGEE GHG:", wb.Worksheets('Results').Cells(Ind_LCGHG_MJ,8).Value, "; GHG:", data_fields.at[i,'GHG_upstream'])


   

# Final Processes
#if os.path.exists(path_Local + "RESULT_Data_Fields_withKevinDefaults.csv"): os.remove(path_Local + "RESULT_Data_Fields_withKevinDefaults.csv")    
#data_fields.to_csv(path_Local +"RESULT_Data_Fields_withKevinDefaults.csv")
#if os.path.exists(path_Local + "RESULT_Data_Fields_withOPGEEdefaults.csv"): os.remove(path_Local + "RESULT_Data_Fields_withOPGEEdefaults.csv")
#data_fields_withOPGEEdefaults.to_csv(path_Local + "RESULT_Data_Fields_withOPGEEdefaults.csv")
#if os.path.exists(path_Local + "RESULT_Index_Exceptions.csv"): os.remove(path_Local + "RESULT_Index_Exceptions.csv")
print("Record results final...")
#pd.DataFrame(Index_Exceptions).to_csv(path_Local + "RESULT_Index_Exceptions.csv")

wb_recordinputs.SaveAs(path_Local + 'RESULT_Data_Fields_recordinputs_22122_alloc3.xlsx')
wb_recordexceptions.SaveAs(path_Local + 'RESULT_Index_Exceptions_22122_alloc3.xlsx')

#wb_recordinputs.Close(True)

#xl.Application.Quit()
#del xl




