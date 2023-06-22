import pandas as pd 
import numpy as np 

def get_data(sheet_name, column, na_values=["-"]):
    df = pd.read_excel(r"C:\Users\USER\Python Programme\uni\fe_assignment\spot_future.xlsx", sheet_name=sheet_name, index_col=0, na_values=na_values)
    if not column == "":
        future_df = df[column]
    else:
        future_df = df.dropna(axis=0)
    return future_df
