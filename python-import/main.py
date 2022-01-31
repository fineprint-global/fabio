"""FABIO v2 Python import.

Script for importing some of the *.CSV and .*RDS files of
the FABIO database v2.

The FABIO database v2 should be placed in the folder data/fabio_v2/fabio_v2/
(otherwise the value of `fabio_filepath` needs to be changed).

The folder should contain at least the following files:
    
    - E.rds
    - io_codes.csv
    - items.csv
    - regions.csv
    - X.rds
    - Y.rds
    - Z_mass.rds
    - Z_value.rds
    
# For units see the `fabio_items` dataframe

This import script was written by
jonas.bunsen@tu-berlin.de

"""

import os

# Change/set working directory (OPTIONAL)
# os.chdir("C:/Users/Username/Project")

import app.fabio_rds_import as fabio
import numpy as np
import pandas as pd

# %% FABIO import

if True:
    
    fabio_filepath = "data/" # Default (can be changed)
    fabio_year = "2013" # Default (can be changed)

    fabio_items = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).items
    
    fabio_regions = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).regions
    
    fabio_io_codes = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).io_codes
    
    fabio_E = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).E() 
    
    fabio_X = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).X()
    
    fabio_Y = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).Y() 
    
    fabio_Z_value = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).Z_value() 
    
    fabio_Z_mass = fabio.read(
        path = fabio_filepath,
        year = fabio_year,
        ).Z_mass() 
    
# %% Some checks

if False:
    
    fabio_Z_value_head = fabio_Z_value.iloc[:100,:100]
    fabio_Z_mass_head = fabio_Z_mass.iloc[:100,:100]
    
# %% Comapre x-imported with x-calculated

if False:
    
    fabio_X_calculated = (
        fabio_Y.sum(axis=1)
        + fabio_Z_mass.sum(axis=1)
        )

    # 24000x True which means T, Y and X import has worked fine. ... so
    # the others should work too ... ;-)
    pd.Series(fabio_X_calculated == fabio_X).value_counts()

# %% Calculation

if False:
     
    print("Determining technical coefficients ...")
    
    # Divide and replace np.nan
    # np.nan occurs if 0/0
    
    fabio_A = fabio_Z_mass.divide(
        fabio_X['x']
        ).replace(np.nan,0)
    
    print("Determining identity matrix ...")
    
    fabio_I = np.identity(len(fabio_Z_mass))
    
    print("Determining Leontief inverse ...")
    # Takes approximately ten minutes
    fabio_L = np.linalg.inv(fabio_I - fabio_A) 