"""Import of the FABIO database.

The Food and Agriculture Biomass Input-Output database

FABIO provides a set of multi-regional physical supply-use and input-output
tables covering global agriculture and forestry. The work is based on mostly
freely available data from FAOSTAT, IEA, EIA, and UN Comtrade/BACI. FABIO
currently covers 191 countries + RoW, 118 processes and 125 commodities for
1986-2013.

Detailed information on the construction of the database can be found in
Bruckner et al (2018): www.doi.org/10.1021/acs.est.9b03554. Please refer to
this paper when citing FABIO.

The database consists of the following main components,
in compressed .rds format:

# =============================================================================
# From the FABIO README
# =============================================================================

Z:
    the inter-commodity input-output matrix, displaying the relationships of
    intermediate use of each commodity in the production of each commodity,
    in physical units (tons). The matrix has 24000 rows and columns
    (125 commodities x 192 regions), and is available in two versions, based
    on the method to allocate inputs to outputs in production processes:
        
        Z_mass (mass allocation) and Z_value (value allocation).
        
    Note that the row sums of the Z matrix (= total intermediate use by
    commodity) are identical in both versions.

Y:
    the final demand matrix, denoting the consumption of all 24000 commodities
    by destination country and final use category. There are six final use
    categories (yielding 192 x 6 = 1152 columns): 1) food use, 2) other use
    (non-food), 3) losses, 4) stock addition, 5) balancing, and 6) unspecified.

X: 
    the total output vector of all 24000 commodities. Total output is equal to
    the sum of intermediate and final use by commodity.

L: 
    the Leontief inverse, computed as (I – A)-1, where A is the matrix of input
    coefficients derived from Z and x. Again, there are two versions, depending
    on the underlying version of Z (L_mass and L_value). 

E: 
    environmental extensions for each of the 24000 commodities, including four
    resource categories: 1) primary biomass extraction (in tons), 2) land use
    (in hectares), 3) blue water use (in m3)., and 4) green water use (in m3). 

# =============================================================================
# Own remarks
# =============================================================================
    
# Satellite accounts.
# land use
# biomass
# green water consumption
# blue water consumption
"E_rds" = readRDS(sprintf("%s/E.rds",source_folder))

# Total output
# A column for each year (28 years)
# 24000 rows
"X_rds" = readRDS(sprintf("%s/X.rds",source_folder))

# Final demand
# 28 years (1986 to 2013)
# 24000 rows and 192 region * 6 final demand categories = 1152 columns
"Y_rds" = readRDS(sprintf("%s/Y.rds",source_folder))

# Transactions
# 28 years (1986 to 2013)
# Each year features a 24000x24000 matrix
# Monetary transactions
# io_codes.csv as row and col index
"Z_value_rds" = readRDS(sprintf("%s/Z_value.rds",source_folder))

# Transactions
# See Z_value.rds
# Physical transactions
"Z_mass_rds" = readRDS(sprintf("%s/Z_mass.rds",source_folder))

# Input-output
# 125 items * 192 area = 24000 rows
"io_codes_csv" = read.csv(sprintf("%s/io_codes.csv",source_folder))

# Supply-use
# 118 items * 192 areas = 22656 rows
"su_codes_csv" = read.csv(sprintf("%s/su_codes.csv",source_folder))

# 125 items
"items_csv" = read.csv(sprintf("%s/items.csv",source_folder))

# 192 regions
"regions_csv" = read.csv(sprintf("%s/regions.csv",source_folder))
    
"""

import numpy as np
import pandas as pd
from scipy import sparse
from rpy2  import robjects
        
# =============================================================================
# Read FABIO v2 database
# =============================================================================
        
class read():
    """Read/import FABIO v2 database.
    
    Parameters
    ----------
    path : STR, optional
        File path to the FABIO v2 database.
        The default is "data/fabio_v2/fabio_v2_csv".
    year : INT, optional
        Year of the FABIO database. The default is 2013.

    Returns
    -------
    None.

    """

    def __init__(
        self,
        path = "data/fabio_v2/fabio_v2/",
        year = 2013
        ):
        """Init.
        
        Parameters
        ----------
        path : STR, optional
            File path to the FABIO v2 database.
            The default is "data/fabio_v2/fabio_v2/".
        year : INT, optional
            Year of the FABIO database. The default is 2013.

        Returns
        -------
        None.

        """
        self.path = path
        self.year = year
        
        print(f"{10*'='} Class `read()` {10*'='}")
        print(f"FABIO year {self.year}")
        print(f"FABIO path {self.path}")
        
        # Items
        # 125 items
        read.items = pd.read_csv(
            f"{self.path}/items.csv"
            )
        
        # Regions
        # 192 regions
        read.regions = pd.read_csv(
            f"{self.path}/regions.csv",
            encoding = "ISO-8859-1"
            )
        
        # IO-codes
        # 192 regions * 125 items = 24000 codes
        read.io_codes = pd.read_csv(
            f"{self.path}/io_codes.csv"
            )
        
        # SU-codes
        # 192 regions * 118 x = 22656 ???
        read.su_codes = pd.read_csv(
            f"{self.path}/su_codes.csv"
            )
        
    def E(self):
        """
        Import E (satellite accounts).

        Returns
        -------
        df : pd.DataFrame()
            Pandas dataframe containing the satellite accounts.

        """
        print("Reading E ...")
        
        readRDS = robjects.r['readRDS']
        
        # Read RDS file
        rds_file = readRDS(f"{self.path}/E.rds")

        # Extract year
        rds_file_year = rds_file[rds_file.names.index(f"{self.year}")]

        # Create index from dict
        df = pd.DataFrame.from_dict(
            {
                key : np.asarray(rds_file_year.rx2(key))
                for key in rds_file_year.names
                }
            )
        
        return df
    
    def X(self):
        """
        Import x (???).

        Returns
        -------
        df : pd.DataFrame()
            Pandas dataframe containing the total output (transactions +
            final demand). 

        """
        print("Reading X ...")
        
        readRDS = robjects.r['readRDS']
        
        rds_file = readRDS(f"{self.path}/X.rds")
        
        df = pd.DataFrame(np.array(rds_file))
        
        # Year 1986 to 2013 as columns
        df.columns = list(range(1986,2013+1))
        
        # Select only one year
        df = df.loc[:,int(self.year)]
        
        # Turn colname into string
        df.columns = [str(self.year)]
        
        # Add MultiIndex
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        
        return df
    
    def Y(self):
        """
        Import Y (final demand).

        Returns
        -------
        df : pd.DataFrame()
            Pandas dataframe containing the final demand.

        """
        print("Reading Y ... ")
        
        readRDS = robjects.r['readRDS']
        
        rds_file = readRDS(f"{self.path}/Y.rds")
        
        # Select year (string, not int)
        rds_year = rds_file.rx2(f"{self.year}")
        
        # Sparse matrix specs
        data    = rds_year.do_slot('x')   # in R: x@x
        indices = rds_year.do_slot('i')   # in R: x@i
        indptr  = rds_year.do_slot('p')   # in R: x@p
        shape   = rds_year.do_slot('Dim') # in R: x@Dim or dim(x)
        
        # Turn into amtrix
        rds_year_matrix = sparse.csc_matrix(
            (data, indices, indptr),
            tuple(shape)
            )
        
        # Turn into array
        data_array = sparse.csc_matrix.toarray(rds_year_matrix)
        
        # Turn to Pandas 
        df = pd.DataFrame(data_array)
        
        # Add column index
        df = df.T
        
        col_index = self.regions
        col_index = ( # Repeat index n-times
            col_index.loc[col_index.index.repeat(6)].reset_index(drop=True)
            )
        col_index["final demand"] = (
            self.regions.shape[0] # 192 regions
            * [
                "balancing",
                "food",
                "losses",
                "other",
                "stock addition",
                "unspecific"
                ]
            )
        
        # Add MultiIndex (columns)
        df.index = pd.MultiIndex.from_frame(
            col_index
            )
        
        df = df.T
        
        # Add MultiIndex (rows)
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        
        return df
    
    def Z_value(self):
        """
        Import transactions matrix.

        Returns
        -------
        df : pd.DataFrame()
            Pandas dataframe containing the transaction matrix.

        """
        print("Reading Z_value ...")
        
        readRDS = robjects.r['readRDS']
        
        rds_file = readRDS(f"{self.path}/Z_value.rds")
        
        # Select year (string, not int)
        rds_year = rds_file.rx2(f"{self.year}")
        
        # Sparse matrix specs
        data    = rds_year.do_slot('x')   # in R: x@x
        indices = rds_year.do_slot('i')   # in R: x@i
        indptr  = rds_year.do_slot('p')   # in R: x@p
        shape   = rds_year.do_slot('Dim') # in R: x@Dim or dim(x)
        
        # Turn into matrix
        rds_year_matrix = sparse.csc_matrix(
            (data, indices, indptr),
            tuple(shape)
            )
        
        # Turn into array
        data_array = sparse.csc_matrix.toarray(rds_year_matrix)
        
        # Turn into pd.DataFrame
        
        df = pd.DataFrame(data_array)
        
        # Add MultiIndex (columns)
        df = df.T
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        df = df.T
        
        # Add MultiIndex (rows)
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        
        return df
        
    def Z_mass(self):
        """
        Import transactions matrix.

        Returns
        -------
        df : pd.DataFrame()
            Pandas dataframe containing the transaction matrix.

        """
        print("Reading Z_mass ...")
        
        readRDS = robjects.r['readRDS']
        
        rds_file = readRDS(f"{self.path}/Z_mass.rds")
        
        # Select year (string, not int)
        rds_year = rds_file.rx2(f"{self.year}")
        
        # Sparse matrix specs
        data    = rds_year.do_slot('x')   # in R: x@x
        indices = rds_year.do_slot('i')   # in R: x@i
        indptr  = rds_year.do_slot('p')   # in R: x@p
        shape   = rds_year.do_slot('Dim') # in R: x@Dim or dim(x)
        
        # Turn into matrix
        rds_year_matrix = sparse.csc_matrix(
            (data, indices, indptr),
            tuple(shape)
            )
        
        # Turn into array
        data_array = sparse.csc_matrix.toarray(rds_year_matrix)
        
        # Turn into pd.DataFrame
        
        df = pd.DataFrame(data_array)
        
        # Add MultiIndex (columns)
        df = df.T
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        df = df.T
        
        # Add MultiIndex (rows)
        df.index = pd.MultiIndex.from_frame(
            self.io_codes
            )
        
        return df