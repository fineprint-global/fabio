"""Import of the FABIO database.

The Food and Agriculture Biomass Input-Output database

FABIO provides a set of multi-regional physical supply-use and input-output
tables covering global agriculture and forestry. The work is based on mostly
freely available data from FAOSTAT, IEA, EIA, and UN Comtrade/BACI.

Detailed information on the construction of the database can be found in
Bruckner et al (2018): www.doi.org/10.1021/acs.est.9b03554. Please refer to
this paper when citing FABIO.
"""
import numpy as np
import pandas as pd
from scipy import sparse
from rpy2  import robjects


class read():
    """
    Attributes
    ----------
    path: File path to the database.
    year: Year of the database.
    version: Version of the database.
    readRDS: readRDS object from the robjects package.
    items: Product names, codes, and other information.
    regions: Region and region codes codes.
    io_codes: Combined product and region codes.
    su_codes: Combined process and region codes.
    start_year: Fist available year of the database.
    end_year: Final available year of database.

    Methods
    -------
    E(): Returns pd.DataFrame with satellite accounts.
    X(): Returns pd.Series with total product output.
    Y(): Returns pd.DataFrame with final demand consumption.
    L(version): Returns pd.DataFrame with the Leontief matrix, if available.
                Version refers to the suffix used for the file name.
    Z(version): Returns pd.DataFrame with the transaction matrix.
                Version refers to the suffix used for the file name.

    """

    def __init__(
            self,
            path="data/",
            year=2013,
            version=1.1,
            ):
        """Read/import FABIO
        Parameters
        ----------
        path : STR, optional
            File path to the FABIO database.
        year : INT, optional
            Year of the FABIO database.
        version : INT, FLOAT, optional
            FABIO version.

        Returns
        -------
        None.
        """
        self.path = path
        self.year = year
        self.version = version

        print(f"{10*'='} Class `read()` {10*'='}")
        print(f"FABIO year {self.year}")
        print(f"FABIO path {self.path}")

        self.readRDS = robjects.r['readRDS']

        # Items
        self.items = pd.read_csv(
            f"{self.path}/items.csv"
        )

        # Regions
        self.regions = pd.read_csv(
            f"{self.path}/regions.csv",
            encoding="ISO-8859-1"
        )

        # TODO: IO-codes and SU-codes should be created from the
        # TODO: ... the items and regions datasets.
        # TODO: ... Group category is missing from the items.csv.
        # IO-codes
        # 192 regions * 125 items = 24000 codes
        self.io_codes = pd.read_csv(
            f"{self.path}/io_codes.csv"
        )

        # SU-codes
        # 192 regions * 118 x = 22656 ???
        self.su_codes = pd.read_csv(
            f"{self.path}/su_codes.csv"
        )

        # Version specific parameters
        if self.version == 1.1:
            self.start_year = 1986
            self.end_year = 2013

        elif self.version == 1.2:
            self.start_year = 1986
            self.end_year = 2020
        
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