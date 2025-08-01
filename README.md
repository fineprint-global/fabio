# FABIO v2

### The Food and Agriculture Biomass Input-Output database
FABIO provides a set of multi-regional physical supply-use and input-output tables covering global agriculture and forestry. The work is based on mostly freely available data from FAOSTAT, IEA, EIA, and UN Comtrade/BACI. 

This repository provides all codes used to generate the FABIO database.

### How to cite
To cite FABIO please refer to this paper:
Bruckner, M., Wood, R., Moran, D., Kuschnig, N., Wieland, H., Maus, V., Börner, J. 2019. FABIO – The Construction of the Food and Agriculture Input–Output Model. Environmental Science & Technology 53(19), 11302–11312. DOI: 10.1021/acs.est.9b03554

FABIO database v1.1:
Bruckner, M. & Kuschnig, N. (2020). Food and Agriculture Biomass Input–Output (FABIO) database (1.1). Zenodo. https://doi.org/10.5281/zenodo.2577067

FABIO database v2:
Not yet published! Do not use or distribute without prior consent by the author. Contact [Martin Bruckner](mailto:martin.bruckner@wu.ac.at) for more information.

### License
This data repository is distributed under the CC BY-NC-SA 4.0 License. You are free to share and adapt the material for non-commercial purposes using proper citation. If you remix, transform, or build upon the material, you must distribute your contributions under the same license as the original. In case you are interested in a collaboration, I am happy to receive inquiries via [email](mailto:martin.bruckner@wu.ac.at).

### Database availability
The FABIO v1 database is available via Zenodo (http://dx.doi.org/10.5281/zenodo.2577067).
The preliminary FABIO v2 database is available upon request.

### Coverage
- 187 regions (186 countries + 1 RoW)
- 119 processes (in the mr_sup and mr_use tables)
- 123 commodities (raw and processed agricultural and food products)
- Years 2010-2022

All R codes and auxilliary data are available on GitHub: https://github.com/fineprint-global/fabio

### Database structure
The database consists of the following main components, in compressed .rds format:

- Z: the inter-commodity input-output matrix, displaying the intermediate use of each commodity in the production of each commodity, in physical units (tonnes, heads). The matrix has 23001 rows and columns (123 commodities x 187 regions), and is available in two versions, where process inputs are allocated to process outputs in relation to their mass or value respectively: Z_mass (mass allocation) and Z_value (value allocation). Note that the row sums of the Z matrices of the two versions (i.e. the total intermediate use by commodity) are identical in both versions.

- Y: the final demand matrix, denoting the consumption of all 23001 commodities by destination country and final use category. There are five final use categories (yielding 187 x 5 = 935 columns): 1) food, 2) other (i.e. non-food), 3) stock_addition, 4) tourist, 5) unspecified.

- X: the total output vector of all 23001 commodities. Total output is equal to the sum of intermediate and final use by commodity.

- L: the Leontief inverse, computed as (I – A)^-1, where A is the matrix of input coefficients derived from Z and x. Again, there are two versions, depending on the underlying version of Z (L_mass and L_value).

- E: environmental extensions for each of the 23616 commodities, including four resource categories: 
--- 1) primary biomass extraction (in tonnes), 
--- 2) cropland use (in hectares), 
--- 3) grassland use (in hectares), 
--- 4) blue water use (in m3).
--- 5) green water use (in m3).
--- 6) phosphorous application (in kg)
--- 7) nitrogen application (in kg)

- E_biodiv: Biodiversity extension in potential species loss per commodity, based on Characterization Factors from Chaudhary et al. (2015). CFs are available for plants, mammals, birds, reptiles and amphibians, for both cropland and pasture and are based on total species populations given in biodiv_labels.csv

Except of X.rds, which contains a matrix and L, which is given separately by year, all variables are organized as lists by year, where each element contains a sparse matrix. Please note that values are always given in physical units, i.e. tonnes or number of animals, as specified in items.csv. The suffixes value and mass only indicate the form of allocation chosen for the construction of the symmetric IO tables (for more details see Bruckner et al. 2019).

RDS files can be opened in R. Information on how to read these files can be obtained here: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/readRDS

Auxiliary information in csv format:
- A description of the rows and columns of the Z matrix (i.e. the covered countries and commodities) can be found in the auxiliary file io_labels.csv.
- Separate lists of the country sample (including ISO3 codes and regional grouping) and commodities are given in the files regions.csv and items.csv, respectively. 
- For a list of the individual processes, see auxiliary file su_labels.csv.
- The columns of Y are described in the auxiliary file fd_labels.csv.
