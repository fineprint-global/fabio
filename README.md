# FABIO v2: Food and Agriculture Biomass Input-Output Database

**FABIO provides a comprehensive set of multi-regional physical supply-use (PSUT) and input-output tables (IOT) covering global food and agriculture flows. It integrates data from FAOSTAT, IEA, EIA, and UN Comtrade/BACI to map the global flow of agricultural biomass from extraction to final consumption.**

This repository contains the **R code** used to generate the database.

## 🚀 Key Features of FABIO v2
* **Broad Coverage:** 181 regions (180 countries + 1 Rest of World).
* **High Resolution:** 119 processes and 123 commodities.
    * *Note:* A high-resolution parallel version featuring **525 commodities** is currently being developed.
* **Temporal Scope:** Data coverage for the years 2010–2023.
* **Allocation Methods:** Includes both **mass** and **value-based** allocation versions for symmetric tables.
* **Units:** All values are in physical units (tonnes or heads) as specified in `items.csv`.
* **Sustainability Metrics:** including land use, water use, nutrient cycles (N/P), GHG emissions, and biodiversity indicators (MSA loss, PDF).

---

## 📂 Database Structure
The database is provided in compressed `.rds` format, optimized for R. Most variables are organized as lists by year containing **sparse matrices** (using the `Matrix` package).

| Component | Description |
| :--- | :--- |
| **Z** | **Inter-commodity IO matrix** (intermediate use). 22,263 x 22,263. Available as `Z_mass` and `Z_value`. |
| **Y** | **Final demand matrix**. Includes categories: Food, Other (non-food), Stock additions, and Tourist consumption. |
| **X** | **Total output vector**. The sum of intermediate and final use for all commodities. |
| **L** | **Leontief inverse** $(I - A)^{-1}$. Provided for both mass and value allocation versions. |
| **E** | **Environmental extensions**. 123 stressors x 22,263 products. |

---

## 🛠 Installation & Usage
All processing is performed in **R**. To read the database files, ensure you have the `Matrix` package installed.

```R
# Example: Reading a FABIO rds file
data <- readRDS("Z_mass_2020.rds")
```

### Auxiliary Metadata (CSV)

For indexing and classification, refer to the following auxiliary files:
* `regions.csv`: Country samples, ISO3 codes, and regional groupings.
* `items.csv`: Commodity definitions, units, and product groupings.
* `io_labels.csv` / `su_labels.csv`: Row and column labels for IO and Supply-Use tables.
* `fd_labels.csv`: Column descriptions for the Final Demand (Y) matrix.

---

## 📜 License & Citation

### License

This repository is distributed under the [CC BY-NC-SA 4.0 License](https://creativecommons.org/licenses/by-nc-sa/4.0/).

* **Non-commercial use only**.
* **Attribution required**.
* **Share-alike** (derivative works must use the same license).

### Citation

If you use FABIO in your research, please cite the following paper:

> Bruckner, M., Wood, R., Moran, D., Kuschnig, N., Wieland, H., Maus, V., Börner, J. 2019. **FABIO – The Construction of the Food and Agriculture Input–Output Model.** *Environmental Science & Technology* 53(19), 11302–11312. [https://doi.org/10.1021/acs.est.9b03554](https://doi.org/10.1021/acs.est.9b03554)
> 

---

## 📬 Access & Collaboration

* **FABIO v1.1** (130 products, 1986-2013): [Available on Zenodo](http://dx.doi.org/10.5281/zenodo.2577067).
* **FABIO v2**: Available upon request.

For database access, collaboration inquiries, or technical support, please contact **Martin Bruckner** at [martin.bruckner@wu.ac.at](mailto:martin.bruckner@wu.ac.at).

---

**GitHub Repository:** [fineprint-global/fabio](https://github.com/fineprint-global/fabio)

