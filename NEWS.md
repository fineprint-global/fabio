
- The code was optimised to run through significantly faster, while also providing extra verbosity. The major contribution here was the use of the `data.table` package. Other optimisations include data structures, loops and redundancies.
- Most steps should now work on a local machine with 16GB of RAM. Estimation and balancing of BTD is more intensive, but should not exceed 32GB of RAM.
- The scripts now avoid iteration as much as possible - most steps are performed on the full data instead of yearly subsets. For balancing the BTD a loop is used to limit memory usage.
- Iterative proportional fitting (RAS) to balance the BTD is now done using `mipfp::Ipfp` - instead of targetting a 1% deviation from the target values (within a maximum of 20 iterations) convergence is checked via the gradient (with a maximum of 100 iterations). This should yield much better results, while still being significantly faster.
- Optimization is now done using a squared loss for production and an asymmetric loss, with squared loss for overpredictions and an absolute loss for underprediction, for processing. No adpatations are performed after optimization.
- Grazing was set to be the remainder of fodder crop demand after allocating supply. Since we have no information on grazing items the use and supply of grazing is now exactly set to the demand obtained from Bouwman et al.
- Fodder crops are now treated as a single feed use. As per Bouwman et al. the demand for fodder crops is derived from the residues.
- Conversion factors were reworked. This includes a bug fix where during the estimation of inputs for co-products (e.g. sugar and molasses) the outputs were overwritten with a fraction of actual outputs.

