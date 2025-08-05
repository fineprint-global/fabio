# Country-Specific Grain Preference Mapping ---------------------------
# Defines penalties for grains used in beer production in accordance with local preferences
get_beer_grain_penalties <- function(area_code) {
  # Define country groups by FAO area codes
  sorghum_countries <- c("BFA", "NGA", "GHA", "TGO", "BEN", "MLI", "TZA", "UGA", "KEN",
                         "RWA", "ETH", "ZAF", "ZWE", "ZMB", "MWI", "BWA", "SDN", "TCD", "NER", "CMR")
  maize_countries   <- c("PER", "BOL", "ECU", "COL", "MEX")
  rice_countries    <- c("JPN", "THA", "VNM", "KOR")
  
  # FAO codes (convert from ISO3 if needed)
  if (area_code %in% countrycode::countrycode(sorghum_countries, "iso3c", "fao")) {
    preferred <- 2518  # Sorghum and products
  } else if (area_code %in% countrycode::countrycode(maize_countries, "iso3c", "fao")) {
    preferred <- 2514  # Maize and products
  } else if (area_code %in% c(countrycode::countrycode(rice_countries, "iso3c", "fao"))) {  
    preferred <- 2807  # Rice and products
  } else {
    preferred <- 2513  # Barley and products (default)
  }
  
  # Return named vector of penalties for beer inputs
  beer_inputs <- c(2511, 2513, 2514, 2515, 2517, 2518, 2520, 2807)  # all beer-relevant grains
  penalties <- setNames(rep(500, length(beer_inputs)), beer_inputs)
  penalties[as.character(preferred)] <- 100
  if (preferred != 2513) penalties["2513"] <- 250  # treat barley neutrally if not preferred
  
  # Separate solution for China
  if (area_code == 41){
    penalties <- setNames(rep(500, length(beer_inputs)), beer_inputs)
    penalties[c("2807")] <- 300   # slightly higher preference for rice
    penalties[c("2514")] <- 150   # higher preference for maize
    penalties[c("2513")] <- 100   # highest preference for barley
  }
  
  # Separate solution for the US
  if (area_code == 231){
    penalties <- setNames(rep(500, length(beer_inputs)), beer_inputs)
    penalties[c("2513", "2514")] <- 50   # higher preference for barley and maize
  }
  
  return(penalties)
}


# Quadratic Input-Output Solver ---------------------------
# Solves input-output flow optimization to exactly match production targets, minimizing input use
solve_flow_qp <- function(area_code, area, year, inp, out, tcf) {
  stopifnot(is.data.table(inp), is.data.table(out), is.data.table(tcf))
  
  inp_codes <- inp[["item_code"]]
  out_codes <- out[["item_code"]]
  
  # Filter relevant flows
  tcf <- tcf[inp_code %in% inp_codes & out_code %in% out_codes]
  tcf[, var_id := .I]
  n_flows <- nrow(tcf)
  if (n_flows == 0) {
    message("No valid transfer coefficients after filtering.")
    return(NULL)
  }
  
  # Prepare penalty matrix (Dmat) and vector (dvec)
  Dmat <- diag(n_flows)
  dvec <- rep(0, n_flows)
  
  # Load beer grain penalties
  beer_out_code <- 2656
  beer_penalties <- get_beer_grain_penalties(area_code)
  
  # Build substitution groups dynamically
  substitution_map <- tcf[, .(inp_codes = list(unique(inp_code))), by = out_code]
  
  # Base scarcity penalties
  availability_map <- inp[, .(inp_code = as.character(item_code), available = processing)]
  availability_map[, raw_penalty := fifelse(available > 0, 1 / available, 1e3)]
  availability_map[, log_penalty := log1p(raw_penalty)]
  
  # Assign scaled penalties per output group
  for (j in seq_len(nrow(substitution_map))) {
    out_code <- substitution_map$out_code[j]
    input_codes <- substitution_map$inp_codes[[j]]
    input_strs <- as.character(input_codes)
    
    # Subset relevant penalties
    penalties <- availability_map[inp_code %in% input_strs]
    
    # Normalize within group
    penalties[, scaled := scale(log_penalty, center = FALSE, 
                                scale = max(log_penalty, na.rm = TRUE))]
    penalties[, final_penalty := scaled * 10]  # Adjust multiplier as needed
    
    # Apply to Dmat
    for (i in which(tcf$out_code == out_code & 
                    as.character(tcf$inp_code) %in% input_strs)) {
      
      inp_code_str <- as.character(tcf[i]$inp_code)
      penalty <- penalties[inp_code == inp_code_str, final_penalty]
      
      # Beer-specific adjustment
      if (out_code == beer_out_code && inp_code_str %in% names(beer_penalties)) {
        penalty <- penalty * beer_penalties[inp_code_str]
      }
      
      if (length(penalty) == 1 && !is.na(penalty)) {
        Dmat[i, i] <- penalty
      }
    }
  }
  
  # Equality constraints: exact target output
  Amat_list <- list()
  bvec <- numeric(0)
  
  for (code in out_codes) {
    idx <- which(tcf$out_code == code)
    if (length(idx) > 0) {
      a <- numeric(n_flows)
      a[idx] <- tcf$value[idx]
      Amat_list[[length(Amat_list) + 1]] <- a
      bvec <- c(bvec, out[item_code == code, production])
    }
  }
  
  Amat <- t(do.call(rbind, Amat_list))
  meq <- length(bvec)
  
  # Solve QP
  qp_result <- tryCatch({
    solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
  }, error = function(e) {
    message("QP failed: ", e$message)
    return(NULL)
  })
  if (is.null(qp_result)) return(NULL)
  
  # Output results
  solution <- round(qp_result$solution, 0)
  tcf[, flow := solution]
  tcf[, output := round(flow * value, 0)]
  
  flow <- tcf[flow > 0, .(inp_code, inp, out_code, out, area_code, year, flow, output)]
  
  input_used <- tcf[, .(input_use = sum(flow)), by = .(inp_code)]
  input_dt <- merge(inp[, .(inp_code = item_code, item, available = processing, area_code, year)],
                    input_used, by = "inp_code", all.x = TRUE)
  input_dt[is.na(input_use), input_use := 0]
  input_dt[, overused := pmax(0, input_use - available)]
  input_dt[, underused := pmax(0, available - input_use)]
  
  underused <- input_dt[underused > 1, .(inp_code, item, input_use, available, area_code, area, year, underused)]
  overused  <- input_dt[overused > 1, .(inp_code, item, input_use, available, area_code, area, year, overused)]
  
  list(
    flow = flow[order(out_code)],
    underused = underused[order(inp_code)],
    overused = overused[order(inp_code)]
  )
}
