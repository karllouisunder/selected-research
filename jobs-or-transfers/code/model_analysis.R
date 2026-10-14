# Research-analysis excerpts from the inspected formal JG project (2026-10-14).
# Source: analysis/gini_decomposition.R and analysis/analyze_results.R.
# Original function bodies and group definitions are retained. File-location,
# figure configuration and whole-project table-generation code are excluded.
# Gini method: Lerman & Yitzhaki (1985), source decomposition.
# Five representative household groups; no population or survey weights.

GROUPS      <- c("low", "lm", "m", "um", "up")
lg     <- function(x) { y <- x; y[, 2:ncol(x)] <- x[, 1:(ncol(x) - 1)]; y }   # lag one column
yd_of   <- function(e) list(low = e$ydl, lm = e$ydlm, m = e$ydm, um = e$ydum, up = e$ydu)

FAC_INC <- c("labor", "policy", "profit", "capOther", "tax", "misc")
inc_sources_grouped <- function(e) {
  rlh1 <- lg(e$rlh); ll1 <- lg(e$ll); llm1 <- lg(e$llm); lm1 <- lg(e$lm)
  rm1 <- lg(e$rm); rb1 <- lg(e$rb)
  m2lm1 <- lg(e$m2lm); m2m1 <- lg(e$m2m); m2um1 <- lg(e$m2um); m2u1 <- lg(e$m2u)
  bm1 <- lg(e$bm); bum1 <- lg(e$bum); bu1 <- lg(e$bu)
  z1 <- e$zeta_1; z2 <- e$zeta_2; z3 <- e$zeta_3; z4 <- e$zeta_4; Z <- 0 * e$wb
  list(
    low = list(labor = e$wb * z1, policy = e$wbg - e$tar + e$tr, profit = Z, capOther = Z,
               tax = -e$taxl, misc = -(rlh1 * ll1) - 0.5 * e$rhos),
    lm  = list(labor = e$wb * z2, policy = Z, profit = Z, capOther = rm1 * m2lm1,
               tax = -e$taxlm, misc = -(rlh1 * llm1) - 0.5 * e$rhos),
    m   = list(labor = e$wb * z3, policy = Z, profit = Z, capOther = rm1 * m2m1 + rb1 * bm1,
               tax = -e$taxm, misc = -(rlh1 * lm1)),
    um  = list(labor = e$wb * z4, policy = Z, profit = Z,
               capOther = rm1 * m2um1 + rb1 * bum1 + e$rhosum_perc * e$rhos, tax = -e$taxum, misc = Z),
    up  = list(labor = e$wb * (1 - z1 - z2 - z3 - z4), policy = Z, profit = e$fdf + e$fb,
               capOther = rm1 * m2u1 + rb1 * bu1 + (1 - e$rhosum_perc) * e$rhos, tax = -e$taxu, misc = Z)
  )
}

ly_contrib <- function(cbg, fk) {          # Lerman-Yitzhaki (Gini)
  tot <- sapply(GROUPS, function(g) sum(cbg[[g]])); n <- length(GROUPS); mu <- mean(tot)
  r <- rank(tot, ties.method = "average")
  sapply(fk, function(k) { xc <- sapply(GROUPS, function(g) cbg[[g]][[k]])
                           (2 / (n * mu)) * mean((xc - mean(xc)) * (r - mean(r))) })
}
cv2_contrib <- function(cbg, fk) {         # squared CV (exact covariance decomposition)
  tot <- sapply(GROUPS, function(g) sum(cbg[[g]])); mu <- mean(tot)
  sapply(fk, function(k) { xc <- sapply(GROUPS, function(g) cbg[[g]][[k]])
                           mean((xc - mean(xc)) * (tot - mu)) / mu^2 })
}

phase3_sfc_residuals <- function(e) {
  total_net <- e$nvl + e$nvlm + e$nvm + e$vum + e$vu
  list(
    horizontal_cash = e$hh - e$hs,
    horizontal_m1 = e$m1h - e$m1s,
    horizontal_m2 = e$m2h - e$m2s,
    horizontal_required_reserves = e$hbd - e$hbs,
    horizontal_extra_reserves = e$hbd_star - e$hbs_star,
    horizontal_advances = e$ad - e$as,
    horizontal_government_bills = e$bb + e$bh + e$bcb - e$bs,
    horizontal_equity = e$eh - e$esr * e$pe,
    vertical_household_low = e$hl + e$m1l - e$ll - e$nvl,
    vertical_household_lower_middle =
      e$hlm + e$m1lm + e$m2lm - e$llm - e$nvlm,
    vertical_household_middle =
      e$hm + e$m1m + e$m2m - e$lm + e$bm - e$nvm,
    vertical_household_upper_middle =
      e$hum + e$m1um + e$m2um + e$bum + e$hosum - e$vum,
    vertical_household_high =
      e$hu + e$m1u + e$m2u + e$bu + e$pe * e$ehh - e$vu + e$hosu,
    vertical_firms = -e$lf - e$esr * e$pe + e$k - e$vf,
    vertical_banks =
      -e$m1s - e$m2s + e$ls + e$bb + e$hbd + e$hbd_star - e$ad,
    vertical_central_bank =
      -e$hs - e$hbd - e$hbd_star + e$as + e$bcb + e$fr,
    net_accumulation =
      total_net - e$bs - e$fr + e$vf - e$k - e$hosu - e$hosum
  )
}

