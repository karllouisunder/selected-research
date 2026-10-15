# Jobs or Transfers? Stock-flow consistent macro-financial model with five household groups.
#
# Sectors: firms, five household income groups (H1 lowest to H5 highest), commercial
# banks, government, central bank and the rest of the world. Variables are matrices with
# one row per simulation path and one column per period. Suffixes l, lm, m, um, u denote
# H1 to H5. Names prefixed phase3_ hold the policy design (S0 baseline; S1-S6 job
# guarantee or transfer, financed by debt or a high-income tax).
#
# Source this file into an environment that defines phase3_spec (see run_example.R).
# Requires the 'ineq' package. The file reads no data and writes no files.

library(ineq)

# ---- STEP 1: Policy and shock design -------------------------------------------
# Policy magnitudes, timing, financing and shock settings are set only here.
# The caller supplies the policy number, shock type and run length in phase3_spec.
if (!exists('phase3_spec', inherits = FALSE)) {
  phase3_spec <- list(
    policy = as.integer(shock), shock_type = 'none', n_periods = 100L
  )
}
phase3_policy <- as.integer(phase3_spec$policy)
phase3_shock_type <- as.character(phase3_spec$shock_type)
phase3_policy_start_period <- 60L
phase3_policy_start_year <- 2026L

# Optional demand-shock study; NULL (default) keeps the standard run.
phase3_demand_study <- phase3_spec$demand_study
phase3_demand_active <- !is.null(phase3_demand_study)
phase3_iteration_limit <- 100L
if (phase3_demand_active) {
  phase3_demand_defaults <- list(s_c=0, s_i=0, exposure="all", path="recovery",
    shock_start=60L, plateau_end=64L, shock_end=69L, policy_start=61L,
    budget_base=59L, mc=10L, diagnostics=TRUE, iteration_limit=100L)
  unknown <- setdiff(names(phase3_demand_study), names(phase3_demand_defaults))
  if (length(unknown)) stop("Unknown demand-study settings: ", paste(unknown,collapse=","))
  phase3_demand_study <- modifyList(phase3_demand_defaults,phase3_demand_study)
  ds <- phase3_demand_study
  scalars <- c(ds$s_c,ds$s_i,ds$shock_start,ds$plateau_end,ds$shock_end,ds$policy_start,ds$budget_base,ds$mc,ds$iteration_limit)
  if (any(!is.finite(scalars)) || ds$s_c<0 || ds$s_c>=1 || ds$s_i<0 || ds$s_i>=1 ||
      !ds$exposure %in% c("all","lower4") || !ds$path %in% c("recovery","persistent") ||
      !ds$mc %in% c(2L,10L) || !ds$iteration_limit %in% c(100L,300L) ||
      ds$budget_base<1L || ds$budget_base>=ds$shock_start ||
      ds$shock_start<2L || ds$plateau_end<ds$shock_start || ds$shock_end<=ds$plateau_end ||
      ds$policy_start<=ds$shock_start || ds$shock_end>phase3_spec$n_periods ||
      phase3_shock_type!="none" || !isTRUE(ds$diagnostics)) stop("Invalid demand-study setting/timing")
  phase3_policy_start_period <- as.integer(ds$policy_start)
  phase3_iteration_limit <- as.integer(ds$iteration_limit)
}
phase3_budget_base_period <- if(phase3_demand_active) as.integer(ds$budget_base) else phase3_policy_start_period-1L
phase3_policy_net_cost_share <- 0.015
# Scale of the common net fiscal cost target, set with options(jg.policy_scale_mode=).
#   prepolicy_gdp (default): GDP in the period before the policy starts, so the
#                            programme is a fixed nominal amount.
#   current_gdp:             contemporaneous GDP y[j,i] (solved within the period;
#                            can be unstable).
#   current_gdp_lag:         last period's GDP y[j,i-1], a constant share of output
#                            on the balanced-growth path.
phase3_policy_scale_mode <- getOption("jg.policy_scale_mode", "prepolicy_gdp")
if (!phase3_policy_scale_mode %in% c("prepolicy_gdp", "current_gdp", "current_gdp_lag")) {
  stop("Unknown jg.policy_scale_mode: ", phase3_policy_scale_mode, call. = FALSE)
}
phase3_jg_wage_ratio <- 0.60
phase3_project_revenue_share <- 0.25

# Programme type: 0 none, 1 employment-scale priority, 2 wage priority,
# 3 direct transfer. Financing type: 0 none, 1 debt, 2 high-income tax.
# Vector order is S0, S1, S2, S3, S4, S5, S6.
phase3_program_type <- c(0L, 1L, 2L, 3L, 3L, 2L, 1L)[phase3_policy + 1L]
phase3_financing_type <- c(0L, 1L, 1L, 2L, 1L, 2L, 2L)[phase3_policy + 1L]

# Temporary shock path and intensity (used only when shock_type is not 'none').
phase3_shock_start_period <- 60L
phase3_shock_peak_period <- 63L
phase3_shock_return_period <- 75L
phase3_shock_strengths <- c(domestic = -0.055, external = -0.50, supply = 0.05)
phase3_shock_parameter_name <- switch(
  phase3_shock_type,
  domestic = 'alpha10', external = 'x00', supply = 'mup', 'none'
)
phase3_shock_transform <- switch(
  phase3_shock_type,
  domestic = 'additive', external = 'proportional', supply = 'additive', 'none'
)
phase3_shock_strength <- if (phase3_shock_type %in% names(phase3_shock_strengths)) {
  unname(phase3_shock_strengths[[phase3_shock_type]])
} else {
  0
}

phase3_make_shock_weights <- function(n_periods) {
  out <- numeric(n_periods)
  rise_periods <- phase3_shock_start_period:phase3_shock_peak_period
  rise_u <- seq_along(rise_periods) / length(rise_periods)
  out[rise_periods] <- 3 * rise_u^2 - 2 * rise_u^3
  fall_periods <- (phase3_shock_peak_period + 1L):phase3_shock_return_period
  fall_u <- seq_along(fall_periods) / length(fall_periods)
  out[fall_periods] <- 1 - (3 * fall_u^2 - 2 * fall_u^3)
  out
}

# ---- STEP 2: Periods and simulation paths --------------------------------------
# Number of periods
nPeriods = if (is.null(phase3_spec$n_periods)) 100 else as.numeric(phase3_spec$n_periods)
phase3_shock_weights <- phase3_make_shock_weights(nPeriods)

# Number of simulation paths; parameter draws are stochastic from path 3 on
mc=10
if (phase3_demand_active) mc <- as.integer(ds$mc)

# ---- STEP 3: Parameters and exogenous variables ---------------------------------
betaw=0.03                                                      # Share of low-skilled workers (2021 share of minimum-living-allowance recipients; http://society.people.com.cn/n1/2022/0102/c1008-32322560.html)
rhol = 0.35                                                     # Ratio of low wage rate to high wage rate
kappa0=matrix(data=1,nrow=mc,ncol=nPeriods)                     # Autonomous capital-output ratio
kappa1=matrix(data=0.1,nrow=mc,ncol=nPeriods)                   # Sensitivity of capital-output ratio to Tobin q
kappa2=matrix(data=-0.1,nrow=mc,ncol=nPeriods)                  # Sensitivity of capital-output ratio to leverage ratio
lambda10 = 0.15                                                 # Parameter in portfolio equation of bills
lambda11 = 0.2                                                  # Parameter in portfolio equation of bills
lambda12 = 0                                                    # Parameter in portfolio equation of bills
lambda13 = -0.1                                                 # Parameter in portfolio equation of bills
lambda14 = 0                                                    # Parameter in portfolio equation of bills
lambda15 = -0.1                                                 # Parameter in portfolio equation of bills (rental return on housing)
lambda20 = 0.40                                                 # Parameter in portfolio equation of cheque deposits
lambda21 = -0.05                                                # Parameter in portfolio equation of cheque deposits
lambda22 = 0                                                    # Parameter in portfolio equation of cheque deposits
lambda23 = 0.2                                                  # Parameter in portfolio equation of cheque deposits
lambda24 = 0                                                    # Parameter in portfolio equation of cheque deposits
lambda25 = -0.15                                                # Parameter in portfolio equation of cheque deposits (rental return on housing)
lambda30 = 0.1                                                  # Parameter in portfolio equation of firms' securities
lambda40 = 0.3                                                  # Housing demand: autonomous share of wealth
lambda41 = 0.1                                                  # Housing demand: response to bills' return
lambda42 = 0                                                    # Housing demand: response to deposits' return
lambda43 = -0.2                                                 # Housing demand: response to disposable income
lambda44 = 0                                                    # Housing demand: response to return on shares
lambda45 =0.1                                                   # Housing demand: response to rental return
lambda31 = -(lambda11+lambda21+lambda41)                        # Parameter in portfolio equation of firms' securities
lambda32 = -(lambda12+lambda22+lambda42)                        # Parameter in portfolio equation of firms' securities
lambda33 = -(lambda13+lambda23+lambda43)                        # Parameter in portfolio equation of firms' securities
lambda34 = -(lambda14+lambda24+lambda44)                        # Parameter in portfolio equation of firms' securities
lambda35 = -(lambda15+lambda25+lambda45)                        # Parameter in portfolio equation of firms' securities
lambdacl = 0.18                                                 # Cash-to-consumption ratio, H1
lambdaclm = 0.18                                                # Cash-to-consumption ratio, H2
lambdacm = 0.18                                                 # Cash-to-consumption ratio, H3
lambdacum = 0.18                                                # Cash-to-consumption ratio, H4
lambdacu = 0.18                                                 # Cash-to-consumption ratio, H5
nu = 0.2                                                        # Speed of adjustment of labour supply to labour demand
gl = 0.03                                                       # Structural growth rate of labour force (UN DESA Population Division)
psi1 = 0.01                                                     # Coefficient of price expectations function
psi2 = 0.01                                                     # Coefficient of price expectations function
chi = 0.001                                                     # Target percentage of investment to be funded by share issues
gammag = 0.4                                                    # Speed of adjustment of JG size to market conditions
m0 = -2.1                                                       # Coefficient of import function
m1 = -0.53                                                      # Coefficient of import function: exchange-rate elasticity (Wu 2023, PhD thesis, Liaoning University, doi:10.27209/d.cnki.glniu.2023.002457)
m2 = 0.5                                                        # Coefficient of import function
m3 = 0                                                          # Coefficient of import function
x1 = 0.5                                                        # Coefficient of export function
x2 = 0.5                                                        # Coefficient of export function
x3 = 0                                                          # Coefficient of export function
gS = 0.03                                                       # Rate at which foreign income growth decays
prf=matrix(data=1,nrow=mc,ncol=nPeriods)                        # Labour productivity in private sector
r_star=matrix(data=0.0265,nrow=mc,ncol=nPeriods)                # Policy rate: 2.65%, China, June 2023 (CEIC)
omega1=matrix(data=0.01,nrow=mc,ncol=nPeriods)                  # PC coefficient: speed of adjustment of un to nun
nun=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Non-inflationary rate of unemployment
alpha0=matrix(data=1.6,nrow=mc,ncol=nPeriods)                   # Stochastic component of propensity to consume
alpha10=matrix(data=1.6,nrow=mc,ncol=nPeriods)                  # Autonomous component of propensity to consume
alpha11=matrix(data=20,nrow=mc,ncol=nPeriods)                   # Sensitivity of propensity to consume to interest rate
alpha12=matrix(data=0.05,nrow=mc,ncol=nPeriods)                 # Sensitivity of propensity to consume to unemployment rate
alpha1=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Propensity to consume out of income of workers
alpha7=matrix(data=0.15,nrow=mc,ncol=nPeriods)                  # Propensity to consume out of income of rentiers and managers
alpha2=matrix(data=0.10,nrow=mc,ncol=nPeriods)                  # Propensity to consume out of wealth: cash
alpha3=matrix(data=0.05,nrow=mc,ncol=nPeriods)                  # Propensity to consume out of wealth: cheque deposits
alpha4=matrix(data=0.025,nrow=mc,ncol=nPeriods)                 # Propensity to consume out of wealth: saving deposits
alpha5=matrix(data=0.01,nrow=mc,ncol=nPeriods)                  # Propensity to consume out of wealth: bills
alpha6=matrix(data=0.01,nrow=mc,ncol=nPeriods)                  # Propensity to consume out of wealth: shares and other firms' securities
delta=matrix(data=0.1,nrow=mc,ncol=nPeriods)                    # Depreciation rate
gamma=matrix(data=0.15,nrow=mc,ncol=nPeriods)                   # Reaction speed of adjustment of capital to its target value
kappa=matrix(data=1,nrow=mc,ncol=nPeriods)                      # Capital-Output ratio
theta = matrix(data=0.02,nrow=mc,ncol=nPeriods)                 # Profit retention rate
tau0 = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Autonomous component of tax revenue (shock)
tau1l = matrix(data=0.03,nrow=mc,ncol=nPeriods)                 # Tax rate on labour income, H1-H2 (China's wage-income brackets run from 3% to 45%)
tau1m = matrix(data=0.1,nrow=mc,ncol=nPeriods)                  # Tax rate on labour income, H3
tau1u = matrix(data=0.2,nrow=mc,ncol=nPeriods)                  # Tax rate on labour income, H4-H5
tau2 = matrix(data=0.2,nrow=mc,ncol=nPeriods)                   # Tax rate on capital income (https://www.fdichina.com/zh-CN/blog/capital-gains-tax-in-china/)
tau3 = matrix(data=1/200,nrow=mc,ncol=nPeriods)                 # Tax revenue rate on wealth
tau4 = matrix(data=5,nrow=mc,ncol=nPeriods)                     # Other transfers
tau5 = matrix(data=5,nrow=mc,ncol=nPeriods)                     # Unemployment benefits (relative to unemployment rate)
sigma0 = matrix(data=10,nrow=mc,ncol=nPeriods)                  # Autonomous component of government spending
sigma1 = matrix(data=0.15,nrow=mc,ncol=nPeriods)                # Dependent component of government spending
mub0 = matrix(data=0.01,nrow=mc,ncol=nPeriods)                  # Coefficient of bills' return rate
mub1 = matrix(data=0.015,nrow=mc,ncol=nPeriods)                 # Coefficient of bills' return rate
mul0 = matrix(data=0.02,nrow=mc,ncol=nPeriods)                  # Coefficient of loans' interest rate: constant mark-up
mul1 = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Coefficient of loans' interest rate: response to firm leverage (0 = off)
mum = matrix(data=0.01,nrow=mc,ncol=nPeriods)                   # Mark-up: saving deposits' return rate
mua = matrix(data=0.005,nrow=mc,ncol=nPeriods)                  # Mark-up: CB advances' return rate
muh = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Mark-up: reserves' return rate
rho3 = matrix(data=0.066,nrow=mc,ncol=nPeriods)                 # Reserve requirement ratio
mup = matrix(data=0.163,nrow=mc,ncol=nPeriods)                  # Mark-up over labour cost
beta1 = matrix(data=0.6,nrow=mc,ncol=nPeriods)                  # Share of notional bills held as bills by banks (rest held as excess reserves)
mulh = matrix(data=0.02,nrow=mc,ncol=nPeriods)                  # Mark-up: interest rate on mortgages
phi = matrix(data=0.01,nrow=mc,ncol=nPeriods)                   # Loans to disposable income ratio with no imitation
alpha_im = matrix(data=0.0001,nrow=mc,ncol=nPeriods)            # Imitative component of consumption
rep = matrix(data=0.01,nrow=mc,ncol=nPeriods)                   # Repayment rate on mortgages
rep1 = matrix(data=0.01,nrow=mc,ncol=nPeriods)                  # Repayment rate on mortgages
rept = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Repayment rate on tax
rhog = matrix(data=phase3_jg_wage_ratio,nrow=mc,ncol=nPeriods)  # Government wage rate to private sector wage rate
wbg_perc = matrix(data=phase3_policy_net_cost_share,nrow=mc,ncol=nPeriods)  # Common net fiscal cost target relative to pre-policy GDP
type_jg = matrix(data=0,nrow=mc,ncol=nPeriods)                  # Job guarantee trigger (for experiments)
tar_perc = matrix(data=phase3_project_revenue_share,nrow=mc,ncol=nPeriods)  # Project revenue to production cost ratio
exr = matrix(data=1,nrow=mc,ncol=nPeriods)                      # Exchange rate (quantity of domestic currency per one unit of foreign currency)
x0 = matrix(data=-2.1,nrow=mc,ncol=nPeriods)                    # Coefficient of export function
x00 = matrix(data=1,nrow=mc,ncol=nPeriods)                      # Stochastic component of export function
gF = matrix(data=0.053,nrow=mc,ncol=nPeriods)                   # Foreign income growth rate
stoc = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Stochasticity trigger
zeta_1 = matrix(data=0.044653,nrow=mc,ncol=nPeriods)            # Wage share, H1 (2021)
zeta_2= matrix(data=0.098844,nrow=mc,ncol=nPeriods)             # Wage share, H2 (2021)
zeta_3= matrix(data=0.155682,nrow=mc,ncol=nPeriods)             # Wage share, H3 (2021)
zeta_4= matrix(data=0.240862,nrow=mc,ncol=nPeriods)             # Wage share, H4 (2021); H5 receives the remainder
gamma_h = matrix(data=0.01,nrow=mc,ncol=nPeriods)               # Rent-to-housing-value ratio (Zezza 2008)
h1 = matrix(data=0.5,nrow=mc,ncol=nPeriods)                     # Response of H3 housing to growth in its mortgage debt (Szymborska 2022)
rhosl_perc= matrix(data=0.5,nrow=mc,ncol=nPeriods)              # Share of total rent paid by each of H1 and H2

# ---- STEP 4: Endogenous variables (rows = paths, columns = periods) -------------
af=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Amortization funds
c=matrix(data=0,nrow=mc,ncol=nPeriods)                          # Total demand for consumption
cl=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Consumption, H1
clm=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Consumption, H2
cm=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Consumption, H3
cum=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Consumption, H4
cu=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Consumption, H5
da=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Depreciation allowances
k=matrix(data=0,nrow=mc,ncol=nPeriods)                          # Stock of capital
kt=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Target stock of capital
lf=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Demand for bank loans
ls=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Supply of bank loans
id=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Investment
m1h=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Cheque deposits held by households
m1l=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Cheque deposits, H1
m1lm=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Cheque deposits, H2
m1m=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Cheque deposits, H3
m1um=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Cheque deposits, H4
m1u=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Cheque deposits, H5
m1s=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Supply of cheque deposits
m2h=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Saving deposits held by households
m2lm=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Saving deposits, H2
m2m=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Saving deposits, H3
m2um=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Saving deposits, H4
m2u=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Saving deposits, H5
m2s=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Supply of saving deposits
nd=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Labour demand
ns=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Labour supply
nn=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Workers who do not have a job in the private sector
wh=matrix(data=0.86,nrow=mc,ncol=nPeriods)                      # Wage rate of skilled workers
wl=matrix(data=rhol*wh,nrow=mc,ncol=nPeriods)                   # Minimum wage rate
w=matrix(data=(betaw*wl+(1-betaw)*wh),nrow=mc,ncol=nPeriods)    # Average wage rate
wb=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Private-sector wage bill
wbg = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Job guarantee wage bill
y=matrix(data=40,nrow=mc,ncol=nPeriods)                         # Output (total income)
ydl=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Disposable income, H1
ydlm=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Disposable income, H2
ydm=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Disposable income, H3
ydum=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Disposable income, H4
ydu=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Disposable income, H5
yd=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Total disposable income of households
ygl=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Income before housing costs, H1
yglm=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Income before housing costs, H2
ygm=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Income before housing costs, H3
ff=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Profits of firms
fuf=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Undistributed firms profits (retained profits)
fdf=matrix(data=0,nrow=mc,ncol=nPeriods)                        # Distributed firms profits (dividends)
tax = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Total tax revenue
taxl = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Taxes paid by H1
taxlm = matrix(data=0,nrow=mc,ncol=nPeriods)                    # Taxes paid by H2
taxm = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Taxes paid by H3
taxum = matrix(data=0,nrow=mc,ncol=nPeriods)                    # Taxes paid by H4
taxu = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Taxes paid by H5
gov = matrix(data=tax,nrow=mc,ncol=nPeriods)                    # Government spending
def = matrix(data=tax,nrow=mc,ncol=nPeriods)                    # Government deficit
fb = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Bank profits
bs = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Bills issued by the Treasury
vl = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Wealth (assets), H1
vlm = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Wealth (assets), H2
vm = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Wealth (assets), H3
vum = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Wealth, H4
vu = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Wealth, H5
nvl = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Net wealth, H1
nvlm = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Net wealth, H2
nvm = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Net wealth, H3
nvum = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Net wealth, H4
nvu = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Net wealth, H5
bh = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Household holdings of bills
bm = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Bills held by H3
bum = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Bills held by H4
bu = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Bills held by H5
bcb = matrix(data=0,nrow=mc,ncol=nPeriods)                      # CB holdings of bills
hs = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Supply of cash
as = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Supply of advances from CB (reserves if < 0)
ad = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Demand for advances (reserves if < 0)
hl = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Cash held by H1
hlm = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Cash held by H2
hm = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Cash held by H3
hum = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Cash held by H4
hu = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Cash held by H5
hh = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Total holdings of cash
mub = matrix(data=0.03,nrow=mc,ncol=nPeriods)                   # Mark-up: bills' return rate (short-term government bond rate, 2018)
mul = matrix(data=0.0345,nrow=mc,ncol=nPeriods)                 # Mark-up: loans' interest rate (one-year loan rate, June 2024)
rb = matrix(data=r_star+mub,nrow=mc,ncol=nPeriods)              # Return rate on bills
rl=matrix(data=r_star+mul,nrow=mc,ncol=nPeriods)                # Rate of interests on banks loans
rm=matrix(data=r_star+mum,nrow=mc,ncol=nPeriods)                # Rate of interests on saving deposits
ra=matrix(data=r_star+mua,nrow=mc,ncol=nPeriods)                # Rate of interests on CB advances
rh=matrix(data=r_star+muh,nrow=mc,ncol=nPeriods)                # Rate of interests on reserves
fcb = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Central bank profit
hbd = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Reserve requirement: demand
hbs = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Reserve requirement: supply
bb = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Bills held by commercial banks
bb_not = matrix(data=0,nrow=mc,ncol=nPeriods)                   # Notional amount of bills held by banks
p = matrix(data=1.03,nrow=mc,ncol=nPeriods)                     # General price level
pf = matrix(data=1.03,nrow=mc,ncol=nPeriods)                    # Unit price of private output
ep = matrix(data=1,nrow=mc,ncol=nPeriods)                       # Expected price level
un = matrix(data=0.051,nrow=mc,ncol=nPeriods)                   # Unemployment rate (initial value: CEIC, 2023)
eh = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Firms' equity, shares and securities held by household
ehh = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Number of firms' shares held by H5
ehum = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Number of firms' shares held by H4 (not used)
esr = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Number of shares issued by firms
re = matrix(data=r_star,nrow=mc,ncol=nPeriods)                  # Return rate on firms' shares
pe = matrix(data=1,nrow=mc,ncol=nPeriods)                       # Unit price of firms' shares
cg = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Capital gains on firms' shares
cgeu = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Capital gains on firms' shares, H5
pi = matrix(data=0.2,nrow=mc,ncol=nPeriods)                     # Inflation rate (initial value: CEIC, 2023)
epi = matrix(data=0.03,nrow=mc,ncol=nPeriods)                   # Expected inflation rate: regressive expectations (initial value: CEIC, 2021)
bpr = matrix(data=1,nrow=mc,ncol=nPeriods)                      # Share of bills purchased by private sector
hbd_star = matrix(data=0,nrow=mc,ncol=nPeriods)                 # Extra reserves demanded by banks
hbs_star = matrix(data=0,nrow=mc,ncol=nPeriods)                 # Extra reserves supplied by the CB
Omega_r = matrix(data=0.5,nrow=mc,ncol=nPeriods)                # Managers' share of wage and salaries
lh = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Mortgages to households
ll = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Mortgages, H1
llm = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Mortgages, H2
lm = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Mortgages, H3
rlh = matrix(data=r_star+mulh,nrow=mc,ncol=nPeriods)            # Interest rate on mortgages
tr = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Total transfers
s3_transfer_amount = matrix(data=0,nrow=mc,ncol=nPeriods)       # Transfer policy (S3/S4): additional transfer to H1
s3_high_income_tax = matrix(data=0,nrow=mc,ncol=nPeriods)       # Transfer policy (S3): earmarked tax paid only by H5
phase3_earmarked_tax = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_special_tax = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_common_net_fiscal_cost_target = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_policy_gross_spending = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_project_revenue = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_policy_net_fiscal_cost = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_policy_deficit_financing = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_shock_weight = matrix(data=0,nrow=mc,ncol=nPeriods)
phase3_shock_parameter_value = matrix(data=0,nrow=mc,ncol=nPeriods)
q = matrix(data=0.8,nrow=mc,ncol=nPeriods)                      # Valuation ratio (Tobin q)
lev = matrix(data=2.816,nrow=mc,ncol=nPeriods)                  # Leverage ratio of firms (initial value: People's Bank of China, 2022)
wg = matrix(data=rhog*w,nrow=mc,ncol=nPeriods)                  # Wage rate paid by the government
ng = matrix(data=0,nrow=mc,ncol=nPeriods)                       # People hired by the government (job guarantee)
ng0 = matrix(data=0.98,nrow=mc,ncol=nPeriods)                   # Coefficient defining share of unemployed hired by the government
def_ratio = matrix(data=0.00387,nrow=mc,ncol=nPeriods)          # Government deficit to GDP ratio (initial value: CEIC, 2023)
deb_ratio = matrix(data=0.2382,nrow=mc,ncol=nPeriods)           # Government debt to GDP ratio (initial value: CEIC, 2023)
tar = matrix(data=0,nrow=mc,ncol=nPeriods)                      # Revenue from selling JG project output
im = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Import
x = matrix(data=0,nrow=mc,ncol=nPeriods)                        # Export
fr = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Foreign reserves
tb = matrix(data=0,nrow=mc,ncol=nPeriods)                       # Trade balance
yF = matrix(data=40,nro=mc,ncol=nPeriods)                       # Foreign income
wh0 = 0.86                                                      # Initial value of skilled workers wage rate
wh1 = 0.5                                                       # Inertia coefficient of skilled workers wages (note: named gamma_w in main text)
incl = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Disposable-income share, H1
inclm = matrix(data=0,nrow=mc,ncol=nPeriods)                    # Disposable-income share, H2
incm = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Disposable-income share, H3
incum = matrix(data=0,nrow=mc,ncol=nPeriods)                    # Disposable-income share, H4
incu = matrix(data=0,nrow=mc,ncol=nPeriods)                     # Disposable-income share, H5
gini= matrix(data=0,nrow=mc,ncol=nPeriods)                      # Income Gini across H1-H5
atkinson=matrix(data=0,nrow=mc,ncol=nPeriods)                   # Atkinson index of income
var_coeff=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Squared coefficient of variation of income
rhos=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Rent paid by H1 and H2
hos=matrix(data=1,nrow=mc,ncol=nPeriods)                        # Total housing value
hosm=matrix(data=0.1,nrow=mc,ncol=nPeriods)                     # Housing value, H3
hosu=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Housing value, H5
hosum=matrix(data=0,nrow=mc,ncol=nPeriods)                      # Housing value, H4
hosum_real=matrix(data=0,nrow=mc,ncol=nPeriods)                 # Real housing value, H4
hosu_real=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Real housing value, H5
rhosum=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Rental return on housing, H4
rhosu=matrix(data=0,nrow=mc,ncol=nPeriods)                      # Rental return on housing, H5
hosd=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Housing demand
hoss=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Housing supply
da_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                      # Debt-to-asset ratio, H2
levy_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                    # Debt-to-income ratio, H2
dsy_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Income-to-debt-service ratio, H2
da_l=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Debt-to-asset ratio, H1
levy_l=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Debt-to-income ratio, H1
dsy_l=matrix(data=0,nrow=mc,ncol=nPeriods)                      # Income-to-debt-service ratio, H1
da_m=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Debt-to-asset ratio, H3
levy_m=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Debt-to-income ratio, H3
dsy_m=matrix(data=0,nrow=mc,ncol=nPeriods)                      # Income-to-debt-service ratio, H3
hosn=matrix(data=0,nrow=mc,ncol=nPeriods)                       # Residential investment (net new housing)
rhosum_perc=matrix(data=0,nrow=mc,ncol=nPeriods)                # H4 share of rental housing (H5 holds the rest)
gini_v=matrix(data=0,nrow=mc,ncol=nPeriods)                     # Gini of wealth across H1-H5
gini_nv=matrix(data=0,nrow=mc,ncol=nPeriods)                    # Gini of net wealth (negative values set to zero)
wb_ratio=matrix(data=0,nrow=mc,ncol=nPeriods)                   # Wage share of output, including JG wages
  ff_ratio=matrix(data=0,nrow=mc,ncol=nPeriods)                 # Profit share of output
gov_ratio=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Tax and central-bank revenue as a share of output
rhosr1_l=matrix(data=0,nrow=mc,ncol=nPeriods)                   # Rent burden, H1: rent / disposable income
rhosr2_l=matrix(data=0,nrow=mc,ncol=nPeriods)                   # Rent burden, H1: rent / income before housing costs
rhosr3_l=matrix(data=0,nrow=mc,ncol=nPeriods)                   # Rent burden, H1: rent / wealth
rhosr1_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Rent burden, H2: rent / disposable income
rhosr2_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Rent burden, H2: rent / income before housing costs
rhosr3_lm=matrix(data=0,nrow=mc,ncol=nPeriods)                  # Rent burden, H2: rent / wealth
vf=matrix(data=0,nrow=mc,ncol=nPeriods)                         # Net worth of firms

# ---- Demand-shock study diagnostics (inactive in the standard run) ---------------

if (phase3_demand_active) {
  demand_budget_rule <- "ceiling"
  phase3_policy_unused_budget <- matrix(0,nrow=mc,ncol=nPeriods)
  demand_h <- numeric(nPeriods)
  demand_h[ds$shock_start:ds$plateau_end] <- 1
  if (ds$path=="persistent") demand_h[ds$shock_start:nPeriods] <- 1 else {
    exit <- (ds$plateau_end+1L):ds$shock_end
    demand_h[exit] <- (1+cos(base::pi*(exit-ds$plateau_end)/(ds$shock_end-ds$plateau_end)))/2
    demand_h[ds$shock_end] <- 0
  }
  demand_c_multiplier <- 1-ds$s_c*demand_h
  demand_i_multiplier <- 1-ds$s_i*demand_h
  alpha1_raw <- alpha1; alpha7_raw <- alpha7; kt_raw <- kt
  demand_diagnostics <- vector("list",mc*(nPeriods-1L))
  demand_diag_count <- 0L
  demand_unknowns <- c("y","c","cl","clm","cm","cum","cu","id","k","kt","kappa","p","ep",
    "ydl","ydlm","ydm","ydum","ydu","alpha1","alpha7","w","wg","wb","nd","ns","ng","un",
    "wbg","tar","ff","fuf","lf","ll","llm","lm","m1l","m1lm","m1m","m1um","m1u",
    "m2lm","m2m","m2um","m2u","nvl","nvlm","nvm","nvum","nvu","bs","hs","hh","pe","fr","hosn")
  demand_snapshot <- function() setNames(vapply(demand_unknowns,function(n)get(n)[j,i],numeric(1)),demand_unknowns)
  demand_check_period <- function(previous) {
    now <- demand_snapshot()
    change <- abs(now-previous)/pmax(1,abs(now),abs(previous))
    eq <- numeric()
    check <- function(name,lhs,rhs) eq[[name]] <<- abs(lhs-rhs)/max(1,abs(lhs),abs(rhs))
    check("alpha1_effective",alpha1[j,i],(alpha10[j,i]-alpha11[j,i]*rlh[j,i-1]-alpha12[j,i]*un[j,i-1]+alpha0[j,i])*demand_c_multiplier[i])
    check("alpha7_effective",alpha7[j,i],alpha7_raw[j,i]*if(ds$exposure=="all") demand_c_multiplier[i] else 1)
    check("target_capital",kt[j,i],kappa[j,i]*y[j,i-1]/ep[j,i]*demand_i_multiplier[i])
    check("investment",id[j,i],gamma[j,i]*(kt[j,i]-k[j,i-1])*p[j,i]+da[j,i])
    check("capital",k[j,i],k[j,i-1]+(id[j,i]-da[j,i])/p[j,i])
    for (g in c("l","lm","m","um","u")) {
      lhs <- get(paste0("c",g))[j,i]
      income <- get(paste0("yd",g))[j,i-1]
      cash <- get(paste0("h",g))[j,i-1]; m1g <- get(paste0("m1",g))[j,i-1]
      wealth <- alpha2[j,i]*cash+alpha3[j,i]*m1g
      if(g!="l") wealth <- wealth+alpha4[j,i]*get(paste0("m2",g))[j,i-1]
      if(g %in% c("m","um","u")) wealth <- wealth+alpha5[j,i]*get(paste0("b",g))[j,i-1]
      if(g %in% c("m","um","u")) {
        # The original middle-group fallback omits its housing term.
        nextg <- c(l="lm",lm="m",m="um",um="u")
        active <- g=="u" || (get(paste0("c",g))[j,i-1]>0 && get(paste0("c",nextg[[g]]))[j,i-1]>0)
        if(g!="m" || active) wealth <- wealth+alpha6[j,i]*get(paste0("hos",g))[j,i-1]
      }
      if(g=="u") wealth <- wealth+alpha6[j,i]*eh[j,i-1]
      rhs <- ((if(g=="u") alpha7[j,i] else alpha1[j,i])*income+wealth)*p[j,i]/ep[j,i]
      if(g!="u") {
        nextg <- c(l="lm",lm="m",m="um",um="u")[[g]]
        a<-get(paste0("c",g))[j,i-1];b<-get(paste0("c",nextg))[j,i-1]
        if(a>0 && b>0) rhs<-rhs+alpha_im[j,i]*b/a*p[j,i]
      }
      check(paste0("consumption_",g),lhs,rhs)
    }
    check("labour_demand",nd[j,i],(y[j,i]-wbg[j,i])/prf[j,i])
    ns_rhs <- if(i<3) 200 else ns[j,i-1]*(1+gl)+nu*(nd[j,i-1]-ns[j,i-1])
    check("labour_supply",ns[j,i],max(ns_rhs,nd[j,i]))
    check("unemployment",un[j,i],1-(nd[j,i]+ng[j,i])/ns[j,i])
    check("GDP",y[j,i],c[j,i]+id[j,i]+gov[j,i]+tb[j,i]+hosn[j,i])
    check("government_budget",def[j,i],gov[j,i]+tr[j,i]+rb[j,i-1]*bs[j,i-1]-tax[j,i]-fcb[j,i]+wbg[j,i]-tar[j,i])
    check("government_debt",bs[j,i],bs[j,i-1]+def[j,i])
    for(g in c("l","lm","m","um","u")) {
      rhs<-get(paste0("nv",g))[j,i-1]+get(paste0("yd",g))[j,i]-get(paste0("c",g))[j,i]
      if(g=="m") rhs<-rhs-(hosm[j,i]-hosm[j,i-1])
      if(g=="u") rhs<-rhs+cgeu[j,i]
      check(paste0("household_budget_",g),get(paste0("nv",g))[j,i],rhs)
    }
    if(type_jg[j,i]>0) check("jg_wage_bill",wbg[j,i],wg[j,i]*ng[j,i])
    check("project_net_cost",phase3_policy_net_fiscal_cost[j,i],phase3_policy_gross_spending[j,i]-phase3_project_revenue[j,i])
    check("unused_budget",phase3_policy_unused_budget[j,i],phase3_common_net_fiscal_cost_target[j,i]-phase3_policy_net_fiscal_cost[j,i])
    check("budget_ceiling",phase3_policy_net_fiscal_cost[j,i],min(phase3_policy_net_fiscal_cost[j,i],phase3_common_net_fiscal_cost_target[j,i]))
    accounts <- c(
      cash=hh[j,i]-hs[j,i], m1=m1h[j,i]-m1s[j,i], m2=m2h[j,i]-m2s[j,i],
      reserves=hbd[j,i]-hbs[j,i],extra_reserves=hbd_star[j,i]-hbs_star[j,i],
      advances=ad[j,i]-as[j,i],bills=bb[j,i]+bh[j,i]+bcb[j,i]-bs[j,i],
      equity=eh[j,i]-esr[j,i]*pe[j,i],
      household_l=hl[j,i]+m1l[j,i]-ll[j,i]-nvl[j,i],
      household_lm=hlm[j,i]+m1lm[j,i]+m2lm[j,i]-llm[j,i]-nvlm[j,i],
      household_m=hm[j,i]+m1m[j,i]+m2m[j,i]-lm[j,i]+bm[j,i]-nvm[j,i],
      household_um=hum[j,i]+m1um[j,i]+m2um[j,i]+bum[j,i]+hosum[j,i]-vum[j,i],
      household_u=hu[j,i]+m1u[j,i]+m2u[j,i]+bu[j,i]+pe[j,i]*ehh[j,i]-vu[j,i]+hosu[j,i],
      firms=-lf[j,i]-esr[j,i]*pe[j,i]+k[j,i]-vf[j,i],
      banks=-m1s[j,i]-m2s[j,i]+ls[j,i]+bb[j,i]+hbd[j,i]+hbd_star[j,i]-ad[j,i],
      central_bank=-hs[j,i]-hbd[j,i]-hbd_star[j,i]+as[j,i]+bcb[j,i]+fr[j,i],
      net_accumulation=nvl[j,i]+nvlm[j,i]+nvm[j,i]+vum[j,i]+vu[j,i]-bs[j,i]-fr[j,i]+vf[j,i]-k[j,i]-hosu[j,i]-hosum[j,i],
      government_budget=def[j,i]-(gov[j,i]+tr[j,i]+rb[j,i-1]*bs[j,i-1]-tax[j,i]-fcb[j,i]+wbg[j,i]-tar[j,i]),
      government_debt=bs[j,i]-bs[j,i-1]-def[j,i],
      capital=k[j,i]-k[j,i-1]-(id[j,i]-da[j,i])/p[j,i],
      wage_bill=wbg[j,i]-wg[j,i]*ng[j,i])
    eq_bad<-if(any(!is.finite(eq))) which(!is.finite(eq))[1L] else which.max(eq)
    account_bad<-if(any(!is.finite(accounts))) which(!is.finite(accounts))[1L] else which.max(abs(accounts))
    change_bad<-if(any(!is.finite(change))) which(!is.finite(change))[1L] else which.max(change)
    reason <- ""
    if(any(!is.finite(now))) reason<-paste0("NONFINITE:",names(now)[which(!is.finite(now))[1L]])
    positive<-c(p=p[j,i],ep=ep[j,i],y=y[j,i],ns=ns[j,i])
    nonnegative<-c(cl=cl[j,i],clm=clm[j,i],cm=cm[j,i],cum=cum[j,i],cu=cu[j,i],id=id[j,i],k=k[j,i],nd=nd[j,i],ng=ng[j,i],alpha1=alpha1[j,i],alpha7=alpha7[j,i],lf=lf[j,i],ll=ll[j,i],llm=llm[j,i],lm=lm[j,i],m1l=m1l[j,i],m1lm=m1lm[j,i],m1m=m1m[j,i],m1um=m1um[j,i],m1u=m1u[j,i],m2lm=m2lm[j,i],m2m=m2m[j,i],m2um=m2um[j,i],m2u=m2u[j,i])
    if(reason=="" && any(positive<=0)) reason<-paste0("DOMAIN:",names(positive)[which(positive<=0)[1L]])
    if(reason=="" && any(nonnegative< -1e-8)) reason<-paste0("DOMAIN:",names(nonnegative)[which(nonnegative< -1e-8)[1L]])
    if(reason=="" && nd[j,i]+ng[j,i]>ns[j,i]+1e-8) reason<-"DOMAIN:employment_exceeds_supply"
    if(reason=="" && (!all(is.finite(accounts)) || max(abs(accounts))>1e-8)) reason<-paste0("ACCOUNTING:",names(accounts)[account_bad])
    if(reason=="" && (!all(is.finite(eq)) || max(eq)>1e-8)) reason<-paste0("EQUATION:",names(eq)[eq_bad])
    if(reason=="" && (!all(is.finite(change)) || max(change)>1e-8)) reason<-paste0("ITERATION:",names(change)[change_bad])
    demand_diag_count <<- demand_diag_count+1L
    demand_diagnostics[[demand_diag_count]] <<- data.frame(path=j,period=i,converged=reason=="",
      iterations=phase3_iteration_limit,max_scaled_variable_change=change[change_bad],
      max_scaled_equation_residual=eq[eq_bad],max_accounting_residual=abs(accounts[account_bad]),
      max_change_variable=names(change)[change_bad],max_residual_equation=names(eq)[eq_bad],
      max_accounting_equation=names(accounts)[account_bad],reason=reason,
      jg_wage_bill_residual=accounts[["wage_bill"]],jg_relative_wage=if(type_jg[j,i]>0 && w[j,i]>0) wg[j,i]/w[j,i] else NA_real_)
    if(reason!="") stop(structure(list(message=paste0("Demand study failed at path ",j,", period ",i,": ",reason),call=NULL,path=j,period=i,reason=reason),class=c("demand_study_failure","error","condition")))
  }
}

# ---- STEP 5: Solution: paths x periods x fixed sweeps per period ---------------

# Simulation paths: 1 = baseline, 2 = deterministic policy run, 3+ = stochastic draws
for (j in 1:mc){

  # Time loop
  for (i in 2:nPeriods){

    # Fixed number of Gauss-Seidel sweeps per period (no residual stopping rule)
    for (iterations in seq_len(phase3_iteration_limit)){

      # Policy design for this period

      # All S1-S6 policies use the same path-specific net fiscal cost target,
      # sized at 1.5% of GDP. Under "prepolicy_gdp" (default) the reference GDP is
      # fixed at the period immediately before policy starts; under "current_gdp"
      # it is contemporaneous GDP y[j,i], resolved by the within-period relaxation.
      # Transfer gross spending equals the target; JG gross spending is increased
      # below so that gross spending minus actual project revenue equals the target.
      if (i>=phase3_policy_start_period && j>=2 && phase3_program_type>0){
        phase3_common_net_fiscal_cost_target[j,i] =
          phase3_policy_net_cost_share * (
            if (phase3_policy_scale_mode == "current_gdp") y[j,i]
            else if (phase3_policy_scale_mode == "current_gdp_lag") y[j,i-1L]
            else y[j,phase3_budget_base_period])
        if (phase3_program_type==1){ type_jg[j,i]=1 }
        if (phase3_program_type==2){ type_jg[j,i]=2 }
        if (phase3_program_type==3){
          s3_transfer_amount[j,i]=phase3_common_net_fiscal_cost_target[j,i]
          if (phase3_financing_type==2){
            s3_high_income_tax[j,i]=s3_transfer_amount[j,i]
          }
        }
      }

      # Stochastic parameter draws from path 3 on
      if (j>=3){
        stoc[j,i]=1
      }

      # Model equations
      # Reset the seed: draws are path-specific and repeat across periods and sweeps
      set.seed(j)

      # Set stochastic components
      alpha0[j,i] = stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Propensity to consume out of income
      kappa0[j,i] = 1 + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Coefficient of capital to output ratio
      kappa1[j,i] = 0.1 + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Coefficient of capital to output ratio
      kappa2[j,i] = -0.1 + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Coefficient of capital to output ratio
      omega1[j,i] = 0.01 + stoc[j,i]*runif(1,min=-0.005,max=0.005)  # Wage function
      x00[j,i] = 1 + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Total export
      Omega_r[j,i] = 0.5 + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Share of managerial labour income (Omega_r)
      ng0[j,i] = runif(1,min = 0.96, max = 1)                   # Share of unemployed hired through JG
      tar_perc[j,i] = phase3_project_revenue_share + stoc[j,i]*(runif(1,min = -0.05, max = 0.05))  # Project revenue share
      rhog[j,i] = phase3_jg_wage_ratio + stoc[j,i]*(runif(1,min = -0.10, max = 0.10))  # Government wage rate to private sector wage rate
      # Temporary exogenous shock from the adjustment block above.
      if (phase3_demand_active) {
        alpha7[j,i] <- alpha7_raw[j,i] * if(ds$exposure=="all") demand_c_multiplier[i] else 1
      }
      phase3_shock_weight[j,i] = phase3_shock_weights[[i]]
      if (phase3_shock_parameter_name == 'alpha10') {
        alpha10[j,i] = 1.6 + phase3_shock_strength*phase3_shock_weight[j,i]
        phase3_shock_parameter_value[j,i] = alpha10[j,i]
      } else if (phase3_shock_parameter_name == 'x00') {
        x00[j,i] = x00[j,i]*(1 + phase3_shock_strength*phase3_shock_weight[j,i])
        phase3_shock_parameter_value[j,i] = x00[j,i]
      } else if (phase3_shock_parameter_name == 'mup') {
        mup[j,i] = 0.163 + phase3_shock_strength*phase3_shock_weight[j,i]
        phase3_shock_parameter_value[j,i] = mup[j,i]
      } else if (phase3_shock_parameter_name == 'prf') {
        prf[j,i] = 1*(1 + phase3_shock_strength*phase3_shock_weight[j,i])
        phase3_shock_parameter_value[j,i] = prf[j,i]
      } else {
        phase3_shock_parameter_value[j,i] = 0
      }

      # 1. Non-financial firms
      y[j,i] = c[j,i] + id[j,i] + gov[j,i] + tb[j,i]+hosn[j,i]  # National income identity (GDP)
      kt[j,i] = kappa[j,i]*y[j,i-1]/ep[j,i]                     # Target capital stock
      if (phase3_demand_active) {
        kt_raw[j,i] <- kt[j,i]
        kt[j,i] <- kt_raw[j,i] * demand_i_multiplier[i]
      }
      da[j,i] = delta[j,i]*k[j,i-1]*p[j,i]                      # Depreciation allowances
      af[j,i] = da[j,i]                                         # Amortization funds
      id[j,i] = gamma[j,i]*(kt[j,i] - k[j,i-1])*p[j,i] + da[j,i]  # Demand for investment: conventional
      kappa[j,i] = kappa0[j,i]+kappa1[j,i]*q[j,i-1]+kappa2[j,i]*lev[j,i-1]  # Capital-output ratio
      k[j,i] = k[j,i-1] + (id[j,i] - da[j,i])/p[j,i]            # Accumulation of capital - Investment behaviour
      ff[j,i] = y[j,i] - rl[j,i-1]*lf[j,i-1] - af[j,i] - wb[j,i]  # Firms profits (residual)
      fdf[j,i] = (1-theta[j,i])*ff[j,i-1]                       # Distributed profits
      fuf[j,i] = ff[j,i] - fdf[j,i]                             # Retained profits
      lf[j,i] = lf[j,i-1] + (id[j,i] - af[j,i] - fuf[j,i] - (esr[j,i]-esr[j,i-1])*pe[j,i])  # Demand for bank loans
      vf[j,i] = -lf[j,i]-esr[j,i]*pe[j,i]+k[j,i]                # Net worth of firms

      if (i<5) {esr[j,i] = 10}
      else {esr[j,i] = esr[j,i-1] + chi*id[j,i]/pe[j,i]}        # Number of shares (equity, shares and bonds) issued by the firms

      # 2. Households: disposable income
      ydl[j,i] = wb[j,i]*zeta_1[j,i] - (rlh[j,i-1])*ll[j,i-1] + tr[j,i] - taxl[j,i] + wbg[j,i] - tar[j,i] -0.5*rhos[j,i]  # Disposable income, H1
      ydlm[j,i]= wb[j,i]*zeta_2[j,i] - (rlh[j,i-1])*llm[j,i-1] + rm[j,i-1]*m2lm[j,i-1] - taxlm[j,i]-0.5*rhos[j,i]  # Disposable income, H2
      ydm[j,i]= wb[j,i]*zeta_3[j,i] -  (rlh[j,i-1])*lm[j,i-1] + rm[j,i-1]*m2m[j,i-1] + rb[j,i-1]*bm[j,i-1] - taxm[j,i]  # Disposable income, H3
      ydum[j,i] = wb[j,i]*zeta_4[j,i] + rm[j,i-1]*m2um[j,i-1] + rb[j,i-1]*bum[j,i-1] + - taxum[j,i]+rhosum_perc[j,i]*rhos[j,i]  # Disposable income, H4
      ydu[j,i] = wb[j,i]*(1-zeta_1[j,i]-zeta_2[j,i]-zeta_3[j,i]-zeta_4[j,i]) + rm[j,i-1]*m2u[j,i-1] + rb[j,i-1]*bu[j,i-1] + fdf[j,i] + fb[j,i] - taxu[j,i]+(1-rhosum_perc[j,i])*rhos[j,i]  # Disposable income, H5
      yd[j,i] = ydl[j,i] + ydlm[j,i] + ydm[j,i] + ydum[j,i] + ydu[j,i]  # Total disposable income

      ygl[j,i] = ydl[j,i] + rlh[j,i-1]*ll[j,i-1] + 0.5*rhos[j,i]  # Income before housing costs, H1
      yglm[j,i] = ydlm[j,i] + rlh[j,i-1]*llm[j,i-1] + 0.5*rhos[j,i]  # Income before housing costs, H2
      ygm[j,i] = ydm[j,i] + rlh[j,i-1]*lm[j,i-1]                # Income before housing costs, H3
      # 2b. Consumption (H1-H4 also imitate the group above them)
      if (cl[j,i-1]>0 && clm[j,i-1]>0){cl[j,i] = (alpha1[j,i]*ydl[j,i-1]+alpha2[j,i]*hl[j,i-1]+alpha3[j,i]*m1l[j,i-1])*p[j,i]/ep[j,i]+alpha_im[j,i]*(clm[j,i-1]/cl[j,i-1])*p[j,i] }
      else{cl[j,i] = (alpha1[j,i]*ydl[j,i-1]+alpha2[j,i]*hl[j,i-1]+alpha3[j,i]*m1l[j,i-1])*p[j,i]/ep[j,i]}  # Consumption, H1

      if (clm[j,i-1]>0 && cm[j,i-1]>0){clm[j,i] = (alpha1[j,i]*ydlm[j,i-1]+alpha2[j,i]*hlm[j,i-1]+alpha3[j,i]*m1lm[j,i-1]+alpha4[j,i]*m2lm[j,i-1])*p[j,i]/ep[j,i]+alpha_im[j,i]*(cm[j,i-1]/clm[j,i-1])*p[j,i] }
      else{clm[j,i] = (alpha1[j,i]*ydlm[j,i-1]+alpha2[j,i]*hlm[j,i-1]+alpha3[j,i]*m1lm[j,i-1]+alpha4[j,i]*m2lm[j,i-1])*p[j,i]/ep[j,i]}  # Consumption, H2

      if (cm[j,i-1]>0 && cum[j,i-1]>0){cm[j,i] = (alpha1[j,i]*ydm[j,i-1]+alpha2[j,i]*hm[j,i-1]+alpha3[j,i]*m1m[j,i-1]+alpha4[j,i]*m2m[j,i-1]+alpha5[j,i]*bm[j,i-1]+alpha6[j,i]*hosm[j,i-1])*p[j,i]/ep[j,i]+alpha_im[j,i]*(cum[j,i-1]/cm[j,i-1])*p[j,i] }
      else{cm[j,i] = (alpha1[j,i]*ydm[j,i-1]+alpha2[j,i]*hm[j,i-1]+alpha3[j,i]*m1m[j,i-1]+alpha4[j,i]*m2m[j,i-1]+alpha5[j,i]*bm[j,i-1])*p[j,i]/ep[j,i]}  # Consumption, H3

      if (cum[j,i-1]>0 && cu[j,i-1]>0){cum[j,i] = (alpha1[j,i]*ydum[j,i-1]+alpha2[j,i]*hum[j,i-1]+alpha3[j,i]*m1um[j,i-1]+alpha4[j,i]*m2um[j,i-1]+alpha5[j,i]*bum[j,i-1]+alpha6[j,i]*hosum[j,i-1])*p[j,i]/ep[j,i]+alpha_im[j,i]*(cu[j,i-1]/cum[j,i-1])*p[j,i] }
      else{cum[j,i] = (alpha1[j,i]*ydum[j,i-1]+alpha2[j,i]*hum[j,i-1]+alpha3[j,i]*m1um[j,i-1]+alpha4[j,i]*m2um[j,i-1]+alpha5[j,i]*bum[j,i-1]+alpha6[j,i]*hosum[j,i-1])*p[j,i]/ep[j,i]}  # Consumption, H4

      cu[j,i] = (alpha7[j,i]*ydu[j,i-1]+alpha2[j,i]*hu[j,i-1]+alpha3[j,i]*m1u[j,i-1]+alpha4[j,i]*m2u[j,i-1]+alpha5[j,i]*bu[j,i-1]+alpha6[j,i]*eh[j,i-1]+alpha6[j,i]*hosu[j,i-1])*p[j,i]/ep[j,i]  # Consumption, H5

      c[j,i] = cl[j,i] + clm[j,i] + cm[j,i] + cum[j,i] +cu[j,i]  # Total consumption

      alpha1[j,i] = alpha10[j,i] - alpha11[j,i]*rlh[j,i-1] - alpha12[j,i]*un[j,i-1] + alpha0[j,i]  # Endogenous propensity to consume
      if (phase3_demand_active) {
        alpha1_raw[j,i] <- alpha1[j,i]
        alpha1[j,i] <- alpha1_raw[j,i] * demand_c_multiplier[i]
      }
      # 2c. Net wealth
      nvl[j,i] = nvl[j,i-1] + ydl[j,i] - cl[j,i]                # Net wealth, H1
      nvlm[j,i] = nvlm[j,i-1] + ydlm[j,i] - clm[j,i]            # Net wealth, H2
      nvm[j,i] = nvm[j,i-1] + ydm[j,i] - cm[j,i]-(hosm[j,i]-hosm[j,i-1])  # Net wealth, H3 (net of housing purchases)
      nvum[j,i] = nvum[j,i-1] + ydum[j,i] - cum[j,i]            # Net wealth, H4
      nvu[j,i] = nvu[j,i-1] + ydu[j,i] - cu[j,i]  + cgeu[j,i]   # Net wealth, H5 (including capital gains)

      # 2d. Wealth (H1-H3: net wealth plus mortgages)
      vl[j,i] = nvl[j,i] +ll[j,i]                               # Wealth, H1
      vlm[j,i] = nvlm[j,i] +llm[j,i]                            # Wealth, H2
      vm[j,i] = nvm[j,i] +lm[j,i]                               # Wealth, H3
      vum[j,i] = vum[j,i-1] + ydum[j,i] - cum[j,i]              # Wealth, H4
      vu[j,i] = vu[j,i-1] + ydu[j,i] - cu[j,i] + cgeu[j,i]      # Wealth, H5 (including capital gains)
      cgeu[j,i] = ehh[j,i-1]*(pe[j,i] - pe[j,i-1])              # Capital gains on shares, H5

      # 2e. Mortgages (H1-H3)
      ll[j,i]=(1-rep1[j,i])*ll[j,i-1] + max(phi[j,i]*cl[j,i],cl[j,i]-ydl[j,i])  # Mortgages, H1
      llm[j,i]=(1-rep[j,i])*llm[j,i-1] + max(phi[j,i]*clm[j,i],clm[j,i]-ydlm[j,i])  # Mortgages, H2
      lm[j,i]=(1-rep[j,i])*lm[j,i-1] + max(phi[j,i]*cm[j,i],cm[j,i]-ydm[j,i])  # Mortgages, H3

      # 3. Commercial banks
      lh[j,i] = ll[j,i] + llm[j,i] + lm[j,i]                    # Total household mortgages
      ls[j,i] = lf[j,i] + lh[j,i]                               # Supply of bank credit (loans and mortgages)
      m1s[j,i] =  m1h[j,i]                                      # Supply of cheque deposits
      m2s[j,i] =  m2h[j,i]                                      # Supply of saving deposits

      fb[j,i] = rl[j,i-1]*lf[j,i-1] + rlh[j,i-1]*lh[j,i-1] + rb[j,i-1]*bb[j,i-1] - rm[j,i-1]*m2s[j,i-1] - ra[j,i-1]*ad[j,i-1] + rh[j,i-1]*(hbd[j,i-1]+hbd_star[j,i-1])  # Bank profits
      bb_not[j,i] = m1s[j,i] + m2s[j,i] - ls[j,i] - hbd[j,i]    # Notional amount of bills held by banks

      if (bb_not[j,i]>0){bb[j,i] = bb_not[j,i]*beta1[j,i]} else {bb[j,i] = 0}  # Bills actually held by commercial banks
      if (bb_not[j,i]>0){hbd_star[j,i] = bb_not[j,i]*(1-beta1[j,i])} else {hbd_star[j,i] = 0}  # Extra reserves held by commercial banks
      if (bb_not[j,i]>0){ad[j,i] = 0} else {ad[j,i] = -bb_not[j,i]}  # Demand for advances

      # 4. Government
      taxl[j,i] = (1-rept[j,i])*(tau1l[j,i]*wb[j,i]*zeta_1[j,i] +tau3[j,i]*m1l[j,i-1] )  # Taxes, H1
      taxlm[j,i] = (1-rept[j,i])*(tau1l[j,i]*wb[j,i]*zeta_2[j,i] + tau3[j,i]*m1lm[j,i-1] + tau2[j,i]*rm[j,i-1]*m2lm[j,i])  # Taxes, H2
      taxm[j,i] = (1-rept[j,i])*(tau1m[j,i]*wb[j,i]*zeta_3[j,i] + tau3[j,i]*m1m[j,i-1] + tau2[j,i]*(rm[j,i-1]*m2m[j,i] + rb[j,i-1]*bm[j,i]) )  # Taxes, H3
      taxum[j,i] = (1-rept[j,i])*(tau0[j,i] + tau1u[j,i]*wb[j,i]*zeta_4[j,i] + tau2[j,i]*(rm[j,i-1]*m2um[j,i-1]+rb[j,i-1]*bum[j,i-1]+rhosum_perc[j,i-1]*rhos[j,i-1]) + tau3[j,i]*(vum[j,i-1]-hum[j,i-1]) )  # Taxes, H4
      # Tax-financed employment programmes cover their current net fiscal cost.
      if (phase3_program_type %in% c(1,2) && phase3_financing_type==2 &&
          i>=phase3_policy_start_period && j>=2) {
        phase3_earmarked_tax[j,i] = max(wbg[j,i]-tar[j,i],0)
      } else {
        phase3_earmarked_tax[j,i] = 0
      }
      taxu[j,i] = (1-rept[j,i])*(tau0[j,i] + tau1u[j,i]*wb[j,i]*(1-zeta_1[j,i]-zeta_2[j,i]-zeta_3[j,i]-zeta_4[j,i]) + tau2[j,i]*(rm[j,i-1]*m2u[j,i-1]+rb[j,i-1]*bu[j,i-1]+fdf[j,i]+fb[j,i]+(1-rhosum_perc[j,i-1])*rhos[j,i-1]) + tau3[j,i]*(vu[j,i-1]-hu[j,i-1])) + s3_high_income_tax[j,i] + phase3_earmarked_tax[j,i]  # Taxes, H5, including earmarked policy taxes
      tax[j,i] = taxl[j,i] + taxlm[j,i]+ taxm[j,i]+ taxum[j,i]+ taxu[j,i]  # Total tax revenue
      tr[j,i] = tau4[j,i]+tau5[j,i]*un[j,i-1]+s3_transfer_amount[j,i]  # Transfers, including the transfer-policy payment to H1
      gov[j,i] = sigma0[j,i] + sigma1[j,i]*y[j,i-1]             # Government spending: purchase of goods and services
      def[j,i] = gov[j,i] + tr[j,i] + rb[j,i-1]*bs[j,i-1] - tax[j,i] - fcb[j,i]+wbg[j,i]-tar[j,i]  # Government deficit
      bs[j,i] = bs[j,i-1] + def[j,i]                            # Supply of bills

      # A) Capped public-employment project with an adjustable JG wage (S1)
      if (type_jg[j,i]==1){
        if (ng[j,i]==0){wg[j,i]=0}
        else {wg[j,i] = wbg[j,i]/ng[j,i]}                       # Wage rate paid under the JG programme
        nn[j,i] = ns[j,i] - nd[j,i]                             # Workers who do not have a job in the private sector
        ng[j,i] = min(ng[j,i-1]+gammag*(nn[j,i]-ng[j,i-1]),nn[j,i])  # Workers hired under JG programme
        wbg[j,i] = phase3_common_net_fiscal_cost_target[j,i]/(1-tar_perc[j,i])  # Gross spending consistent with the common net-cost target
        if (phase3_demand_active && ng[j,i]<=0) {
          wg[j,i] = 0
          wbg[j,i] = 0
        }
        tar[j,i] = tar_perc[j,i]*wbg[j,i]                       # Charges

      }
      else {
        wg[j,i] = 0
        ng[j,i] = 0
        nn[j,i] = ns[j,i] - nd[j,i]
        wbg[j,i] = 0
      }

      # B) Public job guarantee with a fixed relative wage and endogenous employment (S2)
      if (type_jg[j,i]==2){
        wg[j,i] = rhog[j,i]*w[j,i-1]                            # Wage rate paid under JG
        wbg[j,i] = phase3_common_net_fiscal_cost_target[j,i]/(1-tar_perc[j,i])  # Gross spending consistent with the common net-cost target
        nn[j,i] = ns[j,i] - nd[j,i]                             # Workers who do not have a job in the private sector
        ng[j,i] = min(wbg[j,i]/wg[j,i],nn[j,i])                 # Workers hired under JG programme
        if (phase3_demand_active) wbg[j,i] = wg[j,i]*ng[j,i]    # Study budget is a ceiling; pay only actual employment
        tar[j,i] = tar_perc[j,i]*wbg[j,i]                       # Charges

      }

      # Transparent programme gross-spending, revenue, and net-cost accounts.
      phase3_special_tax[j,i] = s3_high_income_tax[j,i] + phase3_earmarked_tax[j,i]
      if (phase3_program_type %in% c(1,2)) {
        phase3_policy_gross_spending[j,i] = wbg[j,i]
        phase3_project_revenue[j,i] = tar[j,i]
      } else if (phase3_program_type==3) {
        phase3_policy_gross_spending[j,i] = s3_transfer_amount[j,i]
        phase3_project_revenue[j,i] = 0
      }
      phase3_policy_net_fiscal_cost[j,i] =
        phase3_policy_gross_spending[j,i] - phase3_project_revenue[j,i]
      phase3_policy_deficit_financing[j,i] =
        phase3_policy_net_fiscal_cost[j,i] - phase3_special_tax[j,i]
      if (phase3_demand_active) phase3_policy_unused_budget[j,i] =
        phase3_common_net_fiscal_cost_target[j,i] - phase3_policy_net_fiscal_cost[j,i]

      # 5. Household portfolios and housing
      ehh[j,i] = esr[j,i]                                       # Number of shares held by households
      eh[j,i] = ehh[j,i]*pe[j,i]                                # Nominal amount of shares demanded by households

      bm[j,i] = lambda10*vm[j,i-1] + lambda11*vm[j,i-1]*rb[j,i-1] + lambda12*vm[j,i-1]*rm[j,i-1] + lambda13*ydm[j,i-1]  # Demand for bills, H3
      bum[j,i] = lambda10*vum[j,i-1] + lambda11*vum[j,i-1]*rb[j,i-1] + lambda12*vum[j,i-1]*rm[j,i-1] + lambda13*ydum[j,i-1]+lambda15*rhosum[j,i-1]*vum[j,i-1]  # Demand for bills, H4
      bu[j,i] = lambda10*vu[j,i-1] + lambda11*vu[j,i-1]*rb[j,i-1] + lambda12*vu[j,i-1]*rm[j,i-1] + lambda13*ydu[j,i-1] + lambda14*vu[j,i-1]*re[j,i-1] +lambda15*rhosu[j,i-1]*vu[j,i-1]  # Demand for bills, H5
      bh[j,i] = bm[j,i] + bum[j,i]+ bu[j,i]                     # Total household bills
      pe[j,i] = (lambda30*(vu[j,i-1]) + lambda31*(vu[j,i-1])*rb[j,i-1] + lambda32*(vu[j,i-1])*rm[j,i-1] + lambda33*(ydu[j,i-1]) + lambda34*(vu[j,i-1])*re[j,i-1]+lambda35*rhosu[j,i-1]*(vu[j,i-1]))/ehh[j,i]  # Share price (clears the equity market)

      hl[j,i] = lambdacl*cl[j,i]*ep[j,i]/p[j,i]                 # Cash held by H1
      hlm[j,i] = lambdaclm*clm[j,i]*ep[j,i]/p[j,i]              # Cash held by H2
      hm[j,i] = lambdacm*cm[j,i]*ep[j,i]/p[j,i]                 # Cash held by H3
      hum[j,i] = lambdacum*cum[j,i]*ep[j,i]/p[j,i]              # Cash held by H4
      hu[j,i] = lambdacu*cu[j,i]*ep[j,i]/p[j,i]                 # Cash held by H5
      hh[j,i] = hl[j,i] + hlm[j,i]+ hm[j,i]+ hum[j,i]+ hu[j,i]  # Demanded stock of banknotes (cash)

      m1l[j,i] = vl[j,i] - hl[j,i]                              # Cheque deposits, H1 (residual asset)
      m1lm[j,i] =  lambda20*vlm[j,i-1] + lambda22*vlm[j,i-1]*rm[j,i-1] + lambda23*ydlm[j,i-1]  # Cheque deposits, H2
      m1m[j,i] = lambda20*vm[j,i-1] + lambda21*vm[j,i-1]*rb[j,i-1] + lambda22*vm[j,i-1]*rm[j,i-1] + lambda23*ydm[j,i-1]  # Cheque deposits, H3
      m1um[j,i] = lambda20*vum[j,i-1] + lambda21*vum[j,i-1]*rb[j,i-1] + lambda22*vum[j,i-1]*rm[j,i-1] + lambda23*ydum[j,i-1]+lambda25*rhosum[j,i-1]*vum[j,i-1]  # Cheque deposits, H4
      m1u[j,i] = lambda20*vu[j,i-1] + lambda21*vu[j,i-1]*rb[j,i-1] + lambda22*vu[j,i-1]*rm[j,i-1] + lambda23*ydu[j,i-1] + lambda24*vu[j,i-1]*re[j,i-1] +lambda25*vu[j,i-1]*rhosu[j,i-1]  # Cheque deposits, H5
      m1h[j,i] = m1l[j,i] + m1lm[j,i] + m1m[j,i]+ m1um[j,i]+ m1u[j,i]  # Total demand for cheque deposits

      m2lm[j,i] = vlm[j,i] - hlm[j,i] - m1lm[j,i]               # Saving deposits, H2 (residual asset)
      m2m[j,i] = vm[j,i] - hm[j,i] - m1m[j,i] - bm[j,i]         # Saving deposits, H3 (residual asset)
      m2um[j,i] = vum[j,i] - hum[j,i] - m1um[j,i] - bum[j,i]-hosum[j,i]  # Saving deposits, H4 (residual asset)
      m2u[j,i] = vu[j,i] - hu[j,i] - m1u[j,i] - bu[j,i] - eh[j,i] -hosu[j,i]  # Saving deposits, H5 (residual asset)
      m2h[j,i] = m2lm[j,i] + m2m[j,i] + m2um[j,i]+ m2u[j,i]     # Total saving deposits

      if(i<5){hosm[j,i]=hosm[j,i]}
      else{hosm[j,i] =hosm[j,i-1]+hosm[j,i-1]*h1[j,i]*((lm[j,i]-lm[j,i-1])/lm[j,i-1])}  # Housing, H3 (grows with its mortgage debt)
      hosu[j,i]= lambda40*vu[j,i-1]+lambda41*vu[j,i-1]*rb[j,i-1]+ lambda42*vu[j,i-1]*rm[j,i-1] + lambda43*ydu[j,i-1] + lambda44*vu[j,i-1]*re[j,i-1] +lambda45*vu[j,i-1]*rhosu[j,i-1]  # Housing demand, H5
      hosum[j,i]= lambda40*vum[j,i-1]+lambda41*vum[j,i-1]*rb[j,i-1]+ lambda42*vum[j,i-1]*rm[j,i-1] + lambda43*ydum[j,i-1] + lambda44*vum[j,i-1]*re[j,i-1] +lambda45*vum[j,i-1]*rhosum[j,i-1]  # Housing demand, H4
      hosn[j,i]=(hosu[j,i]-hosu[j,i-1])+(hosum[j,i]-hosum[j,i-1])+(hosm[j,i]-hosm[j,i-1])  # Residential investment
      hos[j,i]=hos[j,i-1]+hosn[j,i]                             # Housing stock
      rhos[j,i]=gamma_h[j,i-1]*(hosu_real[j,i-1]+hosum_real[j,i-1])  # Rent paid by H1 and H2
      if(i>2){rhosum_perc[j,i]= hosum[j,i]/(hosu[j,i]+hosum[j,i])}  # H4 share of rental housing
      else{rhosum_perc[j,i]=rhosum_perc[j,i]}
      hosu_real[j,i]=hosu[j,i]
      hosum_real[j,i]=hosum[j,i]
      if(i<5){rhosu[j,i]=r_star[j,i]
        rhosum[j,i]=r_star[j,i]
      }
      else{
        rhosu[j,i]=((1-rhosum_perc[j,i])*rhos[j,i])/hosu[j,i]   # Rental return, H5
        rhosum[j,i]=(rhosum_perc[j,i]*rhos[j,i])/hosum[j,i]}    # Rental return, H4

      # 6. Central bank
      bcb[j,i] = bs[j,i] - bh[j,i] - bb[j,i]                    # CB holdings of bills
      hs[j,i] = bcb[j,i] + as[j,i] - (hbs[j,i] + hbs_star[j,i]) + fr[j,i]  # Supply of cash
      as[j,i] = ad[j,i]                                         # Supply of advances
      fcb[j,i] = rb[j,i-1]*bcb[j,i-1] + ra[j,i-1]*as[j,i-1] - rh[j,i-1]*(hbs[j,i-1]+hbs_star[j,i-1])  # Central bank profit (seigniorage)
      hbd[j,i] = rho3[j,i]*(m1s[j,i-1] +m2s[j,i-1])             # Required reserves: demand
      hbs[j,i] = hbd[j,i]                                       # Reserve requirement: supply
      hbs_star[j,i] = hbd_star[j,i]                             # Extra reserves: supply

      # 7. Rest of the world
      im[j,i] = exp(m0 + m1*log(exr[j,i-1]) + m2*log(y[j,i-1]) + m3*log(p[j,i-1]))  # Import
      x[j,i] = (exp(x0[j,i] + x1*log(exr[j,i-1]) + x2*log(yF[j,i-1]) + x3*log(p[j,i-1])))*x00[j,i]  # Export
      yF[j,i] = yF[j,i-1]*(1+gF[j,i])                           # Foreign income
      gF[j,i] = gF[j,i-1]*(1-gS)                                # Foreign income growth rate
      tb[j,i] = x[j,i] - im[j,i]                                # Trade balance
      fr[j,i] = fr[j,i-1] + tb[j,i]                             # Foreign reserves
      # 8. Interest rates
      if (i<5) {re[j,i] = r_star[j,i]}
      else {re[j,i] = (fdf[j,i]+cg[j,i])/eh[j,i-1]}             # Return rate on firms' shares
      rb[j,i] = r_star[j,i] + mub[j,i]                          # Return rate on bills
      rl[j,i] = r_star[j,i] + mul[j,i]                          # Interest rate on loans
      rlh[j,i] = r_star[j,i] + mulh[j,i]                        # Interest rate on mortgages
      rm[j,i] = r_star[j,i] + mum[j,i]                          # Return rate on saving deposits
      ra[j,i] = r_star[j,i] + mua[j,i]                          # Interest rate on CB advances
      rh[j,i] = r_star[j,i] + muh[j,i]                          # Return rate on reserves
      mul[j,i] = mul0[j,i] + mul1[j,i]*lev[j,i-1]               # Mark-up of interest rate on loans: endogenous (Minsky)

      mub[j,i] = mub0[j,i] - mub1[j,i]*(bpr[j,i-1])             # Mark-up of bills yield
      bpr[j,i] = (bh[j,i] + bb[j,i])/bs[j,i]                    # Share of bills demanded by private sector

      # 9. Labour market
      wb[j,i] = w[j,i]*nd[j,i]                                  # Wage bill
      nd[j,i] = (y[j,i]-wbg[j,i])/(prf[j,i])                    # Private employment (labour demand)
      if (i<3) {ns[j,i] = 200}
      else{ ns[j,i] = ns[j,i-1]*(1+gl) + nu*(nd[j,i-1]-ns[j,i-1])}  # Labour force (labour supply)
      if (ns[j,i]<nd[j,i]) {ns[j,i]=nd[j,i]}                    # Labour supply is at least labour demand (un >= 0)
      un[j,i] = 1-((nd[j,i]+ng[j,i])/ns[j,i])
      if (i<3) {wh[j,i] = wh0}
      else{wh[j,i] = wh1*wh0 + (1-wh1)*((1 - omega1[j,i]*(un[j,i-1] - nun[j,i]))*ep[j,i]*wh[j,i-1]/p[j,i-1])}  # Nominal wage rate of skilled workers (linear)
      w[j,i] = betaw*wl[j,i] + (1-betaw)*wh[j,i]                # Average wage rate across workers
      if (type_jg[j,i]==2){
        wl[j,i] = wg[j,i]}  else {wl[j,i]=rhol*w[j,i]}          # Minimum wage rate

      # 10. Prices and expectations
      pf[j,i] = (w[j,i]/prf[j,i])*(1+mup[j,i])                  # Unit price of private output

      if (type_jg[j,i]==0 && j!=11) {p[j,i] = pf[j,i]}          # General price level
      else {p[j,i] = p[j,i-1]+(pf[j,i]-pf[j,i-1])*(1-(wbg[j,i-1]/y[j,i-1])) + (tar[j,i]-tar[j,i-1])*(wbg[j,i-1]/y[j,i-1])}

      pi[j,i] = (p[j,i]/p[j,i-1])-1                             # Inflation rate

      if (i<3) {epi[j,i] = pi[j,i]}
      else {epi[j,i] = pi[j,i-1] + psi1 + psi2*(epi[j,i-1]-pi[j,i-1])}  # Expected inflation rate: regressive expectations
      ep[j,i] = p[j,i-1]*(1+epi[j,i])                           # Expected price level

      # 11. Financial ratios
      q[j,i] = (esr[j,i]*pe[j,i] + lf[j,i])/(p[j,i]*k[j,i])     # Valuation ratio (Tobin q)
      def_ratio[j,i] = def[j,i]/y[j,i]                          # Government deficit to GDP ratio
      deb_ratio[j,i] = bs[j,i]/y[j,i]                           # Government debt to GDP ratio
      lev[j,i] = lf[j,i]/(esr[j,i]*pe[j,i] + lf[j,i])           # Leverage ratio of firms
      # Household debt ratios (H1-H3)
      da_lm[j,i]=llm[j,i]/vlm[j,i]                              # Debt-to-asset ratio, H2
      da_l[j,i]=ll[j,i]/vl[j,i]                                 # Debt-to-asset ratio, H1
      da_m[j,i]=lm[j,i]/vm[j,i]                                 # Debt-to-asset ratio, H3
      levy_lm[j,i]=llm[j,i]/ydlm[j,i]                           # Debt-to-income ratio, H2
      dsy_lm[j,i]= yglm[j,i]/(rlh[j,i-1]*llm[j,i-1])            # Income-to-debt-service ratio, H2
      dsy_m[j,i]= ygm[j,i]/(rlh[j,i-1]*lm[j,i-1])               # Income-to-debt-service ratio, H3
      dsy_l[j,i]= ygl[j,i]/(rlh[j,i-1]*ll[j,i-1])               # Income-to-debt-service ratio, H1
      levy_l[j,i]=ll[j,i]/ydl[j,i]                              # Debt-to-income ratio, H1
      levy_m[j,i]=lm[j,i]/ydm[j,i]                              # Debt-to-income ratio, H3
      rhosr1_l[j,i]= rhosl_perc[j,i]*rhos[j,i]/ydl[j,i]         # Rent burden, H1: rent / disposable income
      rhosr2_l[j,i]= rhosl_perc[j,i]*rhos[j,i]/ygl[j,i]         # Rent burden, H1: rent / income before housing costs
      rhosr3_l[j,i]= rhosl_perc[j,i]*rhos[j,i]/vl[j,i]          # Rent burden, H1: rent / wealth
      rhosr1_lm[j,i]= rhosl_perc[j,i]*rhos[j,i]/ydlm[j,i]       # Rent burden, H2: rent / disposable income
      rhosr2_lm[j,i]= rhosl_perc[j,i]*rhos[j,i]/yglm[j,i]       # Rent burden, H2: rent / income before housing costs
      rhosr3_lm[j,i]= rhosl_perc[j,i]*rhos[j,i]/vlm[j,i]        # Rent burden, H2: rent / wealth
      # Redundant equation, implied by the others and checked rather than solved:
      # hh[j,i] = hs[j,i]

      # Disposable-income shares and functional distribution
      incl[j,i] = ydl[j,i]/yd[j,i]
      inclm[j,i] = ydlm[j,i]/yd[j,i]
      incm[j,i] = ydm[j,i]/yd[j,i]
      incum[j,i] = ydum[j,i]/yd[j,i]
      incu[j,i] = ydu[j,i]/yd[j,i]
      wb_ratio[j,i]=(wbg[j,i]+wb[j,i])/y[j,i]
      ff_ratio[j,i]=(ff[j,i])/y[j,i]
      gov_ratio[j,i]=(tax[j,i]+fcb[j,i])/y[j,i]

      # Inequality across H1-H5 (equal group weights)
      gini_y1=c(ydl[j,i],ydlm[j,i],ydm[j,i],ydum[j,i],ydu[j,i])
      gini_y2=c(vl[j,i],vlm[j,i],vm[j,i],vum[j,i],vu[j,i])
      atkinson[j,i]=ineq(gini_y1,parameter = NULL, type=c("Atkinson"), na.rm = TRUE)
      gini[j,i]=ineq(gini_y1,parameter = NULL, type=c("Gini"), na.rm = TRUE)
      var_coeff[j,i]=ineq(gini_y1,parameter = NULL, type=c("square.var"), na.rm = TRUE)
      gini_v[j,i]=ineq(gini_y2,parameter = NULL, type=c("Gini"), na.rm = TRUE)
      gini_nv_vec=c(nvl[j,i],nvlm[j,i],nvm[j,i],nvum[j,i],nvu[j,i])
      if(all(is.finite(gini_nv_vec)) && any(gini_nv_vec != 0)){
        gini_nv[j,i]=ineq(pmax(gini_nv_vec, 0),parameter = NULL, type=c("Gini"), na.rm = TRUE)
      }

      if (phase3_demand_active && iterations==phase3_iteration_limit-1L) demand_previous <- demand_snapshot()
    }
    if (phase3_demand_active) demand_check_period(demand_previous)
  }
}
