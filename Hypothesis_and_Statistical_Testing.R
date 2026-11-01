# SECTION 1: Loading the prepared data and defining hypotheses --------

# Starting from the datasets exported by Data-Preperation.R.
# This script tests the five research questions and saves the results.

# Checking that the CA2 project folder has been set up
project_dir <- "/Users/corey/Documents/Education/Data Analytics/ATU - Big Data Analytics/Data Science/CA 2/CA2"

stopifnot(
  dir.exists(project_dir)
)

# Identifying the folders created during data preparation
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "output")
figures_dir <- file.path(project_dir, "figures")

# Creating the output and figures folders if they do not already exist
dir.create(output_dir, showWarnings = FALSE)
dir.create(figures_dir, showWarnings = FALSE)

# Checking that the prepared RDS files are available
stopifnot(
  file.exists(file.path(data_dir, "price_data.rds")),
  file.exists(file.path(data_dir, "quarter_data.rds")),
  file.exists(file.path(data_dir, "sligo_clean.rds")),
  file.exists(file.path(data_dir, "price_data_all_records.rds")),
  file.exists(file.path(data_dir, "quarter_data_all_records.rds")),
  file.exists(file.path(data_dir, "sligo_all_records.rds"))
)

# Loading the main datasets for the five research questions
price_data <- readRDS(
  file.path(data_dir, "price_data.rds")
)

quarter_data <- readRDS(
  file.path(data_dir, "quarter_data.rds")
)

categorical_data <- readRDS(
  file.path(data_dir, "sligo_clean.rds")
)

# Loading the datasets containing all formatted records
# These will be used later for the duplicate-policy sensitivity check
price_data_all_records <- readRDS(
  file.path(data_dir, "price_data_all_records.rds")
)

quarter_data_all_records <- readRDS(
  file.path(data_dir, "quarter_data_all_records.rds")
)

categorical_data_all_records <- readRDS(
  file.path(data_dir, "sligo_all_records.rds")
)

# Checking the main-analysis datasets before testing
stopifnot(
  nrow(price_data) == 5070,
  nrow(quarter_data) == 675,
  nrow(categorical_data) == 5957,
  inherits(price_data$sale_date, "Date"),
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  is.factor(quarter_data$quarter),
  is.factor(categorical_data$period),
  is.factor(categorical_data$property_status),
  is.factor(categorical_data$not_full_market_price)
)

# Checking the sensitivity-check datasets before testing
stopifnot(
  nrow(price_data_all_records) == 5074,
  nrow(quarter_data_all_records) == 676,
  nrow(categorical_data_all_records) == 5964
)

# Checking that the comparison groups contain records
stopifnot(
  all(
    table(
      factor(
        price_data$period,
        levels = c("2019", "2020-2021", "2022-2025")
      )
    ) > 0
  ),
  all(
    table(
      factor(
        quarter_data$quarter,
        levels = c("Q1", "Q2", "Q3", "Q4")
      )
    ) > 0
  ),
  all(
    table(
      factor(
        categorical_data$period,
        levels = c("2019", "2020-2021", "2022-2025")
      )
    ) > 0
  )
)

# Setting the significance level for the five primary tests
alpha <- 0.05

# # CA2 research questions and hypotheses 
#
# F_g(x) means the cumulative recorded-price distribution in group g.
# rho_s means the population Spearman rank correlation.
# p_new,g means the proportion of new records in period g.
# p_nonmarket,g means the proportion flagged Yes for
# not-full-market price in period g.
#
# ## RQ1: Recorded prices across analytical periods
# Dataset: price_data
# Variables: price_eur and period
# H0: F_2019(x) = F_2020-2021(x) = F_2022-2025(x), for all x.
# H1: At least one period has a different price distribution.
# Test: Kruskal-Wallis rank sum test.
#
# ## RQ2: Recorded prices across quarters within 2025
# Dataset: quarter_data
# Variables: price_eur and quarter
# H0: F_Q1(x) = F_Q2(x) = F_Q3(x) = F_Q4(x), for all x.
# H1: At least one 2025 quarter has a different price distribution.
# Test: Kruskal-Wallis rank sum test.
#
# ## RQ3: Sale date and recorded price, 2019-2025
# Dataset: price_data
# Variables: sale_date and price_eur
# H0: rho_s = 0.
# H1: rho_s != 0.
# Test: Spearman rank correlation, two-sided.
#
# ## RQ4: Analytical period and property status
# Dataset: categorical_data
# Variables: period and property_status
# H0: p_new,2019 = p_new,2020-2021 = p_new,2022-2025.
# H1: At least one period has a different proportion of new records.
# Test: Pearson chi-squared test of independence.
#
# ## RQ5: Analytical period and not-full-market-price status
# Dataset: categorical_data
# Variables: period and not_full_market_price
# H0: p_nonmarket,2019 =
#       p_nonmarket,2020-2021 =
#       p_nonmarket,2022-2025.
# H1: At least one period has a different proportion flagged Yes.
# Test: Pearson chi-squared test of independence.
#
# The five primary p-values will be adjusted using Holm's method.
# A Holm-adjusted p-value at or below 0.05 supports rejecting H0.
# A Holm-adjusted p-value above 0.05 means there is insufficient
# evidence to reject H0; it does not prove that H0 is true.
#
# RQ2 concerns quarters within 2025 only. It does not test general
# seasonality across years.
#
# The analysis concerns recorded transactions in the PSRA register.
# It does not measure the changing value of an identical property and
# is not a quality-adjusted property price index.
#
# The tests assume independent observations. Repeat sales, shared
# developments and wider market conditions may create dependence.
# Results are interpreted conditionally on this limitation.

# Displaying the datasets loaded for testing
cat("Prepared datasets loaded successfully.\n")
cat("Price-analysis records:", nrow(price_data), "\n")
cat("Eligible 2025 quarter records:", nrow(quarter_data), "\n")
cat("Categorical-analysis records:", nrow(categorical_data), "\n")
cat("Sensitivity-check records:", nrow(categorical_data_all_records), "\n")
cat("Significance level:", alpha, "\n")


