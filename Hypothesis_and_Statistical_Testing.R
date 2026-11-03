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








# SECTION 2: Checking price distributions before testing

# Checking that the price datasets and output folders are available
stopifnot(
  exists("price_data"),
  exists("quarter_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha")
)

# Defining the coral orange used in the preparation charts
chart_colour <- "#FF7F50"

# Defining a function to check normality within each group
check_normality <- function(data, group_variable, question) {
  
  # Checking that the required variables are available
  stopifnot(
    is.numeric(data$price_eur),
    is.factor(data[[group_variable]]),
    !anyNA(data$price_eur),
    !anyNA(data[[group_variable]])
  )
  
  # Splitting prices into the groups being compared
  grouped_prices <- split(
    data$price_eur,
    data[[group_variable]],
    drop = TRUE
  )
  
  # Checking each group separately
  normality_summary <- do.call(
    rbind,
    lapply(names(grouped_prices), function(group_name) {
      
      group_prices <- grouped_prices[[group_name]]
      
      # Checking that Shapiro-Wilk can be applied
      stopifnot(
        length(group_name) > 0,
        length(group_prices) >= 3,
        length(group_prices) <= 5000,
        !anyNA(group_prices),
        all(is.finite(group_prices)),
        length(unique(group_prices)) > 1
      )
      
      # Running the Shapiro-Wilk normality test
      normality_test <- shapiro.test(group_prices)
      
      data.frame(
        question = question,
        group = group_name,
        records = length(group_prices),
        W = unname(normality_test$statistic),
        p_value = normality_test$p.value,
        row.names = NULL
      )
    })
  )
  
  rownames(normality_summary) <- NULL
  
  return(normality_summary)
}

# Checking normality for the three comparison periods
normality_summary <- check_normality(
  price_data,
  "period",
  "RQ1"
)

cat("\nRQ1: Normality checks by period\n")
print(normality_summary, row.names = FALSE)

# Checking normality across quarters within 2025
quarter_normality_summary <- check_normality(
  quarter_data,
  "quarter",
  "RQ2"
)

cat("\nRQ2: Normality checks by quarter within 2025\n")
print(quarter_normality_summary, row.names = FALSE)

# Checking that every price group was checked
stopifnot(
  sum(normality_summary$records) == nrow(price_data),
  sum(quarter_normality_summary$records) == nrow(quarter_data)
)

# Saving the normality results for the report
write.csv(
  normality_summary,
  file.path(output_dir, "01_RQ1_normality_by_period.csv"),
  row.names = FALSE
)

write.csv(
  quarter_normality_summary,
  file.path(output_dir, "02_RQ2_normality_by_quarter.csv"),
  row.names = FALSE
)


# Creating the RQ1 normality charts 

# Splitting eligible prices into the three periods
period_prices <- split(
  price_data$price_eur,
  price_data$period,
  drop = TRUE
)

# Opening a PNG file for the period diagnostic charts
png(
  file.path(figures_dir, "04_RQ1_period_normality.png"),
  width = 2000,
  height = 650 * length(period_prices),
  res = 180
)

# Arranging one histogram and one Q-Q plot for each period
par(
  mfrow = c(length(period_prices), 2),
  mar = c(4, 4, 3, 1),
  las = 1
)

# Creating a histogram and Q-Q plot for each period
for (period_name in names(period_prices)) {
  
  period_prices_in_eur_thousands <-
    period_prices[[period_name]] / 1000
  
  hist(
    period_prices_in_eur_thousands,
    breaks = 30,
    col = chart_colour,
    border = "white",
    main = paste(
      period_name,
      "prices; n =",
      length(period_prices[[period_name]])
    ),
    xlab = "Recorded price (EUR thousands)",
    ylab = "Records"
  )
  
  qqnorm(
    period_prices_in_eur_thousands,
    main = paste(period_name, "normal Q-Q plot"),
    xlab = "Normal quantile",
    ylab = "Recorded price (EUR thousands)",
    pch = 16,
    col = chart_colour
  )
  
  qqline(
    period_prices_in_eur_thousands,
    col = "black",
    lwd = 2
  )
}

# Closing the PNG file
dev.off()


# Creating the RQ2 normality charts 

# Splitting eligible 2025 prices into the four quarters
quarter_prices <- split(
  quarter_data$price_eur,
  quarter_data$quarter,
  drop = TRUE
)

# Opening a PNG file for the quarter diagnostic charts
png(
  file.path(figures_dir, "05_RQ2_2025_quarter_normality.png"),
  width = 2000,
  height = 650 * length(quarter_prices),
  res = 180
)

# Arranging one histogram and one Q-Q plot for each quarter
par(
  mfrow = c(length(quarter_prices), 2),
  mar = c(4, 4, 3, 1),
  las = 1
)

# Creating a histogram and Q-Q plot for each quarter
for (quarter_name in names(quarter_prices)) {
  
  quarter_prices_in_eur_thousands <-
    quarter_prices[[quarter_name]] / 1000
  
  hist(
    quarter_prices_in_eur_thousands,
    breaks = 30,
    col = chart_colour,
    border = "white",
    main = paste(
      quarter_name,
      "2025 prices; n =",
      length(quarter_prices[[quarter_name]])
    ),
    xlab = "Recorded price (EUR thousands)",
    ylab = "Records"
  )
  
  qqnorm(
    quarter_prices_in_eur_thousands,
    main = paste(quarter_name, "2025 normal Q-Q plot"),
    xlab = "Normal quantile",
    ylab = "Recorded price (EUR thousands)",
    pch = 16,
    col = chart_colour
  )
  
  qqline(
    quarter_prices_in_eur_thousands,
    col = "black",
    lwd = 2
  )
}

# Closing the PNG file
dev.off()


# Checking that both diagnostic charts were created 

normality_charts <- file.path(
  figures_dir,
  c(
    "04_RQ1_period_normality.png",
    "05_RQ2_2025_quarter_normality.png"
  )
)

stopifnot(
  all(file.exists(normality_charts)),
  all(file.info(normality_charts)$size > 0)
)

# Displaying the completed section
cat(
  "\nSection 2 completed.",
  "\nRQ1 price records checked:", nrow(price_data),
  "\nRQ2 2025 price records checked:", nrow(quarter_data),
  "\nNormality diagnostic charts saved:", length(normality_charts),
  "\n"
)

print(normality_charts)







# SECTION 3: Testing recorded prices across periods

# Checking that the required objects from Section 2 are available
stopifnot(
  exists("price_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  exists("chart_colour"),
  exists("normality_summary"),
  nrow(price_data) == 5070
)

# Checking that the RQ1 variables are suitable for testing
stopifnot(
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  !anyNA(price_data$price_eur),
  !anyNA(price_data$period),
  all(is.finite(price_data$price_eur))
)

# Checking that all three analytical periods are present
stopifnot(
  setequal(
    levels(droplevels(price_data$period)),
    c("2019", "2020-2021", "2022-2025")
  )
)

# Confirming the RQ1 hypothesis before testing
cat(
  "\nRQ1: Recorded prices across analytical periods",
  "\nH0: The recorded-price distributions are identical across",
  "2019, 2020-2021 and 2022-2025.",
  "\nH1: At least one period has a different recorded-price distribution.",
  "\nTest: Kruskal-Wallis rank sum test.",
  "\n"
)


# Running the primary RQ1 test 

rq1_test <- kruskal.test(
  price_eur ~ period,
  data = price_data
)

cat("\nRQ1: Kruskal-Wallis rank sum test\n")
print(rq1_test)

# Checking that the test returned valid results
stopifnot(
  inherits(rq1_test, "htest"),
  is.finite(rq1_test$statistic),
  is.finite(rq1_test$p.value),
  rq1_test$p.value >= 0,
  rq1_test$p.value <= 1
)


# Calculating eta-squared based on H 

rq1_n <- nrow(price_data)
rq1_k <- nlevels(droplevels(price_data$period))

rq1_eta_squared_H_raw <- (
  unname(rq1_test$statistic) - rq1_k + 1
) / (
  rq1_n - rq1_k
)

# Reporting zero if the bias-corrected estimate is negative
rq1_eta_squared_H <- max(
  0,
  rq1_eta_squared_H_raw
)

cat(
  "\nRQ1 eta-squared based on H:",
  round(rq1_eta_squared_H, 4),
  "\n"
)


# Summarising recorded prices by period 

rq1_grouped_prices <- split(
  price_data$price_eur,
  price_data$period,
  drop = TRUE
)

rq1_descriptives <- do.call(
  rbind,
  lapply(names(rq1_grouped_prices), function(period_name) {
    
    prices <- rq1_grouped_prices[[period_name]]
    
    data.frame(
      period = period_name,
      records = length(prices),
      median_eur = median(prices),
      mean_eur = mean(prices),
      q1_eur = unname(quantile(prices, 0.25)),
      q3_eur = unname(quantile(prices, 0.75)),
      minimum_eur = min(prices),
      maximum_eur = max(prices),
      row.names = NULL
    )
  })
)

cat("\nRQ1: Recorded-price descriptive statistics by period\n")
print(rq1_descriptives, row.names = FALSE)

# Checking that all eligible records are represented
stopifnot(
  sum(rq1_descriptives$records) == nrow(price_data)
)

write.csv(
  rq1_descriptives,
  file.path(
    output_dir,
    "03_RQ1_price_descriptives_by_period.csv"
  ),
  row.names = FALSE
)


# Examining supplementary pairwise period comparisons 

# Creating all three pairwise period comparisons
period_pairs <- combn(
  levels(droplevels(price_data$period)),
  2,
  simplify = FALSE
)

# Running a two-sided Wilcoxon rank-sum test for each pair
rq1_pairwise_results <- do.call(
  rbind,
  lapply(period_pairs, function(pair) {
    
    pairwise_data <- price_data[
      price_data$period %in% pair,
    ]
    
    pairwise_data$period <- droplevels(
      pairwise_data$period
    )
    
    pairwise_test <- wilcox.test(
      price_eur ~ period,
      data = pairwise_data,
      alternative = "two.sided",
      exact = FALSE
    )
    
    data.frame(
      comparison = paste(pair, collapse = " vs "),
      W = unname(pairwise_test$statistic),
      p_value = pairwise_test$p.value,
      row.names = NULL
    )
  })
)

# Adjusting across the three supplementary pairwise comparisons
# This is separate from the primary Holm adjustment in Section 8
rq1_pairwise_results$p_value_holm <- p.adjust(
  rq1_pairwise_results$p_value,
  method = "holm"
)

rq1_pairwise_results$significant_at_0.05 <-
  rq1_pairwise_results$p_value_holm <= alpha

cat(
  "\nRQ1: Supplementary pairwise Wilcoxon tests",
  "with Holm adjustment\n"
)

print(rq1_pairwise_results, row.names = FALSE)


# Saving the primary and supplementary results 

rq1_results <- data.frame(
  question = "RQ1",
  test = "Kruskal-Wallis rank sum test",
  records = rq1_n,
  statistic = unname(rq1_test$statistic),
  df = unname(rq1_test$parameter),
  p_value = rq1_test$p.value,
  eta_squared_H = rq1_eta_squared_H,
  alpha = alpha,
  decision_unadjusted = ifelse(
    rq1_test$p.value <= alpha,
    "Reject H0 before primary Holm adjustment",
    "Insufficient evidence to reject H0"
  ),
  primary_holm_status = "Pending adjustment across RQ1-RQ5",
  row.names = NULL
)

write.csv(
  rq1_results,
  file.path(
    output_dir,
    "04_RQ1_kruskal_wallis_result.csv"
  ),
  row.names = FALSE
)

write.csv(
  rq1_pairwise_results,
  file.path(
    output_dir,
    "05_RQ1_pairwise_wilcoxon_results.csv"
  ),
  row.names = FALSE
)


# Creating the full-range RQ1 price distribution chart 

rq1_chart <- file.path(
  figures_dir,
  "06_RQ1_price_distributions_by_period.png"
)

png(
  rq1_chart,
  width = 2200,
  height = 1400,
  res = 180
)

# Providing enough room for the period labels
par(
  mar = c(5, 8, 4, 2),
  las = 1
)

# Keeping the complete recorded-price range visible
price_limits <- c(
  0,
  max(price_data$price_eur) / 1000
)

boxplot(
  price_eur / 1000 ~ period,
  data = price_data,
  col = chart_colour,
  border = "black",
  horizontal = TRUE,
  main = "Recorded prices by analytical period",
  xlab = "Recorded price (EUR thousands)",
  ylab = "",
  ylim = price_limits,
  outline = FALSE
)

# Adding all observations with reproducible visual jitter
set.seed(123)

stripchart(
  price_eur / 1000 ~ period,
  data = price_data,
  method = "jitter",
  jitter = 0.12,
  vertical = FALSE,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  add = TRUE
)

dev.off()


# Checking that the RQ1 outputs were created 

rq1_outputs <- file.path(
  output_dir,
  c(
    "03_RQ1_price_descriptives_by_period.csv",
    "04_RQ1_kruskal_wallis_result.csv",
    "05_RQ1_pairwise_wilcoxon_results.csv"
  )
)

stopifnot(
  all(file.exists(rq1_outputs)),
  all(file.info(rq1_outputs)$size > 0),
  file.exists(rq1_chart),
  file.info(rq1_chart)$size > 0
)

cat(
  "\nSection 3 completed.",
  "\nRQ1 records:", rq1_n,
  "\nRQ1 Kruskal-Wallis statistic:",
  round(unname(rq1_test$statistic), 3),
  "\nDegrees of freedom:",
  unname(rq1_test$parameter),
  "\nUnadjusted p-value:",
  format.pval(rq1_test$p.value, digits = 4),
  "\nEta-squared based on H:",
  round(rq1_eta_squared_H, 4),
  "\nSupplementary pairwise comparisons:",
  nrow(rq1_pairwise_results),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)

print(rq1_outputs)
print(rq1_chart)








# SECTION 4: Testing recorded prices across 2025 quarters

# Checking that the required datasets and folders are available
stopifnot(
  exists("quarter_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  nrow(quarter_data) == 675,
  is.numeric(quarter_data$price_eur),
  is.factor(quarter_data$quarter),
  !anyNA(quarter_data$price_eur),
  !anyNA(quarter_data$quarter),
  all(is.finite(quarter_data$price_eur))
)

# Confirming that all four quarters contain records
stopifnot(
  setequal(
    levels(droplevels(quarter_data$quarter)),
    c("Q1", "Q2", "Q3", "Q4")
  )
)

# Confirming the research question and hypotheses
cat(
  "\nRQ2: Recorded prices across quarters within 2025",
  "\nH0: The recorded-price distributions are identical across Q1-Q4.",
  "\nH1: At least one quarter has a different distribution.",
  "\nTest: Kruskal-Wallis rank sum test.",
  "\nThis tests differences within 2025, not seasonality across years.",
  "\n"
)


# Running the primary RQ2 test 

rq2_test <- kruskal.test(
  price_eur ~ quarter,
  data = quarter_data
)

cat("\nRQ2: Kruskal-Wallis test\n")
print(rq2_test)

# Checking that the test returned valid results
stopifnot(
  inherits(rq2_test, "htest"),
  is.finite(rq2_test$statistic),
  is.finite(rq2_test$p.value)
)

# Calculating eta-squared based on the H statistic
rq2_n <- nrow(quarter_data)
rq2_k <- nlevels(droplevels(quarter_data$quarter))

rq2_eta_squared_H_raw <- (
  unname(rq2_test$statistic) - rq2_k + 1
) / (rq2_n - rq2_k)

# Reporting zero if the bias-corrected estimate is negative
rq2_eta_squared_H <- max(0, rq2_eta_squared_H_raw)

cat(
  "\nRQ2 eta-squared based on H:",
  round(rq2_eta_squared_H, 4),
  "\n"
)


# Summarising recorded prices by quarter 

rq2_grouped_prices <- split(
  quarter_data$price_eur,
  quarter_data$quarter,
  drop = TRUE
)

rq2_descriptives <- do.call(
  rbind,
  lapply(names(rq2_grouped_prices), function(quarter_name) {
    
    prices <- rq2_grouped_prices[[quarter_name]]
    
    data.frame(
      quarter = quarter_name,
      records = length(prices),
      median_eur = median(prices),
      mean_eur = mean(prices),
      q1_eur = unname(quantile(prices, 0.25)),
      q3_eur = unname(quantile(prices, 0.75)),
      iqr_eur = IQR(prices),
      minimum_eur = min(prices),
      maximum_eur = max(prices),
      row.names = NULL
    )
  })
)

cat("\nRQ2: Descriptive statistics by 2025 quarter\n")
print(rq2_descriptives, row.names = FALSE)

stopifnot(
  sum(rq2_descriptives$records) == nrow(quarter_data)
)


# Examining pairwise quarter comparisons 

# Treating these as supplementary comparisons
# Their Holm adjustment is separate from the five primary tests
rq2_quarter_pairs <- combn(
  names(rq2_grouped_prices),
  2,
  simplify = FALSE
)

rq2_pairwise_results <- do.call(
  rbind,
  lapply(rq2_quarter_pairs, function(pair) {
    
    pairwise_test <- wilcox.test(
      x = rq2_grouped_prices[[pair[1]]],
      y = rq2_grouped_prices[[pair[2]]],
      alternative = "two.sided",
      exact = FALSE
    )
    
    data.frame(
      comparison = paste(pair, collapse = " vs "),
      W = unname(pairwise_test$statistic),
      p_value = pairwise_test$p.value,
      row.names = NULL
    )
  })
)

# Adjusting across the six quarter comparisons
rq2_pairwise_results$p_value_holm <- p.adjust(
  rq2_pairwise_results$p_value,
  method = "holm"
)

rq2_pairwise_results$significant_at_0.05 <-
  rq2_pairwise_results$p_value_holm <= alpha

cat("\nRQ2: Supplementary pairwise Wilcoxon tests\n")
print(rq2_pairwise_results, row.names = FALSE)


# Saving the statistical results 

rq2_results <- data.frame(
  question = "RQ2",
  test = "Kruskal-Wallis rank sum test",
  records = rq2_n,
  statistic = unname(rq2_test$statistic),
  df = unname(rq2_test$parameter),
  p_value = rq2_test$p.value,
  eta_squared_H = rq2_eta_squared_H,
  alpha = alpha,
  decision_unadjusted = ifelse(
    rq2_test$p.value <= alpha,
    "Reject H0 before primary Holm adjustment",
    "Insufficient evidence to reject H0"
  ),
  primary_holm_status = "Pending adjustment across RQ1-RQ5",
  row.names = NULL
)

write.csv(
  rq2_descriptives,
  file.path(output_dir, "06_RQ2_price_descriptives_by_quarter.csv"),
  row.names = FALSE
)

write.csv(
  rq2_results,
  file.path(output_dir, "07_RQ2_kruskal_wallis_result.csv"),
  row.names = FALSE
)

write.csv(
  rq2_pairwise_results,
  file.path(output_dir, "08_RQ2_pairwise_wilcoxon_results.csv"),
  row.names = FALSE
)


# Creating full-range and zoomed quarter charts 

# Using the same zoom limit as the additional RQ1 chart
rq2_zoom_limit <- 600

rq2_above_zoom <- sum(
  quarter_data$price_eur > rq2_zoom_limit * 1000
)

rq2_chart <- file.path(
  figures_dir,
  "07_RQ2_2025_price_distributions_by_quarter.png"
)

png(
  rq2_chart,
  width = 2400,
  height = 2000,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(5, 6, 4, 2),
  las = 1
)

# Using a fixed seed for reproducible jitter
set.seed(123)

# Plotting the full range and a clearly labelled zoomed view
for (view in c("full", "zoom")) {
  
  if (view == "full") {
    price_axis_limits <- c(
      0,
      max(quarter_data$price_eur) / 1000
    )
    chart_title <- "2025 recorded prices by quarter: full range"
  } else {
    price_axis_limits <- c(0, rq2_zoom_limit)
    chart_title <- "2025 recorded prices by quarter: zoomed view"
  }
  
  # Calculating each boxplot from all eligible records
  boxplot(
    price_eur / 1000 ~ quarter,
    data = quarter_data,
    horizontal = TRUE,
    col = "#FF7F50",
    border = "black",
    outline = FALSE,
    ylim = price_axis_limits,
    main = chart_title,
    xlab = "Recorded price (EUR thousands)",
    ylab = ""
  )
  
  stripchart(
    price_eur / 1000 ~ quarter,
    data = quarter_data,
    method = "jitter",
    jitter = 0.12,
    vertical = FALSE,
    pch = 16,
    cex = 0.4,
    col = adjustcolor("black", alpha.f = 0.2),
    add = TRUE
  )
  
  if (view == "zoom") {
    mtext(
      paste0(
        "Zoomed to EUR 600,000; ",
        rq2_above_zoom,
        " records above this range are not shown. All records remain in the analysis."
      ),
      side = 1,
      line = 3.8,
      cex = 0.7
    )
  }
}

dev.off()


# Checking that the outputs were created

rq2_outputs <- file.path(
  output_dir,
  c(
    "06_RQ2_price_descriptives_by_quarter.csv",
    "07_RQ2_kruskal_wallis_result.csv",
    "08_RQ2_pairwise_wilcoxon_results.csv"
  )
)

stopifnot(
  all(file.exists(rq2_outputs)),
  all(file.info(rq2_outputs)$size > 0),
  file.exists(rq2_chart),
  file.info(rq2_chart)$size > 0
)

cat(
  "\nSection 4 completed.",
  "\nRQ2 records:", rq2_n,
  "\nKruskal-Wallis statistic:",
  round(unname(rq2_test$statistic), 3),
  "\nDegrees of freedom:", unname(rq2_test$parameter),
  "\nUnadjusted p-value:",
  format.pval(rq2_test$p.value, digits = 4),
  "\nEta-squared based on H:",
  round(rq2_eta_squared_H, 4),
  "\nSupplementary pairwise comparisons:",
  nrow(rq2_pairwise_results),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)

print(rq2_outputs)
print(rq2_chart)










# SECTION 5: Testing the association between sale date and price

# Checking that the required datasets and folders are available
stopifnot(
  exists("price_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  nrow(price_data) == 5070,
  inherits(price_data$sale_date, "Date"),
  is.numeric(price_data$price_eur),
  !anyNA(price_data$sale_date),
  !anyNA(price_data$price_eur),
  all(is.finite(as.numeric(price_data$sale_date))),
  all(is.finite(price_data$price_eur)),
  length(unique(price_data$sale_date)) > 1,
  length(unique(price_data$price_eur)) > 1
)

# Checking that the dates fall within the research period
stopifnot(
  all(price_data$sale_date >= as.Date("2019-01-01")),
  all(price_data$sale_date <= as.Date("2025-12-31"))
)

# Confirming the research question and hypotheses
cat(
  "\nRQ3: Sale date and recorded price, 2019-2025",
  "\nH0: The population Spearman rank correlation is zero.",
  "\nH1: The population Spearman rank correlation is not zero.",
  "\nTest: Two-sided Spearman rank correlation.",
  "\n"
)


# Running the primary RQ3 test 

# Converting dates to numeric days while retaining chronological order
rq3_date_numeric <- as.numeric(price_data$sale_date)

# Using an approximate p-value because dates and prices contain ties
rq3_test <- cor.test(
  x = rq3_date_numeric,
  y = price_data$price_eur,
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)

cat("\nRQ3: Spearman rank correlation\n")
print(rq3_test)

# Extracting Spearman's rho
rq3_rho <- unname(rq3_test$estimate)

# Checking that the test returned valid results
stopifnot(
  inherits(rq3_test, "htest"),
  is.finite(rq3_rho),
  rq3_rho >= -1,
  rq3_rho <= 1,
  is.finite(rq3_test$statistic),
  is.finite(rq3_test$p.value),
  rq3_test$p.value >= 0,
  rq3_test$p.value <= 1
)

# Describing the direction without assuming statistical significance
rq3_direction <- if (rq3_rho > 0) {
  "Positive"
} else if (rq3_rho < 0) {
  "Negative"
} else {
  "Zero"
}

cat(
  "\nRQ3 records:", nrow(price_data),
  "\nSpearman rho:", round(rq3_rho, 4),
  "\nDirection:", rq3_direction,
  "\nUnadjusted p-value:",
  format.pval(rq3_test$p.value, digits = 4),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)


# Saving the primary test result 

# Spearman's rho is itself the effect-size measure
rq3_results <- data.frame(
  question = "RQ3",
  test = "Spearman rank correlation",
  alternative = "two.sided",
  records = nrow(price_data),
  earliest_sale_date = min(price_data$sale_date),
  latest_sale_date = max(price_data$sale_date),
  statistic_name = names(rq3_test$statistic),
  statistic = unname(rq3_test$statistic),
  spearman_rho = rq3_rho,
  direction = rq3_direction,
  p_value = rq3_test$p.value,
  p_value_method = "Asymptotic approximation; exact = FALSE",
  alpha = alpha,
  decision_unadjusted = ifelse(
    rq3_test$p.value <= alpha,
    "Reject H0 before primary Holm adjustment",
    "Insufficient evidence to reject H0"
  ),
  primary_holm_status = "Pending adjustment across RQ1-RQ5",
  row.names = NULL
)

rq3_output <- file.path(
  output_dir,
  "09_RQ3_spearman_correlation_result.csv"
)

write.csv(
  rq3_results,
  rq3_output,
  row.names = FALSE
)


# Creating full-range and zoomed scatterplots 

# Using the same zoom limit as the earlier price charts
rq3_zoom_limit <- 600

rq3_above_zoom <- sum(
  price_data$price_eur > rq3_zoom_limit * 1000
)

# Defining annual tick marks for the date axis
rq3_year_ticks <- seq(
  from = as.Date("2019-01-01"),
  to = as.Date("2025-01-01"),
  by = "year"
)

rq3_chart <- file.path(
  figures_dir,
  "08_RQ3_sale_date_and_recorded_price.png"
)

png(
  rq3_chart,
  width = 2400,
  height = 2000,
  res = 180
)

par(
  mfrow = c(2, 1),
  mar = c(5, 6, 4, 2),
  las = 1
)

# Showing both the complete range and the main price range
for (view in c("full", "zoom")) {
  
  if (view == "full") {
    
    rq3_price_limits <- c(
      0,
      max(price_data$price_eur) / 1000
    )
    
    rq3_chart_title <-
      "Sale date and recorded price, 2019-2025: full range"
    
  } else {
    
    rq3_price_limits <- c(0, rq3_zoom_limit)
    
    rq3_chart_title <-
      "Sale date and recorded price, 2019-2025: zoomed view"
  }
  
  # Plotting the original dates and prices without jitter or filtering
  plot(
    x = price_data$sale_date,
    y = price_data$price_eur / 1000,
    xlim = c(
      as.Date("2019-01-01"),
      as.Date("2025-12-31")
    ),
    ylim = rq3_price_limits,
    xaxt = "n",
    pch = 16,
    cex = 0.45,
    col = adjustcolor("#FF7F50", alpha.f = 0.35),
    main = rq3_chart_title,
    xlab = "Sale date",
    ylab = "Recorded price (EUR thousands)"
  )
  
  axis.Date(
    side = 1,
    at = rq3_year_ticks,
    format = "%Y"
  )
  
  # Displaying rho without implying causation
  legend(
    "topleft",
    legend = paste0(
      "Spearman rho = ",
      round(rq3_rho, 3),
      "; n = ",
      nrow(price_data)
    ),
    bty = "n",
    cex = 0.85
  )
  
  if (view == "zoom") {
    
    mtext(
      paste0(
        "Zoomed to EUR 600,000; ",
        rq3_above_zoom,
        " records above this range are not shown. All records remain in the test."
      ),
      side = 1,
      line = 3.8,
      cex = 0.7
    )
  }
}

dev.off()


# Checking that the outputs were created 

stopifnot(
  file.exists(rq3_output),
  file.info(rq3_output)$size > 0,
  file.exists(rq3_chart),
  file.info(rq3_chart)$size > 0
)

cat(
  "\nSection 5 completed.",
  "\nRQ3 records:", nrow(price_data),
  "\nSpearman rho:", round(rq3_rho, 4),
  "\nUnadjusted p-value:",
  format.pval(rq3_test$p.value, digits = 4),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)

print(rq3_output)
print(rq3_chart)








# SECTION 6: Testing analytical period and property status

# Checking that the required dataset and folders are available
stopifnot(
  exists("categorical_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  nrow(categorical_data) == 5957,
  is.factor(categorical_data$period),
  is.factor(categorical_data$property_status),
  !anyNA(categorical_data$period),
  !anyNA(categorical_data$property_status)
)

# Checking that the three analytical periods are present
stopifnot(
  setequal(
    levels(droplevels(categorical_data$period)),
    c("2019", "2020-2021", "2022-2025")
  )
)

# Confirming the research question and hypotheses
cat(
  "\nRQ4: Analytical period and property status",
  "\nH0: The proportion of new-property records is the same across periods.",
  "\nH1: At least one period has a different proportion.",
  "\nTest: Pearson chi-squared test of independence.",
  "\n"
)


# Creating the observed frequency table

# Keeping periods in chronological order
rq4_period <- factor(
  categorical_data$period,
  levels = c("2019", "2020-2021", "2022-2025")
)

# Removing unused property-status levels
rq4_property_status <- droplevels(
  categorical_data$property_status
)

# Checking that property status has the two expected categories
stopifnot(
  nlevels(rq4_property_status) == 2
)

rq4_table <- table(
  period = rq4_period,
  property_status = rq4_property_status
)

# Checking that all records are included and margins are positive
stopifnot(
  sum(rq4_table) == nrow(categorical_data),
  all(rowSums(rq4_table) > 0),
  all(colSums(rq4_table) > 0)
)

cat("\nRQ4: Observed counts, including totals\n")
print(addmargins(rq4_table))


# Calculating percentages within each period 

# Each row sums to 100%
rq4_percentages <- prop.table(
  rq4_table,
  margin = 1
) * 100

cat("\nRQ4: Property-status percentages within each period\n")
print(round(rq4_percentages, 2))


# Checking expected counts before testing 

rq4_expected_counts <- outer(
  rowSums(rq4_table),
  colSums(rq4_table)
) / sum(rq4_table)

dimnames(rq4_expected_counts) <- dimnames(rq4_table)

cat("\nRQ4: Expected counts under independence\n")
print(round(rq4_expected_counts, 2))

cat(
  "\nMinimum expected count:",
  round(min(rq4_expected_counts), 2),
  "\nCells with expected counts below 5:",
  sum(rq4_expected_counts < 5),
  "\n"
)

# Using a conservative check for the Pearson approximation
# If this stops, review the expected counts before proceeding
if (any(rq4_expected_counts < 5)) {
  stop(
    paste(
      "At least one expected count is below 5.",
      "Review the table before using the planned Pearson approximation."
    )
  )
}


# Running the primary RQ4 test 

rq4_test <- chisq.test(
  rq4_table,
  correct = FALSE
)

cat("\nRQ4: Pearson chi-squared test\n")
print(rq4_test)

# Checking that the test returned valid results
stopifnot(
  inherits(rq4_test, "htest"),
  is.finite(rq4_test$statistic),
  is.finite(rq4_test$p.value),
  rq4_test$p.value >= 0,
  rq4_test$p.value <= 1
)


# Calculating Cramer's V 

rq4_n <- sum(rq4_table)

rq4_cramers_v <- sqrt(
  unname(rq4_test$statistic) /
    (
      rq4_n *
        min(
          nrow(rq4_table) - 1,
          ncol(rq4_table) - 1
        )
    )
)

cat(
  "\nRQ4 Cramer's V:",
  round(rq4_cramers_v, 4),
  "\n"
)


# Preparing a detailed cell-level table

rq4_cell_summary <- as.data.frame(
  rq4_table,
  responseName = "observed_count",
  stringsAsFactors = FALSE
)

# Table conversion and vectorisation use the same cell order
rq4_cell_summary$within_period_percent <-
  as.vector(rq4_percentages)

rq4_cell_summary$expected_count <-
  as.vector(rq4_test$expected)

rq4_cell_summary$standardised_residual <-
  as.vector(rq4_test$stdres)

cat("\nRQ4: Cell-level summary\n")
print(rq4_cell_summary, row.names = FALSE)


# Saving the primary test result 

rq4_results <- data.frame(
  question = "RQ4",
  test = "Pearson chi-squared test of independence",
  records = rq4_n,
  statistic = unname(rq4_test$statistic),
  df = unname(rq4_test$parameter),
  p_value = rq4_test$p.value,
  cramers_v = rq4_cramers_v,
  minimum_expected_count = min(rq4_test$expected),
  cells_expected_below_5 = sum(rq4_test$expected < 5),
  alpha = alpha,
  decision_unadjusted = ifelse(
    rq4_test$p.value <= alpha,
    "Reject H0 before primary Holm adjustment",
    "Insufficient evidence to reject H0"
  ),
  primary_holm_status = "Pending adjustment across RQ1-RQ5",
  row.names = NULL
)

write.csv(
  rq4_cell_summary,
  file.path(output_dir, "10_RQ4_property_status_by_period.csv"),
  row.names = FALSE
)

write.csv(
  rq4_results,
  file.path(output_dir, "11_RQ4_chi_squared_result.csv"),
  row.names = FALSE
)


# Creating the property-status percentage chart 

rq4_chart <- file.path(
  figures_dir,
  "09_RQ4_property_status_by_period.png"
)

# Matching colours to the displayed property-status order
rq4_colours <- c("#FF7F50", "#5B7083")

png(
  rq4_chart,
  width = 2200,
  height = 1500,
  res = 180
)

par(
  mar = c(6, 6, 6, 2),
  las = 1
)

# Transposing so each bar represents an analytical period
rq4_bar_positions <- barplot(
  t(rq4_percentages),
  beside = FALSE,
  col = rq4_colours,
  border = "white",
  ylim = c(0, 100),
  main = "Property status by analytical period",
  xlab = "Analytical period",
  ylab = "Records within period (%)",
  names.arg = rownames(rq4_table),
  cex.names = 0.95
)

# Adding percentage labels inside each segment
for (period_index in seq_len(nrow(rq4_percentages))) {
  
  segment_percentages <- rq4_percentages[period_index, ]
  
  segment_centres <-
    cumsum(segment_percentages) - segment_percentages / 2
  
  text(
    x = rep(
      rq4_bar_positions[period_index],
      length(segment_percentages)
    ),
    y = segment_centres,
    labels = paste0(
      formatC(
        segment_percentages,
        format = "f",
        digits = 1
      ),
      "%"
    ),
    col = "black",
    cex = 0.9
  )
}

# Putting the legend above the plotting area
legend(
  "top",
  inset = c(0, -0.14),
  legend = colnames(rq4_table),
  fill = rq4_colours,
  horiz = TRUE,
  bty = "n",
  xpd = NA,
  cex = 0.85
)

mtext(
  paste0(
    "Period totals: ",
    paste(
      paste0(
        rownames(rq4_table),
        " (n = ",
        rowSums(rq4_table),
        ")"
      ),
      collapse = "; "
    )
  ),
  side = 1,
  line = 4,
  cex = 0.75
)

dev.off()


# Checking that the outputs were created

rq4_outputs <- file.path(
  output_dir,
  c(
    "10_RQ4_property_status_by_period.csv",
    "11_RQ4_chi_squared_result.csv"
  )
)

stopifnot(
  all(file.exists(rq4_outputs)),
  all(file.info(rq4_outputs)$size > 0),
  file.exists(rq4_chart),
  file.info(rq4_chart)$size > 0
)

cat(
  "\nSection 6 completed.",
  "\nRQ4 records:", rq4_n,
  "\nChi-squared statistic:",
  round(unname(rq4_test$statistic), 3),
  "\nDegrees of freedom:", unname(rq4_test$parameter),
  "\nUnadjusted p-value:",
  format.pval(rq4_test$p.value, digits = 4),
  "\nCramer's V:", round(rq4_cramers_v, 4),
  "\nMinimum expected count:",
  round(min(rq4_test$expected), 2),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)

print(rq4_outputs)
print(rq4_chart)










# SECTION 7: Testing period and not-full-market-price status

# Checking that the required dataset and folders are available
stopifnot(
  exists("categorical_data"),
  exists("output_dir"),
  exists("figures_dir"),
  exists("alpha"),
  nrow(categorical_data) == 5957,
  is.factor(categorical_data$period),
  is.factor(categorical_data$not_full_market_price),
  !anyNA(categorical_data$period),
  !anyNA(categorical_data$not_full_market_price)
)

# Checking that the three analytical periods are present
stopifnot(
  setequal(
    levels(droplevels(categorical_data$period)),
    c("2019", "2020-2021", "2022-2025")
  )
)

# Checking that the flag contains the expected No and Yes categories
stopifnot(
  setequal(
    levels(droplevels(categorical_data$not_full_market_price)),
    c("No", "Yes")
  )
)

# Confirming the research question and hypotheses
cat(
  "\nRQ5: Analytical period and not-full-market-price status",
  "\nH0: The proportion flagged Yes is the same across periods.",
  "\nH1: At least one period has a different proportion flagged Yes.",
  "\nTest: Pearson chi-squared test of independence.",
  "\n"
)


# Creating the observed frequency table 

# Keeping periods in chronological order
rq5_period <- factor(
  categorical_data$period,
  levels = c("2019", "2020-2021", "2022-2025")
)

# Keeping the flag categories in a consistent order
rq5_market_status <- factor(
  categorical_data$not_full_market_price,
  levels = c("No", "Yes")
)

rq5_table <- table(
  period = rq5_period,
  not_full_market_price = rq5_market_status
)

# Checking that all records are included and margins are positive
stopifnot(
  sum(rq5_table) == nrow(categorical_data),
  all(rowSums(rq5_table) > 0),
  all(colSums(rq5_table) > 0)
)

cat("\nRQ5: Observed counts, including totals\n")
print(addmargins(rq5_table))




# Calculating percentages within each period 

# Each row sums to 100%
rq5_percentages <- prop.table(
  rq5_table,
  margin = 1
) * 100

cat("\nRQ5: Flag percentages within each period\n")
print(round(rq5_percentages, 2))


# Checking expected counts before testing 

rq5_expected_counts <- outer(
  rowSums(rq5_table),
  colSums(rq5_table)
) / sum(rq5_table)

dimnames(rq5_expected_counts) <- dimnames(rq5_table)

cat("\nRQ5: Expected counts under independence\n")
print(round(rq5_expected_counts, 2))

cat(
  "\nMinimum expected count:",
  round(min(rq5_expected_counts), 2),
  "\nCells with expected counts below 5:",
  sum(rq5_expected_counts < 5),
  "\n"
)

# Using the same conservative check as Section 6
# If this stops, review the expected counts before proceeding
if (any(rq5_expected_counts < 5)) {
  stop(
    paste(
      "At least one expected count is below 5.",
      "Review the table before using the planned Pearson approximation."
    )
  )
}


# Running the primary RQ5 test 

rq5_test <- chisq.test(
  rq5_table,
  correct = FALSE
)

cat("\nRQ5: Pearson chi-squared test\n")
print(rq5_test)

# Checking that the test returned valid results
stopifnot(
  inherits(rq5_test, "htest"),
  is.finite(rq5_test$statistic),
  is.finite(rq5_test$p.value),
  rq5_test$p.value >= 0,
  rq5_test$p.value <= 1
)


# Calculating Cramer's V 

rq5_n <- sum(rq5_table)

rq5_cramers_v <- sqrt(
  unname(rq5_test$statistic) /
    (
      rq5_n *
        min(
          nrow(rq5_table) - 1,
          ncol(rq5_table) - 1
        )
    )
)

cat(
  "\nRQ5 Cramer's V:",
  round(rq5_cramers_v, 4),
  "\n"
)


# Preparing a detailed cell-level table 

rq5_cell_summary <- as.data.frame(
  rq5_table,
  responseName = "observed_count",
  stringsAsFactors = FALSE
)

# Table conversion and vectorisation use the same cell order
rq5_cell_summary$within_period_percent <-
  as.vector(rq5_percentages)

rq5_cell_summary$expected_count <-
  as.vector(rq5_test$expected)

rq5_cell_summary$standardised_residual <-
  as.vector(rq5_test$stdres)

cat("\nRQ5: Cell-level summary\n")
print(rq5_cell_summary, row.names = FALSE)


# Saving the primary test result 

rq5_results <- data.frame(
  question = "RQ5",
  test = "Pearson chi-squared test of independence",
  records = rq5_n,
  statistic = unname(rq5_test$statistic),
  df = unname(rq5_test$parameter),
  p_value = rq5_test$p.value,
  cramers_v = rq5_cramers_v,
  minimum_expected_count = min(rq5_test$expected),
  cells_expected_below_5 = sum(rq5_test$expected < 5),
  alpha = alpha,
  decision_unadjusted = ifelse(
    rq5_test$p.value <= alpha,
    "Reject H0 before primary Holm adjustment",
    "Insufficient evidence to reject H0"
  ),
  primary_holm_status = "Pending adjustment across RQ1-RQ5",
  row.names = NULL
)

write.csv(
  rq5_cell_summary,
  file.path(
    output_dir,
    "12_RQ5_not_full_market_price_by_period.csv"
  ),
  row.names = FALSE
)

write.csv(
  rq5_results,
  file.path(output_dir, "13_RQ5_chi_squared_result.csv"),
  row.names = FALSE
)


# Creating the percentage chart 

# Plotting the Yes proportion directly for a clear comparison
rq5_yes_percentages <- rq5_percentages[, "Yes"]

rq5_chart <- file.path(
  figures_dir,
  "10_RQ5_not_full_market_price_by_period.png"
)

png(
  rq5_chart,
  width = 2200,
  height = 1500,
  res = 180
)

par(
  mar = c(6, 6, 5, 2),
  las = 1
)

# Starting the percentage axis at zero
rq5_chart_upper <- min(
  100,
  max(10, ceiling(max(rq5_yes_percentages) * 1.25 / 5) * 5)
)

rq5_bar_positions <- barplot(
  rq5_yes_percentages,
  col = "#FF7F50",
  border = "white",
  ylim = c(0, rq5_chart_upper),
  main = "Records flagged as not full market price by period",
  xlab = "Analytical period",
  ylab = "Records flagged Yes within period (%)",
  names.arg = rownames(rq5_table),
  cex.names = 0.95
)

# Adding percentage labels above the bars
text(
  x = rq5_bar_positions,
  y = rq5_yes_percentages,
  labels = paste0(
    formatC(
      rq5_yes_percentages,
      format = "f",
      digits = 1
    ),
    "%"
  ),
  pos = 3,
  cex = 0.95
)

# Reporting denominators so unequal period totals remain clear
mtext(
  paste0(
    "Period totals: ",
    paste(
      paste0(
        rownames(rq5_table),
        " (n = ",
        rowSums(rq5_table),
        ")"
      ),
      collapse = "; "
    )
  ),
  side = 1,
  line = 4,
  cex = 0.75
)

dev.off()


# Checking that the outputs were created 

rq5_outputs <- file.path(
  output_dir,
  c(
    "12_RQ5_not_full_market_price_by_period.csv",
    "13_RQ5_chi_squared_result.csv"
  )
)

stopifnot(
  all(file.exists(rq5_outputs)),
  all(file.info(rq5_outputs)$size > 0),
  file.exists(rq5_chart),
  file.info(rq5_chart)$size > 0
)

cat(
  "\nSection 7 completed.",
  "\nRQ5 records:", rq5_n,
  "\nChi-squared statistic:",
  round(unname(rq5_test$statistic), 3),
  "\nDegrees of freedom:", unname(rq5_test$parameter),
  "\nUnadjusted p-value:",
  format.pval(rq5_test$p.value, digits = 4),
  "\nCramer's V:", round(rq5_cramers_v, 4),
  "\nMinimum expected count:",
  round(min(rq5_test$expected), 2),
  "\nPrimary Holm adjustment: pending RQ1-RQ5.",
  "\n"
)

print(rq5_outputs)
print(rq5_chart)









# SECTION 8: Adjusting the five primary tests using Holm's method

# Checking that the five primary test objects are available
primary_test_names <- c(
  "rq1_test",
  "rq2_test",
  "rq3_test",
  "rq4_test",
  "rq5_test"
)

stopifnot(
  all(
    vapply(
      primary_test_names,
      exists,
      logical(1),
      inherits = TRUE
    )
  ),
  exists("price_data"),
  exists("quarter_data"),
  exists("categorical_data"),
  exists("output_dir"),
  exists("alpha"),
  is.numeric(alpha),
  length(alpha) == 1,
  is.finite(alpha),
  alpha > 0,
  alpha < 1
)

# Collecting the primary tests in research-question order
primary_tests <- list(
  RQ1 = rq1_test,
  RQ2 = rq2_test,
  RQ3 = rq3_test,
  RQ4 = rq4_test,
  RQ5 = rq5_test
)

# Checking that each object contains a hypothesis-test result
stopifnot(
  all(
    vapply(
      primary_tests,
      function(test) inherits(test, "htest"),
      logical(1)
    )
  )
)

cat(
  "\nPrimary multiple-testing adjustment",
  "\nFamily: The five primary tests for RQ1-RQ5.",
  "\nMethod: Holm.",
  "\nSignificance level:", alpha,
  "\nPairwise and normality-test p-values are not included.",
  "\n"
)


# Extracting the original p-values 

primary_p_values <- vapply(
  primary_tests,
  function(test) unname(test$p.value),
  numeric(1)
)

# Checking that all five p-values are valid
stopifnot(
  length(primary_p_values) == 5,
  !anyNA(primary_p_values),
  all(is.finite(primary_p_values)),
  all(primary_p_values >= 0),
  all(primary_p_values <= 1)
)

# Adjusting once across the five primary tests
primary_p_values_holm <- p.adjust(
  primary_p_values,
  method = "holm"
)

# Checking that the adjusted values are valid
stopifnot(
  length(primary_p_values_holm) == 5,
  all(is.finite(primary_p_values_holm)),
  all(primary_p_values_holm >= 0),
  all(primary_p_values_holm <= 1),
  all(primary_p_values_holm >= primary_p_values)
)


# Preparing consistently labelled effect sizes 

# Correcting the earlier terminology:
# (H - k + 1) / (n - k) is eta-squared based on H

rq1_k_primary <- nlevels(droplevels(price_data$period))
rq2_k_primary <- nlevels(droplevels(quarter_data$quarter))

rq1_eta_squared_H_primary <- max(
  0,
  (
    unname(rq1_test$statistic) - rq1_k_primary + 1
  ) / (
    nrow(price_data) - rq1_k_primary
  )
)

rq2_eta_squared_H_primary <- max(
  0,
  (
    unname(rq2_test$statistic) - rq2_k_primary + 1
  ) / (
    nrow(quarter_data) - rq2_k_primary
  )
)

# Calculating Cramer's V from the tested contingency tables
rq4_cramers_v_primary <- sqrt(
  unname(rq4_test$statistic) /
    (
      sum(rq4_test$observed) *
        min(dim(rq4_test$observed) - 1)
    )
)

rq5_cramers_v_primary <- sqrt(
  unname(rq5_test$statistic) /
    (
      sum(rq5_test$observed) *
        min(dim(rq5_test$observed) - 1)
    )
)


# Creating the final primary-results table

primary_results <- data.frame(
  question = names(primary_tests),
  
  research_question = c(
    "Recorded prices across analytical periods",
    "Recorded prices across quarters within 2025",
    "Sale date and recorded price, 2019-2025",
    "Analytical period and property status",
    "Analytical period and not-full-market-price status"
  ),
  
  test = c(
    "Kruskal-Wallis rank sum test",
    "Kruskal-Wallis rank sum test",
    "Two-sided Spearman rank correlation",
    "Pearson chi-squared test of independence",
    "Pearson chi-squared test of independence"
  ),
  
  records = c(
    nrow(price_data),
    nrow(quarter_data),
    nrow(price_data),
    sum(rq4_test$observed),
    sum(rq5_test$observed)
  ),
  
  statistic_name = c(
    "H",
    "H",
    "S",
    "X-squared",
    "X-squared"
  ),
  
  statistic = vapply(
    primary_tests,
    function(test) unname(test$statistic),
    numeric(1)
  ),
  
  df = c(
    unname(rq1_test$parameter),
    unname(rq2_test$parameter),
    NA_real_,
    unname(rq4_test$parameter),
    unname(rq5_test$parameter)
  ),
  
  effect_size_name = c(
    "Eta-squared based on H",
    "Eta-squared based on H",
    "Spearman rho",
    "Cramer's V",
    "Cramer's V"
  ),
  
  effect_size = c(
    rq1_eta_squared_H_primary,
    rq2_eta_squared_H_primary,
    unname(rq3_test$estimate),
    rq4_cramers_v_primary,
    rq5_cramers_v_primary
  ),
  
  p_value = unname(primary_p_values),
  p_value_holm = unname(primary_p_values_holm),
  adjustment_method = "Holm across five primary tests",
  alpha = alpha,
  row.names = NULL,
  stringsAsFactors = FALSE
)

# Recording the original and final decisions separately
primary_results$decision_unadjusted <- ifelse(
  primary_results$p_value <= alpha,
  "Reject H0",
  "Insufficient evidence to reject H0"
)

primary_results$decision_holm <- ifelse(
  primary_results$p_value_holm <= alpha,
  "Reject H0",
  "Insufficient evidence to reject H0"
)

primary_results$decision_changed <-
  primary_results$decision_unadjusted !=
  primary_results$decision_holm

# Checking the completed table
stopifnot(
  nrow(primary_results) == 5,
  identical(
    primary_results$question,
    c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5")
  ),
  all(is.finite(primary_results$effect_size)),
  !anyNA(primary_results$p_value_holm),
  !anyNA(primary_results$decision_holm)
)


# Displaying a readable summary 

# Keeping formatted values separate from the full-precision results
primary_results_display <- data.frame(
  question = primary_results$question,
  records = primary_results$records,
  effect_size_name = primary_results$effect_size_name,
  effect_size = round(primary_results$effect_size, 4),
  p_value = format.pval(
    primary_results$p_value,
    digits = 4
  ),
  p_value_holm = format.pval(
    primary_results$p_value_holm,
    digits = 4
  ),
  decision_holm = primary_results$decision_holm,
  decision_changed = primary_results$decision_changed,
  row.names = NULL
)

cat("\nFinal primary results after Holm adjustment\n")
print(primary_results_display, row.names = FALSE)


# Saving the full-precision primary results

primary_results_output <- file.path(
  output_dir,
  "14_primary_results_holm_adjusted.csv"
)

write.csv(
  primary_results,
  primary_results_output,
  row.names = FALSE
)

# Saving the underlying test objects for reproducibility
primary_tests_output <- file.path(
  output_dir,
  "15_primary_test_objects.rds"
)

saveRDS(
  list(
    alpha = alpha,
    adjustment_method = "holm",
    family = names(primary_tests),
    tests = primary_tests,
    results = primary_results
  ),
  primary_tests_output
)

# Earlier question-specific CSVs retain their original unadjusted results
# Use the combined table above for final primary-test decisions


# Checking that the outputs were created 

primary_outputs <- c(
  primary_results_output,
  primary_tests_output
)

stopifnot(
  all(file.exists(primary_outputs)),
  all(file.info(primary_outputs)$size > 0)
)

cat(
  "\nSection 8 completed.",
  "\nPrimary tests adjusted:", nrow(primary_results),
  "\nAdjustment method: Holm.",
  "\nSignificance level:", alpha,
  "\nNull hypotheses rejected after adjustment:",
  sum(primary_results$p_value_holm <= alpha),
  "\nDecisions changed by adjustment:",
  sum(primary_results$decision_changed),
  "\n"
)

print(primary_outputs)












# SECTION 9: Checking sensitivity to the duplicate-record policy 

# Checking that the required datasets and main results are available
stopifnot(
  exists("price_data_all_records"),
  exists("quarter_data_all_records"),
  exists("categorical_data_all_records"),
  exists("primary_results"),
  exists("output_dir"),
  exists("alpha"),
  nrow(price_data_all_records) == 5074,
  nrow(quarter_data_all_records) == 676,
  nrow(categorical_data_all_records) == 5964,
  nrow(primary_results) == 5,
  identical(
    primary_results$question,
    c("RQ1", "RQ2", "RQ3", "RQ4", "RQ5")
  ),
  all(primary_results$alpha == alpha)
)

cat(
  "\nDuplicate-policy sensitivity check",
  "\nMain analysis: Prepared datasets with the main duplicate policy.",
  "\nSensitivity analysis: All formatted records, retaining duplicates.",
  "\nThe eligibility rules for each research question remain unchanged.",
  "\n"
)


# Checking the sensitivity datasets 

stopifnot(
  is.numeric(price_data_all_records$price_eur),
  inherits(price_data_all_records$sale_date, "Date"),
  is.factor(price_data_all_records$period),
  !anyNA(price_data_all_records$price_eur),
  !anyNA(price_data_all_records$sale_date),
  !anyNA(price_data_all_records$period),
  all(is.finite(price_data_all_records$price_eur)),
  all(is.finite(as.numeric(price_data_all_records$sale_date))),
  length(unique(price_data_all_records$price_eur)) > 1,
  length(unique(price_data_all_records$sale_date)) > 1,
  all(
    price_data_all_records$sale_date >= as.Date("2019-01-01")
  ),
  all(
    price_data_all_records$sale_date <= as.Date("2025-12-31")
  ),
  is.numeric(quarter_data_all_records$price_eur),
  is.factor(quarter_data_all_records$quarter),
  !anyNA(quarter_data_all_records$price_eur),
  !anyNA(quarter_data_all_records$quarter),
  all(is.finite(quarter_data_all_records$price_eur)),
  is.factor(categorical_data_all_records$period),
  is.factor(categorical_data_all_records$property_status),
  is.factor(categorical_data_all_records$not_full_market_price),
  !anyNA(categorical_data_all_records$period),
  !anyNA(categorical_data_all_records$property_status),
  !anyNA(categorical_data_all_records$not_full_market_price)
)

# Checking that all required groups are present
stopifnot(
  setequal(
    levels(droplevels(price_data_all_records$period)),
    c("2019", "2020-2021", "2022-2025")
  ),
  setequal(
    levels(droplevels(quarter_data_all_records$quarter)),
    c("Q1", "Q2", "Q3", "Q4")
  ),
  setequal(
    levels(droplevels(categorical_data_all_records$period)),
    c("2019", "2020-2021", "2022-2025")
  ),
  setequal(
    levels(droplevels(categorical_data_all_records$property_status)),
    c("New", "Second-hand")
  ),
  setequal(
    levels(
      droplevels(
        categorical_data_all_records$not_full_market_price
      )
    ),
    c("No", "Yes")
  )
)


# Creating the sensitivity contingency tables 

sensitivity_period <- factor(
  categorical_data_all_records$period,
  levels = c("2019", "2020-2021", "2022-2025")
)

rq4_sensitivity_table <- table(
  period = sensitivity_period,
  property_status = factor(
    categorical_data_all_records$property_status,
    levels = c("New", "Second-hand")
  )
)

rq5_sensitivity_table <- table(
  period = sensitivity_period,
  not_full_market_price = factor(
    categorical_data_all_records$not_full_market_price,
    levels = c("No", "Yes")
  )
)

stopifnot(
  sum(rq4_sensitivity_table) ==
    nrow(categorical_data_all_records),
  sum(rq5_sensitivity_table) ==
    nrow(categorical_data_all_records),
  all(rowSums(rq4_sensitivity_table) > 0),
  all(colSums(rq4_sensitivity_table) > 0),
  all(rowSums(rq5_sensitivity_table) > 0),
  all(colSums(rq5_sensitivity_table) > 0)
)

# Calculating expected counts before applying Pearson's test
rq4_sensitivity_expected <- outer(
  rowSums(rq4_sensitivity_table),
  colSums(rq4_sensitivity_table)
) / sum(rq4_sensitivity_table)

rq5_sensitivity_expected <- outer(
  rowSums(rq5_sensitivity_table),
  colSums(rq5_sensitivity_table)
) / sum(rq5_sensitivity_table)

dimnames(rq4_sensitivity_expected) <-
  dimnames(rq4_sensitivity_table)

dimnames(rq5_sensitivity_expected) <-
  dimnames(rq5_sensitivity_table)

cat("\nSensitivity RQ4: Expected counts\n")
print(round(rq4_sensitivity_expected, 2))

cat("\nSensitivity RQ5: Expected counts\n")
print(round(rq5_sensitivity_expected, 2))

# Using the same conservative expected-count check as the main analysis
if (
  any(rq4_sensitivity_expected < 5) ||
  any(rq5_sensitivity_expected < 5)
) {
  stop(
    paste(
      "At least one sensitivity expected count is below 5.",
      "Review the tables before proceeding."
    )
  )
}


# Rerunning the five primary tests

rq1_sensitivity_test <- kruskal.test(
  price_eur ~ period,
  data = price_data_all_records
)

rq2_sensitivity_test <- kruskal.test(
  price_eur ~ quarter,
  data = quarter_data_all_records
)

rq3_sensitivity_test <- cor.test(
  x = as.numeric(price_data_all_records$sale_date),
  y = price_data_all_records$price_eur,
  method = "spearman",
  alternative = "two.sided",
  exact = FALSE
)

rq4_sensitivity_test <- chisq.test(
  rq4_sensitivity_table,
  correct = FALSE
)

rq5_sensitivity_test <- chisq.test(
  rq5_sensitivity_table,
  correct = FALSE
)

sensitivity_tests <- list(
  RQ1 = rq1_sensitivity_test,
  RQ2 = rq2_sensitivity_test,
  RQ3 = rq3_sensitivity_test,
  RQ4 = rq4_sensitivity_test,
  RQ5 = rq5_sensitivity_test
)

# Extracting full-precision sensitivity p-values
sensitivity_p_values <- vapply(
  sensitivity_tests,
  function(test) unname(test$p.value),
  numeric(1)
)

stopifnot(
  all(is.finite(sensitivity_p_values)),
  all(sensitivity_p_values >= 0),
  all(sensitivity_p_values <= 1)
)

# Adjusting across the five sensitivity tests only
sensitivity_p_values_holm <- p.adjust(
  sensitivity_p_values,
  method = "holm"
)


# Calculating sensitivity effect sizes 

# Defining a helper for eta-squared based on H
sensitivity_eta_H <- function(test, records, groups) {
  
  max(
    0,
    (
      unname(test$statistic) - groups + 1
    ) / (
      records - groups
    )
  )
}

# Defining a helper for Cramer's V
sensitivity_cramers_v <- function(test) {
  
  sqrt(
    unname(test$statistic) /
      (
        sum(test$observed) *
          min(dim(test$observed) - 1)
      )
  )
}

sensitivity_effect_sizes <- c(
  sensitivity_eta_H(
    rq1_sensitivity_test,
    nrow(price_data_all_records),
    nlevels(droplevels(price_data_all_records$period))
  ),
  sensitivity_eta_H(
    rq2_sensitivity_test,
    nrow(quarter_data_all_records),
    nlevels(droplevels(quarter_data_all_records$quarter))
  ),
  unname(rq3_sensitivity_test$estimate),
  sensitivity_cramers_v(rq4_sensitivity_test),
  sensitivity_cramers_v(rq5_sensitivity_test)
)

stopifnot(
  all(is.finite(sensitivity_effect_sizes))
)


# Creating the sensitivity-results table

sensitivity_results <- data.frame(
  question = names(sensitivity_tests),
  research_question = primary_results$research_question,
  test = primary_results$test,
  duplicate_policy = "All formatted records retained",
  
  records = c(
    nrow(price_data_all_records),
    nrow(quarter_data_all_records),
    nrow(price_data_all_records),
    sum(rq4_sensitivity_table),
    sum(rq5_sensitivity_table)
  ),
  
  statistic_name = primary_results$statistic_name,
  
  statistic = vapply(
    sensitivity_tests,
    function(test) unname(test$statistic),
    numeric(1)
  ),
  
  df = c(
    unname(rq1_sensitivity_test$parameter),
    unname(rq2_sensitivity_test$parameter),
    NA_real_,
    unname(rq4_sensitivity_test$parameter),
    unname(rq5_sensitivity_test$parameter)
  ),
  
  effect_size_name = primary_results$effect_size_name,
  effect_size = sensitivity_effect_sizes,
  p_value = unname(sensitivity_p_values),
  p_value_holm = unname(sensitivity_p_values_holm),
  adjustment_method = "Holm across five sensitivity tests",
  alpha = alpha,
  row.names = NULL,
  stringsAsFactors = FALSE
)

sensitivity_results$decision_holm <- ifelse(
  sensitivity_results$p_value_holm <= alpha,
  "Reject H0",
  "Insufficient evidence to reject H0"
)

stopifnot(
  identical(
    sensitivity_results$question,
    primary_results$question
  ),
  all(is.finite(sensitivity_results$statistic)),
  all(sensitivity_results$p_value_holm >=
        sensitivity_results$p_value),
  all(sensitivity_results$p_value_holm <= 1)
)


# Comparing the main and sensitivity analyses 

duplicate_policy_comparison <- data.frame(
  question = primary_results$question,
  research_question = primary_results$research_question,
  
  main_records = primary_results$records,
  all_records = sensitivity_results$records,
  
  extra_records = sensitivity_results$records -
    primary_results$records,
  
  statistic_name = primary_results$statistic_name,
  main_statistic = primary_results$statistic,
  all_records_statistic = sensitivity_results$statistic,
  
  effect_size_name = primary_results$effect_size_name,
  main_effect_size = primary_results$effect_size,
  all_records_effect_size = sensitivity_results$effect_size,
  
  effect_size_difference = sensitivity_results$effect_size -
    primary_results$effect_size,
  
  main_p_value = primary_results$p_value,
  all_records_p_value = sensitivity_results$p_value,
  
  main_p_value_holm = primary_results$p_value_holm,
  all_records_p_value_holm = sensitivity_results$p_value_holm,
  
  main_decision_holm = primary_results$decision_holm,
  all_records_decision_holm = sensitivity_results$decision_holm,
  
  row.names = NULL,
  stringsAsFactors = FALSE
)

duplicate_policy_comparison$decision_changed <-
  duplicate_policy_comparison$main_decision_holm !=
  duplicate_policy_comparison$all_records_decision_holm

# Checking the expected differences in record counts
stopifnot(
  identical(
    as.numeric(duplicate_policy_comparison$extra_records),
    c(4, 1, 4, 7, 7)
  )
)

# Checking whether the direction of the date-price association changed
rq3_direction_changed <- (
  sign(primary_results$effect_size[
    primary_results$question == "RQ3"
  ]) !=
    sign(sensitivity_results$effect_size[
      sensitivity_results$question == "RQ3"
    ])
)


# Displaying a readable comparison 

duplicate_policy_display <- data.frame(
  question = duplicate_policy_comparison$question,
  main_n = duplicate_policy_comparison$main_records,
  all_records_n = duplicate_policy_comparison$all_records,
  
  main_effect_size = round(
    duplicate_policy_comparison$main_effect_size,
    4
  ),
  
  all_records_effect_size = round(
    duplicate_policy_comparison$all_records_effect_size,
    4
  ),
  
  main_p_holm = format.pval(
    duplicate_policy_comparison$main_p_value_holm,
    digits = 4
  ),
  
  all_records_p_holm = format.pval(
    duplicate_policy_comparison$all_records_p_value_holm,
    digits = 4
  ),
  
  decision_changed = duplicate_policy_comparison$decision_changed,
  row.names = NULL
)

cat("\nDuplicate-policy sensitivity comparison\n")
print(duplicate_policy_display, row.names = FALSE)


# Saving the sensitivity results and comparison 

sensitivity_output <- file.path(
  output_dir,
  "16_all_records_primary_results_holm.csv"
)

comparison_output <- file.path(
  output_dir,
  "17_duplicate_policy_sensitivity_comparison.csv"
)

sensitivity_objects_output <- file.path(
  output_dir,
  "18_duplicate_policy_sensitivity_objects.rds"
)

write.csv(
  sensitivity_results,
  sensitivity_output,
  row.names = FALSE
)

write.csv(
  duplicate_policy_comparison,
  comparison_output,
  row.names = FALSE
)

saveRDS(
  list(
    alpha = alpha,
    adjustment_method = "holm",
    tests = sensitivity_tests,
    sensitivity_results = sensitivity_results,
    comparison = duplicate_policy_comparison,
    rq3_direction_changed = rq3_direction_changed
  ),
  sensitivity_objects_output
)


# Checking that the outputs were created 

sensitivity_outputs <- c(
  sensitivity_output,
  comparison_output,
  sensitivity_objects_output
)

stopifnot(
  all(file.exists(sensitivity_outputs)),
  all(file.info(sensitivity_outputs)$size > 0)
)

cat(
  "\nSection 9 completed.",
  "\nSensitivity primary tests:", nrow(sensitivity_results),
  "\nAdjustment: Holm across the five sensitivity tests.",
  "\nPrimary decisions changed:",
  sum(duplicate_policy_comparison$decision_changed),
  "\nRQ3 association direction changed:",
  rq3_direction_changed,
  "\n"
)

if (
  !any(duplicate_policy_comparison$decision_changed) &&
  !rq3_direction_changed
) {
  
  cat(
    "\nAll five Holm-adjusted decisions and the RQ3 direction",
    "were unchanged under the alternative duplicate policy.",
    "\nReview the effect-size differences before describing",
    "the numerical results as stable.\n"
  )
  
} else {
  
  cat(
    "\nAt least one primary decision or the RQ3 direction changed.",
    "\nReview the comparison table before reporting robustness.\n"
  )
}

print(sensitivity_outputs)