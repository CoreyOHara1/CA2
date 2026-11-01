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
  exists("normality_summary"),
  nrow(price_data) == 5070
)

# Checking that the RQ1 variables are suitable for testing
stopifnot(
  is.numeric(price_data$price_eur),
  is.factor(price_data$period),
  !anyNA(price_data$price_eur),
  !anyNA(price_data$period)
)

# Confirming the RQ1 hypothesis before testing
cat(
  "\nRQ1 hypothesis",
  "\nH0: The recorded-price distributions are the same across 2019, 2020-2021 and 2022-2025.",
  "\nH1: At least one period has a different recorded-price distribution.",
  "\nTest: Kruskal-Wallis rank sum test.",
  "\n"
)

# Running the Kruskal-Wallis test for RQ1
rq1_test <- kruskal.test(
  price_eur ~ period,
  data = price_data
)

# Displaying the RQ1 test result
cat("\nRQ1: Kruskal-Wallis rank sum test\n")
print(rq1_test)

# Checking that the test returned a valid result
stopifnot(
  inherits(rq1_test, "htest"),
  is.finite(rq1_test$statistic),
  is.finite(rq1_test$p.value)
)

# Calculating epsilon-squared from the Kruskal-Wallis statistic
rq1_epsilon_squared <-
  (rq1_test$statistic - length(unique(price_data$period)) + 1) /
  (nrow(price_data) - length(unique(price_data$period)))

# Displaying the RQ1 effect size
cat(
  "\nRQ1 epsilon-squared effect size:",
  round(unname(rq1_epsilon_squared), 4),
  "\n"
)

# Creating a compact table of RQ1 descriptive statistics
rq1_descriptives <- do.call(
  rbind,
  lapply(
    split(
      price_data$price_eur,
      price_data$period,
      drop = TRUE
    ),
    function(prices) {
      data.frame(
        records = length(prices),
        median_eur = median(prices),
        mean_eur = mean(prices),
        q1_eur = quantile(prices, 0.25),
        q3_eur = quantile(prices, 0.75),
        minimum_eur = min(prices),
        maximum_eur = max(prices),
        row.names = NULL
      )
    }
  )
)

# Adding the period names to the descriptive table
rq1_descriptives$period <- names(
  split(
    price_data$price_eur,
    price_data$period,
    drop = TRUE
  )
)

# Reordering the descriptive table
rq1_descriptives <- rq1_descriptives[
  c("period", "records", "median_eur", "mean_eur", "q1_eur", "q3_eur", "minimum_eur", "maximum_eur")
]

# Displaying the RQ1 descriptive statistics
cat("\nRQ1: Recorded-price descriptive statistics by period\n")
print(rq1_descriptives, row.names = FALSE)

# Saving the RQ1 descriptive statistics
write.csv(
  rq1_descriptives,
  file.path(output_dir, "03_RQ1_price_descriptives_by_period.csv"),
  row.names = FALSE
)

# Creating all pairwise period comparisons
period_pairs <- combn(
  levels(droplevels(price_data$period)),
  2,
  simplify = FALSE
)

# Running a Wilcoxon rank-sum test for each pair
rq1_pairwise_results <- do.call(
  rbind,
  lapply(period_pairs, function(pair) {
    
    # Selecting the two periods being compared
    pairwise_data <- price_data[
      price_data$period %in% pair,
    ]
    
    # Dropping unused factor levels
    pairwise_data$period <-
      droplevels(pairwise_data$period)
    
    # Running the pairwise test
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

# Adjusting the pairwise p-values using Holm's method
rq1_pairwise_results$p_value_holm <- p.adjust(
  rq1_pairwise_results$p_value,
  method = "holm"
)

# Identifying comparisons that remain significant after adjustment
rq1_pairwise_results$significant_at_0.05 <-
  rq1_pairwise_results$p_value_holm <= alpha

# Displaying the RQ1 pairwise results
cat("\nRQ1: Pairwise Wilcoxon rank-sum tests with Holm adjustment\n")
print(rq1_pairwise_results, row.names = FALSE)

# Saving the RQ1 test results
rq1_results <- data.frame(
  question = "RQ1",
  test = "Kruskal-Wallis rank sum test",
  statistic = unname(rq1_test$statistic),
  df = unname(rq1_test$parameter),
  p_value = rq1_test$p.value,
  epsilon_squared = unname(rq1_epsilon_squared),
  alpha = alpha,
  decision = ifelse(
    rq1_test$p.value <= alpha,
    "Reject H0",
    "Insufficient evidence to reject H0"
  ),
  row.names = NULL
)

write.csv(
  rq1_results,
  file.path(output_dir, "04_RQ1_kruskal_wallis_result.csv"),
  row.names = FALSE
)

write.csv(
  rq1_pairwise_results,
  file.path(output_dir, "05_RQ1_pairwise_wilcoxon_results.csv"),
  row.names = FALSE
)


# Creating the RQ1 price distribution chart

# Opening a PNG file for the RQ1 chart
png(
  file.path(figures_dir, "06_RQ1_price_distributions_by_period.png"),
  width = 2200,
  height = 1300,
  res = 180
)

# Preparing plotting limits based on all eligible prices
price_limits <- range(price_data$price_eur) / 1000

# Creating a boxplot of prices by period
boxplot(
  price_eur / 1000 ~ period,
  data = price_data,
  col = chart_colour,
  border = "black",
  horizontal = TRUE,
  las = 1,
  main = "Recorded prices by analytical period",
  xlab = "Recorded price (EUR thousands)",
  ylab = "Analytical period",
  ylim = price_limits,
  outline = FALSE
)

# Adding individual observations to show the full distribution
stripchart(
  price_eur / 1000 ~ period,
  data = price_data,
  method = "jitter",
  vertical = FALSE,
  pch = 16,
  cex = 0.35,
  col = adjustcolor("black", alpha.f = 0.15),
  add = TRUE
)

# Closing the PNG file
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

rq1_chart <- file.path(
  figures_dir,
  "06_RQ1_price_distributions_by_period.png"
)

stopifnot(
  all(file.exists(rq1_outputs)),
  all(file.info(rq1_outputs)$size > 0),
  file.exists(rq1_chart),
  file.info(rq1_chart)$size > 0
)

# Displaying the completed section
cat(
  "\nSection 3 completed.",
  "\nRQ1 Kruskal-Wallis statistic:", round(unname(rq1_test$statistic), 3),
  "\nRQ1 p-value:", format.pval(rq1_test$p.value, digits = 4),
  "\nRQ1 epsilon-squared:", round(unname(rq1_epsilon_squared), 4),
  "\nRQ1 pairwise comparisons:", nrow(rq1_pairwise_results),
  "\nRQ1 chart saved:", rq1_chart,
  "\n"
)

print(rq1_outputs)
print(rq1_chart)



# Additional RQ1 chart: Zooming in on the main price distributions ----

# Setting the visible price range in EUR thousands
rq1_zoom_limit <- 600

# Counting records above the visible range
rq1_above_zoom <- sum(
  price_data$price_eur > rq1_zoom_limit * 1000
)

# Saving a separate chart without overwriting the full-range version
png(
  file.path(
    figures_dir,
    "06b_RQ1_price_distributions_zoomed.png"
  ),
  width = 2200,
  height = 1400,
  res = 180
)

# Providing enough space for the period labels and explanatory note
par(
  mar = c(6, 8, 4, 2),
  las = 1
)

# Calculating boxplots using all records, with a zoomed price axis
boxplot(
  price_eur / 1000 ~ period,
  data = price_data,
  horizontal = TRUE,
  col = "#FF7F50",
  border = "black",
  outline = FALSE,
  ylim = c(0, rq1_zoom_limit),
  main = "Recorded prices by analytical period",
  xlab = "Recorded price (EUR thousands)",
  ylab = ""
)

# Adding individual observations within the plotting window
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

# Explaining the zoom without suggesting records were deleted
mtext(
  paste0(
    "Zoomed to EUR 600,000; ",
    rq1_above_zoom,
    " records above this range are not shown. All records remain in the analysis."
  ),
  side = 1,
  line = 4,
  cex = 0.75
)

dev.off()








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










