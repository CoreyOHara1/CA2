# SECTION 1: Importing and inspecting the CA1 master ------------------
# Starting with the combined dataset from CA1, but keeping the original CSV unchanged.

# Checking project setup has been run
stopifnot(
  exists("project_dir"),
  exists("input_file"),
  file.exists(input_file)
)

# Creating a folder for the prep outputs
output_dir <- file.path(project_dir, "output")
dir.create(output_dir, showWarnings = FALSE)

# Reading master as text before setting the variable types
raw <- read.csv(
  input_file,
  check.names = FALSE,
  colClasses = "character",
  na.strings = c("", "NA"),
  fileEncoding = "UTF-8"
)

# Checking column names
expected_columns <- c(
  "Date of Sale (dd/mm/yyyy)",
  "Address",
  "County",
  "Eircode",
  "Price (\u20ac)",
  "Not Full Market Price",
  "VAT Exclusive",
  "Description of Property",
  "Property Size Description"
)

stopifnot(identical(names(raw), expected_columns))

# Checking the number of records, column names and current types
cat("Records imported:", nrow(raw), "\n")
cat("Variables imported:", ncol(raw), "\n")

names(raw)
str(raw)

# Counting missing values in each column
missing_audit <- data.frame(
  variable = names(raw),
  missing_records = colSums(is.na(raw)),
  missing_percent = round(100 * colMeans(is.na(raw)), 2),
  row.names = NULL
)

print(missing_audit, row.names = FALSE)

# Counting additional exact copies without removing them
duplicate_count <- sum(duplicated(raw))
cat("Additional exact copies:", duplicate_count, "\n")

# Checking the county and the values used in the categorical fields
table(raw[["County"]], useNA = "ifany")
table(raw[["Not Full Market Price"]], useNA = "ifany")
table(raw[["VAT Exclusive"]], useNA = "ifany")
table(raw[["Description of Property"]], useNA = "ifany")

# Saving the missing-value audit for the report
write.csv(
  missing_audit,
  file.path(output_dir, "01_missing_value_audit.csv"),
  row.names = FALSE
)


# SECTION 2: Formatting the variables

# Checking that the imported data and output folder are available
stopifnot(
  exists("raw"),
  exists("output_dir")
)

# Creating a working copy and keeping raw unchanged
sligo <- raw

# Renaming the columns for use in the analysis
names(sligo) <- c(
  "sale_date",
  "address",
  "county",
  "eircode",
  "price_eur",
  "not_full_market_price",
  "vat_exclusive",
  "property_status",
  "property_size_description"
)

# Removing surrounding spaces and treating empty text as missing
sligo[] <- lapply(sligo, function(x) {
  x <- trimws(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x
})

# Keeping the date text for checking the conversion
original_dates <- sligo$sale_date

# Converting the dates from day/month/year
sligo$sale_date <- as.Date(
  original_dates,
  format = "%d/%m/%Y"
)

# Removing currency symbols and commas before converting prices
sligo$price_eur <- as.numeric(
  gsub("[\u20ac,]", "", sligo$price_eur)
)

# Checking for failed conversions and non-positive prices
stopifnot(
  !anyNA(sligo$sale_date),
  !anyNA(sligo$price_eur),
  all(is.finite(sligo$price_eur)),
  all(sligo$price_eur > 0),
  all(
    format(sligo$sale_date, "%d/%m/%Y") == original_dates
  )
)

# Checking the values used in the Yes/No fields
stopifnot(
  !anyNA(sligo$not_full_market_price),
  !anyNA(sligo$vat_exclusive),
  all(sligo$not_full_market_price %in% c("No", "Yes")),
  all(sligo$vat_exclusive %in% c("No", "Yes"))
)

# Converting the Yes/No fields to factors
sligo$not_full_market_price <- factor(
  sligo$not_full_market_price,
  levels = c("No", "Yes")
)

sligo$vat_exclusive <- factor(
  sligo$vat_exclusive,
  levels = c("No", "Yes")
)

# Checking the property descriptions before shortening the labels
property_labels <- c(
  "New Dwelling house /Apartment",
  "Second-Hand Dwelling house /Apartment"
)

stopifnot(
  !anyNA(sligo$property_status),
  all(sligo$property_status %in% property_labels)
)

# Converting property status to a factor
sligo$property_status <- factor(
  sligo$property_status,
  levels = property_labels,
  labels = c("New", "Second-hand")
)

# Checking the county and converting it to a factor
stopifnot(
  !anyNA(sligo$county),
  all(sligo$county == "Sligo")
)

sligo$county <- factor(sligo$county)

# Checking that all imported records are still present
stopifnot(nrow(sligo) == nrow(raw))

# Inspecting the updated structure, date range and prices
str(sligo)
range(sligo$sale_date)
summary(sligo$price_eur)

# Checking the category counts after conversion
table(sligo$property_status, useNA = "ifany")
table(sligo$not_full_market_price, useNA = "ifany")
table(sligo$vat_exclusive, useNA = "ifany")

# Recording the updated variable types
variable_types <- data.frame(
  variable = names(sligo),
  data_type = vapply(
    sligo,
    function(x) paste(class(x), collapse = ", "),
    character(1)
  ),
  row.names = NULL
)

print(variable_types, row.names = FALSE)

# Saving the variable types for the report
write.csv(
  variable_types,
  file.path(output_dir, "02_variable_types.csv"),
  row.names = FALSE
)

cat(
  "\nSection 2 completed. Records retained:",
  nrow(sligo),
  "\n"
)



# SECTION 3: Reviewing the exact copies

# Checking that the original and formatted datasets are available
stopifnot(
  exists("raw"),
  exists("sligo"),
  nrow(raw) == nrow(sligo)
)

# Identifying additional exact copies in the original imported data
duplicate_rows <- duplicated(raw)

# Identifying all matching rows, including their first occurrences
matching_rows <- duplicate_rows |
  duplicated(raw, fromLast = TRUE)

# Creating a review table using the formatted values
# Keeping source positions aligned with the original imported data
duplicate_review <- data.frame(
  source_row = which(matching_rows),
  additional_copy = duplicate_rows[matching_rows],
  sligo[matching_rows, ],
  row.names = NULL
)

# Sorting matching records together for easier inspection
duplicate_review <- duplicate_review[
  order(
    duplicate_review$sale_date,
    duplicate_review$address,
    duplicate_review$price_eur,
    duplicate_review$source_row
  ),
]

# Displaying the fields needed for the initial review
print(
  duplicate_review[
    c(
      "source_row",
      "additional_copy",
      "sale_date",
      "address",
      "price_eur",
      "property_status",
      "not_full_market_price",
      "vat_exclusive"
    )
  ],
  row.names = FALSE
)

# Opening all fields in the RStudio data viewer
View(duplicate_review)

# Summarising the review without removing any records
cat("Additional exact copies:", sum(duplicate_rows), "\n")
cat("Records involved in exact matches:", sum(matching_rows), "\n")
cat("Records currently retained:", nrow(sligo), "\n")



# SECTION 4: Applying the duplicate policy

# Checking that the imported and formatted datasets are available
stopifnot(
  exists("raw"),
  exists("sligo"),
  exists("output_dir"),
  nrow(raw) == nrow(sligo)
)

# Identifying exact copies using all original imported columns
duplicate_rows <- duplicated(raw)

# Keeping all formatted records for the later sensitivity check
sligo_all_records <- sligo

# Retaining the first occurrence of each exact match
# Keeping sligo unchanged so this section can be rerun
sligo_clean <- sligo_all_records[!duplicate_rows, ]

# Resetting row names after subsetting
rownames(sligo_clean) <- NULL

# Checking the retained records against the original duplicate flags
stopifnot(
  nrow(sligo_all_records) == nrow(raw),
  nrow(sligo_clean) == nrow(raw) - sum(duplicate_rows),
  !anyDuplicated(raw[!duplicate_rows, ])
)

# Recording the effect of the duplicate policy
duplicate_audit <- data.frame(
  stage = c(
    "Records before duplicate handling",
    "Additional exact copies set aside",
    "Records retained for main analysis"
  ),
  records = c(
    nrow(sligo_all_records),
    sum(duplicate_rows),
    nrow(sligo_clean)
  )
)

print(duplicate_audit, row.names = FALSE)

# Saving the aggregate counts for the report
write.csv(
  duplicate_audit,
  file.path(output_dir, "03_duplicate_handling_audit.csv"),
  row.names = FALSE
)

# Displaying the sizes of both working datasets
cat(
  "\nRecords retained for the main analysis:",
  nrow(sligo_clean),
  "\n"
)

cat(
  "Records retained for the sensitivity check:",
  nrow(sligo_all_records),
  "\n"
)

# Recording the decision for the report
duplicate_policy <- c(
  "Exact matches were identified across all nine imported columns.",
  "The first occurrence of each exact match was retained for the main analysis.",
  "Seven additional exact copies were set aside under this policy.",
  "The full formatted dataset was retained for a later sensitivity check.",
  "Exact matching does not establish that the source records are erroneous."
)

writeLines(
  duplicate_policy,
  file.path(output_dir, "03_duplicate_policy.txt")
)

