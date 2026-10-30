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
