library(deplyr)
library(readxl)

# Read data using readxl
data <- read_excel("input/data.xlsx", na = "NA")

# Remove 2 rows known to be wrong
clean_data <- data %>%
  slice(-c(23, 48))

# Save in RDS format
saveRDS(clean_data, file = "input/clean_data.rds")
