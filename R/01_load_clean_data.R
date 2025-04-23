library(dplyr)
library(readxl)
library(gtExtras)

# Read dgtExtras# Read data using readxl
data <- read_excel("input/data.xlsx", na = "NA")

# Remove 2 rows known to be wrong
clean_data <- data %>%
  slice(-c(23, 48))

complete_data <- na.omit(clean_data)

clean_data$species <- factor(clean_data$species, levels = c("Adelie", "Chinstrap", "Gentoo"))

# Summarize using cool gtExtras package
clean_data %>%
  gt() %>%
  gt_theme_dot_matrix()

# create aggregated dataset
agg_clean = clean_data %>%
  group_by(species) %>%
  summarize(
    Bill.L = list(bill_length_mm),
    Bill.D = list(bill_depth_mm),
    Flipp.L = list(flipper_length_mm),
    BodyMass = list(body_mass_g)
  )

agg_clean %>%
  gt() %>%
  gt_plt_sparkline(Bill.L) %>%
  gt_plt_sparkline(Bill.D) %>%
  gt_plt_sparkline(Flipp.L) %>%
  gt_plt_sparkline(BodyMass)

agg_clean %>%
  gt() %>%
  gt_plt_dist(Bill.L) %>%
  gt_plt_dist(Bill.D) %>%
  gt_plt_dist(Flipp.L) %>%
  gt_plt_dist(BodyMass)

agg_clean %>%
  gt() %>%
  gt_plt_dist(
    Bill.L,
    type = "density" 
  ) %>%
  gt_plt_dist( 
    Bill.D,
    type = "boxplot"
  ) %>%
  gt_plt_dist( 
    Flipp.L,
    type = "histogram"
  ) %>%
  gt_plt_dist(
    BodyMass,
    type = "rug_strip"
  )

## The gt_plt_bar_pct() does not require aggregate data. 
## The chart is actually a score bar that measures how close the value in the cell is to the maximum value in that column.
## This means that the highest value in the table has its bar full.
## Can't handle NAs which is why I'm using `complete_data`

head(complete_data) %>%
  gt() %>%
  gt_plt_bar_pct(
    bill_length_mm,
    labels = TRUE
  ) %>%
  gt_plt_bar_pct(
    bill_depth_mm,
    labels=FALSE,
    fill = "forestgreen"
  )

tail(complete_data) %>%
  gt() %>%
  gt_plt_bar_pct(
    bill_length_mm,
    labels = TRUE
  ) %>%
  gt_plt_bar_pct(
    bill_depth_mm,
    labels=FALSE,
    fill = "forestgreen"
  )

complete_data %>%
  gt() %>%
  gt_plt_bar_pct(
    bill_length_mm,
    labels = TRUE
  ) %>%
  gt_plt_bar_pct(
    bill_depth_mm,
    labels=FALSE,
    fill = "forestgreen"
  )


# Save in RDS format
saveRDS(clean_data, file = "input/clean_data.rds")


