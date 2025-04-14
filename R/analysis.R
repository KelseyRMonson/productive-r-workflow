# Productive R Course
library(tidyverse)
library(dplyr)
library(ggplot2)

## Intro (bad examples) ----
# Base R example highlighting the limitations and drawbacks of many sections
setwd("\\\\researchsan02b.mssm.edu/shr2/immunobio/Zamarin_Lab/Kelsey/Work/productive-r-workflow")
## one day this path will be outdated

data <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")
## read.csv: Not necessarily wrong. But if the file was originally a .xlsx file, the .csv step wasn't necessary. 
## Also: You are reading a file from the web! One day, this URL will be broken and your work lost forever!
## Not the best file name 🙃 Usually a sign that you don't want to lose changes in data.csv. We will see how to avoid this, and never modify raw data manually!

summary(data)

print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Torgersen")$bill_length_mm),2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Biscoe")$bill_length_mm),2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Dream")$bill_length_mm),2))
## Those 3 lines are very repetitive. It is time to learn how to create a function. Removing duplication will boost your productivity.
## Using nested functions makes code hard to read. Using the pipe operator will make things smother for future you and collaborators.


# Plot
penguins_clean <- na.omit(   data  ) #That's a lot of space around data? 🤔 In 1 click, you can remove all the weird formatting issues in this file!
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm, type='n', xlab='Bill Length (mm)', ylab='Bill Depth (mm)', main='Penguin Bill Dimensions')
## Using ggplot2 and more generally the tidyverse in this file will make everything simpler for you and people who read your code.
points(
  penguins_clean$bill_length_mm[penguins_clean$species  ==  "Adelie"], penguins_clean$bill_depth_mm[penguins_clean$species == "Adelie"], col='red', pch=16)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Chinstrap"], penguins_clean$bill_depth_mm[penguins_clean$species == "Chinstrap"], col='green', pch=17)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Gentoo"],
       penguins_clean$bill_depth_mm[penguins_clean$species == "Gentoo"], col='blue', pch=18)
legend("topright", legend=unique(penguins_clean$species),
       col=c('red'
             , 'green',
             'blue'), pch=c(16, 17, 18))

## {Tidyverse} ----
# Seqera AI suggested update using dplyr and ggplot2
# Load required libraries
library(tidyverse)
library(readxl)

## No setwd() call as we have created an R Project for this code

# Read the data
# data <- read_csv("input/data.csv")
## Initial analysis was using .csv file, but we have updated to .xlsx below instead
## It uses read_csv() from readr (part of tidyverse) instead of read.csv(). This is generally faster and creates a tibble instead of a data frame.
## Removed reference to url and instead loads from relative path within the project directory

data <- read_excel("input/data.xlsx", na = "NA")

# Display summary statistics
summary(data)

# Calculate mean bill length for Adelie penguins on different islands
data %>%
  filter(species == "Adelie") %>%
  group_by(island) %>%
  summarise(mean_bill_length = round(mean(bill_length_mm, na.rm = TRUE), 2)) %>%
  print()
## It uses dplyr functions like filter(), group_by(), and summarise() to calculate the mean bill length for Adelie penguins on different islands. This replaces the repetitive subset() and mean() calls.

# Clean data by removing NA values
penguins_clean <- data %>% drop_na()
## It uses drop_na() instead of na.omit() to remove rows with NA values.

# Create the plot
ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point() +
  labs(
    x = "Bill Length (mm)",
    y = "Bill depth (mm)",
    title = "Penguin Bill Dimensions",
    color = "Species",
    shape = "Species"
  ) +
  scale_color_manual(values = c("Adelie" = "red", "Chinstrap" = "green", "Gentoo" = "blue")) +
  scale_shape_manual(values = c("Adelie" = 16, "Chinstrap" = 17, "Gentoo" = 18)) +
  theme_minimal()
## It uses ggplot2 to create the scatter plot, which offers more flexibility and consistency in plotting.
## The color and shape aesthetics are mapped directly in the aes() function, simplifying the code.
## The legend is automatically created by ggplot2, eliminating the need for a separate legend() call.

## Functions ----
# Function to calculate mean bill length for the 3 species, and then call 3 times for each island 
calc_mean_bill <- function(island_name) {
  filtered_data <- subset(na.omit(data), species == "Adelie" & island == island_name)
  mean_bill_length <- mean(filtered_data$bill_length_mm)
  return(round(mean_bill_length, 2))
}

# Call the function for each island
calc_mean_bill("Torgersen")
calc_mean_bill("Biscoe")
calc_mean_bill("Dream")

# Function to multiply a value by 234
## My function:
multiply_by_twothreefour <- function(a) {
  return(a * 234)
}

multiply_by_twothreefour(311)

## Suggested function:
multiplyBy234 <- function(x) {
  return(x * 234)
}

# Function to sum two numbers
## My function:
sumup <- function(a,b) {
  return(a+b)
}

sumup(3256,8934)

## Suggested function:
sum_two_numbers <- function(a, b) {
  return(a + b)
}

## Example for even number
is_even <- function(number) {
  return(number %% 2 == 0)
}

is_even(221)

### Function for bill length and bill depth ----
# Create the plot function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species, island == selected_island)
  # Create scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )

  return(plot)
}

create_scatterplot(data,"Adelie","Torgersen")
create_scatterplot(data,"Adelie","Biscoe")
create_scatterplot(data,"Adelie","Dream")

create_scatterplot(data,"Chinstrap","Torgersen")
create_scatterplot(data,"Chinstrap","Biscoe")
create_scatterplot(data,"Chinstrap","Dream")

create_scatterplot(data,"Gentoo","Torgersen")
create_scatterplot(data,"Gentoo","Biscoe")
create_scatterplot(data,"Gentoo","Dream")

# Split your Code ----
## Created a functions.R file in the R/ directory with the create_scatterplot function above
## Using source() to read in the function contained in it
source(file="R/functions.R")

create_scatterplot(data,"Adelie","Torgersen")

data <- readRDS("input/clean_data.rds")

# Summary
summary(data)



# Key Takeaways ----
### To auto-format your code: ----
# Click the addins button → style active file (uses the "styler" package)

### File structure ----
# Best practices for self-contained project file structure:
# project/
#   - README.md   # Project description written in Markdown (we will talk about this later)
#   - R/          # Where the R scripts live
#   - input/      # Data files
#   - output/     # Results: plots, tables...

### Benefits of R Projects ----
# 1️⃣ No more setwd()
# When you open a RStudio project using the the .Rproj icon, RStudio automatically set the R working directory to be the root of the folder.
# You can check by typing the getwd() command in the console!
  # It means that you don't need to use setwd() anymore. Loading a dataset just requires to type:
  # data <- read.csv("data.csv")

# 2️⃣ Isolation of Workspaces
# Let's say you are working on several projects in parallel. R Studio allows to easily switch from one to the other.
# Check the little dropdown menu at the top right of the screen
# When you switch to another project the current working directory is updated automatically.

# 3️⃣ Project settings
# Many settings are available to customize your project. 
# You can access them by clicking the .RProj file in the Files tab of the bottom right panel.
# It is advised to turn of the 'Restore .RData', 'Save workspace' and 'save history' options

### Benefits of readxl ----
# → Read a specific sheet:
  data <- read_excel("data.xlsx", sheet = "sheetNameOrNumber")

# → Specify how missing data are represented:
  data <- read_excel("data.xlsx", na = "-")

# → Specify column types:
  # readxl is pretty good at guessing. But you can specify manually:
  data <- read_excel("data.xlsx", col_types = c("date", "skip", "guess", "numeric"))

 ### I finally get Git?? ----
  # You can use GitHub Desktop to LINK your local project folder with GitHub
  # That way you can still run your code and analyses locally
  # But update the code on GitHub with a commit!