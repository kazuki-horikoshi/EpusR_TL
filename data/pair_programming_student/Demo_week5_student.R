# Author: Yiting Zhang
# Date: 27/8/2024
# Description: Materials for demo in Week 5

# --------------- Clear your environment --------------
rm(list = ls())

# ----------------- Recap Chapter 1-4 -----------------
### Basics
# 1. Operators (+, -, *, /, ^, %)
  
# 2. Data types (Logical: TRUE, FALSE;
#                Numeric: 1, 999.9;
#                Integer: 1L, 999L;
#                Character: "a", "R for BES";
#                ... ...)
  
# 3. Data structures (list, df, ...)

### Dates and Times (transform to other objects)

### Importing Data
# 1. here(): find your file
# 2. read_csv(): parse your file


# ------------------ Preparation -----------------------
# Preparation: import the tidyverse and ggplot2 packages in R
# You need to install the packages if you haven't already
# install.packages(" ")

library(tidyverse)
library(ggplot2)
library(lubridate)
library(here)

# ------------------- Load Data ------------------------
path <- here()

df <- read_csv(here("data", "df.csv"))
# df_path <- here("data","df.csv")
# df <- read_csv(df_path)

weather_data <- read_csv(here("data", "weather_data.csv"))

# ---------------- Useful Function ---------------------
# 1. filter(): you can filter your data frame based on a specific column value
# Usage
july_data <- df %>% filter(month == 7)

# 2. mutate() allows you to add new columns or modify existing ones without 
# changing the original data frame unless explicitly assigned back to the same variable
df_task1 <- july_data %>%
  mutate(day = as.Date(datetime))

# 3. select(): you can use this to choose specific columns from a data frame
# selected_data <- your_dataframe %>% select(column1, column2, column3)
# selected_data <- your_dataframe[, c("column1", "column2", "column3")]
# Usage
df_task2 <- weather_data %>% select(datetime, 
                                    month, 
                                    direct_normal_radiation, 
                                    diffuse_horizontal_radiation)
df_task3 <- weather_data[, c("datetime", "month", "dry_bulb_temperature")]

# 4. ggplot() & ggsave()
# ggplot(data, aes(x = x_var, y = y_var)) + geom_something()
  # data: The data frame that contains your variables.
  # aes(): Specifies the "aesthetics" of the plot, i.e., which variables to map to axes, color, size, etc.
  # geom_something(): The geometry to use for plotting (e.g., points, lines, bars).

# ggsave("filename.png", plot = last_plot(), width = 6, height = 4, dpi = 300)
  # "filename.png": The name and type of the file to save.
  # plot: The plot to save. If you omit this, it will save the most recent plot (last_plot()).
  # width and height: Specify the dimensions of the plot in inches (optional).
  # dpi: The resolution of the saved image (dots per inch).


# --------------------- Tasks --------------------------
## ############# Task One ###############

### Description: 
  # - Visualize the daily average temperature for the month of July using the 
  #   tidyverse and ggplot2 R packages. 
  # - The x-axis should represent the Days of the month, and the y-axis should 
  #   be Dry-bulb temperature (℃)
  # - Export the plot as a PDF file with width = 18cm and height = 9cm

### Hints on prompt (a bit of description on your DataFrame):
  # I have a DataFrame called df_task1 that records hourly data for an entire year.  
  # The DataFrame contains three columns: 
  # 'datetime' (formatted as 'YYYY-MM-DD HH:MM'), 
  # 'dry_bulb_temperature' (with values such as 7.2°C), 
  # and 'month' (represented as an integer from 1 to 12).

### Code (keep notes for better understanding):
  # 1. Calculate daily average temperature
  # 2. Create the plot as required
  # 3. Export the plot as a PDF








## ############# Task Two ###############

### Description;
  # - (get the weather data from epw file)
  # - Visualize direct normal radiation and diffuse horizontal radiation.
  # - Compare monthly distribution of direct and diffuse radiation data
  # - Select a color palette that (1) is color-blind-friendly and (2) is 
  #   suitable for categorical data (seasons). 

### Code (keep notes for better understanding):
  # 1. Filter the data for non-zero values
  # 2. Visualize monthly distributions of direct radiation using box plots
  # 3. (Optional) Create a side-by-side box plot for both direct and diffuse radiation
  








## ############# Task Three ###############
### Description;
# - Calculate the monthly cooling degree days and visualize them 
#   using the tidyverse and ggplot2 R packages.
# - Assume a CDD base temperature of 18 and a HDD base temperature of 10.

### Code (keep notes for better understanding):
# 1. Group the Data by Month and Calculate the Mean Temperature
# 2. Calculate and visualize Cooling Degree Days (CDD)
# 3. (Optional) Add Heating Degree Days (HDD) Calculation









