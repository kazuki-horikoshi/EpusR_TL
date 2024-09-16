library(here)
library(eplusr)

# first tell R where to find the weather file
path_epw <- here("data", "USA_PA_Pittsburgh-Allegheny", "USA_PA_Pittsburgh-Allegheny.County.AP.725205_TMY3.epw")
epw <- read_epw(path_epw)

# Load necessary library
library(ggplot2)
library(RColorBrewer)


###############################################################################
# Here we are extracting the weather data from the epw object that we 
# loaded into R earlier using read_epw() and visualizing it
###############################################################################

df <- epw$data() %>%
  select(datetime, direct_normal_radiation, diffuse_horizontal_radiation)

weather_data <- read_csv(here("data", "weather_data.csv"))

# The function class() can be used to reveal the class of the object
class(df)

class(df$datetime)

# Extract the relevant columns from weather_data
radiation_data <- weather_data[, c("direct_normal_radiation", "diffuse_horizontal_radiation", "datetime")]

# Inspect the extracted data
head(radiation_data)

# Load the tidyr package
library(tidyr)

# Reshape the data into long format using pivot_longer
radiation_long <- radiation_data %>%
  pivot_longer(cols = c("direct_normal_radiation", "diffuse_horizontal_radiation"),
               names_to = "radiation_type",
               values_to = "radiation_value")

# Inspect the reshaped data
head(radiation_long)
head(radiation_long)

##Step 2: Visualize the direct and diffuse radiation

# Plot direct and diffuse radiation over time
ggplot(radiation_data, aes(x = datetime)) +
  geom_line(aes(y = direct_normal_radiation, color = "Direct Normal Radiation")) +
  geom_line(aes(y = diffuse_horizontal_radiation, color = "Diffuse Horizontal Radiation")) +
  labs(title = "Direct Normal vs Diffuse Horizontal Radiation Over Time",
       x = "Date", 
       y = "Radiation (W/m²)", 
       color = "Radiation Type") +
  theme_minimal()


##Step 3: Compare monthly distributions

# Choose a color-blind-friendly palette from RColorBrewer
colors <- brewer.pal(2, "Set2")  # Set2 is good for distinct categories

# Create the boxplot, separating radiation types
ggplot(radiation_long, aes(x = factor(month), y = radiation_value, fill = radiation_type)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.7) +  # Position boxes side by side
  scale_fill_manual(values = colors) +  # Apply color palette
  labs(title = "Monthly Distribution of Direct Normal and Diffuse Horizontal Radiation",
       x = "Month", 
       y = "Radiation (W/m²)", 
       fill = "Radiation Type") +
  theme_minimal()

