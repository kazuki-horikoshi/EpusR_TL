library(here)
library(eplusr)

# first tell R where to find the weather file
path_epw <- here("data", "USA_PA_Pittsburgh-Allegheny", "USA_PA_Pittsburgh-Allegheny.County.AP.725205_TMY3.epw")
epw <- read_epw(path_epw)

###############################################################################
# Here we are extracting the weather data from the epw object that we 
# loaded into R earlier using read_epw() and visualizing it
###############################################################################

df <- epw$data() %>%
  select(datetime, dry_bulb_temperature)

# The function class() can be used to reveal the class of the object
class(df)

class(df$datetime)

july_data <- df %>% filter(month == 7)
df_task1 <- july_data %>%
  mutate(day = as.Date(datetime))

# Use aggregate function to calculate the daily average temperature
df_daily_avg <- aggregate(df_task1$dry_bulb_temperature, by = list(df_task1$date), FUN = mean, na.rm = TRUE)

# Rename the columns for clarity
colnames(df_daily_avg) <- c("date", "avg_temperature")

# Display the new dataframe
df_daily_avg

# Load necessary library
library(ggplot2)

# Assuming df_daily_avg has 'date' and 'avg_temperature' as columns

# Create the plot
plot <- ggplot(df_daily_avg, aes(x = date, y = avg_temperature)) +
  geom_line(color = "blue") +  # Line plot for avg_temperature over date
  scale_y_continuous(limits = c(15, 30), breaks = seq(15, 30, by = 5)) +  # Set Y-axis range from 15 to 30 with breaks
  labs(title = "Daily Average Temperature in July", 
       x = "Day", 
       y = "Average Temperature (°C)") +
  theme_minimal() +  # Clean theme
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

# Set the size of the plot (18cm x 9cm)
ggsave("daily_avg_temp_plot.png", plot = plot, width = 18, height = 9, units = "cm")

# To display the plot
print(plot)
