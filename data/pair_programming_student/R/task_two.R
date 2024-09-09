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
  select(datetime, direct_normal_radiation, diffuse_horizontal_radiation)

# The function class() can be used to reveal the class of the object
class(df)

class(df$datetime)
