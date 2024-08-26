## Run optimization for impact analysis of individual comfort model on HVAC control
## 2021-03-24 E.Ono
## _rev: adapted to SinBerBEST model and so on, 2021-06-08

## 2024-08-14 K.Horikoshi
## _rev: adapted to SinBerBEST model with set comfort matrix, 2024-08-14

#####################################
# Load libraries
#####################################
# load the packages
library(eplusr)

#source( "C:/Users/F18863/OneDrive - KAJIMA/00_Program/R/ono_functions.R" )

# install here package if not exists
#install.packages("here")
library(here)

#if (!require("here", quietly = TRUE)) {
#   install.packages("here")
#   library(here)
# }

# turn off verbose information of eplusr package
eplusr_option(verbose_info = FALSE)
eplusr_option(autocomplete = TRUE)

#options(timeout = 2000)
#install_eplus(9.4)

# see what EnergyPlus has been installed
avail_eplus()

# use the example file from latest EnergyPlus installed
#ver <- max(avail_eplus())
ver <- "9.4.0"

# parse IDD
idd <- use_idd(ver, download = "auto")

path_wd <- "/home/rstudio/localdir"
path_epw <- paste0(path_wd,"/epw/SGP_Singapore.486980_IWEC.epw")

path_idf <- paste0(path_wd,"/AsimEx/cal/Singapore_Benchmark_Model_V940_ono.idf")
idf <- read_idf(path = path_idf, idd = NULL)

#job <- idf$run(path_epw, wait = TRUE)

#####################################
# Test for understanding the data structure
#####################################
## 2024-08-07 K.Horikoshi

path_idf <- paste0(path_wd,"/AsimEx/Singapore_Benchmark_Model_V940_ono_VAV.idf")
path_cal <- paste0(path_wd,"/AsimEx/cal/VAV") 
path_idf <- paste0(path_wd,"/AsimEx/Singapore_Benchmark_Model_V940_ono_Hybrid.idf")
path_cal <- paste0(path_wd,"/AsimEx/cal/Hybrid") 

idf <- read_idf(path = path_idf, idd = NULL)
idf$run(path_epw,
        dir = path_cal,
        wait = TRUE)
job <- idf$last_job()

tmp <- idf$"Schedule:Compact"[["Sch_Occupancy_Target"]]
sch_occupancy_target <- idf$"Schedule:Compact"[["Sch_Occupancy_Target"]]
dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))

tmp <- idf$"Schedule:Compact"[["zone_index"]]
dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
zone_index <- dt[6,6]


for (i in 1:12){
  dt[(2*i+6),6] <- occ_total[24*(Nday-1)+i+7]/100
}


### test occupancy information

Nocc <- 16 # number of occupant in the zone

path_wd <- "/home/rstudio/localdir"
path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant.csv")
occ_occupant <- read.csv(path_occ_occupant,header=T)
occ_total <- occ_occupant[,(Nocc+1)]


# get Occupancy based on Nday
# assume data are saved for 24rows each day

Nday<-10

start_row <- 24 * (Nday - 1) + 1
end_row <- start_row + 23
occ_day_data <- occ_occupant[start_row:end_row, 1:16]

print(occ_day_data)

occ_total <- occ_occupant[,(Nocc+1)]

print(occ_total)



tasp <- 27
tasp8 <- tasp
tasp9 <- tasp
tasp10 <- tasp
tasp11 <- tasp
tasp12 <- tasp
tasp13 <- tasp
tasp14 <- tasp
tasp15 <- tasp
tasp16 <- tasp
tasp17 <- tasp
tasp18 <- tasp
tasp19 <- tasp

#####################################
# Test for control logic
#####################################
## 2024-08-13 K.Horikoshi

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")

accep_data <- read.csv(path_accep)
# 
# ### Zone Control
# 
# # Filter the data where Fan Mode == 0
# fan_mode_0_data <- subset(accep_data, FanMode == 0)
# 
# # Subject set to columns 7 to 22 (User X2 to X24)
# acceptable_temperature_ranges <- fan_mode_0_data[apply(fan_mode_0_data[, 7:22], 1, function(row) {
#   mean(row == 1) >= 0.75
# }), "Indoor.Temp"]
# 
# # Result
# print(acceptable_temperature_ranges)
# 
# ### Personal Control
# 
# # Get unique temperature range
# unique_temperatures <- unique(accep_data$Indoor.Temp)
# 
# # Search optimal temperatures
# optimal_temperatures <- c()
# 
# for (temp in unique_temperatures) {
#   # Filter data for same temperature
#   temp_data <- subset(accep_data, Indoor.Temp == temp)
#   
#   # For subjecy in columns 7 to 22 (User X2 to X24), confirm over 75% user are comfortable
#   acceptable_rows <- apply(temp_data[, 7:22], 1, function(row) {
#     mean(row == 1) >= 0.75
#   })
#   
#   # Confirm achieving the requirement at least one row 
#   if (any(acceptable_rows)) {
#     optimal_temperatures <- c(optimal_temperatures, temp)
#   }
# }
# 
# # Result
# print(optimal_temperatures)


### Zone Control on time sequence and filter

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")
accep_data <- read.csv(path_accep)

system <- 1

if (system == 1) { # Zone Control
  # Define tasp list to update
  tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                 tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
  
  # Initialize ctrl_mode matrix
  ctrl_mode <- matrix(NA, nrow = 12, ncol = 2 + Nocc)
  colnames(ctrl_mode) <- c("Time", "Max_Temperature", paste0("FanMode_User_", 1:Nocc))
  
  for (time in 8:19) {  # Loop for each hour from 8 to 19
    # Extract occupancy data for the current hour
    occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour
    
    ctrl_mode[time - 7, 1] <- time  # Record the time
    
    if (all(occupancy_data == 0)) {
      # If all occupancy data is 0, record 30 in tasp_list
      tasp_list[time - 7] <- 30
      ctrl_mode[time - 7, 2] <- 30  # Set 30 as the max temperature
      ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
      
      acceptable_temperature_ranges <- NA  # Set to NA if no occupancy
      
    } else {
      # Filter accep_data where Fan Mode == 0
      fan_mode_0_data <- subset(accep_data, FanMode == 0)
      
      # Apply the occupancy filter to accep_data
      filtered_data <- fan_mode_0_data[apply(fan_mode_0_data[, 7:(6 + Nocc)], 1, function(row) {
        mean(row[occupancy_data == 1] == 1) >= 0.75
      }), ]
      
      # Identify temperature ranges where at least 75% of present users are comfortable
      acceptable_temperature_ranges <- filtered_data[,"Indoor.Temp"]
      
      # Update tasp based on the maximum acceptable temperature if available
      if (length(acceptable_temperature_ranges) > 0) {
        tasp_list[time - 7] <- max(acceptable_temperature_ranges)
        ctrl_mode[time - 7, 2] <- max(acceptable_temperature_ranges)  # Set the max temperature
      } else {
        tasp_list[time - 7] <- 30  # Set default temperature
        ctrl_mode[time - 7, 2] <- 30  # Set 30 as the max temperature
      }
      
      # Set Fan mode to 0 for present users and NA for absent users
      ctrl_mode[time - 7, 3:(2 + Nocc)] <- ifelse(occupancy_data == 1, 0, NA)
    }
    
    # Print the result for debugging
    print(paste("Time:", time, "Acceptable Temperature Ranges:"))
    print(acceptable_temperature_ranges)
    
    # Print the result for debugging
    print(paste("Time:", time, "Max Acceptable Temperature:"))
    print(tasp_list[time - 7])
  }
  
  # Output the results as a list
  list(tasp_list = tasp_list, ctrl_mode = ctrl_mode)
}

# if (system == 1) { # Zone Control
#   # Define tasp list to update
#   tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
#                  tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
#   
#   for (time in 8:19) {  # Loop for each hour from 8 to 19
#     # Extract occupancy data for the current hour
#     occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour
#     
#     if (all(occupancy_data == 0)) {
#       # If all occupancy data is 0, record 30 in tasp_list
#       tasp_list[time - 7] <- 30
#     } else {
#       # Filter accep_data where Fan Mode == 0
#       fan_mode_0_data <- subset(accep_data, FanMode == 0)
#       
#       # Apply the occupancy filter to accep_data
#       filtered_data <- fan_mode_0_data[apply(fan_mode_0_data[, 7:22], 1, function(row) {
#         mean(row[occupancy_data == 1] == 1) >= 0.75
#       }), ]
#       
#       # Identify temperature ranges where at least 75% of present users are comfortable
#       acceptable_temperature_ranges <- filtered_data[,"Indoor.Temp"]
#       
#       # Update tasp based on the maximum acceptable temperature if available
#       if (length(acceptable_temperature_ranges) > 0) {
#         tasp_list[time - 7] <- max(acceptable_temperature_ranges)
#       } else {
#         # If no acceptable temperature ranges found, you might want to set a default value or leave tasp unchanged
#         tasp_list[time - 7] <- 30  # Optionally set a default value if nothing is found
#       }
#     }
#     # Print the result for debugging
#     print(paste("Time:", time, "Acceptable Temperature Ranges:"))
#     print(acceptable_temperature_ranges)
#     
#     # Print the result for debugging
#     print(paste("Time:", time, "Max Acceptable Temperature:"))
#     print(tasp_list[time - 7])
#   }
# }
# 
# print(tasp_list)


### Group Control on time sequence and filter
# Based on Zone control as it considers only one fan mode, Need to modify but useful to consider group control

#Initial setting

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")
accep_data <- read.csv(path_accep)

Nocc <- 16  # Number of users
groups <- list(1:4, 5:8, 9:12, 13:16)  # 4 groups with 4 users each
system <- 2
if (system == 2) { # Group Control
  # Define tasp list to update
  tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                 tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
  
  # Initialize ctrl_mode matrix
  ctrl_mode <- matrix(NA, nrow = 12, ncol = 2 + Nocc)
  colnames(ctrl_mode) <- c("Time", "Max_Temperature", paste0("FanMode_User_", 1:Nocc))
  
  for (time in 8:19) {  # Loop for each hour from 8 to 19
    # Extract occupancy data for the current hour
    occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour
    
    ctrl_mode[time - 7, 1] <- time  # Record the time
    
    if (all(occupancy_data == 0)) {
      # If all occupancy data is 0, record 27 in tasp_list
      tasp_list[time - 7] <- 27
      ctrl_mode[time - 7, 2] <- 27  # Set 27 as the max temperature
      ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
    } else {
      # Get unique temperature range
      unique_temperatures <- unique(accep_data$Indoor.Temp)
      
      # Initialize variable to store maximum acceptable temperature
      max_temperature <- NA
      optimal_fan_modes <- rep(NA, Nocc)
      
      for (temp in unique_temperatures) {
        # Filter data for the same temperature
        temp_data <- subset(accep_data, Indoor.Temp == temp)
        
        # Check each group to see if at least 3 out of 4 users are comfortable with any fan mode
        group_satisfaction <- sapply(groups, function(group) {
          sapply(1:nrow(temp_data), function(row_idx) {
            selected_users <- group[occupancy_data[group] == 1]
            if (length(selected_users) >= 3) {
              return(mean(temp_data[row_idx, selected_users + 6] == 1) >= 0.75)  # Check if 3 out of 4 users are satisfied
            } else {
              return(FALSE)
            }
          })
        })
        
        # Check if any combination of fan modes for the groups allows the temperature to be accepted
        if (any(apply(group_satisfaction, 2, all))) {
          max_temperature <- max(max_temperature, temp, na.rm = TRUE)
          
          # Find the minimum fan mode for each user where Accep == 1 and occupancy is 1
          for (group_idx in 1:length(groups)) {
            group <- groups[[group_idx]]
            for (user_idx in group) {
              if (occupancy_data[user_idx] == 1) {
                user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1)
                if (length(user_fan_modes) > 0) {
                  optimal_fan_modes[user_idx] <- min(optimal_fan_modes[user_idx], user_fan_modes[1], na.rm = TRUE)
                }
              } else {
                optimal_fan_modes[user_idx] <- NA  # Set to NA if the user is not present
              }
            }
          }
        }
      }
      
      # Update tasp_list and ctrl_mode with the maximum acceptable temperature if available
      if (!is.na(max_temperature)) {
        tasp_list[time - 7] <- max_temperature
        ctrl_mode[time - 7, 2] <- max_temperature  # Set the max temperature
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- optimal_fan_modes  # Record Fan mode
      } else {
        tasp_list[time - 7] <- 27
        ctrl_mode[time - 7, 2] <- 27  # Set 27 as the max temperature
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
      }
    }
    
    # Print the result for debugging
    print(paste("Time:", time, "Max Acceptable Temperature:"))
    print(tasp_list[time - 7])
  }
  
  # Output the results as a list
  list(tasp_list = tasp_list, ctrl_mode = ctrl_mode)
}


print(tasp_list)


### Personal Control on time sequence and filter

#Initial setting

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")
accep_data <- read.csv(path_accep)

Nocc <- 16  # Number of users
system <- 3

##################################################################
### debug for personal comfort fan mode selection
##################################################################
# Define tasp list to update
# Define tasp list to update
tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
               tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)

# Initialize ctrl_mode matrix
ctrl_mode <- matrix(NA, nrow = 12, ncol = 2 + Nocc)
colnames(ctrl_mode) <- c("Time", "Max_Temperature", paste0("FanMode_User_", 1:Nocc))

# Fixed time for debugging
time <- 11

# Extract occupancy data for the current hour
occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour

ctrl_mode[time - 7, 1] <- time  # Record the time

# Get unique temperature range
unique_temperatures <- unique(accep_data$Indoor.Temp)

# Initialize variable to store maximum acceptable temperature
max_temperature <- NA
optimal_fan_modes <- rep(NA, Nocc)  # Initialize with NA

for (temp in unique_temperatures) {
  # Filter data for the same temperature
  temp_data <- subset(accep_data, Indoor.Temp == temp)
  
  # Check if at least 75% of users are comfortable with any fan mode (columns 7 to 22)
  user_satisfaction <- apply(temp_data[, 7:(7 + Nocc - 1)], 2, function(col) {
    max(col[occupancy_data == 1], na.rm = TRUE) == 1  # Check if user is satisfied with any fan mode
  })
  
  # Debugging: print user satisfaction status
  print(paste("Temperature:", temp, "User Satisfaction:", user_satisfaction))
  
  # Check if 75% or more users are satisfied with any fan mode
  if (mean(user_satisfaction, na.rm = TRUE) >= 0.75) {
    max_temperature <- max(max_temperature, temp, na.rm = TRUE)
    
    # Find the minimum fan mode for each user where Accep == 1 and occupancy is 1
    for (user_idx in 1:Nocc) {
      if (occupancy_data[user_idx] == 1) {
        user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1) - 1
        
        # Debugging: print available fan modes for the user
        print(paste("User index:", user_idx, "Fan modes available:", user_fan_modes))
        
        # Select the minimum fan mode from the available modes
        if (length(user_fan_modes) > 0) {
          optimal_fan_modes[user_idx] <- min(user_fan_modes)
        }
      } else {
        optimal_fan_modes[user_idx] <- NA  # Set to NA if the user is not present
      }
    }
  }
}



# Replace Inf with NA if no fan mode was found
optimal_fan_modes[optimal_fan_modes == Inf] <- NA
print(optimal_fan_modes)

# Update tasp_list and ctrl_mode with the maximum acceptable temperature if available
if (!is.na(max_temperature)) {
  tasp_list[time - 7] <- max_temperature
  ctrl_mode[time - 7, 2] <- max_temperature  # Set the max temperature
  ctrl_mode[time - 7, 3:(2 + Nocc)] <- optimal_fan_modes  # Record Fan mode
} else {
  tasp_list[time - 7] <- 27
  ctrl_mode[time - 7, 2] <- 27  # Set 27 as the default temperature
  ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
}

print(ctrl_mode[time - 7, ])  # Debugging: print the ctrl_mode row for the current time

#####################################
# Test for real acceptance calculation
#####################################

# Load necessary data
env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/env_accep_agent.csv", header = TRUE)

# Initialize vectors to store the total acceptance and total occupancy for each time
total_acceptance <- rep(0, 12)
total_occupancy <- rep(0, 12)

# Loop through each time period (8 to 19)
for (time in 8:19) {
  # Get the corresponding ctrl_mode row for the current time
  ctrl_row <- ctrl_mode[time - 7, ]
  
  # Extract the temperature and fan modes for the current time
  Tair <- ctrl_row["Max_Temperature"]
  fan_modes <- ctrl_row[3:(2 + Nocc)]
  
  # Extract occupancy data for the current time
  occupancy_data <- occ_day_data[time, ]
  
  # Calculate the total occupancy for the current time
  total_occupancy[time - 7] <- sum(occupancy_data == 1)
  
  # Filter env_accep_agent to match the current Tair and each user's FanMode
  matching_rows <- env_accep_agent$Tair == Tair
  
  for (user_idx in 1:Nocc) {
    if (occupancy_data[user_idx] == 1) {
      # Filter by user's FanMode and Tair
      user_fan_mode <- fan_modes[user_idx]
      user_acceptance <- env_accep_agent[matching_rows & env_accep_agent$FanMode == user_fan_mode, 5 + user_idx]
      
      # Sum the acceptance for this time period
      total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
    }
  }
  
  # Calculate the average acceptance considering total occupancy
  if (total_occupancy[time - 7] > 0) {
    total_acceptance[time - 7] <- total_acceptance[time - 7] / total_occupancy[time - 7]
  } else {
    total_acceptance[time - 7] <- NA  # Avoid division by zero
  }
  
  # Print the result for debugging
  print(paste("Time:", time, "Total Acceptance:", total_acceptance[time - 7], "Total Occupancy:", total_occupancy[time - 7]))
}

# Print final total acceptance across all times
print("Final Total Acceptance by Time:")
print(total_acceptance)


#####################################
# Test for understanding the data structure
#####################################
### test occupancy information

Nocc <- 16 # number of occupant in the zone

path_wd <- "/home/rstudio/localdir"
path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant.csv")
occ_occupant <- read.csv(path_occ_occupant,header=T)
occ_total <- occ_occupant[,(Nocc+1)]

# get Occupancy based on Nday
# assume data are saved for 24rows each day

Nday<-10

start_row <- 24 * (Nday - 1) + 1
end_row <- start_row + 23
occ_day_data <- occ_occupant[start_row:end_row, 1:16]

print(occ_day_data)
occ_total <- occ_occupant[,(Nocc+1)]
print(occ_total)

tasp <- 27
tasp8 <- tasp
tasp9 <- tasp
tasp10 <- tasp
tasp11 <- tasp
tasp12 <- tasp
tasp13 <- tasp
tasp14 <- tasp
tasp15 <- tasp
tasp16 <- tasp
tasp17 <- tasp
tasp18 <- tasp
tasp19 <- tasp

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")
accep_data <- read.csv(path_accep)


#####################################
# Test for real acceptance calculation
#####################################

user_idx <- 1  # Debug for user index 1
time <- 11  # Debug for time 11

# Initialize vectors to store the total acceptance and total occupancy for each time
total_acceptance <- rep(0, 12)
total_occupancy <- rep(0, 12)

# Loop through each time period (8 to 19)
#for (time in 8:19) {

# Get the corresponding ctrl_mode row for the current time
ctrl_row <- ctrl_mode[time - 7, ]

# Extract the temperature and fan modes for the current time
Tair <- ctrl_row["Max_Temperature"]
fan_modes <-  as.numeric(ctrl_row[3:(2 + Nocc)])  # Convert to numeric to remove names

# Extract occupancy data for the current time
occupancy_data <- occ_day_data[time, ]    # 16 users' occupancy data for the current hour

# Calculate the total occupancy for the current time
total_occupancy[time - 7] <- sum(occupancy_data == 1)

# Filter env_accep_agent to match the current Tair and each user's FanMode
matching_rows <- env_accep_agent$Tair == Tair
filtered_data <- env_accep_agent[matching_rows, ]
print("Filtered data based on Tair:")
print(filtered_data)

for (user_idx in 1:Nocc) {
  if (occupancy_data[user_idx] == 1) {
    # Filter by user's FanMode and Tair
    user_fan_mode <- fan_modes[user_idx]
    user_acceptance <- env_accep_agent[matching_rows & env_accep_agent$FanMode == user_fan_mode, 5 + user_idx]
    print(user_acceptance)
    
    # Sum the acceptance for this time period
    total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
  }
}

# Calculate the final satisfaction ratio
total_acceptance_sum <- sum(total_acceptance, na.rm = TRUE)
total_occupancy_sum <- sum(total_occupancy, na.rm = TRUE)

if (total_occupancy_sum > 0) {
  satisfaction_ratio <- total_acceptance_sum / total_occupancy_sum
} else {
  satisfaction_ratio <- NA  # Avoid division by zero
}

print(paste("Satisfaction Ratio:", satisfaction_ratio))



#####################################
# Test for real acceptance calculation
#####################################

user_idx <- 1  # Debug for user index 1
time <- 11  # Debug for time 11

# Initialize vectors to store the total acceptance and total occupancy for each time
total_acceptance <- rep(0, 12)
total_occupancy <- rep(0, 12)

# Loop through each time period (8 to 19)
for (time in 8:19) {
  
  # Get the corresponding ctrl_mode row for the current time
  ctrl_row <- ctrl_mode[time - 7, ]
  
  # Extract the temperature and fan modes for the current time
  Tair <- ctrl_row["Max_Temperature"]
  fan_modes <-  as.numeric(ctrl_row[3:(2 + Nocc)])  # Convert to numeric to remove names
  
  # Extract occupancy data for the current time
  occupancy_data <- occ_day_data[time, ]    # 16 users' occupancy data for the current hour
  
  # Calculate the total occupancy for the current time
  total_occupancy[time - 7] <- sum(occupancy_data == 1)
  
  # Filter accep_data to match the current Tair and each user's FanMode
  matching_rows <- accep_data$Indoor.Temp == Tair
  filtered_data <- accep_data[matching_rows, ]
  print("Filtered data based on Tair:")
  print(filtered_data)
  
  for (user_idx in 1:Nocc) {
    if (occupancy_data[user_idx] == 1) {
      # Filter by user's FanMode and Tair
      user_fan_mode <- fan_modes[user_idx]
      user_acceptance <- accep_data[matching_rows & accep_data$FanMode == user_fan_mode, 6 + user_idx]
      print(user_acceptance)
      
      # Sum the acceptance for this time period
      total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
    }
  }
  
  print(total_acceptance[time - 7])
}

# Calculate the final satisfaction ratio
total_acceptance_sum <- sum(total_acceptance, na.rm = TRUE)
total_occupancy_sum <- sum(total_occupancy, na.rm = TRUE)

if (total_occupancy_sum > 0) {
  satisfaction_ratio <- total_acceptance_sum / total_occupancy_sum
} else {
  satisfaction_ratio <- NA  # Avoid division by zero
}

print(paste("Satisfaction Ratio:", satisfaction_ratio))


#####################################
# Test for fan energy calculation
#####################################

user_idx <- 1  # Debug for user index 1
time <- 9  # Debug for time 11

# Fan mode electricity consumption (in kW)
Ecfand <- c(0, 2, 6, 10, 15, 20, 26, 32)/1000  # Ceiling fan electricity from mode 0 to 5

# Initialize a vector to store the total energy consumption for each time period
total_energy_consumption <- rep(0, nrow(ctrl_mode))

# Loop through each time period
for (i in 1:nrow(ctrl_mode)) {
  # Extract the fan modes for all users at the current time
  fan_modes <- as.numeric(ctrl_mode[i, 3:(2 + Nocc)])  # Convert to numeric to remove names
  
  # Calculate energy consumption for each user (exclude NA values)
  energy_consumption <- sapply(fan_modes, function(fan_mode) {
    if (!is.na(fan_mode)) {
      return(Ecfand[fan_mode + 1])  # Add 1 to index since R is 1-based and fan_mode is 0-based
    } else {
      return(0)  # No energy consumption if fan mode is NA
    }
  })
  
  # Sum the energy consumption for all users at the current time
  total_energy_consumption[i] <- sum(energy_consumption)
}

# Print the total energy consumption for each time period
for (i in 1:nrow(ctrl_mode)) {
  print(paste("Time:", ctrl_mode[i, 1], "Total Energy Consumption (kW):", total_energy_consumption[i]))
}





#####################################
# Case loop_not_looping
#####################################

# Load predicted acceptance
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/case3_max30/predicted_acceptance_classes_N8.csv")
accep_data <- read.csv(path_accep)

#Initialize output
mean_acceptance_df <- numeric()
mean_acceptance_list <- numeric()
Eall_matrix <- list()
Eall_list<- list()

for (comfort_model_type in 2){
  for (system in 1){
    # 1: VAV
    # 2: VAV + ceiling fan
    # 3: VAV + personal fan
    
    # we may want to update the registry for each system variation
    if (system == 1){
      path_idf <- paste0(path_wd,"/AsimEx/Singapore_Benchmark_Model_V940_ono_VAV.idf") 
      path_cal <- paste0(path_wd,"/AsimEx/cal/VAV") 
    }else{
      path_idf <- paste0(path_wd,"/AsimEx/Singapore_Benchmark_Model_V940_ono_Hybrid.idf")
      path_cal <- paste0(path_wd,"/AsimEx/cal/Hybrid")
    }
    
    for (opt in 0){
      
      if (opt == 0){
        comfort_model_range <- 3
      }else if (opt == 1){
        comfort_model_range <- 1:3
      }
      
      for (comfort_model in 3){ #comfort_model_range){
        
        week_ini <- 2 # Tuesday
        
        #        for (day in 11){
        for (day in 1:NDays){
          
          week <- day%%7 + week_ini - 1
          Nday_start <- 0
          
          if (week <= 5){             #exclude weekend
            Nday <- Nday_start + day
            
            ###################
            # Update schedules 
            ###################
            
            idf <- read_idf(path = path_idf, idd = NULL)
            
            ### Update occupant load schedules
            
            path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant.csv")
            occ_occupant <- read.csv(path_occ_occupant,header=T)
            occ_total <- occ_occupant[,(Nocc+1)]
            
            # get Occupancy based on Nday
            # assume data are saved for 24rows each day
            start_row <- 24 * (Nday - 1) + 1
            end_row <- start_row + 23
            occ_day_data <- occ_occupant[start_row:end_row, 1:16] #occupancy matrix for one day
            
            ### Update occupant load schedules
            tmp <- idf$"Schedule:Compact"[["Sch_Occupancy_Target"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            
            for (i in 1:12){
              dt[(2*i+6),6] <- occ_total[24*(Nday-1)+i+7]/100
            }
            
            idf$update(dt)
            
            # Update load information except the setpoint
            if (zone_index == 1){ # optimize an interior zone
              str_occupancy_mid1 <- "Sch_Occupancy_target"
              str_occupancy_periE <- "Sch_Occupancy"
              occupancy_method_mid1 <- "People"
              occupancy_method_periE <- "Area/Person"
              occupancy_num_mid1 <- 100
              occupancy_num_periE <- ""
              occupancy_areaperson_mid1 <- ""
              occupancy_areaperson_periE <- 10.77
            }else if (zone_index == 2){ # optimize a perimeter zone
              str_occupancy_mid1 <- "Sch_Occupancy"
              str_occupancy_periE <- "Sch_Occupancy_target"
              occupancy_method_mid1 <- "Area/Person"
              occupancy_method_periE <- "People"
              occupancy_num_mid1 <- ""
              occupancy_num_periE <- 100
              occupancy_areaperson_mid1 <- 10.77
              occupancy_areaperson_periE <- ""
            }
            
            tmp <- idf$People[["Core_Mid1"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt <- dt[1:20,]
            dt[3,6] <- str_occupancy_mid1
            dt[4,6] <- occupancy_method_mid1
            dt[5,6] <- occupancy_num_mid1
            dt[7,6] <- occupancy_areaperson_mid1
            idf$update(dt)
            
            tmp <- idf$People[["Zone_Mid_E"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt <- dt[1:20,]
            dt[3,6] <- str_occupancy_periE
            dt[4,6] <- occupancy_method_periE
            dt[5,6] <- occupancy_num_periE
            dt[7,6] <- occupancy_areaperson_periE
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["Nday"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- Nday
            idf$update(dt)
            
            tmp <- idf$"RunPeriod"[["RUNPERIOD 1"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[2,6] <- month
            dt[3,6] <- day
            dt[5,6] <- month
            dt[6,6] <- day
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["tp_combination"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- tp_combination_index
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["zone_index"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- zone_index
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["comfort_model"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- comfort_model
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["comfort_model_type"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- comfort_model_type
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["str_system"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- system
            idf$update(dt)
            
            tmp <- idf$"Schedule:Compact"[["str_opt"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            dt[6,6] <- opt
            idf$update(dt)
            
            idf$save(overwrite = TRUE)
            
            if (opt == 0){ # No GA optimization
              if (system == 1){ # VAV system
                tasp <- 24
                # mode <- 0
              }else{ # Hybrid system
                tasp <- 27
                # mode <- 3
              }
              
              tasp8 <- tasp
              tasp9 <- tasp
              tasp10 <- tasp
              tasp11 <- tasp
              tasp12 <- tasp
              tasp13 <- tasp
              tasp14 <- tasp
              tasp15 <- tasp
              tasp16 <- tasp
              tasp17 <- tasp
              tasp18 <- tasp
              tasp19 <- tasp
              
              # pass Occupancy information" and "system" update_when calling update_idf function
              result <- update_idf(idf, tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                                   tasp14, tasp15, tasp16, tasp17, tasp18, tasp19, 
                                   occ_day_data, system)
              
              ctrl_mode <- result$ctrl_mode
              
              idf$save(overwrite = TRUE)
              idf$run(path_epw,
                      dir = path_cal,
                      wait = TRUE)
              
              mean_acceptance <- calculate_acceptance_ratio(ctrl_mode, occ_day_data, env_accep_agent,system, Nocc = 16)
              Eall <- get_energy(idf)
              #print(mean_acceptance)
              #print(Eall)
              
              
              # record
              mean_acceptance_list <- c(mean_acceptance_list, mean_acceptance)
              Eall_list[[day]] <- Eall
              
            } 
          }
        }
      }
    }
  }
}

#set directory
output_dir <- "~/localdir/AsimEx/cal/Result"

# convert into df
num_days <- length(mean_acceptance_list)
mean_acceptance_df <- data.frame(Day = 1:num_days, MeanAcceptance = mean_acceptance_list)

Eall_matrix <- do.call(rbind, Eall_list)

# CSV
write.csv(mean_acceptance_df, file = file.path(output_dir, "mean_acceptance.csv"), row.names = FALSE)
write.csv(Eall_matrix, file = file.path(output_dir, "Eall.csv"), row.names = FALSE)


# assign saving path of ctrl_mode
output_path <- "~/localdir/AsimEx/cal/Result/ctrl_mode.csv"

# save ctrl_mode df
write.csv(ctrl_mode, file = output_path, row.names = FALSE)



################################
# backup objective function 2
################################

calculate_acceptance_ratio <- function(ctrl_mode, occ_day_data, env_accep_agent, Nocc = 16) {
  
  # Initialize vectors to store the total acceptance and total occupancy for each time
  total_acceptance <- rep(0, 12)
  total_occupancy <- rep(0, 12)
  
  # # Loop through each time period (8 to 19)
  # for (time in 8:19) {
  #   
  #   # Get the corresponding ctrl_mode row for the current time
  #   ctrl_row <- ctrl_mode[time - 7, ]
  #   
  #   # Extract the temperature and fan modes for the current time
  #   Tair <- ctrl_row["Max_Temperature"]
  #   fan_modes <- as.numeric(ctrl_row[3:(2 + Nocc)])  # Convert to numeric to remove names
  #   
  #   # Extract occupancy data for the current time
  #   occupancy_data <- occ_day_data[time, ]    # Nocc users' occupancy data for the current hour
  #   
  #   # Calculate the total occupancy for the current time
  #   total_occupancy[time - 7] <- sum(occupancy_data == 1)
  #   
  #   # Filter env_accep_agent to match the current Tair and each user's FanMode
  #   matching_rows <- env_accep_agent$Tair == Tair
  #   filtered_data <- env_accep_agent[matching_rows, ]
  #   #print(paste("Filtered data for time", time, "based on Tair:"))
  #   #print(filtered_data)
  #   
  #   for (user_idx in 1:Nocc) {
  #     if (occupancy_data[user_idx] == 1) {
  #       # Filter by user's FanMode and Tair
  #       user_fan_mode <- fan_modes[user_idx]
  #       user_acceptance <- env_accep_agent[matching_rows & env_accep_agent$FanMode == user_fan_mode, 6 + user_idx]
  #       #print(paste("User", user_idx, "acceptance at time", time, ":", user_acceptance))
  #       
  #       # Sum the acceptance for this time period
  #       total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
  #     }
  #   }
  # }
  
  # Loop through each time period (8 to 19)
  for (time in 8:19) {
    
    # Get the corresponding ctrl_mode row for the current time
    ctrl_row <- ctrl_mode[time - 7, ]
    
    # Extract the temperature for the current time
    Tair <- ctrl_row["Max_Temperature"]
    
    # Extract occupancy data for the current time
    occupancy_data <- occ_day_data[time, ]    # Nocc users' occupancy data for the current hour
    
    # Calculate the total occupancy for the current time
    total_occupancy[time - 7] <- sum(occupancy_data == 1)
    
    # Filter env_accep_agent to match the current Tair
    matching_rows <- env_accep_agent$Tair == Tair
    filtered_data <- env_accep_agent[matching_rows, ]
    
    for (user_idx in 1:Nocc) {
      if (occupancy_data[user_idx] == 1) {
        # Extract acceptance data for the user
        user_acceptance_data <- filtered_data[, 6 + user_idx]
        
        # Check if there is at least one '1' in the acceptance data
        if (any(user_acceptance_data == 1, na.rm = TRUE)) {
          total_acceptance[time - 7] <- total_acceptance[time - 7] + 1
        }
      }
    }
  }
  
  
  
  
  # Calculate the final satisfaction ratio
  total_acceptance_sum <- sum(total_acceptance, na.rm = TRUE)
  total_occupancy_sum <- sum(total_occupancy, na.rm = TRUE)
  
  if (total_occupancy_sum > 0) {
    satisfaction_ratio <- total_acceptance_sum / total_occupancy_sum
  } else {
    satisfaction_ratio <- NA  # Avoid division by zero
  }
  
  return(satisfaction_ratio)
}

