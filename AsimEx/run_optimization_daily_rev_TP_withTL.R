## Run optimization for impact analysis of individual comfort model on HVAC control
## 2021-03-24 E.Ono
## _rev: adapted to SinBerBEST model and so on, 2021-06-08
## 2024-08-14 K.Horikoshi
## _rev: adapted to SinBerBEST model with set comfort matrix, 2024-08-14

### not using but might be required to build environment at initial installment
# install devtools package if not exists
#if (!require("devtools", quietly = TRUE)) {
#  install.packages("devtools")
#  library(devtools)}
# install epluspar package if not exists
#if (!require("epluspar", quietly = TRUE)) {
#  devtools::install_github("hongyuanjia/epluspar")
#  library(epluspar)
#}

# install here package if not exists
#install.packages("here")

#options(timeout = 2000)
#install_eplus(9.4)

#####################################
# Load libraries
#####################################
# load the packages
library(eplusr)
library(here)

# turn off verbose information of eplusr package
eplusr_option(verbose_info = FALSE)
eplusr_option(autocomplete = TRUE)

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
# Update IDF file
#####################################

# Cases considering maximizing comfort

#update_idf_max <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
#                        tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L,
#                        occ_day_data, system) {
  
  # create the vector to include temperature schedule for full-day timeline
  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
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
        
        # Initialize variables to store the optimal temperature and maximum satisfaction score
        optimal_temperature <- NA
        max_satisfaction_score <- -Inf
        
        for (temp in unique(fan_mode_0_data$Indoor.Temp)) {
          # Filter data for the current temperature
          temp_data <- subset(fan_mode_0_data, Indoor.Temp == temp)
          
          # Calculate satisfaction score for the current temperature
          satisfaction_score <- mean(apply(temp_data[, 7:(6 + Nocc)], 1, function(row) {
            mean(row[occupancy_data == 1] == 1)
          }), na.rm = TRUE)
          
          # Check if the current satisfaction score is higher than the maximum found so far
          if (satisfaction_score > max_satisfaction_score) {
            max_satisfaction_score <- satisfaction_score
            optimal_temperature <- temp
          }
        }
        
        # Update tasp_list and ctrl_mode with the optimal temperature if available
        if (!is.na(optimal_temperature)) {
          tasp_list[time - 7] <- optimal_temperature
          ctrl_mode[time - 7, 2] <- optimal_temperature  # Set the optimal temperature
        } else {
          tasp_list[time - 7] <- 24  # Set default temperature
          ctrl_mode[time - 7, 2] <- 24  # Set 24 as default temperature
        }
        
        # Set Fan mode to 0 for present users and NA for absent users
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- ifelse(occupancy_data == 1, 0, NA)
      }
    }
    
  } else if (system == 3) { # Personal Control
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
        
      } else {
        # Get unique temperature range
        unique_temperatures <- unique(accep_data$Indoor.Temp)
        
        # Initialize variables to store the optimal temperature and corresponding fan modes
        optimal_temperature <- NA
        optimal_fan_modes <- rep(NA, Nocc)
        max_satisfaction <- -Inf
        
        for (temp in unique_temperatures) {
          # Filter data for the same temperature
          temp_data <- subset(accep_data, Indoor.Temp == temp)
          
          # Calculate satisfaction for each temperature
          user_satisfaction <- apply(temp_data[, 7:(7 + Nocc - 1)], 2, function(col) {
            max(col[occupancy_data == 1], na.rm = TRUE) == 1
          })
          
          # Calculate the overall satisfaction score for the current temperature
          satisfaction_score <- mean(user_satisfaction, na.rm = TRUE)
          
          # Check if the current temperature has a higher satisfaction score than the previous maximum
          if (satisfaction_score > max_satisfaction) {
            max_satisfaction <- satisfaction_score
            optimal_temperature <- temp
            
            # Find the minimum fan mode for each user where Accep == 1 and occupancy is 1
            for (user_idx in 1:Nocc) {
              if (occupancy_data[user_idx] == 1) {
                user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1) - 1
                if (length(user_fan_modes) > 0) {
                  # Select the minimum fan mode from the available modes
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
        
        # Update tasp_list and ctrl_mode with the optimal temperature if available
        if (!is.na(optimal_temperature)) {
          tasp_list[time - 7] <- optimal_temperature
          ctrl_mode[time - 7, 2] <- optimal_temperature  # Set the optimal temperature
          ctrl_mode[time - 7, 3:(2 + Nocc)] <- optimal_fan_modes  # Record Fan mode
        } else {
          tasp_list[time - 7] <- 27
          ctrl_mode[time - 7, 2] <- 27  # Set 27 as the default temperature
          ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
        }
      }
    }
  }
  
  ## Update "Zone cooling setpoint"
  tmp <- idf$"Schedule:Compact"[["zone_index"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  zone_index <- dt[6,6]
  

  if (zone_index == 1){ # set temperature schedule for an interior zone
    tasp_core <- tasp_list
    tasp_peri <- tasp_list - 1
  }else if(zone_index == 2){ #  set temperature schedule for a perimeter zone
    tasp_core <- tasp_list + 1
    tasp_peri <- tasp_list
  }
  
  # set temperature schedule for an core zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_core"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_core[i]
  }
  idf$update(dt)
  
  # set temperature schedule for an interior zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_peri"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_peri[i]
  }
  idf$update(dt)
  
  idf$save(path_idf,"overwrite" = TRUE)
  
  # Return only the control mode data
  list(ctrl_mode = ctrl_mode)
}

update_idf_max_multifan <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
                            tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L,
                            occ_day_data, system) {
  
  # create the vector to include temperature schedule for full-day timeline
  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
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
        
        # Initialize variables to store the optimal temperature and maximum satisfaction score
        optimal_temperature <- NA
        max_satisfaction_score <- -Inf
        
        for (temp in unique(fan_mode_0_data$Indoor.Temp)) {
          # Filter data for the current temperature
          temp_data <- subset(fan_mode_0_data, Indoor.Temp == temp)
          
          # Calculate satisfaction score for the current temperature
          satisfaction_score <- mean(apply(temp_data[, 7:(6 + Nocc)], 1, function(row) {
            mean(row[occupancy_data == 1] == 1)
          }), na.rm = TRUE)
          
          # Check if the current satisfaction score is higher than the maximum found so far
          if (satisfaction_score > max_satisfaction_score) {
            max_satisfaction_score <- satisfaction_score
            optimal_temperature <- temp
          }
        }
        
        # Update tasp_list and ctrl_mode with the optimal temperature if available
        if (!is.na(optimal_temperature)) {
          tasp_list[time - 7] <- optimal_temperature
          ctrl_mode[time - 7, 2] <- optimal_temperature  # Set the optimal temperature
        } else {
          tasp_list[time - 7] <- 24  # Set default temperature
          ctrl_mode[time - 7, 2] <- 24  # Set 24 as default temperature
        }
        
        # Set Fan mode to 0 for present users and NA for absent users
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- ifelse(occupancy_data == 1, 0, NA)
      }
    }
    
  } else if (system == 3) { # Personal Control
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
        
      } else {
        # Get unique temperature range
        unique_temperatures <- unique(accep_data$Indoor.Temp)
        
        # Initialize variables to store the optimal temperature and corresponding fan modes
        optimal_temperature <- NA
        optimal_fan_modes <- vector("list", Nocc)  # Initialize as a list to store multiple fan modes for each user
        max_satisfaction <- -Inf
        
        for (temp in unique_temperatures) {
          # Filter data for the same temperature
          temp_data <- subset(accep_data, Indoor.Temp == temp)
          
          # Calculate satisfaction for each temperature
          user_satisfaction <- apply(temp_data[, 7:(7 + Nocc - 1)], 2, function(col) {
            max(col[occupancy_data == 1], na.rm = TRUE) == 1
          })
          
          # Calculate the overall satisfaction score for the current temperature
          satisfaction_score <- mean(user_satisfaction, na.rm = TRUE)
          
          # Check if the current temperature has a higher satisfaction score than the previous maximum
          if (satisfaction_score > max_satisfaction) {
            max_satisfaction <- satisfaction_score
            optimal_temperature <- temp
            
            # Find all acceptable fan modes for each user where Accep == 1 and occupancy is 1
            for (user_idx in 1:Nocc) {
              if (occupancy_data[user_idx] == 1) {
                user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1) - 1
                if (length(user_fan_modes) > 0) {
                  # Store all available fan modes for each user
                  optimal_fan_modes[[user_idx]] <- user_fan_modes
                } else {
                  optimal_fan_modes[[user_idx]] <- NA  # Set to NA if no fan mode is available
                }
              } else {
                optimal_fan_modes[[user_idx]] <- NA  # Set to NA if the user is not present
              }
            }
          }
        }
        
        # Update tasp_list and ctrl_mode with the optimal temperature if available
        if (!is.na(optimal_temperature)) {
          tasp_list[time - 7] <- optimal_temperature
          ctrl_mode[time - 7, 2] <- optimal_temperature  # Set the optimal temperature
          
          # Record the list of available fan modes for each user, if any
          for (user_idx in 1:Nocc) {
            if (!is.na(optimal_fan_modes[[user_idx]])) {
              ctrl_mode[time - 7, 2 + user_idx] <- paste(optimal_fan_modes[[user_idx]], collapse = ",")
            } else {
              ctrl_mode[time - 7, 2 + user_idx] <- NA
            }
          }
        } else {
          tasp_list[time - 7] <- 27
          ctrl_mode[time - 7, 2] <- 27  # Set 27 as the default temperature
          ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
        }
      }
    }
  }
  
  ## Update "Zone cooling setpoint"
  tmp <- idf$"Schedule:Compact"[["zone_index"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  zone_index <- dt[6,6]
  
  
  if (zone_index == 1){ # set temperature schedule for an interior zone
    tasp_core <- tasp_list
    tasp_peri <- tasp_list - 1
  }else if(zone_index == 2){ #  set temperature schedule for a perimeter zone
    tasp_core <- tasp_list + 1
    tasp_peri <- tasp_list
  }
  
  # set temperature schedule for an core zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_core"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_core[i]
  }
  idf$update(dt)
  
  # set temperature schedule for an interior zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_peri"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_peri[i]
  }
  idf$update(dt)
  
  idf$save(path_idf,"overwrite" = TRUE)
  
  # Return only the control mode data
  list(ctrl_mode = ctrl_mode)
}



# Cases considering achieving some goals

#update_idf_goal <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
#                        tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L,
#                        occ_day_data, system) {
  
  # create the vector to include temperature schedule for full-day timeline
  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
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
          mean(row[occupancy_data == 1] == 1) >= 0.5
        }), ]
        
        # Identify temperature ranges where at least 75% of present users are comfortable
        acceptable_temperature_ranges <- filtered_data[,"Indoor.Temp"]
        
        # Update tasp based on the maximum acceptable temperature if available
        if (length(acceptable_temperature_ranges) > 0) {
          tasp_list[time - 7] <- max(acceptable_temperature_ranges)
          ctrl_mode[time - 7, 2] <- max(acceptable_temperature_ranges)  # Set the max temperature
        } else {
          tasp_list[time - 7] <- 24  # Set default temperature
          ctrl_mode[time - 7, 2] <- 24  # Set 24 as default temperature
        }
        
        # Set Fan mode to 0 for present users and NA for absent users
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- ifelse(occupancy_data == 1, 0, NA)
      }
    }
    
  } else if (system == 3) { # Personal Control
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
        
      } else {
        # Get unique temperature range
        unique_temperatures <- unique(accep_data$Indoor.Temp)
        
        # Initialize variable to store maximum acceptable temperature
        max_temperature <- NA
        optimal_fan_modes <- rep(NA, Nocc)
        
        for (temp in unique_temperatures) {
          # Filter data for the same temperature
          temp_data <- subset(accep_data, Indoor.Temp == temp)
          #temp_data <- subset(env_accep_agent, Tair == temp) #debug is the setting temperature works well
          #print(temp_data)
          
          # Check if at least 75% of users are comfortable with any fan mode (columns 7 to 22)
          user_satisfaction <- apply(temp_data[, 7:(7 + Nocc - 1)], 2, function(col) {
            max(col[occupancy_data == 1], na.rm = TRUE) == 1  # Check if user is satisfied with any fan mode
          })
          
          # Check if 75% or more users are satisfied with any fan mode
          if (mean(user_satisfaction, na.rm = TRUE) >= 0.75) {
            max_temperature <- max(max_temperature, temp, na.rm = TRUE)
            
            # Find the minimum fan mode for each user where Accep == 1 and occupancy is 1
            for (user_idx in 1:Nocc) {
              if (occupancy_data[user_idx] == 1) {
                user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1) - 1
                if (length(user_fan_modes) > 0) {
                  # Select the minimum fan mode from the available modes
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
      }
    }
  }
  
  ## Update "Zone cooling setpoint"
  tmp <- idf$"Schedule:Compact"[["zone_index"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  zone_index <- dt[6,6]
  
  
  if (zone_index == 1){ # set temperature schedule for an interior zone
    tasp_core <- tasp_list
    tasp_peri <- tasp_list - 1
  }else if(zone_index == 2){ #  set temperature schedule for a perimeter zone
    tasp_core <- tasp_list + 1
    tasp_peri <- tasp_list
  }
  
  # set temperature schedule for an core zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_core"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_core[i]
  }
  idf$update(dt)
  
  # set temperature schedule for an interior zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_peri"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_peri[i]
  }
  idf$update(dt)
  
  idf$save(path_idf,"overwrite" = TRUE)
  
  # Return only the control mode data
  list(ctrl_mode = ctrl_mode)
}

update_idf_goal_multifan <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
                             tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L,
                             occ_day_data, system) {
  
  # create the vector to include temperature schedule for full-day timeline
  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
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
          mean(row[occupancy_data == 1] == 1) >= 0.5
        }), ]
        
        # Identify temperature ranges where at least 50% of present users are comfortable
        acceptable_temperature_ranges <- filtered_data[,"Indoor.Temp"]
        
        # Update tasp based on the maximum acceptable temperature if available
        if (length(acceptable_temperature_ranges) > 0) {
          tasp_list[time - 7] <- max(acceptable_temperature_ranges)
          ctrl_mode[time - 7, 2] <- max(acceptable_temperature_ranges)  # Set the max temperature
        } else {
          tasp_list[time - 7] <- 24  # Set default temperature
          ctrl_mode[time - 7, 2] <- 24  # Set 24 as default temperature
        }
        
        # Set Fan mode to 0 for present users and NA for absent users
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- ifelse(occupancy_data == 1, 0, NA)
      }
    }
    
  } else if (system == 3) { # Personal Control
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
        
      } else {
        # Get unique temperature range
        unique_temperatures <- unique(accep_data$Indoor.Temp)
        
        # Initialize variable to store maximum acceptable temperature
        max_temperature <- NA
        optimal_fan_modes <- vector("list", Nocc)  # Initialize as a list to store multiple fan modes for each user
        
        for (temp in unique_temperatures) {
          # Filter data for the same temperature
          temp_data <- subset(accep_data, Indoor.Temp == temp)
          
          # Check if at least 75% of users are comfortable with any fan mode (columns 7 to 22)
          user_satisfaction <- apply(temp_data[, 7:(7 + Nocc - 1)], 2, function(col) {
            max(col[occupancy_data == 1], na.rm = TRUE) == 1  # Check if user is satisfied with any fan mode
          })
          
          # Check if 75% or more users are satisfied with any fan mode
          if (mean(user_satisfaction, na.rm = TRUE) >= 0.75) {
            max_temperature <- max(max_temperature, temp, na.rm = TRUE)
            
            # Find all acceptable fan modes for each user where Accep == 1 and occupancy is 1
            for (user_idx in 1:Nocc) {
              if (occupancy_data[user_idx] == 1) {
                user_fan_modes <- which(temp_data[, 7 + user_idx - 1] == 1) - 1
                if (length(user_fan_modes) > 0) {
                  # Store all available fan modes for each user
                  optimal_fan_modes[[user_idx]] <- user_fan_modes
                } else {
                  optimal_fan_modes[[user_idx]] <- NA  # Set to NA if no fan mode is available
                }
              } else {
                optimal_fan_modes[[user_idx]] <- NA  # Set to NA if the user is not present
              }
            }
          }
        }
        
        # Update tasp_list and ctrl_mode with the maximum acceptable temperature if available
        if (!is.na(max_temperature)) {
          tasp_list[time - 7] <- max_temperature
          ctrl_mode[time - 7, 2] <- max_temperature  # Set the max temperature
          
          # Record the first available fan mode for each user, if any
          for (user_idx in 1:Nocc) {
            if (!is.na(optimal_fan_modes[[user_idx]])) {
              ctrl_mode[time - 7, 2 + user_idx] <- paste(optimal_fan_modes[[user_idx]], collapse = ",")
            } else {
              ctrl_mode[time - 7, 2 + user_idx] <- NA
            }
          }
        } else {
          tasp_list[time - 7] <- 27
          ctrl_mode[time - 7, 2] <- 27  # Set 27 as the default temperature
          ctrl_mode[time - 7, 3:(2 + Nocc)] <- NA  # Record NA for Fan mode
        }
      }
    }
  }
  
  ## Update "Zone cooling setpoint"
  tmp <- idf$"Schedule:Compact"[["zone_index"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  zone_index <- dt[6,6]
  
  
  if (zone_index == 1){ # set temperature schedule for an interior zone
    tasp_core <- tasp_list
    tasp_peri <- tasp_list - 1
  }else if(zone_index == 2){ #  set temperature schedule for a perimeter zone
    tasp_core <- tasp_list + 1
    tasp_peri <- tasp_list
  }
  
  # set temperature schedule for an core zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_core"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_core[i]
  }
  idf$update(dt)
  
  # set temperature schedule for an interior zone
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_peri"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_peri[i]
  }
  idf$update(dt)
  
  idf$save(path_idf,"overwrite" = TRUE)
  
  # Return only the control mode data
  list(ctrl_mode = ctrl_mode)
}

################################
# calculate objective function 1
################################
get_energy <- function (idf) {
  
  # path_idf <- paste0(path_wd,"Singapore_Benchmark_Model_V940_ono_VAV.idf")
  # idf <- read_idf(path = path_idf, idd = NULL)
  # job <- idf$run(path_epw, wait = TRUE)
  
  job <- idf$last_job()
  stopifnot(!is.null(job))
  
  # calculate energy consumption
  Ecfand <- c(0,2,6,10,15,20,26,32)/1000 # ceiling fan electricity from mode 0 to 7
  # met <- 1.1
  # clo <- 0.4
  
  # setting of occupants, floors, timesteps
  Nocc <- 16
  Nfloor <- 15
  Nstep <- 24

  # getting infor from idf
  zone_index <- job$read_table("Schedules")[schedule_name=="ZONE_INDEX",schedule_maximum]
  tp_combination_index <- job$read_table("Schedules")[schedule_name=="TP_COMBINATION",schedule_maximum]
  comfort_model <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL",schedule_maximum]
  comfort_model_type <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL_TYPE",schedule_maximum]
  system <- job$read_table("Schedules")[schedule_name=="STR_SYSTEM",schedule_maximum]
  opt <- job$read_table("Schedules")[schedule_name=="STR_OPT",schedule_maximum]
  Nday <- job$read_table("Schedules")[schedule_name=="NDAY",schedule_maximum]

  
  # calculating the energy
  Qcoil_bot <- unlist(job$report_data("VAV_BOT_COIL","Cooling Coil Total Cooling Rate")[,6])/1000
  Qcoil_mid <- unlist(job$report_data("VAV_MID_COIL","Cooling Coil Total Cooling Rate")[,6])/1000
  Qcoil_top <- unlist(job$report_data("VAV_TOP_COIL","Cooling Coil Total Cooling Rate")[,6])/1000
  Qchiller1 <- unlist(job$report_data("SGP_CHILLER_1","Chiller Evaporator Cooling Rate")[,6])/1000
  Qchiller2 <- unlist(job$report_data("SGP_CHILLER_2","Chiller Evaporator Cooling Rate")[,6])/1000
  Echiller1 <- unlist(job$report_data("SGP_CHILLER_1","Chiller Electricity Rate")[,6])/1000
  Echiller2 <- unlist(job$report_data("SGP_CHILLER_2","Chiller Electricity Rate")[,6])/1000
  Ect1 <- unlist(job$report_data("SGP_COOLTOWER_1","Cooling Tower Fan Electricity Rate")[,6])/1000
  Ect2 <- unlist(job$report_data("SGP_COOLTOWER_2","Cooling Tower Fan Electricity Rate")[,6])/1000
  Epch <- unlist(job$report_data("CHW_PUMP","Pump Electricity Rate")[,6])/1000
  Epcd <- unlist(job$report_data("CNDW_PUMP","Pump Electricity Rate")[,6])/1000
  Eahu <- unlist(job$report_data("VAV_MID_FAN","Fan Electricity rate")[,6])/1000
  Vmid1 <- unlist(job$report_data("CORE_MID1_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmid2 <- unlist(job$report_data("CORE_MID2_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmid3 <- unlist(job$report_data("CORE_MID3_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmid4 <- unlist(job$report_data("CORE_MID4_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmidn <- unlist(job$report_data("ZONE_MID_N_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmide <- unlist(job$report_data("ZONE_MID_E_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmids <- unlist(job$report_data("ZONE_MID_S_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vmidw <- unlist(job$report_data("ZONE_MID_W_VAV_BOX_OUTLET_NODE","System Node Mass Flow Rate")[,6])
  Vahu <- unlist(job$report_data("VAV_MID_FAN_OUTLET","System Node Mass Flow Rate")[,6])
  
  Eplant <- Echiller1 + Echiller2 + Ect1 + Ect2 + Epch + Epcd
  Qcoil <- Qcoil_bot + Qcoil_mid + Qcoil_top
  
  Qmid1 <- Qcoil_mid*Vmid1/Vahu/Nfloor
  Qmid2 <- Qcoil_mid*Vmid2/Vahu/Nfloor
  Qmid3 <- Qcoil_mid*Vmid3/Vahu/Nfloor
  Qmid4 <- Qcoil_mid*Vmid4/Vahu/Nfloor
  Qmidn <- Qcoil_mid*Vmidn/Vahu/Nfloor
  Qmide <- Qcoil_mid*Vmide/Vahu/Nfloor
  Qmids <- Qcoil_mid*Vmids/Vahu/Nfloor
  Qmidw <- Qcoil_mid*Vmidw/Vahu/Nfloor
  
  # choose optimizing target zone(interior or perimeter)
  if (zone_index == 1){ # optimize an interior zone (mid1)
    zone_name <- "CORE_MID1"
    Vtarget <- Vmid1
  }else if (zone_index == 2){ # optimize a perimeter zone ()
    zone_name <- "ZONE_MID_E"
    Vtarget <- Vmide
  }

  # calculating the environment
  ta <- unlist(job$report_data(zone_name,"Zone Mean Air Temperature")[,6])
  tr <- unlist(job$report_data(zone_name,"Zone Mean Radiant Temperature")[,6])
  rh <- unlist(job$report_data(zone_name,"Zone Air Relative Humidity")[,6])
  
  # assuming proportional to target air volume
  Eahu_target <- Eahu*Vtarget/Vahu/Nfloor
  Eplant_target <- Eplant*(Qcoil_mid*Vtarget/Vahu)/Qcoil/Nfloor
  Ecfan <- numeric(Nstep)
  
  # Calculate fan energy consumption based on ctrl_mode
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
    Ecfan[i] <- sum(energy_consumption)
  }
  
  Eall <- Eahu_target + Eplant_target + Ecfan

  obj <-Eall
  return(obj)
}

################################
# calculate objective function 2
################################

calculate_acceptance_ratio <- function(ctrl_mode, occ_day_data, env_accep_agent, system, Nocc = 16) {
  
  # Initialize vectors to store the total acceptance and total occupancy for each time
  total_acceptance <- rep(0, 12)
  total_occupancy <- rep(0, 12)
  
  # Loop through each time period (8 to 19)
  for (time in 8:19) {
    
    # Get the corresponding ctrl_mode row for the current time
    ctrl_row <- ctrl_mode[time - 7, ]
    
    # Extract the temperature and fan modes for the current time
    Tair <- ctrl_row["Max_Temperature"]
    fan_modes <- as.numeric(ctrl_row[3:(2 + Nocc)])  # Convert to numeric to remove names
    
    # Extract occupancy data for the current time
    occupancy_data <- occ_day_data[time, ]    # Nocc users' occupancy data for the current hour
    
    # Calculate the total occupancy for the current time
    total_occupancy[time - 7] <- sum(occupancy_data == 1)
    
    if (system == 1) {
      # System 1: Evaluate based on matching Tair and FanMode
#      matching_rows <- env_accep_agent$Tair == Tair
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
      filtered_data <- env_accep_agent[matching_rows, ]
      
      for (user_idx in 1:Nocc) {
        if (occupancy_data[user_idx] == 1) {
          # Filter by user's FanMode and Tair
          user_fan_mode <- fan_modes[user_idx]
          user_acceptance <- env_accep_agent[matching_rows & env_accep_agent$FanMode == user_fan_mode, 6 + user_idx]
          
          # Sum the acceptance for this time period
          total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
        }
      }
    } else if (system == 3) {
      # System 3: Evaluate based on matching Tair, ignoring FanMode
      #      matching_rows <- env_accep_agent$Tair == Tair
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
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

calculate_acceptance_ratio_multifan <- function(ctrl_mode, occ_day_data, env_accep_agent, system, Nocc = 16) {
  
  # Initialize vectors to store the total acceptance and total occupancy for each time
  total_acceptance <- rep(0, 12)
  total_occupancy <- rep(0, 12)
  
  # Loop through each time period (8 to 19)
  for (time in 8:19) {
    
    # Get the corresponding ctrl_mode row for the current time
    ctrl_row <- ctrl_mode[time - 7, ]
    
    # Extract the temperature and fan modes for the current time
    Tair <- ctrl_row["Max_Temperature"]
    fan_modes <- ctrl_row[3:(2 + Nocc)]  # Keep as character to handle multiple modes correctly
    
    # Extract occupancy data for the current time
    occupancy_data <- occ_day_data[time, ]    # Nocc users' occupancy data for the current hour
    
    # Calculate the total occupancy for the current time
    total_occupancy[time - 7] <- sum(occupancy_data == 1)
    
    if (system == 1) {
      # System 1: Evaluate based on matching Tair and FanMode
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
      filtered_data <- env_accep_agent[matching_rows, ]
      
      for (user_idx in 1:Nocc) {
        if (occupancy_data[user_idx] == 1) {
          # Filter by user's FanMode and Tair
          user_fan_mode <- fan_modes[user_idx]
          user_acceptance <- env_accep_agent[matching_rows & env_accep_agent$FanMode == user_fan_mode, 6 + user_idx]
          
          # Sum the acceptance for this time period
          total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
        }
      }
    } else if (system == 3) {
      # System 3: Evaluate based on matching Tair and any of the provided FanModes
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
      filtered_data <- env_accep_agent[matching_rows, ]
      
      for (user_idx in 1:Nocc) {
        if (occupancy_data[user_idx] == 1) {
          # Extract acceptance data for the user and check for multiple fan modes
          user_acceptance_data <- filtered_data[, 6 + user_idx]
          user_fan_modes <- as.numeric(unlist(strsplit(as.character(fan_modes[user_idx]), ",")))          
          
          # Check if there is at least one '1' in the acceptance data for any of the specified fan modes
          if (any(user_acceptance_data[filtered_data$FanMode %in% user_fan_modes] == 1, na.rm = TRUE)) {
            total_acceptance[time - 7] <- total_acceptance[time - 7] + 1
          }
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


#averaging comfort average every hour, that connects to lower comfort
#calculate_acceptance_ratio_multifan <- function(ctrl_mode, occ_day_data, env_accep_agent, system, Nocc = 16) {
  
  # Initialize vectors to store the total acceptance and total occupancy for each time
  total_acceptance <- rep(0, 12)
  total_occupancy <- rep(0, 12)
  satisfaction_ratios <- rep(NA, 12)  # Initialize vector to store satisfaction ratio for each time
  
  # Loop through each time period (8 to 19)
  for (time in 8:19) {
    
    # Get the corresponding ctrl_mode row for the current time
    ctrl_row <- ctrl_mode[time - 7, ]
    
    # Extract the temperature and fan modes for the current time
    Tair <- ctrl_row["Max_Temperature"]
    fan_modes <- ctrl_row[3:(2 + Nocc)]  # Keep as character to handle multiple modes correctly
    
    # Extract occupancy data for the current time
    occupancy_data <- occ_day_data[time, ]    # Nocc users' occupancy data for the current hour
    
    # Calculate the total occupancy for the current time
    total_occupancy[time - 7] <- sum(occupancy_data == 1)
    
    if (system == 1) {
      # System 1: Evaluate based on matching Tair and FanMode
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
      filtered_data <- env_accep_agent[matching_rows, ]
      
      for (user_idx in 1:Nocc) {
        if (occupancy_data[user_idx] == 1) {
          # Filter by user's FanMode and Tair
          user_fan_mode <- as.numeric(fan_modes[user_idx])
          user_acceptance <- filtered_data[filtered_data$FanMode == user_fan_mode, 6 + user_idx]
          
          # Sum the acceptance for this time period
          total_acceptance[time - 7] <- total_acceptance[time - 7] + sum(user_acceptance, na.rm = TRUE)
        }
      }
    } else if (system == 3) {
      # System 3: Evaluate based on matching Tair and any of the provided FanModes
      matching_rows <- env_accep_agent$Indoor.Temp == Tair
      filtered_data <- env_accep_agent[matching_rows, ]
      
      for (user_idx in 1:Nocc) {
        if (occupancy_data[user_idx] == 1) {
          # Extract acceptance data for the user and check for multiple fan modes
          user_acceptance_data <- filtered_data[, 6 + user_idx]
          user_fan_modes <- as.numeric(unlist(strsplit(as.character(fan_modes[user_idx]), ",")))
          
          # Check if there is at least one '1' in the acceptance data for any of the specified fan modes
          if (any(user_acceptance_data[filtered_data$FanMode %in% user_fan_modes] == 1, na.rm = TRUE)) {
            total_acceptance[time - 7] <- total_acceptance[time - 7] + 1
          }
        }
      }
    }
    
    # Calculate the satisfaction ratio for the current time period
    if (total_occupancy[time - 7] > 0) {
      satisfaction_ratios[time - 7] <- total_acceptance[time - 7] / total_occupancy[time - 7]
    } else {
      satisfaction_ratios[time - 7] <- NA  # Avoid division by zero
    }
  }
  
  # Calculate the average satisfaction ratio for the entire day
  average_satisfaction_ratio <- mean(satisfaction_ratios, na.rm = TRUE)
  
  return(average_satisfaction_ratio)
}

#####################################
# Setting
#####################################

tp_combination_index <- 1

Nocc <- 16 # number of occupant in the zone

model_name <- c("zone","group","personal")
model_type <- c("TP","TP_AMP")
system_name <- c("VAV","Cfan","Pfan")
zone_name <- c("interior","perimeter")
zone_index <- 1
comfort_model_type <- 1

month <- 10
Nday_start <- 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 # September 30
NDays <- 2  # 30days

path_wd <- "/home/rstudio/localdir"

# Load true acceptability
#env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/predicted_acceptance_classes_SDE2", header = TRUE)
#env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/predicted_tp_ctg_classes_SDE2_full.csv", header = TRUE)
env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/case4_fulllearning240902/predicted_tp_ctg_classes.csv", header = TRUE)

filterlist<-c(3,9,10,12,14,20)
drop_columns <- 6 + filterlist
env_accep_agent <- env_accep_agent[, -drop_columns]

path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant/occ_occupant_base.csv")
occ_occupant <- read.csv(path_occ_occupant,header=T)

#####################################
# N loop (N=2~20)
#####################################
  
for (N in seq(2, 2, 2)) {

# Load predicted acceptability
# path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/ASHRAE/predicted_acceptance_classes", ".csv")
# path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TA_case1/predicted_acceptance_classes_N", N, ".csv")
# path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TA_case2_fulllearning/predicted_acceptance_classes", ".csv")
# accep_data <- read.csv(path_accep)
  
    
# Load predicted no-change preference

###ASHRAE base model
#  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/ASHRAE_TP/predicted_tp_ctg_classes", ".csv")
  
###true comfort model as reference
#  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TP_case2_fulllearning/predicted_tp_ctg_classes", ".csv")
#  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TP_case3_fulllearning/predicted_tp_ctg_classes_SDE2_full", ".csv")
#  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TP_case4_fulllearning240902/predicted_tp_ctg_classes", ".csv")
  
  
### Predicted preference for N = 2~20

  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/SDE2_TP_case1/predicted_tp_ctg_classes_N", N, ".csv")


  accep_data <- read.csv(path_accep)
# filterlist<-c(3,9,10,12,14,20)
  filterlist<-c(3,9,10,12,14,20)
  
  drop_columns <- 6 + filterlist
  accep_data <- accep_data[, -drop_columns]

  #####################################
  # Case loop
  #####################################
  
  #Initialize output
  mean_acceptance_df <- numeric()
  mean_acceptance_list <- numeric()
  Eall_matrix <- list()
  Eall_list <- list()
  
  for (comfort_model_type in 2){
    for (system in 3){
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
          
          #for (day in 11){
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
                result <- update_idf_max_multifan(idf, tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                                     tasp14, tasp15, tasp16, tasp17, tasp18, tasp19, 
                                     occ_day_data, system)
                
                ctrl_mode <- result$ctrl_mode
                #print(ctrl_mode)
                
                idf$save(overwrite = TRUE)
                idf$run(path_epw,
                        dir = path_cal,
                        wait = TRUE)
                
                zone_name <- "CORE_MID1"
                job <- idf$last_job()
                ta <- unlist(job$report_data(zone_name,"Zone Mean Air Temperature")[,6])
                tr <- unlist(job$report_data(zone_name,"Zone Mean Radiant Temperature")[,6])
                rh <- unlist(job$report_data(zone_name,"Zone Air Relative Humidity")[,6])
                
                # Continue with adding ta, tr, rh data
                # Define the row indices for data from 8 AM to 7 PM (19:00)
                time_indices <- 8:19
                
                # Filter data for the period from 8 AM to 7 PM
                ta_filtered <- ta[time_indices]
                tr_filtered <- tr[time_indices]
                rh_filtered <- rh[time_indices]
                
                # Check if ctrl_mode is a character vector or already a data frame
                if (is.character(ctrl_mode)) {
                  # Determine the number of columns for the data frame
                  number_of_columns <- 2 + Nocc  # Adjust this number based on your actual data
                  
                  # Convert character vector to a data frame correctly
                  ctrl_mode <- as.data.frame(matrix(ctrl_mode, ncol = number_of_columns, byrow = FALSE))
                  
                  # Add appropriate column names
                  colnames(ctrl_mode) <- c("Time", "Max_Temperature", paste0("FanMode_User_", 1:(ncol(ctrl_mode) - 2)))
                  
                } else if (!is.data.frame(ctrl_mode)) {
                  # If ctrl_mode is not a data frame and not a character vector, convert it to a data frame
                  ctrl_mode <- as.data.frame(ctrl_mode)
                }
                
                # Convert necessary columns from character to numeric, if applicable
                ctrl_mode$Time <- as.numeric(ctrl_mode$Time)
                ctrl_mode$Max_Temperature <- as.numeric(ctrl_mode$Max_Temperature)
                for (i in 1:Nocc) {
                  ctrl_mode[[paste0("FanMode_User_", i)]] <- as.character(ctrl_mode[[paste0("FanMode_User_", i)]])
                }
                
                
                # Ensure the filtered data is the correct length and add to ctrl_mode
                if (length(ta_filtered) == nrow(ctrl_mode) && length(tr_filtered) == nrow(ctrl_mode) && length(rh_filtered) == nrow(ctrl_mode)) {
                  ctrl_mode$Temperature_Air <- ta_filtered
                  ctrl_mode$Temperature_Radiant <- tr_filtered
                  ctrl_mode$Relative_Humidity <- rh_filtered
                } else {
                  stop("Filtered data length does not match ctrl_mode row count.")
                }
                
                # Check the result
                #print(ctrl_mode)
                        
                # record
                Eall <- get_energy(idf)
                Eall_list[[day]] <- Eall
                
                
              } 
            }
          }
        }
      }
    }
  }
  
  # Set output directory and create sub-directory for each N
  output_dir <- "~/localdir/AsimEx/cal/Result"
  sub_dir <- file.path(output_dir, paste0("N_", N))
  dir.create(sub_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Convert energy into df
  Eall_matrix <- do.call(rbind, Eall_list)
  
  # Save CSV for energy data
  write.csv(Eall_matrix, file = file.path(sub_dir, paste0("Eall_N", N, ".csv")), row.names = FALSE)
  
  # Save CSV for ctrl_mode data for each day
  for (day in 1:NDays) {
    ctrl_mode_path <- file.path(sub_dir, paste0("ctrl_mode_N", N, "_Day_", day, ".csv"))
    write.csv(ctrl_mode, file = ctrl_mode_path, row.names = FALSE)
  }
}

################################
# summary and save 
################################

# Load necessary libraries
#install.packages("dplyr")
library(dplyr)

# Specify the directory where the original CSV files are stored
input_dir <- "~/localdir/AsimEx/cal/Result/TP/Personal/240902_similar_truecomfort"
# Define the output directory for the results
output_dir <- input_dir

# Set the range of N values (2 to 30, with step size of 2)
N_values <- seq(2, 20, 2)

# Initialize a dataframe to store the results
results <- data.frame(N = integer(), mean_acceptance_sum = numeric(), Eall_sum = numeric())

#N<-20

# Loop over each N value
for (N in N_values) {
  
  # Set the file paths for mean_acceptance and Eall
  mean_acceptance_path <- file.path(input_dir, paste0("mean_acceptance_N", N, ".csv"))
  Eall_path <- file.path(input_dir, paste0("Eall_N", N, ".csv"))
  
  # Read the CSV files
  mean_acceptance_df <- read.csv(mean_acceptance_path)
  Eall_df <- read.csv(Eall_path)
  
  # Calculate the average of mean_acceptance and the sum of Eall
  mean_acceptance_avg <- mean(mean_acceptance_df$MeanAcceptance, na.rm = TRUE)
  Eall_sum <- sum(as.matrix(Eall_df), na.rm = TRUE)  # Eall_df is a matrix, sum all elements
  
  # Append the results to the dataframe
  results <- rbind(results, data.frame(N = N, mean_acceptance_avg = mean_acceptance_avg, Eall_sum = Eall_sum))
}

# Save the results to a CSV file
output_csv_path <- file.path(output_dir, "N_summary.csv")
write.csv(results, file = output_csv_path, row.names = FALSE)

# Completion message
cat("Results saved to", output_csv_path, "\n")

################################
# Extract setpoint
################################
# Load necessary libraries
library(dplyr)

# Specify the directory where the original CSV files are stored
input_dir <- "~/localdir/AsimEx/cal/Result/TP/Personal/240829_TP_comparefull_75%"
# Define the output directory for the results
output_dir <- input_dir

# Set the range of N values (2 to 20, with step size of 2)
N_values <- seq(2, 20, 2)

# Initialize a dataframe to store the combined results
combined_results <- NULL

# Loop over each N value
for (N in N_values) {
  
  # Set the file path for ctrl_mode
  ctrl_mode_path <- file.path(input_dir, paste0("ctrl_mode_N", N, ".csv"))
  
  # Read the CSV file
  ctrl_mode_df <- read.csv(ctrl_mode_path)
  
  # Extract the first column (time label) and the second column (data)
  if (is.null(combined_results)) {
    # Initialize combined_results with the first time label and first data column
    combined_results <- data.frame(Time = ctrl_mode_df[, 1], Data = ctrl_mode_df[, 2])
    colnames(combined_results)[2] <- paste0("N", N)  # Rename the second column to N{N}
  } else {
    # Add the new data column to the combined_results
    combined_results <- cbind(combined_results, ctrl_mode_df[, 2])
    colnames(combined_results)[ncol(combined_results)] <- paste0("N", N)  # Rename the new column
  }
}

# Save the combined results to a CSV file
output_csv_path <- file.path(output_dir, "ctrl_mode_combined_summary.csv")
write.csv(combined_results, file = output_csv_path, row.names = FALSE)

# Completion message
cat("Combined results saved to", output_csv_path, "\n")
