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
# added occupancy matrix and system
update_idf <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
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
          mean(row[occupancy_data == 1] == 1) >= 0.75
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
          #temp_data <- subset(env_accep_agent, Tair == temp)
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
    tasp_core <- tasp
    tasp_peri <- tasp - 1
  }else if(zone_index == 2){ #  set temperature schedule for a perimeter zone
    tasp_core <- tasp + 1
    tasp_peri <- tasp
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
  

  # # occupants info update
  # path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant.csv")
  # 
  # occ_occupant <- read.csv(path_occ_occupant,header=T)
  # occ_occupant <- occ_occupant[(24*(Nday-1)+1):(24*Nday),1:Nocc]  #containing the presence for 24hours, for 16 occupants
  # 
  # comfort model types 
  # model_name <- c("zone","group","personal")
  # model_type <- c("TP","TP_AMP")

  # comfort model types 
  # path_prob_sim <- paste0(path_wd2,model_type[comfort_model_type],"model_",model_name[comfort_model],".csv")
  # path_prob_eval1 <- paste0(path_wd2,"TPmodel_personal.csv")
  # path_prob_eval2 <- paste0(path_wd2,"TP_AMPmodel_personal.csv")
  # 
  # prob_sim <- read.csv(path_prob_sim, skip=0, header=T)
  # prob_eval1 <- read.csv(path_prob_eval1, skip=0, header=T)
  # prob_eval2 <- read.csv(path_prob_eval2, skip=0, header=T)
  
  # comfort model types 
    # 1: VAV
    # 2: VAV + ceiling fan
    # 3: VAV + personal fan

  # # defining occ number per group, Nocc is set in the fundamental setting 
  # if (system == 1){
  #   Ngroup <- 1
  # }else if (system == 2){
  #   Ngroup <- 4
  # }else if (system == 3){
  #   Ngroup <- 16
  # }
  # Nocc_group <- Nocc/Ngroup # number of occupants per group : Nocc is defined 16
  
  # # defining mode range accordind to system types
  # if (system == 1){
  #   mode_range <- 0
  # }else if (system > 1 && opt == 0){
  #   mode_range <- 3
  # }else{
  #   mode_range <- 0:5
  # }
  
  # # create 24-length vector
  # Nsatisfied_ave_all_sim <- numeric(Nstep)
  # Nsatisfied_ave_all_eval1 <- numeric(Nstep)
  # Nsatisfied_ave_all_eval2 <- numeric(Nstep)
  
  # Ecfan <- numeric(Nstep)
  # mode <- matrix(numeric(Ngroup*Nstep),nrow=Nstep)
  
  # # ta <- rep(26.5,24)
  # # tr <- ta + 2
  # 
  #   for (i in 1:Nstep){
  #   if (Eahu[i] > 0){
  # 
  #     # occupant presence for each time steps
  #     presence <- unlist(occ_occupant[i,1:Nocc])
  #     Npresent <- sum(presence)
  #     
  #     # assining the possibility of comfort, from each comfort model csv
  #     # the comfort model represents of comfort possibility, with temperature range for columns and each occupants models for rows 
  # 
  #     if (Npresent == 0){
  # 
  #     }else{
  #       
  #       # prob_sim_j <- prob_sim[prob_sim["ta"]==round(ta[i]*10)/10,1:(Nocc+2)]
  #       prob_sim_j <- prob_sim[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
  #       prob_eval1_j <- prob_eval1[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
  #       prob_eval2_j <- prob_eval2[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
  #       
  #       prob_sim_j <- t(prob_sim_j)
  #       prob_eval1_j <- t(prob_eval1_j)
  #       prob_eval2_j <- t(prob_eval2_j)
  # 
  #       # assign 0 for who is absent 
  #       prob_sim_j[presence==0,] <- 0
  #       prob_eval1_j[presence==0,] <- 0
  #       prob_eval2_j[presence==0,] <- 0
  #       
  #       for (group in 1:Ngroup){
  #         # for VAV system
  #         if (length(mode_range) == 1){
  #           # sum satisfaction for all the occupant
  #           Nsatisfied_group_sim <- sum(prob_sim_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
  #           Nsatisfied_group_eval1 <- sum(prob_eval1_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
  #           Nsatisfied_group_eval2 <- sum(prob_eval2_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
  #           
  #           mode[i,group] <- mode_range
  #           Nsatisfied_ave_all_sim[i] <- Nsatisfied_ave_all_sim[i] + Nsatisfied_group_sim
  #           Nsatisfied_ave_all_eval1[i] <- Nsatisfied_ave_all_eval1[i] + Nsatisfied_group_eval1
  #           Nsatisfied_ave_all_eval2[i] <- Nsatisfied_ave_all_eval2[i] + Nsatisfied_group_eval2
  #           
  #         # for operation optimization
  #         }else{
  #           # sum for all occupant comfort
  #           if (Nocc_group > 1){
  #             Nsatisfied_group_sim <- apply(prob_sim_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
  #             Nsatisfied_group_eval1 <- apply(prob_eval1_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
  #             Nsatisfied_group_eval2 <- apply(prob_eval2_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
  #           
  #           # just extract whole row when only one occupant exist 
  #           }else{
  #             Nsatisfied_group_sim <- prob_sim_j[group,]
  #             Nsatisfied_group_eval1 <- prob_eval1_j[group,]
  #             Nsatisfied_group_eval2 <- prob_eval2_j[group,]
  #           }
  #           
  #           ind_mode <- which.max(Nsatisfied_group_sim)
  #           mode[i,group] <- mode_range[ind_mode]
  #           Nsatisfied_ave_all_sim[i] <- Nsatisfied_ave_all_sim[i] + Nsatisfied_group_sim[ind_mode]
  #           Nsatisfied_ave_all_eval1[i] <- Nsatisfied_ave_all_eval1[i] + Nsatisfied_group_eval1[ind_mode]
  #           Nsatisfied_ave_all_eval2[i] <- Nsatisfied_ave_all_eval2[i] + Nsatisfied_group_eval2[ind_mode]
  #         }
  #         
  #       }
  # 
  #       Ecfan[i] <-  sum(Ecfand[mode[i,]+1])
  #     }
  # 
  #   }
  #   }
  
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
      matching_rows <- env_accep_agent$Tair == Tair
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
NDays <- 30  # 30days

path_wd <- "/home/rstudio/localdir"

# Load true acceptance
env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/env_accep_agent.csv", header = TRUE)
#env_accep_agent <- read.csv("~/localdir/AsimEx/Comfort_models/true_acceptance/user_comf_agent.csv", header = TRUE)

#####################################
# N loop (N=2~30)
#####################################

for (N in seq(2, 30, 2)) {
  
  # Load predicted acceptance
  path_accep <- paste0(path_wd, "/AsimEx/Comfort_models/predicted_acceptance/case3_max30/predicted_acceptance_classes_N", N, ".csv")
  accep_data <- read.csv(path_accep)
  
  #####################################
  # Case loop
  #####################################
  
  #Initialize output
  mean_acceptance_df <- numeric()
  mean_acceptance_list <- numeric()
  Eall_matrix <- list()
  Eall_list <- list()
  
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
  
  # set directory
  output_dir <- "~/localdir/AsimEx/cal/Result"
  
  # convert into df
  num_days <- length(mean_acceptance_list)
  mean_acceptance_df <- data.frame(Day = 1:num_days, MeanAcceptance = mean_acceptance_list)
  
  Eall_matrix <- do.call(rbind, Eall_list)
  
  # CSV
  write.csv(mean_acceptance_df, file = file.path(output_dir, paste0("mean_acceptance_N", N, ".csv")), row.names = FALSE)
  write.csv(Eall_matrix, file = file.path(output_dir, paste0("Eall_N", N, ".csv")), row.names = FALSE)
  
  # assign saving path of ctrl_mode
  output_path <- file.path(output_dir, paste0("ctrl_mode_N", N, ".csv"))
  
  # save ctrl_mode df
  write.csv(ctrl_mode, file = output_path, row.names = FALSE)
}

################################
# summary and save 
################################

# Load necessary libraries
#install.packages("dplyr")
library(dplyr)

# Define the output directory for the results
output_dir <- "~/localdir/AsimEx/cal/Result"

# Specify the directory where the original CSV files are stored
input_dir <- "~/localdir/AsimEx/cal/Result/Zone/Case3_Zone"

# Set the range of N values (2 to 30, with step size of 2)
N_values <- seq(2, 30, 2)

# Initialize a dataframe to store the results
results <- data.frame(N = integer(), mean_acceptance_sum = numeric(), Eall_sum = numeric())

# Loop over each N value
for (N in N_values) {
  
  # Set the file paths for mean_acceptance and Eall
  mean_acceptance_path <- file.path(input_dir, paste0("mean_acceptance_N", N, ".csv"))
  Eall_path <- file.path(input_dir, paste0("Eall_N", N, ".csv"))
  
  # Read the CSV files
  mean_acceptance_df <- read.csv(mean_acceptance_path)
  Eall_df <- read.csv(Eall_path)
  
  # Calculate the sum of mean_acceptance and Eall
  mean_acceptance_sum <- sum(mean_acceptance_df$MeanAcceptance, na.rm = TRUE)
  Eall_sum <- sum(as.matrix(Eall_df), na.rm = TRUE)  # Eall_df is a matrix, sum all elements
  
  # Append the results to the dataframe
  results <- rbind(results, data.frame(N = N, mean_acceptance_sum = mean_acceptance_sum, Eall_sum = Eall_sum))
}

# Save the results to a CSV file
output_csv_path <- file.path(output_dir, "N_summary.csv")
write.csv(results, file = output_csv_path, row.names = FALSE)

# Completion message
cat("Results saved to", output_csv_path, "\n")







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

