## Run optimization for impact analysis of individual comfort model on HVAC control
## 2021-03-24 E.Ono
## _rev: adapted to SinBerBEST model and so on, 2021-06-08

## 2024-08-14 K.Horikoshi
## _rev: adapted to SinBerBEST model with set comfort matrix, 2024-08-14

# not using 
# install devtools package if not exists
#if (!require("devtools", quietly = TRUE)) {
#  install.packages("devtools")
#  library(devtools)}
# install epluspar package if not exists
#if (!require("epluspar", quietly = TRUE)) {
#  devtools::install_github("hongyuanjia/epluspar")
#  library(epluspar)
#}

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
# Update IDF file
#####################################

# added occupancy matrix and system
update_idf <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
                        tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L,
                        occ_day_data, system) {

  # create the vector to include temperature schedule for full-day timeline
  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
  # test temperature
  # tasp <- c(21.36003937,22.67353829,23.76306684,23.43645242,23.12542441,23.51378413,23.03272871,22.85321603,23.41572812,23.78539024,22.43757939,22.28954219)
  
  
  if (system == 1) { # Zone Control
    # Define tasp list to update
    tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                   tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
    
    for (time in 8:19) {  # Loop for each hour from 8 to 19
      # Extract occupancy data for the current hour
      occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour
      
      if (all(occupancy_data == 0)) {
        # If all occupancy data is 0, record 30 in tasp_list
        tasp_list[time - 7] <- 30
      } else {
        # Filter accep_data where Fan Mode == 0
        fan_mode_0_data <- subset(accep_data, FanMode == 0)
        
        # Apply the occupancy filter to accep_data
        filtered_data <- fan_mode_0_data[apply(fan_mode_0_data[, 7:22], 1, function(row) {
          mean(row[occupancy_data == 1] == 1) >= 0.75
        }), ]
        
        # Identify temperature ranges where at least 75% of present users are comfortable
        acceptable_temperature_ranges <- filtered_data[,"Indoor.Temp"]
        
        # Update tasp based on the maximum acceptable temperature if available
        if (length(acceptable_temperature_ranges) > 0) {
          tasp_list[time - 7] <- max(acceptable_temperature_ranges)
        } else {
          # If no acceptable temperature ranges found, you might want to set a default value or leave tasp unchanged
          tasp_list[time - 7] <- 30  # Optionally set a default value if nothing is found
        }
      }
    }
  
  }else if (system == 3) { # Personal Control
    # Define tasp list to update
    tasp_list <- c(tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                   tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
    
    for (time in 8:19) {  # Loop for each hour from 8 to 19
      # Extract occupancy data for the current hour
      occupancy_data <- occ_day_data[time, ]  # 16 users' occupancy data for the current hour
      
      if (all(occupancy_data == 0)) {
        # If all occupancy data is 0, record 30 in tasp_list
        tasp_list[time - 7] <- 30
      } else {
        # Get unique temperature range
        unique_temperatures <- unique(accep_data$Indoor.Temp)
        
        # Initialize variable to store maximum acceptable temperature
        max_temperature <- NA
        
        for (temp in unique_temperatures) {
          # Filter data for the same temperature
          temp_data <- subset(accep_data, Indoor.Temp == temp)
          
          # Check if at least 75% of users are comfortable in any fan mode (columns 7 to 22)
          acceptable_rows <- apply(temp_data[, 7:22], 1, function(row) {
            mean(row[occupancy_data == 1] == 1) >= 0.75
          })
          
          # If the requirement is met in at least one row, update max_temperature
          if (any(acceptable_rows)) {
            max_temperature <- max(max_temperature, temp, na.rm = TRUE)
          }
        }
        
        # Update tasp_list with the maximum acceptable temperature if available
        if (!is.na(max_temperature)) {
          tasp_list[time - 7] <- max_temperature
        } else {
          tasp_list[time - 7] <- 30  # Optionally set a default value if nothing is found
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

  # set case name
  str1 <- sprintf(round((tasp[1] - 20)*10)/10,fmt = "%0.1f")
  for (i in 2:6){
    str1 <- paste0(str1,as.character(round((tasp[i] - 20)*10)))
  }
  
  str2 <- sprintf(round((tasp[7] - 20)*10)/10,fmt = "%0.1f")
  for (i in 8:12){
    str2 <- paste0(str2,as.character(round((tasp[i] - 20)*10)))
  }
  
  tmp <- idf$"Schedule:Compact"[["case_name1"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  dt[6,6] <- as.numeric(str1)
  idf$update(dt)
  
  tmp <- idf$"Schedule:Compact"[["case_name2"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  dt[6,6] <- as.numeric(str2)
  idf$update(dt)

  #idf$save(path_idf,"overwrite" = TRUE)
  idf
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
  
  source( "/Users/eikichiono/Documents/07_Program_codes/R/ono_functions.R" )
  
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

  # # setting paths
  # path_wd <- "/Users/eikichiono/Documents/02_Research/MBDC/Github/Document/Rule optimization/Code/Impact_analysis_of_individual_model/"
  # path_wd2 <- "/Users/eikichiono/Documents/02_Research/Mihara-san PhD experiment/"
  # 
  # # setting case names
  # case_name1 <- job$read_table("Schedules")[schedule_name=="CASE_NAME1",schedule_maximum]
  # case_name2 <- job$read_table("Schedules")[schedule_name=="CASE_NAME2",schedule_maximum]
  # 
  # case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
  #                     as.character(system),as.character(opt),as.character(Nday),
  #                     as.character(case_name1*10^11),as.character(case_name2*10^11))
  # 
  # case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")

  
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

    
  ###Need to edit here

  
  # occupants info update
  path_occ_occupant <- paste0(path_wd,"data/occ_occupant.csv")

  occ_occupant <- read.csv(path_occ_occupant,header=T)
  occ_occupant <- occ_occupant[(24*(Nday-1)+1):(24*Nday),1:Nocc]  #containing the presence for 24hours, for 16 occupants

  # cpmfort model types 
  model_name <- c("zone","group","personal")
  model_type <- c("TP","TP_AMP")

  # comfort model types 
  path_prob_sim <- paste0(path_wd2,model_type[comfort_model_type],"model_",model_name[comfort_model],".csv")
  path_prob_eval1 <- paste0(path_wd2,"TPmodel_personal.csv")
  path_prob_eval2 <- paste0(path_wd2,"TP_AMPmodel_personal.csv")

  prob_sim <- read.csv(path_prob_sim, skip=0, header=T)
  prob_eval1 <- read.csv(path_prob_eval1, skip=0, header=T)
  prob_eval2 <- read.csv(path_prob_eval2, skip=0, header=T)
  
  # comfort model types 
    # 1: VAV
    # 2: VAV + ceiling fan
    # 3: VAV + personal fan

  # defining occ number per group, Nocc is set in the fundamental setting 
  if (system == 1){
    Ngroup <- 1
  }else if (system == 2){
    Ngroup <- 4
  }else if (system == 3){
    Ngroup <- 16
  }
  Nocc_group <- Nocc/Ngroup # number of occupants per group : Nocc is defined 16
  
  # defining mode range accordind to system types
  if (system == 1){
    mode_range <- 0
  }else if (system > 1 && opt == 0){
    mode_range <- 3
  }else{
    mode_range <- 0:5
  }
  
  # create 24-length vector
  Nsatisfied_ave_all_sim <- numeric(Nstep)
  Nsatisfied_ave_all_eval1 <- numeric(Nstep)
  Nsatisfied_ave_all_eval2 <- numeric(Nstep)
  
  Ecfan <- numeric(Nstep)
  mode <- matrix(numeric(Ngroup*Nstep),nrow=Nstep)
  
  # ta <- rep(26.5,24)
  # tr <- ta + 2

    for (i in 1:Nstep){
    if (Eahu[i] > 0){

      # occupant presence for each time steps
      presence <- unlist(occ_occupant[i,1:Nocc])
      Npresent <- sum(presence)
      
      # assining the possibility of comfort, from each comfort model csv
      # the comfort model represents of comfort possibility, with temperature range for columns and each occupants models for rows 

      if (Npresent == 0){

      }else{
        
        # prob_sim_j <- prob_sim[prob_sim["ta"]==round(ta[i]*10)/10,1:(Nocc+2)]
        prob_sim_j <- prob_sim[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
        prob_eval1_j <- prob_eval1[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
        prob_eval2_j <- prob_eval2[((round(ta[i]*10)-220)*6+1):((round(ta[i]*10)-219)*6),3:(Nocc+2)]
        
        prob_sim_j <- t(prob_sim_j)
        prob_eval1_j <- t(prob_eval1_j)
        prob_eval2_j <- t(prob_eval2_j)

        # assign 0 for who is absent 
        prob_sim_j[presence==0,] <- 0
        prob_eval1_j[presence==0,] <- 0
        prob_eval2_j[presence==0,] <- 0
        
        for (group in 1:Ngroup){
          # for VAV system
          if (length(mode_range) == 1){
            # sum satisfaction for all the occupant
            Nsatisfied_group_sim <- sum(prob_sim_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
            Nsatisfied_group_eval1 <- sum(prob_eval1_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
            Nsatisfied_group_eval2 <- sum(prob_eval2_j[((group-1)*Nocc_group+1):(group*Nocc_group),mode_range+1])
            
            mode[i,group] <- mode_range
            Nsatisfied_ave_all_sim[i] <- Nsatisfied_ave_all_sim[i] + Nsatisfied_group_sim
            Nsatisfied_ave_all_eval1[i] <- Nsatisfied_ave_all_eval1[i] + Nsatisfied_group_eval1
            Nsatisfied_ave_all_eval2[i] <- Nsatisfied_ave_all_eval2[i] + Nsatisfied_group_eval2
            
          # for operation optimization
          }else{
            # sum for all occupant comfort
            if (Nocc_group > 1){
              Nsatisfied_group_sim <- apply(prob_sim_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
              Nsatisfied_group_eval1 <- apply(prob_eval1_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
              Nsatisfied_group_eval2 <- apply(prob_eval2_j[((group-1)*Nocc_group+1):(group*Nocc_group),],2,sum)
            
            # just extract whole row when only one occupant exist 
            }else{
              Nsatisfied_group_sim <- prob_sim_j[group,]
              Nsatisfied_group_eval1 <- prob_eval1_j[group,]
              Nsatisfied_group_eval2 <- prob_eval2_j[group,]
            }
            
            ind_mode <- which.max(Nsatisfied_group_sim)
            mode[i,group] <- mode_range[ind_mode]
            Nsatisfied_ave_all_sim[i] <- Nsatisfied_ave_all_sim[i] + Nsatisfied_group_sim[ind_mode]
            Nsatisfied_ave_all_eval1[i] <- Nsatisfied_ave_all_eval1[i] + Nsatisfied_group_eval1[ind_mode]
            Nsatisfied_ave_all_eval2[i] <- Nsatisfied_ave_all_eval2[i] + Nsatisfied_group_eval2[ind_mode]
          }
          
        }

        Ecfan[i] <-  sum(Ecfand[mode[i,]+1])
      }

    }
  }
  
  Eall <- Eahu_target + Eplant_target + Ecfan


  Npresent <- apply(occ_occupant,1,sum)

  A <- cbind(Eall,Npresent,Nsatisfied_ave_all_sim,Nsatisfied_ave_all_eval1,Nsatisfied_ave_all_eval2,ta,Eplant_target,Eahu_target,Ecfan,Eplant,Qcoil,Qmid1,Qmid2,Qmid3,Qmid4,Qmidn,Qmide,Qmids,Qmidw,mode)
  A[is.nan(A)==TRUE] <- 0
  A <- rbind(apply(A,2,sum),A[8:19,])
  Rdissatisfied <- 1 - A[,"Nsatisfied_ave_all_sim"]/A[,"Npresent"]
  Rdissatisfied_true_TP <- 1 - A[,"Nsatisfied_ave_all_eval1"]/A[,"Npresent"]
  Rdissatisfied_true_TP_AMP <- 1 - A[,"Nsatisfied_ave_all_eval2"]/A[,"Npresent"]
  A <- cbind(Rdissatisfied,Rdissatisfied_true_TP,Rdissatisfied_true_TP_AMP,A)
  
  A <- as.data.frame(A)
  write.csv(as.matrix(A),case_name_path, row.names=FALSE)

  tmp <- A[1,1:4]
  colnames(tmp) <- c("Rdis","Rdis1","Rdis2","Eall")
  print(tmp)
  
  obj <- A[1,"Eall"]
  return(obj/100)
}


################################
# calculate objective function 2
################################
get_discomfort <- function (idf) {
  
  job <- idf$last_job()
  stopifnot(!is.null(job))

#  path_wd <- "/Users/eikichiono/Documents/02_Research/MBDC/Github/Document/Rule optimization/Code/Impact_analysis_of_individual_model/"
  
  tp_combination_index <- job$read_table("Schedules")[schedule_name=="TP_COMBINATION",schedule_maximum]
  comfort_model <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL",schedule_maximum]
  comfort_model_type <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL_TYPE",schedule_maximum]
  system <- job$read_table("Schedules")[schedule_name=="STR_SYSTEM",schedule_maximum]
  opt <- job$read_table("Schedules")[schedule_name=="STR_OPT",schedule_maximum]
  Nday <- job$read_table("Schedules")[schedule_name=="NDAY",schedule_maximum]
  
  # case_name1 <- job$read_table("Schedules")[schedule_name=="CASE_NAME1",schedule_maximum]
  # case_name2 <- job$read_table("Schedules")[schedule_name=="CASE_NAME2",schedule_maximum]
  # case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
  #                     as.character(system),as.character(opt),as.character(Nday),
  #                     as.character(case_name1*10^11),as.character(case_name2*10^11))
  
  # case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")

    A <- read.csv(case_name_path, header=T)
  Rdissatisfied <- A[1,"Rdissatisfied"]
  
  print(Rdissatisfied)
  
  return(Rdissatisfied)
}

job <- idf$last_job()

case_name1 <- job$read_table("Schedules")[schedule_name=="CASE_NAME1",schedule_maximum]
case_name2 <- job$read_table("Schedules")[schedule_name=="CASE_NAME2",schedule_maximum]
case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
                    as.character(system),as.character(opt),as.character(Nday),
                    as.character(case_name1*10^11),as.character(case_name2*10^11))


#####################################
# Setting
#####################################

tp_combination_index <- 1

Nocc <- 16 # number of occupant in the zone
Nsub <- 26 # number of subjects at thermal comfort experiment
model_name <- c("zone","group","personal")
model_type <- c("TP","TP_AMP")
system_name <- c("VAV","Cfan","Pfan")
zone_name <- c("interior","perimeter")
zone_index <- 1
comfort_model_type <- 1

month <- 10
Nday_start <- 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 # September 30

#####################################
# Case loop
#####################################

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
        
        for (day in 11){
          
          week <- day%%7 + week_ini - 1
          Nday_start <- 0
          
          if (week <= 5){
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
              
              # pass Occupancy information" and "system" update_when callung update_idf function
              update_idf(idf, tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                         tasp14, tasp15, tasp16, tasp17, tasp18, tasp19, 
                         occ_day_data, system)
              
              idf$save(overwrite = TRUE)
              
              idf$run(path_epw,
                      dir = path_cal,
                      wait = TRUE)
              print("calculation finish")
              
              Rdissatisfied <- get_discomfort(idf)
              print(Rdissatisfied)
              
              Eall <- get_energy(idf)
              print(Eall)
              
            } 
          }
        }
      }
    }
  }
}


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

if (system == 3) { # Personal Control
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
      
      # Update tasp_list and ctrl_mode with the maximum acceptable temperature if available
      if (!is.na(max_temperature)) {
        tasp_list[time - 7] <- max_temperature
        ctrl_mode[time - 7, 2] <- max_temperature  # Set the max temperature
        ctrl_mode[time - 7, 3:(2 + Nocc)] <- optimal_fan_modes  # Record Fan mode
      } else {
        tasp_list[time - 7] <- 30
        ctrl_mode[time - 7, 2] <- 30  # Set 30 as the max temperature
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

print(ctrl_mode)


# system <- 3
# if (system == 3) { # Personal Control
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
#       # Get unique temperature range
#       unique_temperatures <- unique(accep_data$Indoor.Temp)
#       
#       # Initialize variable to store maximum acceptable temperature
#       max_temperature <- NA
#       
#       for (temp in unique_temperatures) {
#         # Filter data for the same temperature
#         temp_data <- subset(accep_data, Indoor.Temp == temp)
#         
#         # Check if at least 75% of users are comfortable with any fan mode (columns 7 to 22)
#         user_satisfaction <- apply(temp_data[, 7:22], 2, function(col) {
#           max(col[occupancy_data == 1], na.rm = TRUE) == 1  # Check if user is satisfied with any fan mode
#         })
#         
#         # Check if 75% or more users are satisfied with any fan mode
#         if (mean(user_satisfaction, na.rm = TRUE) >= 0.75) {
#           max_temperature <- max(max_temperature, temp, na.rm = TRUE)
#         }
#       }
#       
#       # Update tasp_list with the maximum acceptable temperature if available
#       if (!is.na(max_temperature)) {
#         tasp_list[time - 7] <- max_temperature
#       } else {
#         tasp_list[time - 7] <- 27  # Optionally set a default value if nothing is found
#       }
#     }
#     # Print the result for debugging
#     print(paste("Time:", time, "Max Acceptable Temperature:"))
#     print(tasp_list[time - 7])
#   }
# }

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

