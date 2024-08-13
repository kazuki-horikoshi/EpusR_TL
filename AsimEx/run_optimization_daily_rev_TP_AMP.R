## Run optimization for impact analysis of individual comfort model on HVAC control
## 2021-03-24 E.Ono
## _rev: adapted to SinBerBEST model and so on, 2021-06-08

#####################################
# Load libraries
#####################################
# load the packages
library(eplusr)

# install devtools package if not exists
#if (!require("devtools", quietly = TRUE)) {
#  install.packages("devtools")
#  library(devtools)}


# install epluspar package if not exists
#if (!require("epluspar", quietly = TRUE)) {
#  devtools::install_github("hongyuanjia/epluspar")
#  library(epluspar)
  #}

#source( "C:/Users/F18863/OneDrive - KAJIMA/00_Program/R/ono_functions.R" )

# install here package if not exists
install.packages("here")
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

job <- idf$run(path_epw, wait = TRUE)

#####################################
# Update IDF file during optimization
#####################################
update_idf <- function (idf, tasp8=26L, tasp9=26L, tasp10=26L, tasp11=26L, tasp12=26L, tasp13=26L, 
                        tasp14=26L, tasp15=26L, tasp16=26L, tasp17=26L, tasp18=26L, tasp19=26L) {

  tasp <- c(tasp8,tasp9,tasp10,tasp11,tasp12,tasp13,tasp14,tasp15,tasp16,tasp17,tasp18,tasp19)
  
  # tasp <- c(21.36003937,22.67353829,23.76306684,23.43645242,23.12542441,23.51378413,23.03272871,22.85321603,23.41572812,23.78539024,22.43757939,22.28954219)
  
  ## Update "Zone cooling setpoint"
  
  tmp <- idf$"Schedule:Compact"[["zone_index"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  zone_index <- dt[6,6]
  
  if (zone_index == 1){ # optimize an interior zone
    tasp_core <- tasp
    tasp_peri <- tasp - 1
  }else if(zone_index == 2){ # optimize a perimeter zone
    tasp_core <- tasp + 1
    tasp_peri <- tasp
  }

  tmp <- idf$"Schedule:Compact"[["sch_setpoint_core"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_core[i]
  }
  
  idf$update(dt)
  
  tmp <- idf$"Schedule:Compact"[["sch_setpoint_peri"]]
  dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
  
  for (i in 1:12){
    dt[(2*i+6),6] <- tasp_peri[i]
  }
  
  idf$update(dt)
  
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

  # setting paths
  path_wd <- "/Users/eikichiono/Documents/02_Research/MBDC/Github/Document/Rule optimization/Code/Impact_analysis_of_individual_model/"
  path_wd2 <- "/Users/eikichiono/Documents/02_Research/Mihara-san PhD experiment/"

  # getting infor from idf
  zone_index <- job$read_table("Schedules")[schedule_name=="ZONE_INDEX",schedule_maximum]
  tp_combination_index <- job$read_table("Schedules")[schedule_name=="TP_COMBINATION",schedule_maximum]
  comfort_model <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL",schedule_maximum]
  comfort_model_type <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL_TYPE",schedule_maximum]
  system <- job$read_table("Schedules")[schedule_name=="STR_SYSTEM",schedule_maximum]
  opt <- job$read_table("Schedules")[schedule_name=="STR_OPT",schedule_maximum]
  Nday <- job$read_table("Schedules")[schedule_name=="NDAY",schedule_maximum]

  # setting case names
  case_name1 <- job$read_table("Schedules")[schedule_name=="CASE_NAME1",schedule_maximum]
  case_name2 <- job$read_table("Schedules")[schedule_name=="CASE_NAME2",schedule_maximum]
  
  case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
                      as.character(system),as.character(opt),as.character(Nday),
                      as.character(case_name1*10^11),as.character(case_name2*10^11))
  
  case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")

  
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
  
  #idf <- read_idf(path = path_idf, idd = NULL)
  #job <- idf$run(path_epw, wait = TRUE)
  job <- idf$last_job()
  stopifnot(!is.null(job))

  path_wd <- "/Users/eikichiono/Documents/02_Research/MBDC/Github/Document/Rule optimization/Code/Impact_analysis_of_individual_model/"
  
  tp_combination_index <- job$read_table("Schedules")[schedule_name=="TP_COMBINATION",schedule_maximum]
  comfort_model <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL",schedule_maximum]
  comfort_model_type <- job$read_table("Schedules")[schedule_name=="COMFORT_MODEL_TYPE",schedule_maximum]
  system <- job$read_table("Schedules")[schedule_name=="STR_SYSTEM",schedule_maximum]
  opt <- job$read_table("Schedules")[schedule_name=="STR_OPT",schedule_maximum]
  Nday <- job$read_table("Schedules")[schedule_name=="NDAY",schedule_maximum]
  
  case_name1 <- job$read_table("Schedules")[schedule_name=="CASE_NAME1",schedule_maximum]
  case_name2 <- job$read_table("Schedules")[schedule_name=="CASE_NAME2",schedule_maximum]
  case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
                      as.character(system),as.character(opt),as.character(Nday),
                      as.character(case_name1*10^11),as.character(case_name2*10^11))
  
  case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")
  A <- read.csv(case_name_path, header=T)
  Rdissatisfied <- A[1,"Rdissatisfied"]
  
  print(Rdissatisfied)
  
  return(Rdissatisfied)
}

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
            
            # Update schedules
      
            path_occ_occupant <- paste0(path_wd,"/AsimEx/occ_occupant.csv")
            occ_occupant <- read.csv(path_occ_occupant,header=T)
            occ_total <- occ_occupant[,(Nocc+1)]
            
            tmp <- idf$"Schedule:Compact"[["Sch_Occupancy_Target"]]
            dt <- data.table::rbindlist(c(list(tmp$to_table()), lapply(tmp$ref_to_object(), function (x) x$to_table())))
            
            for (i in 1:12){
              dt[(2*i+6),6] <- occ_total[24*(Nday-1)+i+7]/100
            }
            idf$update(dt)
            
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
              
              update_idf(idf, tasp8, tasp9, tasp10, tasp11, tasp12, tasp13, 
                                      tasp14, tasp15, tasp16, tasp17, tasp18, tasp19)
              
              idf$save(overwrite = TRUE)
              idf$run(path_epw,
                      dir = path_cal,
                      wait = TRUE)
              
              Eall <- get_energy(idf)
              Rdissatisfied <- get_discomfort(idf)
              
              population <- data.frame(index_gen=1,index_ind=1,tasp8=tasp8,tasp9=tasp9, 
                                       tasp10=tasp10,tasp11=tasp11,tasp12=tasp12,tasp13=tasp13, 
                                       tasp14=tasp14,tasp15=tasp15,tasp16=tasp16,tasp17=tasp17,
                                       tasp18=tasp18, tasp19=tasp19,get_energy=Eall*100,
                                       get_discomfort=Rdissatisfied)
              pareto <- population[,2:16]
                
            }else{
      
            # comment out for optimization part
              
              ################################
              # Run GA optimization
              ################################

              # create a GA optimization job
              ga <- GAOptimJob$new(path_idf, path_epw)

              ####################
              ## apply_measure {{{
              ####################

              if (system == 1){
                Tmin <- 22
                Tmax <- 26
              }else{
                Tmin <- 24
                Tmax <- 28
              }

              ga$apply_measure(
                measure = update_idf,
                tasp8 = float_space(Tmin, Tmax),
                tasp9 = float_space(Tmin, Tmax),
                tasp10 = float_space(Tmin, Tmax),
                tasp11 = float_space(Tmin, Tmax),
                tasp12 = float_space(Tmin, Tmax),
                tasp13 = float_space(Tmin, Tmax),
                tasp14 = float_space(Tmin, Tmax),
                tasp15 = float_space(Tmin, Tmax),
                tasp16 = float_space(Tmin, Tmax),
                tasp17 = float_space(Tmin, Tmax),
                tasp18 = float_space(Tmin, Tmax),
                tasp19 = float_space(Tmin, Tmax)
              )

              ga$objective(get_energy, get_discomfort ,.dir = "min")

              options("warning.length" = 8170)
              ga$validate()

              # specify how to mix solutions
              ga$recombinator()
              # specify how to change parts of one solution randomly
              ga$mutator()
              # specify how to select best solutions
              ga$selector()
              # specify the conditions when to terminate the computation


              # condition.fun <- function(log) {
              #   # fitness of current individual
              #   fit <- log$env$pop[[log$env$n.gens]]$fitness
              #   fit <- fit[, ncol(fit)]
              #   # check if creteria are met
              #   fit["get_energy"] <= 0.15 && fit["get_discomfort"] <= 0.1
              # }
              # ga$terminator(condition.fun)

              if (comfort_model == 1){ # Individual model increases the number of iterations to converge
                Nmax_gen <- 40
              }else{
                Nmax_gen <- 40
              }

              condition.fun <- function(log) {
                # fitness of current individual
                Ngen <- log$env$n.gens
                if (Ngen == 1){
                  FALSE
                }else{
                  fit_pre <- mean(log$env$pop[[Ngen-1]]$fitness)
                  fit <- mean(log$env$pop[[Ngen]]$fitness)
                  # check if creteria are met
                  abs(fit_pre - fit)/fit_pre < 1e-5
                }

              }

              ga$terminator(fun = condition.fun, "MeetCreteria", "Terminated_by_convergence_criteria", max_gen = Nmax_gen)

              ga$run(mu = 20, dir = here::here("results"))


              # get all population
              population <- ga$population()
              # get Pareto set
              pareto <- ga$pareto_set()

              population[,"get_energy"] <- population[,"get_energy"]*100
              pareto[,"get_energy"] <- pareto[,"get_energy"]*100

            }

            tmp <- c()
            tmp2 <- c()
            for (i in 1:nrow(pareto)){
              tasp <- unlist(pareto[i,2:13])
              str1 <- sprintf(round((tasp[1] - 20)*10)/10,fmt = "%0.1f")
              for (j in 2:6){
                str1 <- paste0(str1,as.character(round((tasp[j] - 20)*10)))
              }

              str2 <- sprintf(round((tasp[7] - 20)*10)/10,fmt = "%0.1f")
              for (j in 8:12){
                str2 <- paste0(str2,as.character(round((tasp[j] - 20)*10)))
              }

              case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
                                  as.character(system),as.character(opt),as.character(Nday),
                                  as.character(as.numeric(str1)*10^11),as.character(as.numeric(str2)*10^11))
              case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")
              A <- read.csv(case_name_path, header=T)
              tmp <- rbind(tmp,A[1,])
              tmp2 <- rbind(tmp2,A[2:nrow(A),])
            }
            tmp <- as.data.frame(tmp)
            tmp2 <- as.data.frame(tmp2)
            pareto <- cbind(pareto,tmp)

            tmp <- c()
            for (i in 1:nrow(population)){
              tasp <- unlist(population[i,3:14])
              str1 <- sprintf(round((tasp[1] - 20)*10)/10,fmt = "%0.1f")
              for (j in 2:6){
                str1 <- paste0(str1,as.character(round((tasp[j] - 20)*10)))
              }

              str2 <- sprintf(round((tasp[7] - 20)*10)/10,fmt = "%0.1f")
              for (j in 8:12){
                str2 <- paste0(str2,as.character(round((tasp[j] - 20)*10)))
              }

              case_name <- paste0(as.character(tp_combination_index),as.character(comfort_model_type),as.character(comfort_model),
                                  as.character(system),as.character(opt),as.character(Nday),
                                  as.character(as.numeric(str1)*10^11),as.character(as.numeric(str2)*10^11))
              case_name_path <- paste0(path_wd,"tmp/",case_name,".csv")
              A <- read.csv(case_name_path, header=T)
              tmp <- rbind(tmp,A[1,])
            }
            tmp <- as.data.frame(tmp)
            population <- cbind(population,tmp)

            fname_pareto <- paste0(path_wd,"data3/pareto_",zone_name[zone_index],"_combination",tp_combination_index,"_",system_name[system],"_",model_type[comfort_model_type],"_",model_name[comfort_model],"_opt",opt,"_Nday",Nday,".csv")
            fname_pareto2 <- paste0(path_wd,"data3/pareto_hourly_",zone_name[zone_index],"_combination",tp_combination_index,"_",system_name[system],"_",model_type[comfort_model_type],"_",model_name[comfort_model],"_opt",opt,"_Nday",Nday,".csv")
            fname_population <- paste0(path_wd,"data3/population_",zone_name[zone_index],"_combination",tp_combination_index,"_",system_name[system],"_",model_type[comfort_model_type],"_",model_name[comfort_model],"_opt",opt,"_Nday",Nday,".csv")
            write.csv(as.matrix(pareto),fname_pareto, row.names=FALSE)
            write.csv(as.matrix(tmp2),fname_pareto2, row.names=FALSE)
            write.csv(as.matrix(population),fname_population, row.names=FALSE)
            
            # end
            
          }
          
        }
      }
    }
  }
}

#####################################
# Test for control logic
#####################################
## 2024-08-13 K.Horikoshi

path_wd <- "/home/rstudio/localdir"
path_accep <- paste0(path_wd,"/AsimEx/Comfort_models/predicted_acceptance/predicted_acceptance_N2.csv")

data <- read.csv(path_accep)

### Test Zone Control

# Fan Modeが0のデータのみをフィルタリング
fan_mode_0_data <- subset(data, FanMode == 0)

# 7列目から22列目（X2からX24）を対象にする
acceptable_temperature_ranges <- fan_mode_0_data[apply(fan_mode_0_data[, 7:22], 1, function(row) {
  mean(row == 1) >= 0.75
}), "Indoor.Temp"]

# 結果を表示
print(acceptable_temperature_ranges)

### Test Personal Hybrid Control

# ユニークな温度帯を取得
unique_temperatures <- unique(data$Indoor.Temp)

# 各温度帯ごとに最適な環境を探す
optimal_temperatures <- c()

for (temp in unique_temperatures) {
  # 同じ温度の行をフィルタリング
  temp_data <- subset(data, Indoor.Temp == temp)
  
  # 7列目から22列目（X2からX24）を対象に、行ごとに75%以上のユーザーが1であるかを確認
  acceptable_rows <- apply(temp_data[, 7:22], 1, function(row) {
    mean(row == 1) >= 0.75
  })
  
  # 少なくとも1行で条件を満たしているか確認
  if (any(acceptable_rows)) {
    optimal_temperatures <- c(optimal_temperatures, temp)
  }
}

# 結果を表示
print(optimal_temperatures)

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

# Nstep<-24
# Nsatisfied_ave_all_sim <- numeric(Nstep)
