# Ono's functions

clearConsole = clc = function(){
  # Clear console messages
  cat( "\014" )
}

clearPlots = function () {
  # Clear plots
  if( dev.cur() > 1 ) dev.off()
}

clearWorkspace = function () {
  # Clear global workspace
  rm( list = ls( envir = globalenv() ), envir = globalenv() )
}

clearAll = function(){
  # Clear console, plots, and workspace
  clearConsole()
  clearPlots()
  clearWorkspace()
}

data_average = function(data,ap){
  nr <- floor(nrow(data)/ap)
  
  if (length(nr) == 0){
    nr <- floor(length(data)/ap)
    Average <- numeric(nr)
    for (i in 1:nr) {
      Average[i] <- mean(data[(ap*(i-1)+1):(ap*i)])
    }
  }else{
    nc <- ncol(data)
    Average <- matrix(numeric(nr*nc),nr,nc)
    for (i in 1:nr) {
      Average[i,] <- apply(data[(ap*(i-1)+1):(ap*i),],2,mean,na.rm=TRUE)
    }
  }
  return(Average)
}

data_average_per = function(data,ap){
  nr <- floor(nrow(data)/ap)
  
  if (length(nr) == 0){
    nr <- floor(length(data)/ap)
    sumup <- numeric(ap)
    for (i in 1:nr){
      sumup <- sumup + data[((i-1)*ap+1):(i*ap)]
    }
    Average <- sumup/nr
  }else{
    sumup <- numeric(ap)
    for (i in 1:nr){
      sumup <- sumup + data[((i-1)*ap+1):(i*ap),]
    }
    Average <- sumup/nr
  }
  
  return(Average)
}

data_sumup = function(data,sp) {
  nr <- floor(nrow(data)/sp)
  
  if (length(nr) == 0){
    nr <- floor(length(data)/sp)
    Sumup <- numeric(nr)
    for (i in 1:nr) {
      Sumup[i] <- sum(data[(sp*(i-1)+1):(sp*i)])
    }
  }else{
    nc <- ncol(data)
    Sumup <- matrix(numeric(nr*nc),nr,nc)
    for (i in 1:nr) {
      Sumup[i,] <- apply(data[(sp*(i-1)+1):(sp*i),],2,sum,na.rm=TRUE)
    }
  }

  return(Sumup)
}

monthlySum <- function(data) {
  
  nday <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  nr <- 12
  nc <- ncol(data)
  
  Sumup <- matrix(numeric(nr*nc),nr,nc)
  ss <- 0
  for (i in 1:nr) {
    Sumup[i,] <- apply(data[(ss + 1):(ss + nday[i]),],2,sum)
    ss <- ss + nday[i]
  }
  return(Sumup)
}

nday_calc <- function(md){
  monthlyDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  nday <- sum(monthlyDays[1:(md[1]-1)]) + md[2]
  return(nday)
}

# ASHRAE Standard 55 2017‚CBE Comfort Tool
# 2018.9.4 E.Ono
set_calc_cbe = function(u){ 

ta <- u[1]
tr <- u[2]
vel <- u[3]
rh <- u[4]
met <- u[5]
clo <- u[6]
wme <- u[7]

PATM <- 101.325
VaporPressure <- rh * FindSaturatedVaporPressureTorr(ta) / 100
AirVelocity <- max(vel, 0.1)
KCLO <- 0.25
BODYWEIGHT <- 69.9
BODYSURFACEAREA <- 1.8258
METFACTOR <- 58.2
SBC <- 0.000000056697 # Stefan-Boltzmann constant (W/m2K4)
CSW <- 170
CDIL <- 120
CSTR <- 0.5

TempSkinNeutral <- 33.7 #setpoint (neutral) value for Tsk
TempCoreNeutral <- 36.8 #setpoint value for Tcr
TempBodyNeutral <- 36.49 #setpoint for Tb (.1*TempSkinNeutral + .9*TempCoreNeutral)
SkinBloodFlowNeutral <- 6.3 #neutral value for SkinBloodFlow

#INITIAL VALUES - start of 1st experiment
TempSkin <- TempSkinNeutral
TempCore <- TempCoreNeutral
SkinBloodFlow <- SkinBloodFlowNeutral
MSHIV <- 0.0
ALFA <- 0.1
ESK <- 0.1 * met

#Start new experiment here (for graded experiments)
#UNIT CONVERSIONS (from input variables)

#     p <- psy.PROP.Patm / 1000 # TH : interface?
  
  PressureInAtmospheres <- PATM * 0.009869
LTIME <- 60.0
TIMEH <- LTIME / 60.0
RCL <- 0.155 * clo
# AdjustICL(RCL, Conditions)  TH: I don't think this is used in the software

FACL <- 1.0 + 0.15 * clo ## INCREASE IN BODY SURFACE AREA DUE TO CLOTHING
LR <- 2.2 / PressureInAtmospheres #Lewis Relation is 2.2 at sea level
RM <- met * METFACTOR
M <- met * METFACTOR

if (clo <= 0){
    WCRIT <- 0.38 * AirVelocity^(-0.29)
    ICL <- 1.0
}else{
    WCRIT <- 0.59 * AirVelocity^(-0.08)
    ICL <- 0.45
}

CHC <- 3.0 * PressureInAtmospheres^0.53
CHCV <- 8.600001 * (AirVelocity * PressureInAtmospheres)^0.53
CHC <- max(CHC, CHCV)

#initial estimate of Tcl
CHR <- 4.7
CTC <- CHR + CHC
RA <- 1.0 / (FACL * CTC) #resistance of air layer to dry heat transfer
TOP <- (CHR * tr + CHC * ta) / CTC
TCL <- TOP + (TempSkin - TOP) / (CTC * (RA + RCL))

# <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-  BEGIN ITERATION
#
# Tcl and CHR are solved iteratively using: H(Tsk - To) <- CTC(Tcl - To),
#  where H <- 1/(Ra + Rcl) and Ra <- 1/Facl*CTC
#

TCL_OLD <- TCL
flag <- 1

for (TIM in 1:LTIME){
    for (i in 1:1000){
        if (flag == 1){
            TCL_OLD <- TCL
            CHR <- 4.0 * SBC * (((TCL + tr) / 2.0 + 273.15)^3.0) * 0.72
            CTC <- CHR + CHC
            RA <- 1.0 / (FACL * CTC) #resistance of air layer to dry heat transfer
            TOP <- (CHR * tr + CHC * ta) / CTC
        }
        TCL <- (RA * TempSkin + RCL * TOP) / (RA + RCL)
        flag <- 1
        if (abs(TCL - TCL_OLD) < 0.01){
            break
        }
    }
    flag <- FALSE
    DRY <- (TempSkin - TOP) / (RA + RCL)
    HFCS <- (TempCore - TempSkin) * (5.28 + 1.163 * SkinBloodFlow)
    ERES <- 0.0023 * M * (44.0 - VaporPressure)
    CRES <- 0.0014 * M * (34.0 - ta)
    SCR <- M - HFCS - ERES - CRES - wme
    SSK <- HFCS - DRY - ESK
    TCSK <- 0.97 * ALFA * BODYWEIGHT
    TCCR <- 0.97 * (1 - ALFA) * BODYWEIGHT
    DTSK <- (SSK * BODYSURFACEAREA) / (TCSK * 60.0) #deg C per minute
    DTCR <- SCR * BODYSURFACEAREA / (TCCR * 60.0) #deg C per minute
    TempSkin <- TempSkin + DTSK
    TempCore <- TempCore + DTCR
    TB <- ALFA * TempSkin + (1 - ALFA) * TempCore
    SKSIG <- TempSkin - TempSkinNeutral
    WARMS <- max(0,SKSIG)
    WARMS <- max(0,SKSIG)
    COLDS <- max(0,-1.0 * SKSIG)
    CRSIG <- (TempCore - TempCoreNeutral)
    WARMC <- max(0,CRSIG)
    COLDC <- max(0,-1.0 * CRSIG)
    BDSIG <- TB - TempBodyNeutral
    WARMB <- max(0,BDSIG)
    COLDB <- max(0,-1.0 * BDSIG)
    SkinBloodFlow <- (SkinBloodFlowNeutral + CDIL * WARMC) / (1 + CSTR * COLDS)
    if (SkinBloodFlow > 90.0){
        SkinBloodFlow <- 90.0
    }
    if (SkinBloodFlow < 0.5){
        SkinBloodFlow <- 0.5
    }
    REGSW <- CSW * WARMB * exp(WARMS / 10.7)
    if (REGSW > 500.0){
        REGSW <- 500.0
    }
    ERSW <- 0.68 * REGSW
    REA <- 1.0 / (LR * FACL * CHC) #evaporative resistance of air layer
    RECL <- RCL / (LR * ICL) #evaporative resistance of clothing (icl<-.45)
    EMAX <- (FindSaturatedVaporPressureTorr(TempSkin) - VaporPressure) / (REA + RECL)
    PRSW <- ERSW / EMAX
    PWET <- 0.06 + 0.94 * PRSW
    EDIF <- PWET * EMAX - ERSW
    ESK <- ERSW + EDIF
    if (PWET > WCRIT){
        PWET <- WCRIT
        PRSW <- WCRIT / 0.94
        ERSW <- PRSW * EMAX
        EDIF <- 0.06 * (1.0 - PRSW) * EMAX
        ESK <- ERSW + EDIF
    }
    if (EMAX < 0){
        EDIF <- 0
        ERSW <- 0
        PWET <- WCRIT
        PRSW <- WCRIT
        ESK <- EMAX
    }
    ESK <- ERSW + EDIF
    MSHIV <- 19.4 * COLDS * COLDC
    M <- RM + MSHIV
    ALFA <- 0.0417737 + 0.7451833 / (SkinBloodFlow + 0.585417)
}

#Define new heat flow terms, coeffs, and abbreviations
STORE <- M - wme - CRES - ERES - DRY - ESK #rate of body heat storage

HSK <- DRY + ESK #total heat loss from skin
RN <- M - wme #net metabolic heat production
ECOMF <- 0.42 * (RN - (1 * METFACTOR))
if (ECOMF < 0.0){
    ECOMF <- 0.0 #from Fanger
}
EREQ <- RN - ERES - CRES - DRY
EMAX <- EMAX * WCRIT
HD <- 1.0 / (RA + RCL)
HE <- 1.0 / (REA + RECL)
W <- PWET
PSSK <- FindSaturatedVaporPressureTorr(TempSkin)
# Definition of ASHRAE standard environment... denoted "S"
CHRS <- CHR
if (met < 0.85) {
    CHCS <- 3.0
} else {
    CHCS <- 5.66 * (met - 0.85)^0.39
    if (CHCS < 3.0) {
        CHCS <- 3.0
    }
}
CTCS <- CHCS + CHRS
RCLOS <- 1.52 / ((met - wme / METFACTOR) + 0.6944) - 0.1835
RCLS <- 0.155 * RCLOS
FACLS <- 1.0 + KCLO * RCLOS
FCLS <- 1.0 / (1.0 + 0.155 * FACLS * CTCS * RCLOS)
IMS <- 0.45
ICLS <- IMS * CHCS / CTCS * (1 - FCLS) / (CHCS / CTCS - FCLS * IMS)
RAS <- 1.0 / (FACLS * CTCS)
REAS <- 1.0 / (LR * FACLS * CHCS)
RECLS <- RCLS / (LR * ICLS)
HD_S <- 1.0 / (RAS + RCLS)
HE_S <- 1.0 / (REAS + RECLS)

# SET* (standardized humidity, clo, Pb, and CHC)
# determined using Newton#s iterative solution
# FNERRS is defined in the GENERAL SETUP section above

DELTA <- 0.0001

dx <- 100.0
X_OLD <- TempSkin - HSK / HD_S #lower bound for SET
while (abs(dx) > 0.01) {
    ERR1 <- (HSK - HD_S * (TempSkin - X_OLD) - W * HE_S * (PSSK - 0.5 * FindSaturatedVaporPressureTorr(X_OLD)))
    ERR2 <- (HSK - HD_S * (TempSkin - (X_OLD + DELTA)) - W * HE_S * (PSSK - 0.5 * FindSaturatedVaporPressureTorr((X_OLD + DELTA))))
    X <- X_OLD - DELTA * ERR1 / (ERR2 - ERR1)
    dx <- X - X_OLD
    X_OLD <- X
}
SET <- X

return(SET)
}

FindSaturatedVaporPressureTorr <- function(ta){
pressure <- exp(18.6686 - 4030.183/(ta + 235.0))
return(pressure)
}