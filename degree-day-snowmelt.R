# Degree Day Snowmelt Routines

# Do setup / read SNOTEL data ---------------------------------------------

rm(list=ls())

setwd("C:/Users/khafe/Desktop/Classes/WR_502_EnviroHydroModeling/data")
fn = "snotel_klondike_2017.csv"

indat <- read.csv(fn)
swe <- indat$WTEQ.I.1..in.
precip <- indat$PREC.I.1..in.
precip.cm <- precip*2.54
tmin <- indat$TMIN.D.1..degC.
tmax <- indat$TMAX.D.1..degC.
tavg <- indat$TAVG.D.1..degC.
sdepth <- indat$SNWD.I.1..in.
sdepth.cm <- sdepth*2.54
swe.cm <- swe*2.54


# test --------------------------------------------------------------------

par <- c(0.229, 0.0, 3.0, 0.0)

#swe.mod <- ifelse(tavg < par[3] & tavg > par[4], precip.cm*((tavg-par[4])/(par[3]-par[4])), ifelse(tavg <= par[4], precip.cm, 0.0))
swe.melt <- par[1]*(tavg-par[2])
swe.cum <- vector(mode="double", length = length(swe))
precip.cm <- vector(mode="double", length = length(precip))
swe.mod <- vector(mode="double", length = length(precip))

precip.cm[1] <- precip[1] * 2.54
swe.mod[1] <- ifelse(tavg[1] < par[3] & tavg[1] > par[4], precip.cm[i]*((tavg[1]-par[4])/(par[3]-par[4])), 
                                                                    ifelse(tavg[1] <= par[4], precip.cm[1], 0.0))
swe.cum[1] <- ifelse(swe.mod[1] - swe.melt[1] > 0, swe.mod[1] - swe.melt[1], 0.0)
for (i in 2:length(swe.mod))
{
  precip.cm[i] <- (precip[i] - precip[i-1]) * 2.54
  swe.mod[i] <- ifelse(tavg[i] < par[3] & tavg[i] > par[4], precip.cm[i]*((tavg[i]-par[4])/(par[3]-par[4])), 
                                                                     ifelse(tavg[i] <= par[4], precip.cm[i], 0.0))
  swe.cum[i] <- ifelse((swe.mod[i]+swe.cum[i-1]-swe.melt[i])>0, swe.mod[i]+swe.cum[i-1]-swe.melt[i], 0.0)
}


# test func ---------------------------------------------------------------

rmse(c(0.229, 0.0, 3.0, 0.0), swe.cm, precip.cm, tavg)


# test optimization -------------------------------------------------------

rmse.min <- optim(par=c(0.229, 0.0, 3.0, 0.0), swe=swe.cm, precip=precip.cm, temp=tavg, fn=rmse)

# Functions ---------------------------------------------------------------

rmse <- function(par, swe, precip, temp)
{
  #par order = k, tbase, train, tsnow
  swe.melt <- par[1]*(tavg-par[2])
  swe.cum <- vector(mode="double", length = length(swe.mod))
  precip.cm <- vector(mode="double", length = length(precip))
  swe.mod <- vector(mode="double", length = length(precip))
  
  precip.cm[1] <- precip[1] * 2.54
  swe.mod[1] <- ifelse(temp[1] < par[3] & temp[1] > par[4], precip.cm[i]*((temp[1]-par[4])/(par[3]-par[4])), 
                       ifelse(temp[1] <= par[4], precip.cm[1], 0.0))
  swe.cum[1] <- ifelse(swe.mod[1] - swe.melt[1] > 0, swe.mod[1] - swe.melt[1], 0.0)
  for (i in 2:length(swe.mod))
  {
    precip.cm[i] <- (precip[i] - precip[i-1]) * 2.54
    swe.mod[i] <- ifelse(temp[i] < par[3] & temp[i] > par[4], precip.cm[i]*((temp[i]-par[4])/(par[3]-par[4])), 
                         ifelse(temp[i] <= par[4], precip.cm[i], 0.0))
    swe.cum[i] <- ifelse((swe.mod[i]+swe.cum[i-1]-swe.melt[i])>0, swe.mod[i]+swe.cum[i-1]-swe.melt[i], 0.0)
  }
  
  sqrt(mean((swe.cum - swe)^2))
}
