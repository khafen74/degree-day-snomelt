# Degree Day Snowmelt Routines

# Do setup / read SNOTEL data ---------------------------------------------

rm(list=ls())

setwd("C:/Users/khafe/Desktop/Classes/WR_502_EnviroHydroModeling/data")
fn = "snotel_klondike_2017.csv"

indat <- read.csv(fn)
swe <- indat$WTEQ.I.1..in.[1:nrow(indat)-1]
precip <- indat$PREC.I.1..in.[1:nrow(indat)-1]
tmin <- indat$TMIN.D.1..degC.[2:nrow(indat)]
tmax <- indat$TMAX.D.1..degC.[2:nrow(indat)]
tavg <- indat$TAVG.D.1..degC.[2:nrow(indat)]
precip.cm <- precip*2.54
swe.cm <- swe*2.54


# test optimization -------------------------------------------------------

rmse.min <- optim(par=c(0.229, 0.0, 3.0, 0.0), swe=swe.cm, precip=precip.cm, temp=tavg, fn=rmse.swe.usace)

swe.ace <- swe.usace(c(0.229, 0.0, 3.0, 0.0), swe.cm, precip.cm, tavg)
swe.ace.opt <- swe.usace(rmse.min$par, swe.cm, precip.cm, tavg)
swe.maid <- swe.maidment(c(0.229, 0.0, 3.0, 0.0), swe.cm, precip.cm, tavg, tmin, tmax)

plot(swe.ace$swe_obs, type="l", xlab="Daily index (Oct 1, 2016 - Sept 30, 2017)", ylab="SWE (cm)")
lines(swe.ace$swe_mod, col="blue")
lines(swe.ace.opt$swe_mod, col="red")
lines(swe.maid$swe_mod, col="green")

# Functions ---------------------------------------------------------------

swe.maidment <- function(par, swe, precip, tmean, tmin, tmax)
{
  swe.melt <- ifelse(tmin > 0, 0.3*(tmean+(tmin/4.4)*(((tmax-tmin)/8)+tmin)), 0.0)
  swe.cum <- vector(mode="double", length = length(swe))
  precip.cm <- vector(mode="double", length = length(precip))
  swe.mod <- vector(mode="double", length = length(precip))
  
  precip.cm[1] <- precip[1]
  swe.mod[1] <- ifelse(tmean[1] < par[3] & tmean[1] > par[4], precip.cm[i]*((tmean[1]-par[4])/(par[3]-par[4])), 
                       ifelse(tmean[1] <= par[4], precip.cm[1], 0.0))
  swe.cum[1] <- ifelse(swe.mod[1] - swe.melt[1] > 0, swe.mod[1] - swe.melt[1], 0.0)
  for (i in 2:length(swe.mod))
  {
    precip.cm[i] <- (precip[i] - precip[i-1])
    swe.mod[i] <- ifelse((tmean[i] < par[3]) & (tmean[i] > par[4]), precip.cm[i]*((tmean[i]-par[4])/(par[3]-par[4])), 
                         ifelse(tmean[i] <= par[4], precip.cm[i], 0.0))
    swe.cum[i] <- ifelse((swe.mod[i]+swe.cum[i-1]-swe.melt[i])>0, swe.mod[i]+swe.cum[i-1]-swe.melt[i], 0.0)
    
    #print(paste(i,"modeled =",round(swe.mod[i], 1), "cumulative =", round(swe.cum[i],1), "melted =", round(swe.melt[i],1), "observed = ", round(swe[i], 1)))
  }
  
  data.frame(swe_obs = swe, swe_mod = swe.cum)
}

swe.usace <- function(par, swe, precip, temp)
{
  #par order = k, tbase, train, tsnow
  swe.melt <- ifelse(par[1]*(tavg-par[2]) > 0, par[1]*(tavg-par[2]), 0.0)
  swe.cum <- vector(mode="double", length = length(swe))
  precip.cm <- vector(mode="double", length = length(precip))
  swe.mod <- vector(mode="double", length = length(precip))
  
  precip.cm[1] <- precip[1]
  swe.mod[1] <- ifelse(temp[1] < par[3] & temp[1] > par[4], precip.cm[i]*((temp[1]-par[4])/(par[3]-par[4])), 
                       ifelse(temp[1] <= par[4], precip.cm[1], 0.0))
  swe.cum[1] <- ifelse(swe.mod[1] - swe.melt[1] > 0, swe.mod[1] - swe.melt[1], 0.0)
  for (i in 2:length(swe.mod))
  {
    precip.cm[i] <- (precip[i] - precip[i-1])
    swe.mod[i] <- ifelse((temp[i] < par[3]) & (temp[i] > par[4]), precip.cm[i]*((temp[i]-par[4])/(par[3]-par[4])), 
                         ifelse(temp[i] <= par[4], precip.cm[i], 0.0))
    swe.cum[i] <- ifelse((swe.mod[i]+swe.cum[i-1]-swe.melt[i])>0, swe.mod[i]+swe.cum[i-1]-swe.melt[i], 0.0)
    
    #print(paste(i,"modeled =",round(swe.mod[i], 1), "cumulative =", round(swe.cum[i],1), "melted =", round(swe.melt[i],1), "observed = ", round(swe[i], 1)))
  }
  
  data.frame(swe_obs = swe, swe_mod = swe.cum)
}

rmse.swe.usace <- function(par, swe, precip, temp)
{
  swe.dat <- swe.mod(par, swe, precip, temp)
  rmse(swe.dat$swe_mod, swe.dat$swe_obs)
}

rmse <- function(modeled, observed)
{
  sqrt(mean((modeled-observed)^2))
}

md <- function(modeled, observed)
{
  mean(modeled-observed)
}
