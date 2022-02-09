# Day2 part b of the geoChronR hack-a-thon---- 
##2022-02-09
##drtlfletcher@gmail.com

##Chaper 5. Correllation ----

#setup
library(lipdR)
library(geoChronR)
library(ggplot2)
library(magrittr)
library(egg)

##5.1 First Look ----
# Read in the data
hulu <- readLipd("Hulucave.Wang.2001-ens.lpd")
gisp2 <- readLipd("GISP2.Alley.2000-ens.lpd")

#Hulu
#map the ensemble to the palaeodata
hulu <- mapAgeEnsembleToPaleoData(hulu,
                                  age.var = "ageEnsemble",
                                  model.num = 1,
                                  paleo.depth.var = "depth", 
                                  paleo.meas.table.num = 1)
#Grab out the data of interest
hulu.do <- selectData(hulu, var.name = "d18o", meas.table.num = 1) 
hulu.ae <- selectData(hulu, var.name = "ageEnsemble", meas.table.num = 1) 

#repeat for gisp2

#because nick said so...
gisp2$paleoData[[1]]$measurementTable[[1]]$year <- NULL

#map the ensemble to the palaeodata
gisp2 <- mapAgeEnsembleToPaleoData(gisp2,
                                  age.var = "ageEnsemble",
                                  model.num = 1,
                                  paleo.meas.table.num = 1)
#Grab out the data of interest
gisp2.Temp <- selectData(gisp2, var.name = "Temp", meas.table.num = 1) 
gisp2.ae <- selectData(gisp2, var.name = "ageEnsemble", meas.table.num = 1)


##make ribbon plot with 
hulu.rib <- plotTimeseriesEnsRibbons(X = hulu.ae,Y = hulu.do, 
                         alp = .7,
                         color.high = "DarkBlue",
                         color.line = "Blue")
gisp2.rib <- plotTimeseriesEnsRibbons(X = gisp2.ae,Y = gisp2.Temp,
                           alp = .7,
                           color.high = "DarkGreen",
                           color.line = "Green")


ggarrange(plots = list(hulu.rib + xlim(c(31860, 50987)) + ggtitle("d18O at Hulu"), gisp2.rib + xlim(c(31860, 50987)) + ggtitle("Temperature from d18O at GISP2")), nrow = 2)
                       
                       