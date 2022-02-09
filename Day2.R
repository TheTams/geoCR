#My Day 2 of the geoChronR hack-a-thon----
##2022-02-09
##drtlfletcher@gmail.com

###Chapter 4====
#generating the ensembles to be used in ahe uncertainty analysis

#load needed libraries and read in the data

library(lipdR)
library(geoChronR)
library(ggplot2)
library(magrittr)

tana <- readLipd("https://lipdverse.org/Temp12k/1_0_2/TanaLake.Loomis.2015.lpd")

##4.1 Bacon ----
#Run Bacon
tana <- runBacon(tana,
                 lab.id.var = 'LabID', 
                 age.14c.var = 'age14C',
                 age.14c.uncertainty.var = 'age14CUnc', 
                 age.var = 'age', 
                 age.uncertainty.var = '1SD', 
                 depth.var = 'depth', 
                 reservoir.age.14c.var = NULL, 
                 reservoir.age.14c.uncertainty.var = NULL, 
                 rejected.ages.var = NULL,
                 accept.suggestions = TRUE)

plotChronEns(tana) + ggtitle("Tana Lake - default Bacon model")


##Exercise 4.1 Explore the parameter choices in plotChronEns.---- #Can you a) change the confidence interval colors and 

plotChronEns(tana, color.low = "blue", color.high = "cyan") + ggtitle("Tana Lake - colored Bacon model")

#b) quantiles? 

plotChronEns(tana, probs = c(0.1, 0.2, 0.5, 0.8, 0.9), color.low = "blue", color.high = "cyan") + ggtitle("Tana Lake - colored quantile Bacon model")

#c) Change the type of distribution plotted for the dates 

plotChronEns(tana, probs = c(0.1, 0.2, 0.5, 0.8, 0.9), color.low = "blue", color.high = "cyan", dist.type = "up") + ggtitle("Tana Lake - colored quantile 'up' Bacon model")

#d) and their color and transparency? 

plotChronEns(tana, probs = c(0.1, 0.2, 0.5, 0.8, 0.9), color.low = "blue", color.high = "cyan", dist.type = "up", dist.color = "dark green") + ggtitle("Tana Lake - colored quantile 'up' green Bacon model")

#Alpha is for transparency, and is abbreviated to alp
plotChronEns(tana, probs = c(0.1, 0.2, 0.5, 0.8, 0.9), color.low = "blue", color.high = "cyan", dist.type = "up", dist.color = "dark green", dist.alp = 0.6) + ggtitle("Tana Lake - colored quantile 'up' green alp Bacon model")

#e) what does truncate.dist do?

plotChronEns(tana, probs = c(0.1, 0.2, 0.5, 0.8, 0.9), color.low = "blue", color.high = "cyan", dist.type = "up", dist.color = "dark green", dist.alp = 0.6, truncate.dist = 1e-2) + ggtitle("Tana Lake - colored quantile 'up' green alpha trunc Bacon model")
