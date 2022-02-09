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

## 4.2 Bchron ----
#Run Bchron
tana <- runBchron(tana,
                  iter = 10000,
                  model.num = 2,
                  lab.id.var = 'LabID', 
                  age.14c.var = 'age14C',
                  age.14c.uncertainty.var = 'age14CUnc', 
                  age.var = 'age', 
                  age.uncertainty.var = '1SD', 
                  depth.var = 'depth', 
                  reservoir.age.14c.var = NULL, 
                  reservoir.age.14c.uncertainty.var = NULL, 
                  rejected.ages.var = NULL)

#plot the ensemble from Bchron
plotChronEns(tana,model.num = 2,truncate.dist = .0001) + ggtitle("Tana Lake - default Bchron model")


## 4.3 OxCal ----
#Run OxCal
tana <- runOxcal(tana,model.num = 3,
                 lab.id.var = 'LabID', 
                 age.14c.var = 'age14C',
                 age.14c.uncertainty.var = 'age14CUnc', 
                 age.var = 'age', 
                 age.uncertainty.var = '1SD', 
                 depth.var = 'depth', 
                 reservoir.age.14c.var = NULL, 
                 reservoir.age.14c.uncertainty.var = NULL, 
                 rejected.ages.var = NULL,
                 events.per.unit.length = .05,
                 depth.interval = 20)

#plot OxCal ensembles
plotChronEns(tana,model.num = 3,truncate.dist = .0001) + ggtitle("Tana Lake - Oxcal model")

## 4.3.1 Compare the models

#pull the depth and age Ensemble variables

ensBacon <- selectData(tana,
                       var.name = "ageEnsemble",
                       paleo.or.chron = "chronData",
                       model.num = 1,
                       table.type = "ensemble")

depthBacon <- selectData(tana,
                         var.name = "depth",
                         paleo.or.chron = "chronData",
                         model.num = 1,
                         table.type = "ensemble")

ensBchron <- selectData(tana,
                        var.name = "ageEnsemble",
                        paleo.or.chron = "chronData",
                        model.num = 2,
                        table.type = "ensemble")

depthBchron <- selectData(tana,
                          var.name = "depth",
                          paleo.or.chron = "chronData",
                          model.num = 2,
                          table.type = "ensemble")

ensOxcal <- selectData(tana,
                       var.name = "ageEnsemble",
                       paleo.or.chron = "chronData",
                       model.num = 3,
                       table.type = "ensemble")

depthOxcal <- selectData(tana,
                         var.name = "depth",
                         paleo.or.chron = "chronData",
                         model.num = 3,
                         table.type = "ensemble")

# plot each of the models and its uncertainties together (with colour and transparecy settings to distinguish them)

plotTimeseriesEnsRibbons(X = ensBacon,Y = depthBacon) %>% 
  plotTimeseriesEnsRibbons(X = ensBchron,Y = depthBchron,
                           alp = .7,
                           color.high = "DarkGreen",
                           color.line = "Green") %>% 
  plotTimeseriesEnsRibbons(X = ensOxcal,Y = depthOxcal,
                           alp = .7,
                           color.high = "DarkBlue",
                           color.line = "Blue") %>% 
  plotModelDistributions(tana,add.to.plot = .) + #here we use the ggplot +
  scale_y_reverse()

## Exercise 4.2 ----

#Where do the models agree? 
## Where there are the most tie points that are fairly linear
#Where do they differ? 
##where their appears to be 'steps' in the dating data
#Do you think one is better than the others?
## It might be, if you have knoweldge of the data and reason to beleive the deposition was more or less linear, and did or did not contain hiatus

#The OxCal model is considerably more flexible than the Bacon model, which leaves outliers off the main trend. If you wanted to make the OxCal model less flexible, which parameter(s) would you change?
## I would increase eventsPerUnitLength

#Alternatively, if you wanted to make the Bacon model more flexible, which parameter(s) would you change in the Bacon model?
## increase interval length by increasing interval size, using bacon.thick = , which defaults to 5.

#Run Bacon, thickness 10
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
                 accept.suggestions = TRUE, 
                 bacon.thick = 10)
#Run Bacon, thickness 20
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
                 accept.suggestions = TRUE, 
                 bacon.thick = 20)

plotChronEns(tana, model.num = 1) + ggtitle("Tana Lake - thickness 5 Bacon model")
plotChronEns(tana, model.num = 4) + ggtitle("Tana Lake - thickness 10 Bacon model")
plotChronEns(tana, model.num = 5) + ggtitle("Tana Lake - thickness 20 Bacon model")

#Compare this models again, this time using model 4 for Bacon
#pull the depth and age Ensemble variables

ensBacon20 <- selectData(tana,
                        var.name = "ageEnsemble",
                        paleo.or.chron = "chronData",
                        model.num = 5,
                        table.type = "ensemble")

depthBacon20 <- selectData(tana,
                          var.name = "depth",
                          paleo.or.chron = "chronData",
                          model.num = 5,
                          table.type = "ensemble")


# plot each of the models and its uncertainties together (with colour and transparecy settings to distinguish them)

plotTimeseriesEnsRibbons(X = ensBacon20,Y = depthBacon20) %>% 
  plotTimeseriesEnsRibbons(X = ensBchron,Y = depthBchron,
                           alp = .7,
                           color.high = "DarkGreen",
                           color.line = "Green") %>% 
  plotTimeseriesEnsRibbons(X = ensOxcal,Y = depthOxcal,
                           alp = .7,
                           color.high = "DarkBlue",
                           color.line = "Blue") %>% 
  plotModelDistributions(tana,add.to.plot = .) + #here we use the ggplot +
  scale_y_reverse()



#Finally, how should you decide whether a more or less flexible model is better?
## I would need prior knowldge of the system that is being described. I may have reson to beleive the deposition was continuous and even, or reason to believe it contain hiatus or high variability in deposition


## 4.3.2 Creating a multimodel ensemble ----
#build your multimodel ensemble based on just the first three models

tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval =10,
                                 n.ens = 1000)

## Exercise 4.3 ----
#Use plotChronEns() and plotModelDistributions() to visualize your multi-model age model.

plotChronEns(tana, model.num = 6)  %>% 
  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")

# same as
ribbonsOnly <- plotChronEns(tana, model.num=6)
fullPlot <- plotModelDistributions(add.to.plot = ribbonsOnly, L = tana, model.num = 1)



## Exercise 4.4 ----
#Now that you’ve got plotting working, try changing the choices made in createMultiModelEnsemble. Specifically, what is the impact of changing 

#depth.interval
tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval = 20,
                                 n.ens = 1000)

plotChronEns(tana, model.num = 7)  %>%  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")

tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval = 5,
                                 n.ens = 1000)

plotChronEns(tana, model.num = 8)  %>%  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")

#n.ens
tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval = 10,
                                 n.ens = 2000)

plotChronEns(tana, model.num = 9)  %>%  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")

tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval = 10,
                                 n.ens = 3000)

plotChronEns(tana, model.num = 10)  %>%  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")

#depth.sequence
#depth.interval
tana <- createMultiModelEnsemble(tana,
                                 models.to.combine = 1:3,
                                 depth.interval = 20,
                                 n.ens = 1000,
                                 depth.seq = 2)

plotChronEns(tana, model.num = 11)  %>%  plotModelDistributions(tana, model.num = 1, truncate.dist = .0001, add.to.plot = .) + scale_y_reverse() + ggtitle("Tana Lake - multimodel ensemble")


##Exercise 4.5 ---- 
#Add your final multi model ensemble to the figure that showed the three original age models above. Does it look like a combination of the three? Yesish?


plotTimeseriesEnsRibbons(X = ensBacon,Y = depthBacon, 
                         alp = .7,
                         color.high = "Orange",
                         color.line = "Yellow") %>% 
  plotTimeseriesEnsRibbons(X = ensBchron,Y = depthBchron,
                           alp = .7,
                           color.high = "DarkGreen",
                           color.line = "Green") %>% 
  plotTimeseriesEnsRibbons(X = ensOxcal,Y = depthOxcal,
                           alp = .7,
                           color.high = "DarkBlue",
                           color.line = "Blue") %>% 
  plotModelDistributions(tana,add.to.plot = .) %>%
  plotChronEns(tana, model.num = 6, add.to.plot = ., alp = 0.3) + #here we use the ggplot +
  scale_y_reverse()

## 4.3.3 Mapping the age ensemble to the paleoData measurements

#First, create a tibble from the paleoata
paleo <- extractTs(tana) %>% ts2tibble()
#Now you can explore that much more easily - here are all the variable names in all the measurementTables in the paleoData.
paleo$paleoData_variableName

tana <- mapAgeEnsembleToPaleoData(tana,
                                  age.var = "ageEnsemble",
                                  model.num = 6,
                                  paleo.depth.var = "Composite_depth", 
                                  paleo.meas.table.num = 1)

paleo <- extractTs(tana) %>% ts2tibble()

paleo$paleoData_variableName

##4.3.4 Creating a timeseries plot as a spaghetti plot of lines----
tana.ae <- selectData(tana,var.name = "ageEnsemble", meas.table.num = 1)

tana.temp <- selectData(tana,var.name = "temperature",meas.table.num = 1)


tana.ts.plot <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.05,n.ens.plot = 50,color = "blue")
print(tana.ts.plot)

##Exercise 4.6 ----
#plotTimeseriesEnsLines() has options that control the output. Take a look at the documentation, and then change the following parameters, and understand how that affects the output:
#alp
tana.ts.plot.alp <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.1,n.ens.plot = 50,color = "blue")
print(tana.ts.plot.alp)
#color (What does “Blues” or “Set2” do? How does it work) - seems to just select the first colour in the palette
tana.ts.plot.cols <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.05,n.ens.plot = 50,color = "Blues")
print(tana.ts.plot.cols)
tana.ts.plot.cols <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.05,n.ens.plot = 50,color = "Set2")
print(tana.ts.plot.cols)

#n.ens.plot draws more examples
tana.ts.plot.nens <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.05,n.ens.plot = 150, color = "blue")
print(tana.ts.plot.nens)

#Change the limits of the plot to only show the Holocene (~12,000-0 yr BP)
tana.ts.plot.Holo <-  plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, alp = 0.05,n.ens.plot = 50,color = "blue") + xlim(12000, 0)
print(tana.ts.plot.Holo)

## 4.3.5 Creating a timeseries plot with a ribbon confidence intervals

tana.ribbon.plot <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp)
print(tana.ribbon.plot)

##Exercise 4.7 ----
#plotTimeseriesEnsRibbons () has many more options that control the output. Take a look at the documentation for that function, and then change the following parameters, and understand how that affects the output:
#probs
tana.ribbon.plot.probs <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, probs = c(c(0.05, 0.3, 0.5, 0.7, 0.95)))
print(tana.ribbon.plot.probs)
#color.high
tana.ribbon.plot.col <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, color.high = "red")
print(tana.ribbon.plot.col)
#n.bins
tana.ribbon.plot.bins <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, n.bins = 1000)
print(tana.ribbon.plot.bins)
#export.quantiles
tana.ribbon.plot.quan <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, export.quantiles = TRUE)
print(tana.ribbon.plot.quan)
#limit.outliers.x
tana.ribbon.plot.out <- plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, limit.outliers.x = 0.05)
print(tana.ribbon.plot.out)

##4.3.6 Combining the two kinds of timeseries plots ----
##Exercise 4.8---- 
#Use plotTimeseriesEnsRibbons(), plotTimeseriesEnsLines(), and the “add.to.plot” parameter to create a figure that shows the uncertainty ribbons in the background, and a with 5 ensemble members to highlight the variability of the data.

plotTimeseriesEnsRibbons(X = tana.ae,Y = tana.temp, color.line = "red", color.high = "orange") %>% plotTimeseriesEnsLines(X = tana.ae, Y = tana.temp, n.ens.plot = 5, color = "red", alp = 0.4, add.to.plot = .)

##4.4 Banded Age Modelling ----

tana <- runBam(tana,
               paleo.meas.table.num = 1,
               n.ens = 1000,
               model.num = 5,
               make.new = TRUE,
               ens.table.number = 1,
               model = list(name = "poisson",
                            param = 0.05, 
                            resize = 0, 
                            ns = 1000))

tana.ye <- selectData(tana,var.name = "yearEnsemble",meas.table.num = 1)

tana.ae.bam <- convertAD2BP(tana.ye)

tana.ribbon.plot.bam <- plotTimeseriesEnsRibbons(X = tana.ae.bam,Y = tana.temp)

#we can compare this to the original age model supplied by the paper (which used the Heegaard et al., 2005 model, so a whole other approach)

tana.orig.age <- selectData(tana,var.name = "age",meas.table.num = 1)

tana.ribbon.plot.bam <- tana.ribbon.plot.bam +
  geom_line(aes(x = tana.orig.age$values, y = tana.temp$values),color = "red")

tana.ribbon.plot.bam

#install.packages(egg)
library(egg)
ggarrange(plots = list(tana.ribbon.plot + xlim(c(15000,0)) + ggtitle("Temperature on Multimodel age model"),
                       tana.ribbon.plot.bam + xlim(c(15000,0)) + ggtitle("Temperature on BAM")),
          nrow = 2)

##4.5 Chapter project ----

#Exercise 4.9 Using tools learned in this chapter, create ensemble timeseries figures for 18O leaf wax from Tana Lake using, Bacon, Bchron, OxCal and BAM. Arrange these four figures vertically into a 4 panel figure with constant x-axes to allow comparison. How does the choice of age modelling algorithm impact the record? Which parts of the dataset are most vulnerable to age uncertainties?

tana <- mapAgeEnsembleToPaleoData(tana,
                                  age.var = "ageEnsemble",
                                  model.num = 6,
                                  paleo.depth.var = "age", 
                                  paleo.meas.table.num = 3)

paleo <- extractTs(tana) %>% ts2tibble()

paleo$paleoData_variableName

tana.ae3 <- selectData(tana, var.name = "ageEnsemble", meas.table.num = 3)
tana.wax <- selectData(tana, var.name = "ddwax corrected", meas.table.num = 3)

bacon.plot <- plotTimeseriesEnsRibbons(X = tana.ae3, Y = tana.wax)
bchron.plot <- plotTimeseriesEnsRibbons(X = tana.ae3, Y = tana.wax)
oxcal.plot <-  plotTimeseriesEnsRibbons(X = tana.ae3, Y = tana.wax)
bam.plot <-  plotTimeseriesEnsRibbons(X = tana.ae3, Y = tana.wax)

ggarrange(plots = list(bacon.plot + xlim(c(15000,0)) + ggtitle("18O leafwax on Bacon"), bchron.plot + xlim(c(15000,0)) + ggtitle("18O leafwax on Bchron"), oxcal.plot + xlim(c(15000,0)) + ggtitle("18O leafwax on Bchron"), bam.plot + xlim(c(15000,0)) + ggtitle("18O leafwax on BAM")), nrow = 2)
