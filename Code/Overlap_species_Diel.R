packages.needed <- c("overlap","lubridate","dplyr","tidyr","purrr")

packages.installed <- installed.packages()
packages.installed

packages2install <- as.list(packages.needed[!packages.needed %in% packages.installed])

if(length(packages2install) > 0){
  install.packages(packages2install)
} else {
  cat("All required packages are already installed...")
}

library(overlap)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)

dat <- read.csv("SVILLE_SSFA2021.csv")
head(dat)

#Convert the time from timestamp into minutes then into radians
dat$time <- (hour(dat$start_time)*60 + minute(dat$start_time))/(24*60)  #time of day as fraction
dat$radtime <- dat$time * 2 * pi
head(dat) #mke sure code worked

#Single species activity patterns
#We need to extract into a vector for these analyses is just the time 
#of the sightings in radians.
coyote <- dat[dat$common_name== "Coyote",]$radtime
squirrel <- dat[dat$common_name== "Eastern Gray Squirrel",]$radtime
deer <- dat[dat$common_name== "White-tailed Deer",]$radtime

#We plot the daily activity cycles using the function ‘densityPlot’ applied to our 
#vectors of camera trap sightings. Specifying ‘rug=TRUE’ adds records of the number of 
#photos displayed across the bottom of the plots.
densityPlot((squirrel), rug = TRUE, col = "black", lty = 1, main = "Diel Activity of Squirrels")

#Plot activity patterns of 2 species over each other
overlapPlot(coyote, squirrel, adjust=2, main="Coyote/Squirrel Overlap")
legend("topleft", lty = c(1,2), col = c("blue","black"), legend = c("Coyote","Squirrel"), cex = 1.2)


#We can then calculate a ‘coefficient of overlap’ to quantify the amount of temporal overlap. 
#The ‘overlap’ package uses general nonparametric estimators the calculate the area underlying 
#both of the density curves (Δ). You can specify one of three Δ’s: Δ_hat1, Δ_hat4 , or Δ_hat5. 
#According to a simulation study done by Ridout & Linkie (2009), Δ_hat5 is kind of a useless 
#estimator, while Δ_hat1 performed best for sample sizes < 50 and Δ_hat4 did best for samples 
#> 75. Here, 50 and 75 refer to the sample size of the smaller sample. For our analyses, we 
#have huge sample sizes for both predators and prey (thousands of entries), so we’ll use the 
#Δ_hat4 estimator.
(coyote_squirrel_est <- overlapEst(coyote, deer, type="Dhat4"))
(coyote_squirrel_est <- overlapEst(coyote, deer, type="Dhat1"))

#To create confidence intervals for this estimate, you can bootstrap the data. 
#That is to say, you can fit a kernel density to the original data then draw random 
#simulated observations from this distribution.
coyote_boot <- resample(coyote, 1000)
deer_boot <- resample(deer, 1000)

#Then use the function ‘bootEst’ with your chosen Δ estimator to bootstrap overlap values 
#and take the mean and CI of this collection of samples:
coyote_deer_boot <- bootEst(coyote_boot, deer_boot, type="Dhat4")
(BSmean <- mean(coyote_deer_boot)) #bootstrapped mean overlap

bootCI(coyote_boot, deer_boot) #use 'basic0' output as your CI


##General Additive Mixed Models
library(mgcv)
library(lubridate)
library(plyr)











