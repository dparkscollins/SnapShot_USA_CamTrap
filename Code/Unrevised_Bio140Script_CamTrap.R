#Using CamTrapR with csv file from Camelot

#Set working directory
setwd("/Users/danielcollins/Desktop/PracticeData/Research")

#Rename the csv file that was uploaded from Camelot as mydata
mydata <- read.csv("/Users/danielcollins/GitHub/SnapShot_USA_CamTrap/Raw_Data/sequences.csv", header = T)


head(mydata)

library(camtrapR)



# all species at once

activityDensity(recordTable = mydata,
                allSpecies  = TRUE,
                writePNG    = FALSE,
                plotR       = TRUE,
                add.rug     = TRUE)

#Create density plots
species4activity <- "Canis latrans"
activityDensity(recordTable = mydata, allSpecies = TRUE, 
                writePNG = FALSE, plotR = TRUE, add.rug = TRUE, col = "forest green")




#Overlap animal activity
speciesA_for_activity <- "Canis latrans"
speciesB_for_activity <- "Sciurus carolinensis"




activityOverlap(recordTable = mydata, speciesA = speciesA_for_activity, 
                speciesB = speciesB_for_activity, writePNG = FALSE, 
                plotR = TRUE, createDir = FALSE, pngMaxPix = 1000, 
                linecol = c("red", "blue"), linewidth = c(3,3), 
                add.rug = TRUE, legendPosition = "topright")


activityHistogram(recordTable = mydata, species = speciesA_for_activity,
                  allSpecies = FALSE, plotR = TRUE)



 
activityRadial(recordTable=mydata, species="Canis latrans",
               allSpecies=FALSE, plotR = TRUE, rp.type = "p")



