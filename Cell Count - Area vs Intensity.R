# Plot nuclear area vs integrated intensity

library(xlsx)
library(ggplot2)
library(ggridges)
library(forcats)
library(Cairo)

CairoWin()

# Set the working directory
setwd("D:/TB model microscopy/5dg/")


inputfolder = "D:/TB model microscopy/5dg/"

filelist = list.files(path=inputfolder, pattern = ".xlsx", all.files=FALSE)


filelist = list.files(path=inputfolder, pattern = ".csv", all.files=FALSE)
resultbox=data.frame()

for (experimentid in filelist){

#fullsheet = read.xlsx(experimentid, sheetIndex=1)

fullsheet = read.csv(experimentid)

colnames(fullsheet)[1] <- "imagename"
colnames(fullsheet)[2] <- "numberplanes"


imagelist = unique(fullsheet$imagename)

for (i in imagelist){
  smalltable = subset(fullsheet, fullsheet$imagename ==i)
  numplanes = max(smalltable$numberplanes, na.rm = TRUE)
  
  resulthandler = data.frame("Experiment" = experimentid, "Image"=i, "Total Planes"=numplanes)
  resultbox = rbind(resultbox,resulthandler)
}
}

# Save summary stats
#resultsFile = paste(saveDir, exptName, ".xlsx", sep="")
write.xlsx(resultbox, "planestats.xlsx")



fullsheet = read.csv(filelist[1])

fullsheet[fullsheet==""] <- NA
fullsheet <- na.omit(fullsheet)

# Give the column with the name labels a shorter name for calling it later
colnames(fullsheet)[3] <- "wellid"
colnames(fullsheet)[6] = "nucarea"
colnames(fullsheet)[7] = "nucintint"




welllist = unique(fullsheet$wellid)


well1 = subset(fullsheet, fullsheet$wellid == "5dg5_4h_2 B2_")

well2 = subset(fullsheet, fullsheet$wellid == "5dg5_24h_ B2_")

CairoWin()


plota = ggplot(well1, aes(x = nucarea, y = nucintint)) +
 geom_point(alpha=0.1) +
  ggtitle("Single Cell") +
  theme_bw() +
  scale_x_log10(expand=c(0,0)) +
  guides(fill=FALSE) +
  scale_y_log10(expand=c(0.05,0)) +
  labs(title="Meera", 
       fill=NULL) +
  #scale_fill_manual(values = manualfill) +
  annotation_logticks(sides="b") 

plota