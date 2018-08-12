# This script generates jitter plots from single cell nuclear translocation data.

library(xlsx)
library(ggplot2)

##################################

#SETTINGS TO INPUT

##################################

#Name your experiment
exptName = "IRF3 PLUS NFKB"

#Locate your input data (csv format)
csvFile = "/Volumes/HK HD PC/IF/141217 HIV1 HIV2 SIVsm opti/141217 HIV HIV2 SIVsm.csv"

#Choose where to save. MAKE THIS FOLDER BEFORE RUNNING.
saveDir = "/Volumes/HK HD PC/IF/141217 HIV1 HIV2 SIVsm opti/Output/"

#Set your translocation threshold
greenTransThreshold = 0.5

#Set your GFP threshold
redTransThreshold = 0.5

#Set your Y axis Label
yAxisLabelGreen = "NFKB Translocation Coefficient"
yAxisLabelRed = "IRF3 Translocation Coefficient"

#Give me a negative well for plotting the single well data against
controlwell = "A1_"

##################################

if (file.exists(saveDir)){
  setwd(file.path(saveDir))
} else {
  dir.create(file.path(saveDir))
  setwd(file.path(saveDir))
}
#Primary Data Import
fullsheet = read.csv(csvFile, header=TRUE, stringsAsFactors=FALSE)

#Remove Blank Rows
fullsheet[fullsheet==""] <- NA
fullsheet <- na.omit(fullsheet)

# Give the column with the name labels a shorter name for calling it later
colnames(fullsheet)[3] <- "wellid"
colnames(fullsheet)[9] = "greencoefficient"
colnames(fullsheet)[15] = "redcoefficient"

# Generate a list of well names from the main sheet.
welllist = unique(fullsheet$wellid)

# Generate summary stats and put them into a table.
resultbox=data.frame()
for (i in welllist){
  atable = subset(fullsheet, fullsheet$wellid == i)
  greenPos =length(which(atable$greencoefficient > greenTransThreshold))
  greenNeg =length(which(atable$greencoefficient <= greenTransThreshold))
  redPos =length(which(atable$redcoefficient > redTransThreshold))
  redNeg =length(which(atable$redcoefficient <= redTransThreshold))
  
  
  total = (length(atable$wellid))
  percentGreenTranslocated = (greenPos/total*100)
  percentRedTranslocated = (redPos/total*100)

  resulthandler = data.frame("Well ID"=i, "Total Cells"=total, "Green Translocated Cells" = greenPos, "Red Translocated Cells" = redPos, "Percent Green Translocated" = percentGreenTranslocated, "Percent Red Translocated" = percentRedTranslocated)
  
  resultbox = rbind(resultbox,resulthandler)
}

# Save summary stats
resultsFile = paste(saveDir, exptName, ".xlsx", sep="")
write.xlsx(resultbox, resultsFile)

#Plot big graph for translocation of green
all = ggplot(fullsheet, aes(wellid, greencoefficient))
all = all + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab(yAxisLabelGreen)
all = all + theme_bw() + scale_y_continuous(limits = c(-1, 1))
format = ".png"
png(filename = (paste(saveDir, exptName, " Translocation Green", format, sep="")), width = 800, height = 400, units = "px")
plot(all)
dev.off()

#Plot big graph for translocation of red
all = ggplot(fullsheet, aes(wellid, redcoefficient))
all = all + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab(yAxisLabelRed)
all = all + theme_bw() + scale_y_continuous(limits = c(-1, 1))
format = ".png"
png(filename = (paste(saveDir, exptName, " Translocation Red", format, sep="")), width = 800, height = 400, units = "px")
plot(all)
dev.off()


###################################### AUTOMATED SHIZ ENDS HERE ###############################

all

# Change the "desired" list to match the wells you want. Remember the underscores! All are 3 characters long.
desired = c("C1_", "C2_", "C3_", "C4_", "C5_", "C6_", "C7_", "C8_", "C9_", "D1_") 
selected = subset(fullsheet, wellid %in% desired)

# Change this to select long runs of wells which are in order. Type 'welllist' in the thing below to see a list.
selected = subset(fullsheet, wellid %in% welllist[0:9])
selected = subset(fullsheet, wellid %in% welllist[c(10:10, 14:14, 18:18, 0:9)])


# Use this to rename a well as you need it. Run this on as many wells as you want.
selected$wellid[selected$wellid == "B1_"] <- "2 B1"
selected$wellid[selected$wellid == "B5_"] <- "B5 Empty"

#Run this to generate the plot!
subPlot = ggplot(selected, aes(wellid, greencoefficient ))
subPlot = subPlot + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab(yAxisLabelGreen)
subPlot = subPlot + theme_bw() + scale_y_continuous(limits = c(-1, 1))
subPlot


subPlot = ggplot(selected, aes(wellid, redcoefficient ))
subPlot = subPlot + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab(yAxisLabelRed)
subPlot = subPlot + theme_bw() + scale_y_continuous(limits = c(-1, 1))
subPlot

#fullsheet[10] = (fullsheet$Cell..Mean.Inner.Intensity / fullsheet$Cell..Mean.Outer.Intensity)

#all = ggplot(fullsheet, aes(wellid, Cell..Mean.Inner.Intensity ))
#all = all + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="point", shape=95, size=8, color="#4242ff") + ylab("Correlation Coefficient")
#all = all + theme_bw()
#all

#singlewell = subset(fullsheet, fullsheet$wellid == "D4_")
#fun = ggplot(singlewell, aes(x=greencoefficient, y=redcoefficient)) + geom_point()
#fun
