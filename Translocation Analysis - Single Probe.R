# Generates one big translocation analysis jitter plot from a data set.

library(xlsx)
library(ggplot2)

##################################

#SETTINGS TO INPUT

##################################

#Name your experiment
exptName = "EXPERIMENT NAME HERE"

#Locate your input data (csv format)
csvFile = "C:/Users/daves/Google Drive/David/To Process/Hataf20171128.csv"

#Choose where to save. MAKE THIS FOLDER BEFORE RUNNING.
saveDir = "C:/Users/daves/Google Drive/David/To Process/Hataf/DotPlots2/"

#Set your translocation threshold
transThreshold = 0.5


##################################

#Primary Data Import
fullsheet = read.csv(csvFile, header=TRUE)

#Remove Blank Rows
fullsheet[fullsheet==""] <- NA
fullsheet <- na.omit(fullsheet)

# Give the column with the name labels a shorter name for calling it later
colnames(fullsheet)[3] <- "wellid"

# Generate a list of well names from the main sheet.
welllist = unique(fullsheet$wellid)

# Generate summary stats and put them into a table.
resultbox=data.frame()
index=1
for (i in welllist){
  atable = subset(fullsheet, fullsheet$wellid == i)
  redPos =length(which(atable$Cell..Correlation.Coefficient > transThreshold))
  total = (length(atable$wellid))
  percentTranslocated = (redPos/total*100)
  resulthandler = data.frame("Well ID"=i, "Total Cells"=total, "Translocated Cells" = redPos, "Percent Translocated" = percentTranslocated)
  resultbox = rbind(resultbox,resulthandler)
  index = index+1
}
# Save summary stats

resultsFile = paste(saveDir, exptName, ".xlsx", sep="")
write.xlsx(resultbox, resultsFile)

#Plot big graph
all = ggplot(fullsheet, aes(wellid, Cell..Correlation.Coefficient))
all = all + geom_jitter(alpha = 0.3, width = 0.25, shape = 19, size = 0.7) + ggtitle(exptName) + stat_summary(fun.y=mean, geom="point", shape=95, size=8, color="#4242ff") + ylab("Correlation Coefficient")
all = all + theme_bw()
format = ".png"
png(filename = (paste(saveDir, exptName, format, sep="")), width = 800, height = 400, units = "px")
plot(all)
dev.off()
