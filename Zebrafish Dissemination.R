# Prototype script for analysis of bacterial dissemination in zebrafish

library(xlsx)
library(ggplot2)

# Set the working directory
setwd("D:/David/Google Drive/David/To Process/Plots/")
setwd("C:/Users/daves/Google Drive/David/To Process/Plots/")

# C:/Users/daves
##################################

#SETTINGS TO INPUT

##################################

#Name your experiment
exptName = "Zebrafish"

#Locate your input data (csv format)
csvFile = "D:/David/Google Drive/David/Experiments/Zebrafish/clusters.csv"

csvFile = "C:/Users/daves/Google Drive/David/Imaging/ZF Test Data/leica.csv"

csvFile = "C:\\Users\\daves\\Desktop\\ZF\\New\\clusExpt3_ss_Hermes.csv"
csvFile = "C:/Users/daves/Google Drive/David/Imaging/ZF Test Data/Hermes output - Log2.csv"

#Choose where to save. MAKE THIS FOLDER BEFORE RUNNING.
saveDir = "D:/David/Google Drive/David/To Process/Plots/"
saveDir = "C:/Users/daves/Google Drive/David/To Process/Plots/"


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
colnames(fullsheet)[1] <- "image"
colnames(fullsheet)[4] <- "area"
colnames(fullsheet)[8] = "integrated"
colnames(fullsheet)[8] = "nucavint"

# Generate a list of well names from the main sheet.
welllist = unique(fullsheet$image)

selected = fullsheet
hermes = fullsheet

pooled = data.frame()
for (i in welllist){
 temptable = subset(fullsheet, fullsheet$image == i)
 temptable <- temptable[order(temptable$area),]
 temptable[,"cum_int"] <- cumsum(temptable$integrated)
 maximumv = max(temptable$cum_int)
 temptable[,"cum_percentint"] <- 100 * temptable$cum_int/maximumv
 pooled = rbind(pooled, temptable)
 
}
# Change this to select long runs of wells which are in order. Type 'welllist' in the thing below to see a list.
selected = subset(fullsheet, image %in% welllist[c(4:5)])

onewell = subset(selected, image %in% welllist[c(4)]) #Dissem
onewell2 = subset(selected, image %in% welllist[c(5)]) #Not Dissem
[12

selected = subset(fullsheet, image %in% welllist[c(10, 26, 43)])
selected = subset(fullsheet, image %in% welllist[c(5, 26, 43)])
selected = subset(fullsheet, wellid %in% welllist[c(10:10, 14:14, 18:18, 0:9)])


# Use this to rename a well as you need it. Run this on as many wells as you want.
selected$wellid[selected$wellid == "B1_"] <- "2 B1"
selected$wellid[selected$wellid == "B5_"] <- "B5 Empty"

leica = selected[!selected$condition == "Uninjected",]

onewell$image <- "Disseminated"
onewell2$image <- "Not Disseminated"

onewell <- onewell[order(onewell$area),]
onewell2 <- onewell2[order(onewell2$area),] 

onewell[,"cum_int"] <- cumsum(onewell$integrated)
onewell2[,"cum_int"] <- cumsum(onewell2$integrated)
onewell[,"cum_percentint"] <- 100 * onewell$cum_int/max(onewell$cum_int)
onewell2[,"cum_percentint"] <- 100 * onewell2$cum_int/max(onewell2$cum_int)

both = rbind(onewell, onewell2)

# GGPLOT Output

library(MASS)
library(ggplot2)
library(viridis)
library(ggridges)
library(forcats)
library(Cairo)
selected = onewell
CairoWin()

experimentname = "Both"

ylabels = c("25 cfu", "100 cfu", "400 cfu", "400 cfu\n+ Isoniazid")

manualfill = c("gray88", "dodgerblue", "#ff9999", "red")


###################################################################################################################################################WORK HERE
density = ggplot(selected, aes(area, group=image, fill=image), legend=TRUE) +
  geom_density(alpha=0.4, aes(y=..count..)) +
  ggtitle(experimentname) +
  theme_bw() +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  scale_x_log10(expand=c(0,0)) +
  #scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Leica Images", 
       y="Density",
       x="Area")
#scale_fill_manual(values = manualfill)
density

#######################################DUAL PLOTS

density = ggplot(both, aes(area, group=image, fill=image), legend=TRUE) +
  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  scale_x_log10(expand=c(0,0)) +
  #scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Dissemination", 
       y="Density",
       x="Area")
#scale_fill_manual(values = manualfill)
density

cumfreq = ggplot(both, aes(area, group=image, color=image), legend=TRUE) +
  stat_ecdf() +
#  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  scale_x_log10(expand=c(0,0)) +
  #scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Dissemination", 
       y="Cumulative Frequency",
       x="Area")
#scale_fill_manual(values = manualfill)
cumfreq


############################################################################Cumulative Line
xy = ggplot(both, aes(x = area, y = integrated, group=image, color=image), legend=TRUE) +
  geom_point() +
  #  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  #scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  scale_x_log10(expand=c(0,0)) +
  scale_y_log10() +
  #  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Dissemination", 
       y="Intensity",
       x="Area")
#scale_fill_manual(values = manualfill)
xy


cumline = ggplot(both, aes(x = area, y = cum_int, group=image, color=image), legend=TRUE) +
  geom_step() + geom_point() +
  #  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  #scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  #scale_x_log10(expand=c(0,0)) +
  #scale_y_log10() +
  #  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Hermes Images", 
       y="Density",
       x="Area")
#scale_fill_manual(values = manualfill)
cumline

cumstep = ggplot(both, aes(x = area, y = cum_percentint, group=image, color=image), legend=TRUE) +
  geom_step(size=1) + #geom_point() +
  #  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  #scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  scale_x_continuous() +
  geom_hline(yintercept=100, linetype = "dashed")+
  #scale_x_log10(expand=c(0,0)) +
  #scale_y_log10() +
  #  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Dissemination Analysis", 
       y='Cumulative Fluorescnce (%)',
       x="Cluster Size")
#scale_fill_manual(values = manualfill)
cumstep

CairoWin()

cumstep = ggplot(pooled, aes(x = area, y = cum_int, group=image, color=image), legend=TRUE) +
  geom_step(size=1) + #geom_point() +
  #  geom_density(alpha=0.4) +
  ggtitle(experimentname) +
  theme_bw() +
  #scale_y_continuous(expand=c(0,0), limits=c(0,1)) +
  #guides(fill=TRUE) +
  #scale_x_continuous() +
  geom_hline(yintercept=100, linetype = "dashed")+
  scale_x_log10(expand=c(0,0)) +
  #scale_y_log10() +
  #  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Dissemination Analysis", 
       y='Cumulative Fluorescnce (%)',
       x="Cluster Size")
#scale_fill_manual(values = manualfill)
cumstep
CairoWin()
pooled$image = substr(pooled$image, 54, 56)

experimentname = "ZFC"
library(Cairo)


ggsave(cumfreq, filename = paste(experimentname, "CumFreq.png"), type = "cairo",
       width = 5, height = 5, units = "in")


ggsave(cumstep, filename = paste(experimentname, "Cumulative Step.png"), type = "cairo",
       width = 5, height = 5, units = "in")

ggsave(freqpoly2, filename = paste(experimentname, "No Dissemination.png"), type = "cairo",
       width = 5.5, height = 3, units = "in")

CairoWin()


