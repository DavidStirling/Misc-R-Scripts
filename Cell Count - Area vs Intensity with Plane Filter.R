# Scatter plot of area vs intensity after filtering down to specific planes within an image. Also counts cells in those planes.

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
colnames(fullsheet)[6] <- "nucarea"



imagelist = unique(fullsheet$imagename)

for (i in imagelist){
  smalltable = subset(fullsheet, fullsheet$imagename ==i)
  numplanes = max(smalltable$numberplanes, na.rm = TRUE)
  
  
  
  
  filteredwell = subset(smalltable, smalltable$numberplanes %in% c(13:16, 23:26, 31:34))
  
  fullplanelist = unique(smalltable$numberplanes)
  planelist = unique(filteredwell$numberplanes)
  totalplanes = length(fullplanelist)
  lenplanelist = length(planelist)
  
  numcells = length(smalltable$imagename)
  numfilteredcells = length(filteredwell$imagename)
  avgunfiltered = median(smalltable$nucarea)
  avgfiltered = median(filteredwell$nucarea)
  cellsperimg = numcells/totalplanes
  filteredcellsperimg = numfilteredcells / lenplanelist
  
  
  
  resulthandler = data.frame("Experiment" = experimentid, "Image" = i, "Total Planes" = totalplanes, "Filtered Planes" = lenplanelist, "Total Cells" = numcells, "Filtered Cells" = numfilteredcells, "Total Cells/Img" = cellsperimg, "Filtered Cells/Img" = filteredcellsperimg, "Median Size" = avgunfiltered, "Filtered Median Size" = avgfiltered)
  resultbox = rbind(resultbox,resulthandler)
}
}

# Save summary stats
#resultsFile = paste(saveDir, exptName, ".xlsx", sep="")
write.xlsx(resultbox, "advancedplanestats.xlsx")



fullsheet = read.csv(filelist[1])

fullsheet[fullsheet==""] <- NA
fullsheet <- na.omit(fullsheet)

# Give the column with the name labels a shorter name for calling it later
colnames(fullsheet)[3] <- "wellid"
colnames(fullsheet)[6] = "nucarea"
colnames(fullsheet)[7] = "nucintint"
colnames(fullsheet)[8] = "nucavint"



welllist = unique(fullsheet$wellid)

welllist

well1 = subset(fullsheet, fullsheet$wellid == "5dg7_4h_ B2_")

well2 = subset(fullsheet, fullsheet$wellid == "5dg7_24h B2_")

well2filter = subset(fullsheet, fullsheet$wellid == "5dg5_4h_2 B2_")

filteredwell = subset(well2filter, well2filter$Image.Plane %in% c(13:16, 23:26, 31:34))

fullplanelist = unique(well2filter$Image.Plane)
planelist = unique(filteredwell$Image.Plane)
totalplanes = length(fullplanelist)
lenplanelist = length(planelist)
CairoWin()


plota = ggplot(well1, aes(x = nucarea, y = nucintint)) +
 geom_point(alpha=0.1, aes(colour="red")) +
  geom_point(data=well2, alpha=0.2,aes(colour="blue")) +
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

wells = rbind(well1, well2)


d = ggplot(well1, aes(x = nucarea, y = nucavint)) +
  scale_x_log10(limits = c(10, 10000)) +
  #scale_y_log10()+
  scale_y_continuous(limits=c(0, 66000)) +
  labs(title="4h", 
       fill=NULL) +
  geom_point(alpha = 0.2, shape = 19, size = 0.7) + 
  stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  theme_bw() +   annotation_logticks()
d



e = ggplot(well2, aes(x = nucarea, y = nucavint)) +
  scale_x_log10(limits = c(10, 10000)) +
  #scale_y_log10()+
  scale_y_continuous(limits=c(0, 66000)) +
  labs(title="24h", 
       fill=NULL) +
  geom_point(alpha = 0.2, shape = 19, size = 0.7) + 
  stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  theme_bw() +   annotation_logticks()
e

e = ggplot(well2, aes(x = nucarea, y = nucavint)) +
  scale_x_log10(limits = c(10, 10000)) +
  scale_y_log10(limits = c(100000, 100000000))+
  labs(title="24h", 
       fill=NULL) +
  geom_point(alpha = 0.2, shape = 19, size = 0.7) + 
  stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  theme_bw() +   annotation_logticks()
e



f = ggplot(well2filter, aes(x = nucarea, y = nucavint)) +
  scale_x_log10(limits = c(10, 10000)) +
  scale_y_log10(limits = c(10000, 100000))+
  labs(title="24h", 
       fill=NULL) +
  geom_point(alpha = 0.2, shape = 19, size = 0.7) + 
  stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  theme_bw() +   annotation_logticks()
f


g = ggplot(filteredwell, aes(x = nucarea, y = nucavint)) +
  scale_x_log10(limits = c(10, 10000)) +
  scale_y_log10(limits = c(10000, 100000))+
  labs(title="24h", 
       fill=NULL) +
  geom_point(alpha = 0.2, shape = 19, size = 0.7) + 
  stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE) +
  scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  theme_bw() +   annotation_logticks()
g


ggsave(d, filename = paste("5dg7", "4h Scatter.png"), type = "cairo",
       width = 5, height = 5, units = "in")

ggsave(e, filename = paste("5dg7", "24h Scatter.png"), type = "cairo",
       width = 5, height = 5, units = "in")

