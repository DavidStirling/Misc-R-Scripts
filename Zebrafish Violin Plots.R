# Create violin plots for zebrafish infection

library(xlsx)
library(ggplot2)

# Set the working directory
setwd("D:/David/Google Drive/David/To Process/Plots/")
setwd("C:/Users/daves/Google Drive/David/To Process/Plots/")

# C:/Users/daves
##################################

#SETTINGS FOR HATAF TO INPUT

##################################

#Name your experiment
exptName = "Zebrafish"

#Locate your input data (csv format)
csvFile = "D:/David/Google Drive/David/To Process/Expt3.csv"
csvFile = "C:/Users/daves/Google Drive/David/Imaging/ZF Test Data/leica.csv"

csvFile = "D:/David/Google Drive/David/To Process/Expt3_Hermes.csv"
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
colnames(fullsheet)[1] <- "condition"
colnames(fullsheet)[2] <- "intensity"
colnames(fullsheet)[6] = "nucarea"
colnames(fullsheet)[8] = "nucavint"

# Generate a list of well names from the main sheet.
welllist = unique(fullsheet$Channel)

selected = fullsheet
leica = fullsheet


# Change this to select long runs of wells which are in order. Type 'welllist' in the thing below to see a list.
selected = subset(fullsheet, wellid %in% welllist[0:9])
selected = subset(fullsheet, wellid %in% welllist[c(10:10, 14:14, 18:18, 0:9)])


# Use this to rename a well as you need it. Run this on as many wells as you want.
selected$wellid[selected$wellid == "B1_"] <- "2 B1"
selected$wellid[selected$wellid == "B5_"] <- "B5 Empty"

fullsheet$Channel[fullsheet$Channel == "100cfu"] <- "3"
fullsheet$Channel[fullsheet$Channel == "25cfu"] <- "2"
fullsheet$Channel[fullsheet$Channel == "400cfu"] <- "4"
fullsheet$Channel[fullsheet$Channel == "400cfu+i"] <- "5"
fullsheet$Channel[fullsheet$Channel == "wt"] <- "1"

leica = selected[!selected$condition == "Uninjected",]


leica$intensity[leica$intensity == 0] <- 20480
hermes$intensity[hermes$intensity == 0] <- 15360

leica$intensity[leica$intensity == 20480] <- NA
hermes$intensity[hermes$intensity == 15360] <- NA

leica$Channel[leica$Channel == "1"] <- NA
hermes$Channel[hermes$Channel == "1"] <- NA
leica <- na.omit(leica)
hermes <- na.omit(hermes)

# GGPLOT Output

library(MASS)
library(ggplot2)
library(viridis)
library(ggridges)
library(forcats)
library(Cairo)

CairoWin()

# ESSENTIAL FUNCTIONS
theme_david <- function (base_size = 11, base_family = "", font_size=12) 
{
  half_line <- font_size / 2
  small_rel <- 0.857
  axis_just=1
  small_size <- small_rel * font_size
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(panel.border = element_rect(fill="transparent"), panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                                                                       size = 0.5), legend.key = element_blank(), strip.background = element_rect(fill = "white", 
                                                                                                                                                  colour = "black", size = 1), complete = TRUE,
          
          text              = element_text(family = base_family, face = "plain", colour = "black",
                                           size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                           margin = ggplot2::margin(), debug = FALSE),
          axis.text.y       = element_text(margin = ggplot2::margin(r = small_size / 4), hjust = 1, vjust = 0)
    )
}

# SETTINGS TO INPUT

experimentname = "Leica"

ylabels = c("25 cfu", "100 cfu", "400 cfu", "400 cfu\n+ Isoniazid")

manualfill = c("gray88", "dodgerblue", "#ff9999", "red")

# Red Joyplot
leica=fullsheet

fishleica = ggplot(leica, aes(x = intensity, y = fct_rev(condition))) +
  geom_density_ridges2(scale = 1.3, aes(fill=condition), panel_scaling=TRUE, quantile_lines = TRUE) +
  ggtitle(experimentname) +
  theme_david() +
  guides(fill=FALSE) + scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Leica Images", 
       y="Density",
       x="Fluorescence Intensity (Log2)",
       fill=NULL)
#scale_fill_manual(values = manualfill)
fishleica


fishhermes = ggplot(hermes, aes(x = intensity, y = fct_rev(condition))) +
  geom_density_ridges2(scale = 1.3, aes(fill=condition), panel_scaling=TRUE, quantile_lines = TRUE) +
  ggtitle(experimentname) +
  theme_david() +
  #scale_x_log10(expand=c(0,0)) +
  guides(fill=FALSE) + scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand=c(0.05,0), labels=rev(ylabels)) +
  labs(title="Hermes Images", 
       y="Density",
       x="Fluorescence Intensity (Log2)",
       fill=NULL)
#scale_fill_manual(values = manualfill)
fishhermes



fishviolin1 = ggplot(hermes, aes(y = intensity, x = Channel, fill = Channel)) +
  geom_violin(scale = "width", trim=FALSE, alpha =0.4, draw_quantiles =  c(0.25, 0.50,0.75)) +
  ggtitle(experimentname) +
  theme_david() +
  scale_y_log10(expand=c(0,0), limits=c(1000, 100000000000)) +
  guides(fill=FALSE) +
  annotation_logticks(sides="l")+
  scale_x_discrete(labels=ylabels) +
  theme(text = element_text(size=18), axis.text=element_text(size=14)) +
  labs(title="Hermes Images", 
       x="Bacterial Dose",
       y="Fluorescence Intensity",
       fill=NULL)

#scale_fill_manual(values = manualfill)
fishviolin1


fishviolin2 = ggplot(leica, aes(y = intensity, x = Channel, fill = Channel)) +
  geom_violin(scale = "width", trim=FALSE, alpha =0.4, draw_quantiles =  c(0.25, 0.50,0.75)) +
  ggtitle(experimentname) +
  theme_david() +
  scale_y_log10(expand=c(0,0), limits=c(1000, 10000000000)) +
  guides(fill=FALSE) +
  annotation_logticks(sides="l")+
  scale_x_discrete(labels=ylabels) +
  theme(text = element_text(size=18), axis.text=element_text(size=14)) +
  labs(title="Manual Images", 
       x="Bacterial Dose",
       y="Fluorescence Intensity",
       fill=NULL)

#scale_fill_manual(values = manualfill)
fishviolin2


experimentname = "ZF Titration 3"
#Plot Saver

ggsave(fishviolin2, filename = paste(experimentname, "OS Leica 3 Axis.png"), type = "cairo",
       width = 4.5, height = 6, units = "in")
ggsave(fishviolin1, filename = paste(experimentname, "OS Hermes 3.png"), type = "cairo",
       width = 4.5, height = 6, units = "in")


ggsave(fishviolin2, filename = paste(experimentname, "OS Leica 2.png"), type = "cairo",
       width = 3, height = 3, units = "in")
ggsave(fishviolin1, filename = paste(experimentname, "OS Hermes 2.png"), type = "cairo",
       width = 3, height = 3, units = "in")

onewell = subset(fullsheet, wellid == "B2_")
onewell = subset(fullsheet, wellid == "C2_")
onewell = subset(fullsheet, wellid == "G2_")
onewell = subset(fullsheet, wellid == "E2_")

twoaxis = ggplot(onewell, aes(x = redintensity, y = farredintensity)) + geom_point(alpha = 0.3, shape = 19, size = 0.7) +
  scale_x_log10(limits = c(1000, 100000)) +
  scale_y_log10(limits = c(100, 100000))
twoaxis


d = ggplot(onewell, aes(x = redintensity, y = farredintensity)) +
  scale_x_log10(limits = c(1000, 100000), breaks=c(1000, 10000, 100000)) +
  scale_y_log10(limits = c(100, 100000), breaks=c(100, 1000, 10000, 100000)) +
  labs(title=experimentname,
       subtitle = "Zymosan",
       y="TNF-a Average Intensity",
       x="Il-10 Average Intensity",
       fill=NULL)
d = d + geom_point(alpha = 0.5, shape = 19, size = 0.7) 
#d = d + geom_density2d(colour = "blue", size = 0.7)
d = d + stat_density_2d(aes(fill= ..level.., alpha = ..level..), geom = "polygon", show.legend = FALSE)
d = d + scale_fill_gradient(low = "yellow", high = "red") + scale_alpha(range = c(0.00, 0.5), guide = FALSE)
d = d + geom_hline(yintercept=1000, linetype = "dashed", colour = "blue", size = 1)
d = d + geom_vline(xintercept=7500, linetype = "dashed", colour = "blue", size = 1)
d = d + theme_david() +   annotation_logticks()
d


ggsave(d, filename = paste(experimentname, "Zymosan Coexp.png"), type = "cairo",
       width = 5, height = 5, units = "in")

CairoWin()

