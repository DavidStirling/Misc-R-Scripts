# Generate graphs to demonstrate bacterial growth over time. Proof of Hermes sensitivity.

library(xlsx)
library(ggplot2)

#Hermes Data
hermessheet = read.csv("C:/Users/daves/Desktop/Meera Data/distribution bug intensity for david.csv", header=TRUE)
confsheet = hermessheet

hermessheet = read.csv("C:/Users/daves/Desktop/Meera Data/2017-11-08_ 5dg5_4h_B2_histogram.csv", header=TRUE)
confsheet = read.csv("C:/Users/daves/Desktop/Meera Data/2017-11-08_ 5dg5_120h_B2_histogram.csv", header=TRUE)

hermessheet$Stage.Label = "04h"
confsheet$Stage.Label = "120h"

hermessheet[hermessheet==""] <- NA
hermessheet <- na.omit(hermessheet)

colnames(confsheet)[1] = "not4h"
colnames(confsheet)[2] = "4h"
colnames(hermessheet)[1] <- "4h"
colnames(hermessheet)[2] <- "120h"

hermessheet$Intensity = "04h"
confsheet$Intensity = "120h"
#confsheet$file <- "Confocal"
#hermessheet$file <- "Hermes"
confsheet$not4h <- NULL
hermessheet$`120h` <- NULL

all = ggplot(hermessheet, aes(Intensity, `4h`))
all = all + theme_bw() + scale_y_log10() + geom_violin() + geom_violin(data = confsheet)
all = all + geom_jitter(alpha = 0.2, width = 0.2, size = 0.5) + ggtitle("Integration Site Analysis") + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab("% Migration")
all = all + geom_jitter(data = confsheet, alpha = 0.2, width = 0.2, size = 0.5) + stat_summary(data = confsheet, fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + xlab("Image Source")
all + 

all = ggplot(hermessheet, aes(Intensity, `4h`))
all = all + theme_bw() + geom_violin() + geom_violin(data = confsheet)
all = all + ggtitle("Mycobacterial Infection") + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab("Bacterial Load")
all = all + stat_summary(data = confsheet, fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + xlab("Time Post-Infection")
all + scale_y_log10() + geom_boxplot(linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=1)

all = ggplot(hermessheet, aes(Intensity, `4h`))
all = all + theme_bw() + geom_violin(fill='#e0e0e0') + geom_violin(data = confsheet, fill='#e0e0e0')
all = all + ggtitle("Mycobacterial Infection") + ylab("Bacterial Load (Integrated Intensity)")
all = all + xlab("Time Post-Infection")
all = all + geom_boxplot(linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5) + geom_boxplot(data = confsheet, linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5)
all + scale_y_log10(labels=c("", "<1e+05", "1e+06", "1e+07",""))
all

all = ggplot(hermessheet, aes(file, Percent.Migration))
all = all + geom_violin() + ggtitle("Integration Site Analysis") + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab("% Migration")
all = all + geom_violin(data = confsheet) + stat_summary(data = confsheet, fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + xlab("Image Source")
all = all + geom_jitter(alpha = 1, width = 0.2, size = 1.5) + geom_jitter(data = confsheet, alpha = 1, width = 0.2, size = 1.5) 
all = all +  theme_bw() + scale_y_continuous(limits = c(0, 100))
all
hermessheet$`4h`[hermessheet$`4h`<=100000] = 100000
confsheet$`4h`[confsheet$`4h`<=100000]  <- 100000

confsheet <- rbind(confsheet, c(11432400, 1))


all = ggplot(hermessheet, aes(Stage.Label, Cell..W3.Cell.Integr.Intensity))
all = all + theme_bw() + geom_violin(fill='#e0e0e0') + geom_violin(data = d, fill='#e0e0e0')
all = all + ggtitle("Mycobacterial Infection") + ylab("Bacterial Load (Integrated Intensity)")
all = all + xlab("Time Post-Infection")
all = all + geom_boxplot(linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5) + geom_boxplot(data = d, linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5)
all + scale_y_log10(labels=c("", "<1e+05", "1e+06", "1e+07",""))
all + scale_y_log10()


t1<-subset(hermessheet, Cell..W3.Stained.Integr.Intensity>0)
t2<-subset(confsheet, confsheet$Cell..W3.Stained.Integr.Intensity>0)


all = ggplot(t1, aes(Stage.Label, Cell..W3.Stained.Integr.Intensity))
all = all + theme_bw() + geom_violin(fill='#e0e0e0') + geom_violin(data = t2, fill='#e0e0e0')
all = all + ggtitle("Mycobacterial Infection") + ylab("Bacterial Load (Integrated Intensity)")
all = all + xlab("Time Post-Infection")
all = all + geom_boxplot(linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5) + geom_boxplot(data = t2, linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5)
all + scale_y_log10()

t1$Cell..W3.Stained.Integr.Intensity[t1$Cell..W3.Stained.Integr.Intensity<=100000] = 100000
t2$Cell..W3.Stained.Integr.Intensity[t2$Cell..W3.Stained.Integr.Intensity<=100000]  <- 100000

all = ggplot(t1, aes(Stage.Label, Cell..W3.Stained.Integr.Intensity))
all = all + theme_bw() + theme(text = element_text(size=16)) + geom_violin(fill='#e0e0ff') + geom_violin(data = t2, fill='#e0e0ff')
all = all + ggtitle("Mycobacterial Infection") + ylab("Bacterial Load (Integrated Intensity)")
all = all + xlab("Time Post-Infection")
all = all + geom_boxplot(linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5) + geom_boxplot(data = t2, linetype=1, outlier.shape=NA, fill=NA, colour='#4242ff', size=0.6, width=0.5)
all + scale_y_log10(breaks=c(100000, 1000000, 10000000, 100000000), labels=c("<1e+05", "1e+06", "1e+07", "1e+08"))
