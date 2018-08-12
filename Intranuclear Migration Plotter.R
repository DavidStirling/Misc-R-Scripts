library(xlsx)
library(ggplot2)

#Hermes Data
hermessheet = read.csv("D:/David/Google Drive/David/Scripts/MigraMeasure/RichHermes.csv", header=TRUE)
hermessheet[hermessheet==""] <- NA
hermessheet <- na.omit(hermessheet)

#Conf Data
confsheet = read.csv("D:/David/Google Drive/David/Scripts/MigraMeasure/RichOther.csv", header=TRUE)
confsheet[confsheet==""] <- NA
confsheet <- na.omit(confsheet)

confsheet$file <- " Manual"
hermessheet$file <- "Hermes"


all = ggplot(hermessheet, aes(file, Percent.Migration))
all = all + geom_jitter(alpha = 1, width = 0.2, size = 1.5) + ggtitle("Integration Site Analysis") + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab("% Migration")
all = all + geom_jitter(data = confsheet, alpha = 1, width = 0.2, size = 1.5) + stat_summary(data = confsheet, fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + xlab("Image Source")
all = all + theme_bw() + scale_y_continuous(limits = c(0, 100))
all

all = ggplot(hermessheet, aes(file, Percent.Migration))
all = all + geom_violin() + ggtitle("Integration Site Analysis") + stat_summary(fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + ylab("% Migration")
all = all + geom_violin(data = confsheet) + stat_summary(data = confsheet, fun.y=mean, geom="crossbar", fun.ymin=mean, fun.ymax=mean, color="#4242ff") + xlab("Image Source")
all = all + geom_jitter(alpha = 1, width = 0.2, size = 1.5) + geom_jitter(data = confsheet, alpha = 1, width = 0.2, size = 1.5) 
all = all +  theme_bw() + scale_y_continuous(limits = c(0, 100))
all
