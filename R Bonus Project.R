library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)

#analysis #1
lumps<- read.csv("2019 Growth Data.csv", stringsAsFactors = TRUE, header = TRUE)
lumps<- lumps %>%
  mutate(TankID=as.factor(TankID),DPH=as.factor(DPH))
lumps$Date<-as.Date(lumps$Date, "%m/%d/%Y")
str(lumps)


ggplot(data=lumps, aes(x=DPH, y=BatchWeight, color=TankID, group=TankID))+geom_point()+theme(legend.position = 'top')+geom_smooth(method = 'lm')+theme_bw()

a23<- lm(data = lumps, formula = BatchWeight ~ TankID+DPH)
summary(a23)
anova(a23)
plot(a23)

#analysis #2

ggplot(data=lumps, aes(x=Temp, y=BatchWeight))+geom_point()+theme(legend.position = 'top')+geom_smooth(method = 'lm')+theme_bw()

a24<- lm(data = lumps, formula = BatchWeight ~ Temp)
summary(a24)
anova(a24)
plot(a24)

a<-ggplot(data = lumps, aes(x=DPH, y=BatchWeight, color=TankID, group = TankID))+geom_point()+geom_line()+theme_bw()+ggtitle("2019 Lumpfish Growth")+xlab("Days Post Hatch")+ylab("Weight (g)")
b<-a+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                 "#F0E442"))
b

stats<-lumps%>%
   group_by(TankID) %>%
   get_summary_stats(Temp, type="mean_se")
view(stats)

g<-ggplot(stats, aes(fill=TankID, y=mean, x=TankID)) + 
  geom_bar(position="dodge", stat="identity")+geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, colour="black",position=position_dodge(.9))+xlab("Tank ID")+ylab("Average Temperature")+scale_y_continuous(expand = c(0,0))+theme_bw()+ggtitle("2019 Lumpfish Temperatures Per Tank")
g+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73",
                             "#F0E442"))+theme(plot.title = element_text(hjust = 0.5))+expand_limits(y = c(0,15))



f<-ggplot(data = lumps, aes(x=Temp, y=BatchWeight))+geom_point(color="Black")+theme_bw()+ggtitle("2019 Lumpfish Weight and Temperature Relation")+xlab("Temperature")+ylab("Weight (g)")+geom_smooth(method = 'lm', color="#009E73")

f
