########################################################
#### IFI Project: Bar Chart of Outcomes ################
########################################################

# REVISED BY SARAH LU
# July 14, 2017

rm(list=ls())
setwd("/Users/sarah/Google Drive/IFIs_and_HR/Data")

library(foreign)
library(ggplot2)
library(MASS)
library(plotly)
dat<-read.dta("data/dta files/Cleaned_WBHR_Data_06212017.dta")

attach(dat) #makes the variables in the .dta file usable in R (can directly use variables like 'cao')

#use table(row, col) function to get counts of the outcomes for cao and non-cao(IP)
mytable1=table(harm_acknowledged,cao) 
mytable2=table(bank_noncompliance,cao)
mytable3=table(project_halted,cao)
mytable4=table(project_change,cao)
mytable5=table(compensation,cao)
mytable6=table(perpetrators_punished,cao)

#get proportions (by column), store in new matrices, convert to percentage
m1 = prop.table(mytable1, 2) * 100
m2 = prop.table(mytable2, 2) * 100
m3 = prop.table(mytable3, 2) * 100
m4 = prop.table(mytable4, 2) * 100
m5 = prop.table(mytable5, 2) * 100

#create data frame with cao, Outcomes, and percentage vectors
dat1 =data.frame(
  cao = factor(c("IP", "CAO", "IP", "CAO", "IP", "CAO", "IP", "CAO", "IP", "CAO"), 
               levels = c("IP", "CAO")),
  
  Outcomes = factor(c("Harm Acknowledged", "Harm Acknowledged",
                      "Bank Noncompliance", "Bank Noncompliance",
                      "Project Halted", "Project Halted",
                      "Project Change", "Project Change",
                      "Compensation", "Compensation"), 
                    levels = c("Harm Acknowledged", 
                    "Bank Noncompliance", 
                    "Project Halted", 
                    "Project Change",
                    "Compensation")),
  
  Percentage = c(m1[2,], m2[2,], m3[2,], m4[2,], m5[2,])
)


#make bar chart
p= ggplot(data = dat1, aes(x = Outcomes, y = Percentage, fill = cao))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))+

  #modify legend, change bar colors
  scale_fill_manual(values=c("#181818", "#BFBFBF"), 
                      name="", #legend title
                      breaks=c("IP", "CAO"),
                      labels=c("Inspection Panel", "Compliance Advisor/Ombudsman"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=18),
        legend.position = c(0.75, 0.85),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #y axis title
  ylab("Percentage of Cases")

p

###saves to the directory. If you want to change where it saves, do path = ./(path)/nameofgraph.png(or whatever)


