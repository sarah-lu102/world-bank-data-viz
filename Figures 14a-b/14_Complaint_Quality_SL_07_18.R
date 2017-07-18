##################################################################
#### IFI Project: Bar Chart of Percentage of NGO #################
##################Involvement by Complaint Quality ###############
##################################################################
# REVISED BY SARAH LU
# July 13, 2017

setwd("/Users/sarah/Google Drive/IFIs_and_HR/Data")

library(foreign)
library(ggplot2)
library(plyr)
dat <- read.dta("data/dta files/Cleaned_Complete_WBHR_Data_07052017.dta")
attach(dat)

# table1 <- table(raw_data, foreign_ngo, domestic_ngo) #three way table

####################################################################
#############get percentages for complaint variable##################
####################################################################

#count function turns frequency table into a data frame
df1 <- count(dat, c('complaint', 'foreign_ngo', 'domestic_ngo')) 

complaint_freq <- df1[6, "freq"] #gets frequency of complaint for no ngos
complaint_total <- 0

#get total count for raw_data = 1
for(i in 6:9){
  complaint_total = complaint_total + df1[i, "freq"]
}

no_ngo_complaint_pct <- 100*complaint_freq/complaint_total
ngo_complaint_pct <- 100 - no_ngo_complaint_pct



####################################################################
#############get percentages for raw data variable##################
####################################################################

#count function turns frequency table into a data frame
df2 <- count(dat, c('raw_data', 'foreign_ngo', 'domestic_ngo')) 

raw_freq <- df2[5, "freq"] #gets frequency of raw_data for no ngos
raw_total <- 0

#get total count for raw_data = 1
for(i in 5:8){
  raw_total = raw_total + df2[i, "freq"]
}

no_ngo_raw_pct <- 100*raw_freq/raw_total
ngo_raw_pct <- 100 - no_ngo_raw_pct


####################################################################
#############get percentages for testimony data variable##################
####################################################################

#count function turns frequency table into a data frame
df3 <- count(dat, c('personal_testimony', 'foreign_ngo', 'domestic_ngo')) 

testimony_freq <- df3[5, "freq"] #gets frequency of testimony for no ngos
testimony_total <- 0

#get total count for raw_data = 1
for(i in 5:7){
  testimony_total = testimony_total + df3[i, "freq"]
}

no_ngo_test_pct <- 100*testimony_freq/testimony_total
ngo_test_pct <- 100 - no_ngo_test_pct


####################################################################
#############get percentages for bank policy variable##################
####################################################################

#count function turns frequency table into a data frame
df4 <- count(dat, c('bank_policy', 'foreign_ngo', 'domestic_ngo')) 

policy_freq <- df4[5, "freq"] #gets frequency of raw_data for no ngos
policy_total <- 0

#get total count for bank policy = 1
for(i in 5:8){
  policy_total = policy_total + df4[i, "freq"]
}

no_ngo_policy_pct <- 100*policy_freq/policy_total
ngo_policy_pct <- 100 - no_ngo_policy_pct


####################################################################
#############get percentages for policy violated variable##################
####################################################################

#count function turns frequency table into a data frame
df5 <- count(dat, c('bank_policy_viol', 'foreign_ngo', 'domestic_ngo')) 

viol_freq <- df5[5, "freq"] #gets frequency of raw_data for no ngos
viol_total <- 0

#get total count for bank policy violated = 1
for(i in 5:8){
  viol_total = viol_total + df5[i, "freq"]
}

no_ngo_viol_pct <- 100*viol_freq/viol_total
ngo_viol_pct <- 100 - no_ngo_viol_pct



####################CREATE DATA FRAME#################################################################
ngotype1 <- c("NGO Involvement", "No NGO Involvement")
ngotype2 <- rep(ngotype1, 5)

finalDF =data.frame(
  ngotype = factor(ngotype2, 
               levels = c("NGO Involvement", "No NGO Involvement")),
  
  complaint_quality = factor(c("Complaint", "Complaint",
                      "Raw Data", "Raw Data",
                      "Personal Testimony", "Personal Testimony",
                      "Bank Policy Citation", "Bank Policy Citation",
                      "Bank Policy Violation", "Bank Policy Violation"), 
                    levels = c("Complaint", 
                               "Raw Data", 
                               "Personal Testimony", 
                               "Bank Policy Citation",
                               "Bank Policy Violation")),
  
  Percentage = c(ngo_complaint_pct, no_ngo_complaint_pct, ngo_raw_pct, no_ngo_raw_pct, 
                 ngo_test_pct, no_ngo_test_pct, ngo_policy_pct, no_ngo_policy_pct, 
                 ngo_viol_pct, no_ngo_viol_pct)
)

##########################make FIGURE 14################################

p= ggplot(data = finalDF, aes(x = complaint_quality, y = Percentage, fill = ngotype))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,75), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c("#181818", "#BFBFBF"),
                    name="")+ #legend title
                    #breaks=c("IP", "CAO"),
                    #labels=c("Inspection Panel", "Compliance Advisor/Ombudsman"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=18),
        legend.position = c(0.9, 0.95),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #y axis title
  ylab("Percentage of Cases")

p


##########################make FIGURE 15 (no complaint variable) ################################

DF_withoutcomplaint <- finalDF[c(3:10),] #remove first 2 rows of old df by subsetting

q= ggplot(data = DF_withoutcomplaint, aes(x = complaint_quality, y = Percentage, fill = ngotype))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,75), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c("#181818", "#BFBFBF"),
                    name="")+ #legend title
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=18),
        legend.position = c(0.9, 0.95),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #y axis title
  ylab("Percentage of Cases")

q
