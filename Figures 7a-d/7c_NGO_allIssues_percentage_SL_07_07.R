##################################################################
#### IFI Project: Bar Chart of NGO involvement for issues ########
##################################################################
# REVISED BY SARAH LU
# June 21, 2017

#rm(list=ls())
setwd("/Users/sarah/Google Drive/IFIs_and_HR/Data")

library(foreign)
library(ggplot2)
dat <- read.dta("data/dta files/Cleaned_WBHR_Data_06212017.dta")
attach(dat)

#initalize vector for IP ngo counts
ip_vec <- c(0)


###############################################
######create IP percentage vector################################
###############################################

#initialize counters for ngo type (dcount = domestic ngo, 
#                                 fcount = foreign ngo,
#                                 tcount = total ngo)
dcount <- 0
fcount <- 0
tcount <- 0
total <- 0

#loop through issues
for(i in 1:9){
  
  #loop through rows (in which cao = 0)
  for(j in 1:130){
    domestic <- 0 #set up a boolean
    
    if(j!= 34){
      
      if(dat[j, "issue_primary"] == i || dat[j, "issue_secondary"] == i || dat[j, "issue_tertiary"] == i){
        total = total + 1
      }
      
      #look for domestic ngo occurrences for that issue
      if(dat[j, "issue_primary"] == i && dat[j, "domestic_ngo"] == 1){
        dcount = dcount + 1
        domestic <- 1
      }
      if(dat[j, "issue_secondary"] == i && dat[j, "domestic_ngo"] == 1){
        dcount = dcount + 1
        domestic <- 1
      }
      if(dat[j, "issue_tertiary"] == i && dat[j, "domestic_ngo"] == 1){
        dcount = dcount + 1
        domestic <- 1
      }
      
      if(domestic == 0){ #if no domestic ngos found for that row, look for foreign ngos
      
        #look for foreign ngo occurrences for that issue
        if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 1){
          fcount = fcount + 1
        }
        if(dat[j, "issue_secondary"] == i && dat[j, "foreign_ngo"] == 1){
          fcount = fcount + 1
        }
        if(dat[j, "issue_tertiary"] == i && dat[j, "foreign_ngo"] == 1){
          fcount = fcount + 1
        }
        
      }
      
    }
    
  }
  tcount <- dcount + fcount #get total ngo count
  tcount <- tcount/total #turn into percentage
  
  #add counts to vector
  ip_vec <- append(ip_vec, tcount)
  
  dcount <- 0
  fcount <- 0
  tcount <- 0
  total <- 0
  
}
ip_vec <- ip_vec[2:10] #get rid of first placeholder value
ip_vec <- ip_vec * 100


###############################################
######create CAO percentage vector################################
###############################################

#initalize vector for CAO ngo counts
cao_vec <- c(0)
#initialize counters for ngo type (dcount = domestic ngo, 
#                                 fcount = foreign ngo,
#                                 tcount = domestic + foreign)
dcount1 <- 0
fcount1 <- 0
tcount1 <- 0
total1 <- 0

#loop through issues
for(i in 1:9){
  
  #loop through rows (in which cao = 1)
  for(j in 131:247){
    domestic <- 0 #set up a boolean
    
    #count the total number of cases with that issue
    if(dat[j, "issue_primary"] == i || dat[j, "issue_secondary"] == i || dat[j, "issue_tertiary"] == i){
      total1 <- total1 + 1
    }
    
    #look for domestic ngo occurrences for that issue
    if(dat[j, "issue_primary"] == i && dat[j, "domestic_ngo"] == 1){
      dcount1 = dcount1 + 1
      domestic <- 1
    }
    if(dat[j, "issue_secondary"] == i && dat[j, "domestic_ngo"] == 1){
      dcount1 = dcount1 + 1
      domestic <- 1
    }
    if(dat[j, "issue_tertiary"] == i && dat[j, "domestic_ngo"] == 1){
      dcount1 = dcount1 + 1
      domestic <- 1
    }
    
    if(domestic == 0){
    
      #look for foreign ngo occurrences for that issue
      if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 1){
        fcount1 = fcount1 + 1
      }
      if(dat[j, "issue_secondary"] == i && dat[j, "foreign_ngo"] == 1){
        fcount1 = fcount1 + 1
      }
      if(dat[j, "issue_tertiary"] == i && dat[j, "foreign_ngo"] == 1){
        fcount1 = fcount1 + 1
      }
      
    }
    
  }
  
  tcount1 <- fcount1 + dcount1
  tcount1 <- tcount1/total1 #turn into percentage
  
  
  #add counts to vector
  cao_vec <- append(cao_vec, tcount1)
  
  
  dcount1 <- 0
  fcount1 <- 0
  tcount1 <- 0
  total1 <- 0
  
}
cao_vec <- cao_vec[2:10] #get rid of first placeholder value
cao_vec <- cao_vec * 100

######merge cao and ip percentage vectors########
total_vec <- c(0)
for(k in 1:9){
  total_vec <- append(total_vec, ip_vec[k])
  total_vec <- append(total_vec, cao_vec[k])
}

total_vec <- total_vec[2:19]

#create vector for institution type
temp1 <- c("IP","CAO")
temp2 <- rep(temp1, 9)

########create data frame##############

#create vector for issues
issueslist <- c("1", "1", "2", "2", "3", "3", "4", "4", "5", "5", "6", "6", "7", "7", "8", "8", "9","9")

df <- data.frame(
  
  Issue = factor(issueslist,
                 levels = c("1","2","3","4","5","6","7","8","9")),
  
  Percentages = total_vec,
  
  Institution = factor(temp2, 
                       levels = c("IP", "CAO"))
  
)

###create bar graph#######
p= ggplot(data = df, aes(x = Issue, y = Percentages, fill = Institution))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,100), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c("#555555", "#b2b2b2"), 
                    name="", #legend title
                    breaks=c("IP", "CAO"),
                    labels=c("Inspection Panel", "Compliance Advisor/Ombudsman"))+
  
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   labels = c("Involuntary Resettlement", 
                              "Environmental Damage", "Economic Damage",
                              "Labor Rights Violations", "Cultural Heritage", 
                              "Physical Integrity Rights",
                              "Failure to Communicate", 
                              "Corruption", "Misalignment/Project Failure"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=14),
        legend.position = c(0.75, 0.85),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #y axis title
  ylab("Percentage of cases with NGO Involvement")

p


