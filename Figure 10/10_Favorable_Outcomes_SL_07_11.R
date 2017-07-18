##################################################################
#### IFI Project: Bar Chart of Outcomes for Cases ################
##################################################################
# REVISED BY SARAH LU
# July 11, 2017

setwd("/Users/sarah/Google Drive/IFIs_and_HR/Data")

library(foreign)
library(ggplot2)
dat <- read.dta("data/dta files/Cleaned_Complete_WBHR_Data_07052017.dta")
attach(dat)

ip_vec <- c(0)

###############################################
######IP GRAPH#################################
###############################################

#initialize counters for outcome types
no_pos_outcome <- 0
harm_ack <- 0
proj_change <- 0
compens <- 0

#loop through project types
for(i in 1:9){
  
  #loop through all rows in dataframe
  for(j in 1:nrow(dat)){
    
    #look for no positive outcome (no harm acknowledged, project change, or compensation)
    #also sorts through IP data only (cao = 0)
    
    if(dat[j, "cao"] == 0 && dat[j, "project_type"] == i && dat[j, "harm_acknowledged"] == 0 
       && dat[j, "project_change"] == 0 && dat[j, "compensation"] == 0){
      
      no_pos_outcome <- no_pos_outcome + 1
    }

    #look for harm acknowledged
    #also sorts through IP data only (cao = 0) 
    if(dat[j, "cao"] == 0 && dat[j, "project_type"] == i && dat[j, "harm_acknowledged"] == 1){
      
      harm_ack <- harm_ack + 1
    }

    #look for project change
    #also sorts through IP data only (cao = 0) 
    if(dat[j, "cao"] == 0 && dat[j, "project_type"] == i && dat[j, "project_change"] == 1){
  
      proj_change <- proj_change + 1
    }

    #look for compensation
    #also sorts through IP data only (cao = 0) 
    if(dat[j, "cao"] == 0 && dat[j, "project_type"] == i && dat[j, "compensation"] == 1){
  
      compens <- compens + 1
    }
  }
  
  #add counts to vector
  ip_vec <- append(ip_vec, no_pos_outcome)
  ip_vec <- append(ip_vec, harm_ack)
  ip_vec <- append(ip_vec, proj_change)
  ip_vec <- append(ip_vec, compens)
  
  no_pos_outcome <- 0
  harm_ack <- 0
  proj_change <- 0
  compens <- 0

}      
      
ip_vec <- ip_vec[2:37] #get rid of first placeholder value

###################################
#####create data frame for IP plot

#create vector for NGO type
outcomes1 <- c("No Positive Outcome", "Harm Acknowledged","Project Change", "Compensation")
outcomes2 <- rep(outcomes1, 9)

#create vector for project type
projecttype <- c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "9", "9", "9", "9")

IP_df <- data.frame(
  Outcome_type = factor(outcomes2, 
                    levels = c("No Positive Outcome", "Harm Acknowledged","Project Change", "Compensation")),
  
  Project = factor(projecttype,
                 levels = c("1","2","3","4","5","6","7","8","9")),
  
  Num_cases = ip_vec
)

#make IP bar chart
p= ggplot(data = IP_df, aes(x = Project, y = Num_cases, fill = Outcome_type))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 45, brings the y axis down to 0
  scale_y_continuous(limits = c(0,45), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c( "#b2b2b2", "#777777", "#444444", "#000000"), 
                    name="", #legend title
                    breaks=c("No Positive Outcome", "Harm Acknowledged", "Project Change", "Compensation"))+
  
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   labels = c("Infrastructure", 
                              "Mining/Resource Extraction", "Land Management/Administration",
                              "Environmental Sustainability", "Agriculture and Forestry", 
                              "Manufacturing and Services",
                              "Other (General Poverty Reduction)", 
                              "Government Capacity Building", "Private Sector Capacity Building"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=14),
        legend.position = c(0.85, 0.9),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #x and y axis title
  ylab("Number of Cases")+
  #xlab("Issue")+
  
  #plot title
  labs(title = "Inspection Panel")

p




###############################################
######CAO GRAPH#################################
###############################################

#initalize vector for CAO ngo counts
cao_vec <- c(0)

#initialize counters for outcome types
no_pos_outcome1 <- 0
harm_ack1 <- 0
proj_change1 <- 0
compens1 <- 0

#loop through project types
for(i in 1:9){
  
  #loop through all rows in dataframe
  for(j in 1:nrow(dat)){
    
    #look for no positive outcome (no harm acknowledged, project change, or compensation)
    #also sorts through CAO data only (cao = 1)
    
    if(          !is.na(dat[j, "project_type"])
              && !is.na(dat[j, "harm_acknowledged"])
              && !is.na( dat[j, "project_change"])
              && !is.na(dat[j, "compensation"])
       && dat[j, "cao"] == 1 && dat[j, "project_type"] == i && dat[j, "harm_acknowledged"] == 0 
       && dat[j, "project_change"] == 0 && dat[j, "compensation"] == 0 ){
      
      no_pos_outcome1 <- no_pos_outcome1 + 1
    }
    
    #look for harm acknowledged
    #also sorts through CAO data only (cao = 1)
    if(!is.na(dat[j, "project_type"]) && !is.na(dat[j, "harm_acknowledged"]) &&
      dat[j, "cao"] == 1 && dat[j, "project_type"] == i && dat[j, "harm_acknowledged"] == 1){
      
      harm_ack1 <- harm_ack1 + 1
    }
    
    #look for project change
    #also sorts through CAO data only (cao = 1)
    if(!is.na(dat[j, "project_type"]) && !is.na(dat[j, "project_change"]) &&
      dat[j, "cao"] == 1 && dat[j, "project_type"] == i && dat[j, "project_change"] == 1){
      
      proj_change1 <- proj_change1 + 1
    }
    
    #look for compensation
    #also sorts through CAO data only (cao = 1)
    if(!is.na(dat[j, "project_type"]) && !is.na(dat[j, "compensation"]) &&
      dat[j, "cao"] == 1 && dat[j, "project_type"] == i && dat[j, "compensation"] == 1){
      
      compens1 <- compens1 + 1
    }
  }
  
  #add counts to vector
  cao_vec <- append(cao_vec, no_pos_outcome1)
  cao_vec <- append(cao_vec, harm_ack1)
  cao_vec <- append(cao_vec, proj_change1)
  cao_vec <- append(cao_vec, compens1)
  
  no_pos_outcome1 <- 0
  harm_ack1 <- 0
  proj_change1 <- 0
  compens1 <- 0
  
}      

cao_vec <- cao_vec[2:37] #get rid of first placeholder value

###################################
#####create data frame for CAO plot

#create vector for outcome
outcomes1 <- c("No Positive Outcome", "Harm Acknowledged","Project Change", "Compensation")
outcomes2 <- rep(outcomes1, 9)

#create vector for project type
projecttype <- c("1", "1", "1", "1", "2", "2", "2", "2", "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "5", "5", "6", "6", "6", "6", "7", "7", "7", "7", "8", "8", "8", "8", "9", "9", "9", "9")

CAO_df <- data.frame(
  Outcome_type = factor(outcomes2, 
                        levels = c("No Positive Outcome", "Harm Acknowledged","Project Change", "Compensation")),
  
  Project = factor(projecttype,
                   levels = c("1","2","3","4","5","6","7","8","9")),
  
  Num_cases = cao_vec
)

#make CAO bar chart
q= ggplot(data = CAO_df, aes(x = Project, y = Num_cases, fill = Outcome_type))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 45, brings the y axis down to 0
  scale_y_continuous(limits = c(0,45), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c( "#b2b2b2", "#777777", "#444444", "#000000"), 
                    name="", #legend title
                    breaks=c("No Positive Outcome", "Harm Acknowledged", "Project Change", "Compensation"))+
  
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   labels = c("Infrastructure", 
                              "Mining/Resource Extraction", "Land Management/Administration",
                              "Environmental Sustainability", "Agriculture and Forestry", 
                              "Manufacturing and Services",
                              "Other (General Poverty Reduction)", 
                              "Government Capacity Building", "Private Sector Capacity Building"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=14),
        legend.position = c(0.85, 0.9),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #x and y axis title
  ylab("Number of Cases")+
  #xlab("Issue")+
  
  #plot title
  labs(title = "Compliance Advisor/Ombudsman")

q


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p,q, cols=1)
      