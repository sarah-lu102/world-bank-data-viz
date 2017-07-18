##################################################################
#### IFI Project: Bar Chart of NGO involvement for issues ########
##################################################################
# REVISED BY SARAH LU
# June 16, 2017

setwd("/Users/sarah/Google Drive/IFIs & Human Rights/Data")

library(foreign)
library(ggplot2)
dat <- read.dta("data/dta files/Cleaned_WBHR_Data_06212017.dta")
attach(dat)

#initalize vector for IP ngo counts
ip_vec <- c(0)


###############################################
######IP GRAPH#################################
###############################################

#initialize counters for ngo type (dcount = domestic ngo, 
#                                 fcount = foreign ngo,
#                                 ncount = neither)
dcount <- 0
fcount <- 0
ncount <- 0

#loop through issues
for(i in 1:9){
  
  #loop through rows (in which cao = 0)
  for(j in 1:130){
    
    if(j!= 34){
      
      #look for domestic ngo occurrences for that issue
      if(dat[j, "issue_primary"] == i && dat[j, "domestic_ngo"] == 1){
        dcount = dcount + 1
      }
      
      #look for foreign ngo occurrences for that issue
      if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 1){
        fcount = fcount + 1
      }
      
      #look for no ngo occurrences for that issue
      if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 0 && dat[j, "domestic_ngo"] == 0){
        ncount = ncount + 1
      }
      
    }
    
  }
  #add counts to vector
  ip_vec <- append(ip_vec, dcount)
  ip_vec <- append(ip_vec, fcount)
  ip_vec <- append(ip_vec, ncount)
  
  dcount <- 0
  fcount <- 0
  ncount <- 0
  
}
ip_vec <- ip_vec[2:28] #get rid of first placeholder value


#create data frame for IP plot

#create vector for NGO type
ngolist1 <- c("Domestic NGO", "Foreign NGO","No NGO")
ngolist2 <- rep(ngolist1, 9)

#create vector for issues
issueslist <- c("1", "1", "1", "2", "2", "2", "3", "3", "3", "4", "4", "4", "5", "5", "5", "6", "6", "6", "7", "7", "7", "8", "8", "8", "9", "9", "9")

IP_df <- data.frame(
  NGO_type = factor(ngolist2, 
                    levels = c("Domestic NGO", "Foreign NGO","No NGO")),
  
  Issue = factor(issueslist,
                 levels = c("1","2","3","4","5","6","7","8","9")),
  
  Num_cases = ip_vec
)

#make IP bar chart
p= ggplot(data = IP_df, aes(x = Issue, y = Num_cases, fill = NGO_type))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,50), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c( "#b2b2b2", "#555555", "#000000"), 
                    name="", #legend title
                    breaks=c("Domestic NGO", "Foreign NGO", "No NGO"))+
  
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   labels = c("Involuntary Resettlement", 
                              "Environmental Damage", "Economic Damage",
                              "Labor Rights Violations", "Cultural Heritage", 
                              "Physical Integrity Rights",
                              "Failure to Communicate", 
                              "Corruption", "Misalignment/Project Failure"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=14),
        legend.position = c(0.85, 0.85),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45))+ #no x axis title
  
  #x and y axis title
  ylab("Number of Cases")+
  #xlab("Issue")+
  
  #plot title
  labs(title = "Inspection Panel- Primary Issue Only")

p

###############################################
######CAO GRAPH################################
###############################################

#initalize vector for CAO ngo counts
cao_vec <- c(0)
#initialize counters for ngo type (dcount = domestic ngo, 
#                                 fcount = foreign ngo,
#                                 ncount = neither)
dcount1 <- 0
fcount1 <- 0
ncount1 <- 0

#loop through issues
for(i in 1:9){
  
  #loop through rows (in which cao = 0)
  for(j in 131:247){
    
    #look for domestic ngo occurrences for that issue
    if(dat[j, "issue_primary"] == i && dat[j, "domestic_ngo"] == 1){
      dcount1 = dcount1 + 1
    }
    
    #look for foreign ngo occurrences for that issue
    if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 1){
      fcount1 = fcount1 + 1
    }
    
    #look for no ngo occurrences for that issue
    if(dat[j, "issue_primary"] == i && dat[j, "foreign_ngo"] == 0 && dat[j, "domestic_ngo"] == 0){
      ncount1 = ncount1 + 1
    }
    
  }
  #add counts to vector
  cao_vec <- append(cao_vec, dcount1)
  cao_vec <- append(cao_vec, fcount1)
  cao_vec <- append(cao_vec, ncount1)
  
  dcount1 <- 0
  fcount1 <- 0
  ncount1 <- 0
  
  
}
cao_vec <- cao_vec[2:28] #get rid of first placeholder value


#create data frame for IP plot

#create vector for NGO type
ngolist3 <- c("Domestic NGO", "Foreign NGO","No NGO")
ngolist4 <- rep(ngolist3, 9)

#create vector for issues
issueslist <- c("1", "1", "1", "2", "2", "2", "3", "3", "3", "4", "4", "4", "5", "5", "5", "6", "6", "6", "7", "7", "7", "8", "8", "8", "9", "9", "9")

CAO_df <- data.frame(
  NGO_type = factor(ngolist4, 
                    levels = c("Domestic NGO", "Foreign NGO","No NGO")),
  
  Issue = factor(issueslist,
                 levels = c("1","2","3","4","5","6","7","8","9")),
  
  Num_cases = cao_vec
)

#make CAO bar chart
q= ggplot(data = CAO_df, aes(x = Issue, y = Num_cases, fill = NGO_type))+
  geom_bar(stat = "identity", position = position_dodge(), width = 0.75)+
  
  #change y axis to go from 0 - 100, brings the y axis down to 0
  scale_y_continuous(limits = c(0,50), expand = c(0, 0))+
  
  #modify legend, change bar colors
  scale_fill_manual(values=c( "#b2b2b2", "#555555", "#000000"), 
                    name="", #legend title
                    breaks=c("Domestic NGO", "Foreign NGO", "No NGO"))+
  
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                   labels = c("Involuntary Resettlement", 
                              "Environmental Damage", "Economic Damage",
                              "Labor Rights Violations", "Cultural Heritage", 
                              "Physical Integrity Rights",
                              "Failure to Communicate", 
                              "Corruption", "Misalignment/Project Failure"))+
  
  theme_classic() + theme(panel.grid=element_blank(), panel.border=element_blank())+
  theme(text = element_text(size=14),
        legend.position = c(0.85, 0.85),
        axis.title.x = element_blank(), #no x axis title
        axis.text.x = element_text(hjust=1, angle=45))+ 
  
  #x and y axis title
  ylab("Number of Cases")+
  #xlab("Issue")+
  
  #plot title
  labs(title = "Compliance Advisor/Ombudsman- Primary Issue Only")

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
