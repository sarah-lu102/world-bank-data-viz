##################################################################
#### IFI Project: Heatmap of case frequency across countries #####
##################################################################
# REVISED BY SARAH LU
# June 29, 2017

setwd("/Users/sarah/Google Drive/IFIs_and_HR/Data")

library(foreign)
library(choroplethr)
library(choroplethrMaps)
library(dplyr)
library(plyr)

#data(country.map) #choroplethr country names, check if our data matches up

library(ggplot2)
dat <- read.dta("data/dta files/Cleaned_WBHR_Data_06272017.dta")
attach(dat)


#Split/subset a data frame by factors in one column
separated <- split(dat, dat$cao) #splits dataframe by cao values (0 or 1)


IPdata <- separated$"0" #make a dataframe with only IP data

allcountries <- sapply(IPdata$country, tolower) #get the column "country" from the data, transform to lowercase

#edit country names to match with choroplethr data
allcountries <- replace(allcountries, allcountries=="congo, democratic republic of the", "democratic republic of the congo")
allcountries <- replace(allcountries, allcountries=="kyrgyz republic", "kyrgyzstan")
allcountries <- replace(allcountries, allcountries=="yemen, republic of", "yemen")
allcountries <- replace(allcountries, allcountries=="tanzania", "united republic of tanzania")
allcountries <- replace(allcountries, allcountries=="serbia", "republic of serbia")

countries <- factor(allcountries) #split into factors

countrycount <- table(countries) #turn into a table with countries + their counts

df <- as.data.frame(countrycount) #turn table into dataframe

df <- rename(df, region = countries, value = Freq)

df["value"] <- log(df["value"]) + 1 #log the numbers

country_choropleth(df, title = "", legend = "cases", num_colors = 1) +
  scale_fill_continuous(low="#e6e6e6", high="#262626", na.value="#ffffff")

