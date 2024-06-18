#*******************************************************************************
#...............................................................................
#........................Sara_R_Project_Renewable_Energy........................
#...............................................................................
#*******************************************************************************

#_______________________________________________________________________________
#Changing directory
#_______________________________________________________________________________

# find the working directory
getwd()

# set the working directory
setwd("C:\\Sara\\Data SCIENCE\\R\\My Project")

# clean the working environment
rm(list = ls()) # this will remove EVERYTHING, so be careful!

#_______________________________________________________________________________
# Input csv File
#_______________________________________________________________________________

# To read Excel  files in R, we will use the package openxlsx.
#install.packages("openxlsx")
#To use the package openxlsx, run the following command.

#Input as CSV File
#The csv file is a text file in which the values in the columns are separated
#by a comma. Let's consider the following data 
#present in the file named input.csv.

#You can create this file using windows notepad by copying and pasting this 
#data. Save the file as input.csv using the save 
#As All files(*.*) option in notepad.

#_______________________________________________________________________________
# loading csv data to Dataframe 
#_______________________________________________________________________________

# Method1
# REC <- read.csv("modern-renewable-energy-consumption.csv")
# REC

# or 
# Method2
# data <- read.csv(file.choose()) 

# or
# Method3
REC<-read.table("modern-renewable-energy-consumption.csv", header = TRUE, sep = ",")
REC
# You can type ?read.table and ?read.delim in to the R console to find out more 
# about these functions (the help files for both are in the same place). 


REC[REC=='']<-NA # converting Null to Na

#_______________________________________________________________________________
#...................................  EDA DATA  ................................
#_______________________________________________________________________________

#*******************************************************************************
#                             (A) Data Understanding
#*******************************************************************************
# For data understanding we need to calculate:
# dim, head, tail, str, mean, std, visualization, 

# make a copy
REC_org <- REC

#_______________________________________________________________________________
# Getting familiar with data for Data Understanding in EDA
#_______________________________________________________________________________

# Dimensions of an Object
dim(REC) # 5095    9

# Return the First or Last Parts of an Object
# Checking the head of the dataset
head(REC)

# Checking the tail of the dataset
tail(REC)

# Compactly Display the Structure of an Arbitrary R Object
str(REC)

# Row and Column Names
colnames(REC)
# OR
names(REC)

# Getting the summary of data
summary(REC)
# Rename columns by using "colnames" function
colnames(REC) <- c("Entity","Code","Year","Hydropower", "Solar","Wind",	"OtherRenewables")
REC

# Object Classes
class(REC) 

# The Type of an Object
typeof(REC)         

# print(c(dim(REC), head(REC), tail(REC), str(REC), colnames(REC), summary(REC)))

#*******************************************************************************
#                           (B) Data preparation
#*******************************************************************************

#I saw that the last observations are Na, I will drop them when I drop
#duplicated data in data cleansing part.

# Data preparation or Data cleaning is:
#1)Handling duplicate data
#2)Handling Missing Values
#3)Handling outliers

#make a copy
REC_org1 <- REC

#_______________________________________________________________________________
# Duplicate Data
#_______________________________________________________________________________

# 1) handling duplicate data 
duplicated(REC)            
# it returns for you for observation that this duplicated otherwise returns false
# Note: one observation consider duplicated if it has the same value of all
# features are exactly the same as another observation

# How many duplicated data are there?
sum(duplicated(REC))       
# there are 4 duplicated data that I will drop at the beginning

# Rid of Duplicated Data
RowDuplicate <- which(duplicated(REC))
RowDuplicate 
# The Duplicated data located this Rows: 8  9 10 16

REC <- REC[-RowDuplicate,]
REC

missing(REC)  #  FALSE

#is.na(REC)
# is.na() for data frame returns a dataframe, gives you True if it is NA 
# otherwise gives you False
#complete.cases(REC)
# returns a vector, If you have at least one NA for any row you will get False
# for that row 
#complete.cases(REC)# returns a vector, If you have at least one NA for any 
#row you will get True for that row 

# What is the distribution of target(each column you need)?
# How many missing values we have for each data column?

#sum(is.na(REC)) # 11268 number of missing values
#sum(!complete.cases(REC))# 5091 number of rows that have at least one missing values
#which(!complete.cases(REC))# Row number that have at least one missing values
#colMeans(is.na(REC))*100   # For each column shows you percentage of Missing values
#colSums(is.na(REC))

# What is the distribution of each column you need?
# How many missing values we have in each column?
sum(is.na(REC$Year))
sum(is.na(REC$Hydropower))
sum(is.na(REC$Solar ))          #  13
sum(is.na(REC$Wind))            #  13
sum(is.na(REC$OtherRenewables)) #  54

#since we have  just only one missing value for target,I'll drop that observation.
#miss1 <- which(is.na(REC$Solar)) # [1] 219 220 221 222 223 224 225 226 227 228 229 230 231
#miss1
#REC <- REC[-miss1,]

##miss2 <- which(is.na(REC$Wind)) # [1] 219 220 221 222 223 224 225 226 227 228 229 230 231
#miss2
#REC <- REC[-miss2,]

#miss3 <- which(is.na(REC$OtherRenewables))#  [1] 3213 3214 3215 3216 3217 3218 3219 3220 3221 3222 3223 3224 3225 3226 3227 3228 3229 3230 3231 3232 3233 3234
#[23] 3235 3236 3237 3238 3239 3240 3241 3242 3243 3244 3245 3246 3247 3248 3249 3250 3251 3252 3253 3254 3255 3256
#[45] 3257 3258 3259 3260 3261 3262 3263 3264 3265 3266
#miss3
#REC <- REC[-miss3,]

#*******************************************************************************
#*******************************************************************************
#separating rows that have at least one missing value

#extract the complete rows
##REC[complete.cases(REC),]
#extract the rows with missing REC
#REC[!complete.cases(REC),]

#method2
#missrow1<-which(!complete.cases(REC))
#missrow1
#miss1<-REC[missrow1,]
#miss1
#notmiss1<-REC[-missrow1,]
#notmiss1

# na.omit : 
# removes all observations(rows) that have at least one missing values
# na.omit returns the object with incomplete cases removed.
# na.omit(REC)   

# na.fail 
# na.fail returns the object if it does not contain any missing values,
# na.fail(REC) 

#_______________________________________________________________________________
# Uni_variate Analysis for Numerical columns For ROW DATA
#_______________________________________________________________________________

#... 1) Find average of all columns
#....2) Find max of all columns
#....3) Find min of all columns
#....4) Find range of all columns
#....5) Find sd of all columns
#....5) Find mad of all columns
#....5) Find IQR of all columns
#....5) Find quantile of all columns
#....5) Find fivenum of all columns

#________________________________ Descriptive Data______________________________
# Create Descriptive Summary Statistics Tables in R with table1

install.packages("table1")
library("table1")
table1::label(REC$Hydropower) <- "Hydropower"
table1::label(REC$Solar) <- "Solar"
table1::label(REC$Wind) <- "Wind"
table1::label(REC$OtherRenewables) <- "OtherRenewables"
table1::table1(~Hydropower + Solar + Wind + OtherRenewables | Entity, data = REC)

# In the code below, we are showing how to create a table without stratification by any group.
library("table1") 
table1::table1(~Hydropower + Solar + Wind + OtherRenewables, data = REC)


# By considering APPLY, SAPPLY and LAPPLY, we can recognize in the REC"If there 
# any missing value, outlier and so on or not."
apply (REC,2, median)   # we have to remove not numeric columns,
lapply(REC, median)
sapply(REC, median)

summary(REC, na.rm = TRUE)	
# for categorical columns returns frequency of each level  and for numeric
# columns returns Five-Number_summary and mean 
# (min, max, mean, median, 1st & 3rd quartiles)

# REMOVE CATEGORICAL VARIABLE
NUMdata <- REC[,-c(1,2)]
NUMdata
apply (NUMdata,2,mean, na.rm = TRUE)   # we have to remove not numeric columns,
sapply(NUMdata,mean, na.rm = TRUE)
lapply(NUMdata,mean, na.rm = TRUE)

sapply(NUMdata, median, na.rm = TRUE) # we have to remove not numeric columns
sapply(NUMdata, sd, na.rm = TRUE)
sapply(NUMdata, mad, na.rm = TRUE) # standard deviation, median absolute deviation
sapply(NUMdata, IQR, na.rm = TRUE)
sapply(NUMdata, quantile, probs = seq(0, 1, 1/10), na.rm = TRUE) 
# by default quartiles(five-number-summary)same as finenum() function
sapply(NUMdata, mean, na.rm = TRUE)
sapply(NUMdata, mean, na.rm = TRUE, trim = 0.1)
sapply(NUMdata, fivenum, na.rm = TRUE)
sapply(NUMdata, range, na.rm = TRUE)

#excluding missing values
mean(REC$Year, na.rm = TRUE)
mean(REC$Hydropower, na.rm = TRUE)
mean(REC$Solar, na.rm = TRUE)
mean(REC$Wind, na.rm = TRUE)
mean(REC$OtherRenewables, na.rm = TRUE)

min(REC$Year, na.rm = TRUE)
min(REC$Hydropower, na.rm = TRUE)
min(REC$Solar, na.rm = TRUE)
min(REC$Wind, na.rm = TRUE)
min(REC$OtherRenewables, na.rm = TRUE)

max(REC$Year, na.rm = TRUE)
max(REC$Hydropower, na.rm = TRUE)
max(REC$Solar, na.rm = TRUE)
max(REC$Wind, na.rm = TRUE)
max(REC$OtherRenewables, na.rm = TRUE)

quantile(REC$Year, na.rm = TRUE)
quantile(REC$Hydropower, na.rm = TRUE)
quantile(REC$Solar, na.rm = TRUE)
quantile(REC$Wind, na.rm = TRUE)
quantile(REC$OtherRenewables, na.rm = TRUE)

#min(REC)#[,1:4]);
# we have to remove not numeric columns(only defined on a data frame with all
# numeric variables)

# Min, Max, Range all of numeric data
min  (REC[,3:7],  na.rm = TRUE)
max  (REC[,3:7],  na.rm = TRUE)
range(REC[,3:7],  na.rm = TRUE)
# we have to remove not numeric columns(only defined on a data frame with all
# numeric variables)

#...............................................................................
# bi-variate Analysis for Numerical columns
#...............................................................................
#_______________________________________________________________________________
# extract meaningful features from
#_______________________________________________________________________________
# dropping features are only for identification and we don't have any knowledge
# to extract meaningful features from them
REC[,c("Code")] <- NULL
REC


# ADD A COLUMN FOR MY DATA SET, THE NAME OF THAT IS: "total.REC"
REC$total.REC <- NA
REC$total.REC <- rowSums(REC[ ,c(3:6)], na.rm=TRUE)
head(REC)

# KEEP IMPORTANT COLUMNS. 
Per.REC <- REC[ ,-6]
Per.REC

# For test: keep Year2018
year2018 <- subset(Per.REC, Year=="2018")
year2018


#....................****** install.packages("dplyr") ******....................
install.packages("dplyr")                       # Install dplyr package
library("dplyr")                                # Load dplyr package

# filter variable greater than a value

REC0 <- Per.REC %>% 
  filter(Year > 2000)
dfYless <- subset(Per.REC, Year >= 1965 & Year <= 2000)
dfYless
REC1 <- subset(dfYless,  Year == 2000, select = c(1:6))
REC1


dfYless1 <- subset(REC0, Year >= 2001 & Year <= 2007)
dfYless1
REC2 <- subset(dfYless1,  Year == 2007, select = c(1:6))
REC2


REC3 <- Per.REC %>% 
  filter(Year > 2007)
REC3
# USE FULL JOIN FOR MAKE A NEW DATA
NewREC0 <- merge(x = REC1, y = REC2, all = TRUE)
NewREC0
NEWREC <- merge(x = NewREC0, y = REC3, all = TRUE)
NEWREC

# USE FULL JOIN CREATE A NEW DATASET AND MAKE A COPY OF THAT
NEWREC_ORIGINAL <- NEWREC

#....................****** install.packages("ggplot2") ******....................
install.packages("ggplot2")
library(ggplot2)

# saving plot in working directory 
#--------------------------------------
#1.Open a pdf file
pdf("Renewable Energy.pdf")

# Making a Stacked Area Graph
ggplot(Per.REC, aes(x = Year, y = total.REC, fill = Entity)) +
  geom_area()
# Close the pdf file
dev.off()# you should close the pdf file before open it to see the garph
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
plot_rev <- ggplot(NEWREC, aes(Year, total.REC, fill = Entity, ymax = max(total.REC)+100,
                               title = "total.REC ( per Country per Year "))
plot_rev+geom_bar(stat = "identity", position = "dodge")

# THIS GRAPH ( USE REC4 DATAFRAME)BETTER THAN LAST GRAPH BECAUSE THE VALUE OF 
# WORLD VERY HIGH AND WECAN NOT COMAPRE THE DATA
REC4 <- NEWREC[-grep("World",NEWREC$Entity),]
plot_rev <- ggplot(REC4, aes(Year, total.REC, fill = Entity, ymax = max(total.REC)+100,
                             title = "total.REC ( per Country per Year "))
plot_rev+geom_bar(stat = "identity", position = "dodge")

#-------------------------------------------------------------------------------
# Method1: SELECT CONTINENT OR REGION OF THE CONTINENT TO COMPARE OF THE
# CONSUMPTIN OF RENEWABLE OF ENERGY IN THE WORLD
REC.ORGIN <- NEWREC[ !(NEWREC$Entity %in% c( "Central America","Eastern 
                      Africa", "Europe","Europe (other)","Middle Africa",
                                             "Middle East","Westren Africa","United States", "China")), ]
# Method1: select Region 
REC.ORGIN <- subset(NEWREC, Entity %in% c( "Central America",
                                           "Eastern Africa", "Europe","Europe (other)","Middle Africa",
                                           "Middle East", "Westren Africa","United States","China"))
print (REC.ORGIN)

plot_rev <- ggplot(REC.ORGIN, aes(Year, total.REC, fill = Entity, ymax = max(total.REC)+100,
                                  title = "total.REC ( per Country per Year "))
plot_rev+geom_bar(stat = "identity", position = "dodge")


# create new column in dataset that name is GROUPEntity
NEWREC$GROUPEntity <- NEWREC %>% group_by(Entity)

# create new column in dataset that contains cumulative Total REC
NEWREC$cum_total <- cumsum(NEWREC$total.REC)


#................****** install.packages("dplyr") ******..................

install.packages("dplyr")
library("dplyr")
# https://stackoverflow.com/questions/19824601/how-calculate-growth-rate-in-long-format-data-frame

# Growth rate per annul
# ADD COLUMN FOR MY DATA SET BY NAMING "Growth.rate"
RECF <- NEWREC %>% group_by(Entity) %>% mutate(Growth.rate = (total.REC - lag(total.REC))/lag(total.REC))  
RECF1 <- RECF[,-1]

library(ggplot2)
ggplot(data = RECF , aes(x = Year, y = Growth.rate)) + geom_line() +
  facet_wrap(~Entity)

library(ggplot2)
ggplot(data = NEWREC, aes(x = Solar, y = Hydropower )) + geom_line() +
  facet_wrap(~Entity)

head(RECF)


library("dplyr")                                # Load dplyr package

# filter variable greater than a value
RECcon <- RECF %>% 
  filter(Year > 2007)
RECcon

REC.CON <- subset(RECF, Year >= 2007 & Year <= 2017)
REC.CON


library("dplyr")
library("ggplot2")
ggplot(data = REC.CON, aes(x = Solar, y = total.REC)) +
  geom_bar(stat = "identity") +
  coord_flip() # Horizontal bar plot
#_______________________________________________________________________________
# Sort data on based of total.REC
#_______________________________________________________________________________

# sort by  total.REC

SRECF <- sort(REC.CON$total.REC, decreasing = TRUE, na.last = TRUE)
SRECF
consrew <- aggregate(total.REC ~Entity , data=REC.CON , FUN = sort)
consrew 

# Order data with Base R
# Method1:
TOP.REC <- REC.CON[order(REC.CON$Year, REC.CON$total.REC, decreasing = TRUE),]
TOP.REC
class(TOP.REC)

Year2018 <- subset(TOP.REC, Year==2018)
Year2018
class(Year2018)
typeof(Year2018)


World <- subset(REC.CON, Entity=="World")
World

attach(World)

ggplot(data = World, aes(x =  total.REC, y = Hydropower)) +
  geom_bar(stat = "identity") +
  coord_flip() # Horizontal bar plot

ggplot(data = World, aes(x =  total.REC, y = Solar)) +
  geom_bar(stat = "identity") +
  coord_flip() # Horizontal bar plot

ggplot(data = World, aes(x =  total.REC, y = Wind)) +
  geom_bar(stat = "identity") +
  coord_flip() # Horizontal bar plot

#-------------------------------------------------------------------------------
# boxplot
#-------------------------------------------------------------------------------
# Groped boxplot
boxplot(total.REC~Year,
        xlab="Year", ylab="consumption REC",
        col=rainbow(10))

detach(World)

# round off in R with 2 decimal places - with R round function
REC$Rtotal.REC = round(REC$total.REC,2)
REC
attach(World)
install.packages("plotrix")
library("plotrix")
pie3D(Year,labels = total.REC, explode = 0.2,
      main=" Consumption of Energy in the World in Year 2018")
detach(World)

# Method2: 
TOP.REC <- REC.CON[with(REC.CON, order(Year, total.REC)), ]  
top <- tail(TOP.REC, 10) 

library("plotrix")

# histogram with added parameters
hist(total.REC,
     main="Density of Renewable Energy in The World",
     xlab="Toatal of Renewable of Energy",
     xlim=c(3000,7000),
     col="darkmagenta",
     freq=FALSE
)
# Method3:
# Install dplyr package
library("dplyr")                                     # Load dplyr package
arrange(REC.CON, Year, total.REC)


#_______________________________________________________________________________
# 
# what is the distribution of total.REC and 
# is there any relation between total.REC and Entity?

attach(REC.CON)
summary(REC.CON$total.REC)
REC.CON$total.REC
hist(REC.CON$total.REC, breaks = 4, main = "Consumption of Renewable energy in
                                    the last 10 year ",col="green")

detach(World)


# Box-plot of total.REC and Entity
boxplot(total.REC ~ Entity ,data = REC.ORGIN, main="Consumption of Renewable energy",
        xlab="Entity", ylab="total.REC",col="blue")


#Bi-variate Analysis for continuous(total.REC ) Vs. categorical (Entity)
# Visualization

library(ggplot2)
qplot(Entity, Hydropower, data = REC.ORGIN  , 
      geom="boxplot", fill = Entity)

# Changing histogram plot fill colors by Entity and using semi-transparent fill
p<-ggplot(REC.ORGIN, aes(x=Hydropower, fill=Entity, color=Entity)) +
  geom_histogram(position="identity", alpha=0.5)
p

qplot(Entity, Solar, data = REC.ORGIN  , 
      geom="boxplot", fill = Entity)

# Changing histogram plot fill colors by Entity and using semi-transparent fill
p<-ggplot(REC.ORGIN, aes(x=Solar, fill=Entity, color=Entity)) +
  geom_histogram(position="identity", alpha=0.5)
p

qplot(Entity, Wind, data = REC.ORGIN  , 
      geom="boxplot", fill = Entity)

# Changing histogram plot fill colors by Entity and using semi-transparent fill
p<-ggplot(REC.ORGIN, aes(x=Wind, fill=Entity, color=Entity)) +
  geom_histogram(position="identity", alpha=0.5)
p


# Add mean lines
library(plyr)
mu <- ddply(REC.ORGIN, "Entity", summarise, grp.mean=mean(total.REC,na.rm=T))
head(mu)
p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Entity),
                linetype="dashed")
p

# Add density
p<-ggplot(REC.ORGIN, aes(x=total.REC, fill=Entity, color=Entity)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.6)
p

# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Entity),
             linetype="dashed") + theme(legend.position="top")+labs(title=
"Consumption of renewable Energy histogram plot",x="Solar", y = "Density")


# Summarizing
names(REC.ORGIN)
agg1 <- aggregate( total.REC ~ Entity, REC.ORGIN , mean)
agg1
names(agg1) <- c("Entity","total.REC")
agg1


agg2 <- cbind(aggregate( total.REC ~ Entity , REC , min),
              aggregate( total.REC ~ Entity , REC  , max)[,2],
              aggregate( total.REC ~ Entity , REC  , mean)[,2])

names(agg2) <- c("total.REC","min_REC","max_REC","mean_REC")
agg2

write.table(agg2, file = "agg2.csv",
            sep = "\t", row.names = F)

saveRDS(object = agg2, file = "agg2.rds")
#_______________________________________________________________________________
# Test of independence: Anova
#_______________________________________________________________________________ 
# Null Hypothesis: µGood = µBad (the means of both populations are equal)
# Alternate Hypothesis: µGood ??? µBad (the means of both populations are not equal)

# Perform the ANOVA test:

# One-way ANOVA
# In the one-way ANOVA example, we are modeling crop total.REC as a function of the
# type of Entity used. First we will use aov() to run the model, then we 
# will use summary() to print the summary of the model.

one.way <- aov(total.REC~Entity, data = REC.ORGIN)

summary(one.way)

#Two-way ANOVA
# In the two-way ANOVA example, we are modeling crop total.REC as a function of 
# type of Entity and  Year. First we use aov() to run the model,
# then we use summary() to print the summary of the model.

two.way <- aov(total.REC~Entity + Year, data = REC.ORGIN)
summary(two.way)

# Adding interactions between variables
# Sometimes you have reason to think that two of your independent variables
# have an interaction effect rather than an additive effect.

interaction <- aov(total.REC~Entity * Year, data =  REC.ORGIN)
summary(interaction)

# Adding a Solaring variable
# If you have grouped your experimental treatments in some way, or if you have
# a confounding variable that might affect the relationship you are interested
# in testing, you should include that element in the model as a Solaring
# variable. The simplest way to do this is just to add the variable into the
# model with a '+'.

Solaring <- aov(total.REC~Entity + Year + Solar, data =  REC.ORGIN)
summary(Solaring)

# Find the best-fit model:
# There are now four different ANOVA models to explain the data. How do you
# decide which one to use? Usually you will want to use the 'best-fit' model - 
# the model that best explains the variation in the dependent variable.

install.packages("AICcmodavg")
library("AICcmodavg")

model.set <- list(one.way, two.way, interaction, Solaring)
model.names <- c("one.way", "two.way", "interaction", "Solaring")

aictab(model.set, modnames = model.names)

# Check for homoscedasticity
# To check whether the model fits the assumption of homoscedasticity, look at 
# the model diagnostic plots in R using the plot() function:

par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

# since target is categorical variable, in uni-variate Analysis for summarizing I
# will find frequency and for visualization I plot: pie chart or bar-chart 
tbl<-table(REC$ Entity)
tbl
tbl<-tbl[2:6]
tbl
#_______________________________________________________________________________


###############################################################################
# Regional consumption pattern 2019
# Renewables consumption*
# Growth rate per annum  2019 2008-18
# Energy per capita: Distribution across countries 
# Growth rate per annum
# Renewables: Renewable power generation*
# Renewable energy: Generation by source*
# 1965-2000 36% , 2001,....2019  Efficiency	 	 
# Year(s)	 Efficiency factor
################################################################################
################################################################################
install.packages("ggplot2")
library(ggplot2)


df <- aggregate(total.REC~ Solar + Wind+ Hydropower , data=Per.REC, FUN = sum)
typeof(df)

ggplot(REC.CON, aes(x = Hydropower, y = Solar)) + 
  geom_point(aes(color = factor(Hydropower)) + stat_smooth(method = "lm"))
                                                               
ggplot(REC.CON, aes(x = Hydropower, y = Wind)) + 
  geom_point(aes(color = factor(Hydropower)) + stat_smooth(method = "lm"))

pairs(~total.REC +Solar + Wind+ , col = factor(Per.REC$Entity), pch = 19, data = Per.REC , lower.panel = NULL)

##Continuous Vs. Continuous   : For Visualization scatter plot,...
#                              For test of independence: pearson correlation or spearman or  ...
install.packages("ggpubr")
library("ggpubr")
cor(Hydropower, Solar, method = c("pearson", "kendall", "spearman"))
cor.test(Hydropower, Solar, method=c("pearson", "kendall", "spearman"))

install.packages("ggpubr")
library("ggpubr")
ggscatter(Per.REC, x = "Hydropower", y = "Solar", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Hydropower(terawatt-hours)", ylab = "Solar(terawatt-hours)")

#.................................mapping..................................
#=================
# INSTALL PACKAGES
#=================
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)



