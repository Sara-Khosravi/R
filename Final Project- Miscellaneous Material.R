     ########################################################
     #......................SARA KHOSRAVI.................. #
     ########################################################
     #........................PROJECT R.....................#

setwd("C:\\Sara\\Data SCIENCE\\R\\My Class")
getwd()

df<-read.csv("credit_train.csv")
df<- read.csv(file.choose())
df[df=='']<-NA
dim(df)
head(df)
tail(df)
str(df)
colnames(df)
# print(c(dim(df),head(df),tail(df),str(df),colnames(df)))

#...............................................................................
#duplicate data
#data cleaning is 1)handling duplicate data 2)handling missing value 3)handling outliers

duplicated(df)            #it returns for you for observation that this duplicated otherwise returns false
# Note: one observation consider duplicated if it has the same value for all features are exactly the same and another observation
# How many duplicated data are there?

sum(duplicated(df))       #--> there are 10728 duplicated data that I will drop at the beginning
r1<-which(duplicated(df))
df<-df[-r1,]

# make a copy
df_org<-df
#df<- df_org1

# checking columns
names(df)
#OR
colnames(df)
str(df)

# Getting the summary of data
summary(df)




readUrl <- function(url) {
  out <- tryCatch(
    
    ########################################################
    # Try part: define the expression(s) you want to "try" #
    ########################################################
    
    {
      # Just to highlight: 
      # If you want to use more than one R expression in the "try part" 
      # then you'll have to use curly brackets. 
      # Otherwise, just write the single expression you want to try and 
      
      message("This is the 'try' part")
      readLines(con = url, warn = FALSE) 
    },
    
    ########################################################################
    # Condition handler part: define how you want conditions to be handled #
    ########################################################################
    
    # Handler when a warning occurs:
    warning = function(cond) {
      message(paste("Reading the URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      
      # Choose a return value when such a type of condition occurs
      return(NULL)
    },
    
    # Handler when an error occurs:
    error = function(cond) {
      message(paste("This seems to be an invalid URL:", url))
      message("Here's the original error message:")
      message(cond)
      
      # Choose a return value when such a type of condition occurs
      return(NA)
    },
    
    ###############################################
    # Final part: define what should happen AFTER #
    # everything has been tried and/or handled    #
    ###############################################
    
    finally = {
      message(paste("Processed URL:", url))
      message("Some message at the end\n")
    }
  )    
  return(out)
}
