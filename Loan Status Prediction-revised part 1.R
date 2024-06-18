##############################################################################################
#Changing directory
##############################################################################################
setwd('C:\\Sara\\Data SCIENCE\\R\\My Class')
getwd()






##############################################################################################
# loading csv data to dataframe 
##############################################################################################
df<-read.csv("credit_train.csv")
#train<-read.csv("credit_train.csv")

#or
#train<-read.csv(file.choose()) 


#test<-read.csv("credit_test.csv")

#train$source<-"train"
#test$source<-"test"
# smartbind the dataframes together
#library(plyr)
#df<-rbind.fill(train, test)#Combine data.frames by row, filling in missing columns



df[df=='']<-NA #converting Null to Na
##############################################################################################
#Getting familiar with data
##############################################################################################
dim(df)
colnames(df)
str(df)
summary(df)

# Checking the head of the dataset
head(df)
head(df,10)


# Checking the tail of the dataset
tail(df)

# number of missing values:
colSums(is.na(df))
#Percentage of missing values:
round(colMeans(is.na(df))*100,2)#2:digit=2


#I saw that the last observations are Na, I will drop them when I drop duplicated data in data cleansing part.

#data cleansing is 1)Handling duplicate data 2)Handling Missing Values 3)Handling outliers
#make a copy
df_org<-df
##########################################################################################
#Duplicate Data
#########################################################################################
duplicated(df)# returns for you TRUE for observation that is duplicated otherwise returns FALSE
#Note: an observation considered duplicated if values of all features are exactly the same as another observation

#How many duplicated data are there?
sum(duplicated((df)))#--> there are 10728 duplicated data that I will drop at the beginning
r1<-which(duplicated(df))
df<-df[-r1,]

#make a copy
df_org1<-df
#df<-df_org1

# number of missing values:
colSums(is.na(df))
#Percentage of missing values:
round(colMeans(is.na(df))*100,2)#2:digit=2

tail(df)
# because all the features for last row are NA you can remove it
#df<-df[-100001,]

#checking columns 
names(df)
#or
colnames(df)
str(df)

# Getting the summary of Data
summary(df)


#dropping features are only for identification and we don't have any knowlege to extract meaningfull features from them
df[,c("Loan.ID","Customer.ID")]<-NULL







#Q1.what is the distribution of target(Loan.Status)?
#how many missing values we have for loan.Status
sum(is.na(df$Loan.Status))

#since we have  just only one missing value for target,I'll drop that observation.
r1<-which(is.na(df$Loan.Status))
r1
df<-df[-r1,]
#since target is categorical variable, in univaraite Analysis for summarization I will find frequency and for visualization I plot: pie chart or barchart 

tbl<-table(df$Loan.Status)
tbl
tbl<-tbl[2:3]
tbl
#or

tbl<-aggregate(df$Loan.Status,list(df$Loan.Status),length)
tbl


# Pie Chart with Percentages
count<-table(df$Loan.Status)
count
freq1 <- c(count[2], count[3])
lbls <- c("Charged Off", "Fully Paid")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Loan.Status")
#as you can see we have 75% fully paid and 25% charged off, so we are dealing with unbalaced data

# Simple Bar Plot
counts <- table(df$Loan.Status)
counts
barplot(c(count[2], count[3]), main="Loan Status",
        ylab="Number",col = 'blue',horiz = FALSE)


#you can repeat this process for all categorical columns for example for Term

#Q2.what is the distribution of Term?
#how many missing values we have for Term
sum(is.na(df$Term))

#we don't have  any missing value for Term

#since Term is categorical variable, in univaraite Analysis for summarization I will find frequency and for visualization I plot: pie chart or barchart 

tbl<-table(df$Term)
tbl
tbl<-tbl[2:3]
tbl



# Pie Chart with Percentages
count<-table(df$Term)
count
freq1 <- c(count[2], count[3])
lbls <- c("Charged Off", "Fully Paid")
pct <- round(freq1/sum(freq1)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(freq1,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Term")

# Simple Bar Plot
counts <- table(df$Term)
counts
barplot(c(count[2], count[3]), main="Term",
        ylab="Number",col = 'blue',horiz = FALSE)

#Q3.Is there any relation between Term and target(Loan.Status)?
#bivariate analysis for Term Vs. Loan.statuse
#bivariate analysis for categorical vs. categorical:for visualization:stacked barchart or grouped bar chart, ...
#                                                   for summarization: Contingency table(two-way table)
#                                                   for test of independence: chi-square test


# Stacked Bar Plot with Colors and Legend
tbl<-table(df$Term,df$Loan.Status)
tbl
counts<-tbl[2:3,2:3]
counts
barplot(counts, main="Term Vs. Loan Status",
        xlab="Loan Status", col=c("darkblue","red"),
        legend = rownames(counts))


add<-addmargins(xtabs(~ Term+Loan.Status,data=df))
add
add[2:4,2:4]
prop.table(xtabs(~ Term+Loan.Status,data=df))[2:3,2:3]

##########################################################################################

#Chi-squared Test of Independence(Pearson's Chi-squared)

##########################################################################################
#The chi-square test of independence is used to analyze the frequency table (i.e. contengency table) formed by
#two categorical variables. The chi-square test evaluates whether there is a significant association between the
#categories of the two variables.In other word Chi-Square test is a statistical method which used to determine if 
#two categorical variables have a significant correlation between them.


#The Chi-square test statistic can be used if the following conditions are satisfied:
#1. N, the total frequency, should be reasonably large, say greater than 50.
#2. The sample observations should be independent. This implies that no individual item should be included twice or more in the sample.
#3. No expected frequencies should be small. Small is a relative term. Preferably each expected frequencies should be larger than 10 but in any case not less than 5.

library(MASS)       # load the MASS package
#Problem:
#Test the hypothesis whether the Loan.Status is independent of the Term  at .05 significance level.
# Null hypothesis  Loan status is independent of Term of loan
#Solution
#We apply the chisq.test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <- table(df$Loan.Status,df$Term)
tbl
tbl <-tbl[2:3,2:3]   #tbl[-1,-1]           # the contingency table
tbl
chisq.test(tbl)
# or Mosaic plots provide a way to visualize contingency tables.A mosaic plot is a visual representation of the association between two variables.
library(vcd)
mosaic(tbl, shade=TRUE) 

#or
#Association Plots
assoc(tbl, shade=TRUE)
#Answer:
#As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
#the Loan.Status is independent of the Term and conclude that in our data, the Loan.Status and the Term are statistically significantly associated (p-value = 0)

#Q4.what is the distribution of Years.in.current.job and is there any relation between Years.in.current.job and target(Loan.Status)?
#levels(df$Years.in.current.job)
table(df$Years.in.current.job)
addmargins(xtabs(~ Years.in.current.job+Loan.Status,data=df))
prop.table(xtabs(~ Years.in.current.job+Loan.Status,data=df))

#Problem:
#Test the hypothesis whether the Loan.Status is independent of the Years.in.current.job  at .05 significance level.
# Null hypothesis  Loan status is independent of  Years.in.current.job


#Solution
#We apply the chisq.test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <-table(df$Loan.Status,df$Years.in.current.job)
tbl
tbl<-tbl [-1,-1]                # the contingency table
chisq.test(tbl)
#Answer:
#As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
#the Years.in.current.job is independent of the Term so there is association between loan status
# and years in current job at 5% significant level


#Q5.what is the distribution of Home.Ownership and is there any relation between Home.Ownership and target(Loan.Status)?
table(df$Home.Ownership)
df$Home.Ownership[df$Home.Ownership=="HaveMortgage"]<-"Home Mortgage"
addmargins(xtabs(~ Home.Ownership+Loan.Status,data=df))
prop.table(xtabs(~ Home.Ownership+Loan.Status,data=df))

#Problem:
#Test the hypothesis whether the Loan.Status is independent of the Home.Ownership at .05 significance level.
# Null hypothesis  Loan status is independent of  Home Ownership

#Solution
#We apply the chisq.test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <- table(df$Loan.Status,df$Home.Ownership)
tbl
tbl <-tbl[-1,-c(1,2)]
tbl# the contingency table
chisq.test(tbl)
#Answer:
#As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
#the Loan.Status is independent of the Home.Ownership
#so the Loan.Status and Home ownership are statistically significantly associated (p-value = 0)



#Feature Engineering 
#1)Find and Replace (for numeric variables some function created and applied)(For example converting Other to other or replacing 'HaveMortgage' with 'Home Mortgage') 
#2)segmentation
#3)Encoding
#3-1) Mapping with numbers for ordinal categorical variables
#3-2) One hot encoding (convert categorical variable into dummy): Create dummy variables from categorical variable that are nominal (not ordinal) categorical variable. It's also a good practice to drop the first one to avoid linear dependency between the resulted features.

#4)Create new columns or PCA (Principal Component Analysis)
#I will create RMI (ratio of Monthly Debt to Annual Income) and RMC (ratio of Monthly Debt to Maximum Open Credit) after cleaning data for test and train datasets

#5)Scaling: I used StandardScaler to scale my data.



#Q6.what is the distribution of Purpose and is there any relation between Purpose and target(Loan.Status)?
table(df$Purpose)
df$Purpose[df$Purpose=="Other"]<-"other"
table(df$Purpose)

addmargins(xtabs(~ Purpose+Loan.Status,data=df))
prop.table(xtabs(~ Purpose+Loan.Status,data=df))


tbl <-table(df$Loan.Status,df$Purpose)
tbl
tbl<-tbl[-1,-c(1,12)]                 # the contingency table
tbl
#what is the relation between Loan status and Purpose?

chisq.test(tbl)
#Chi-squared approximation may be incorrect because you have a group will small frequency
#Hint: when you have lots off levels in one column you can try segmentation

df$Purpose[df$Purpose=="renewable_energy"]<-"other"
tbl <-table(df$Loan.Status,df$Purpose)
tbl
tbl<-tbl[-1,-c(1,12,13)]                 # the contingency table
tbl

chisq.test(tbl)
#Answer:
#As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
#the Loan.Status is independent of the purpose of loan
#so the Loan Status and the purpose of loan are statistically significantly associated (p-value = 0)


#Q7.Is there any relation between Home.Ownership and Purpose?
addmargins(xtabs(~ Home.Ownership+Purpose,data=df))
prop.table(xtabs(~ Home.Ownership+Purpose,data=df))
tbl = table(df$Home.Ownership,df$Purpose)
tbl
tbl[-c(1,2),-c(1,12,13)]                 # the contingency table
tbl



#make a copy
df_orginal<-df
#df<-df_orginal

#credit score



#Q8.what is Credit.Score distribution?

summary(df$Credit.Score)

#histogram
hist(df$Credit.Score, breaks = 5, main = "Credit.Score",col="blue",xlab="Credit.Score",ylab="Frequency")


# Boxplot of Credit.Score by Loan.Status
boxplot(Credit.Score ~ Loan.Status,data=df, main="Credit.Score",
        xlab="Loan.Status", ylab="Credit.Score",col="blue")

summary(df["Credit.Score"])
#If you notice the maximum of credit scoe is 7510 which is strange considering the credit score are within the range of 300-850.
# let's try to find pattern of them if exist
dfc<-df[which(df["Credit.Score"]>850),]
dfc$Credit.Score
summary(dfc["Credit.Score"])
nrow(dfc)

#it looks like some of the credit score are just scaled up by 10. let me make sure
count<-0
for (val in dfc$Credit.Score){
  if (val%%10 !=0) {count=count+1}
}


count#--> count = 0 means all the credit scores greater than 850 are scaled up by 10 



df$Credit.Score<-ifelse(df$Credit.Score>850, df$Credit.Score/10, df$Credit.Score)

summary(df["Credit.Score"])

#Q9. Is there any relationship between Credit score and Loan Status?
#You can do same approach when you are doing A/B test
#For sure Group column is categorical column , if target is categorical ( for example buying the product or not) you should run chi-square test
#If target is continues (like order amount or time spend on web) you should run t-test for A/B test or ANOVA for A/B/C/. 

#Continouse Vs. Categorical  : For summaraization: group by categorical column an aggregate for numerical column
#                              For visualization: Grouped box plot,...
#                              For test of independence :1) if categorical column has only two levels :t-test
#                                                        2) if categorical column has more than two levels: ANOVA

agg1 <- aggregate(Credit.Score~ Loan.Status, df , mean)
agg1
names(agg1) <- c("Loan Status","mean of Credit Score")
agg1

agg2<- cbind(aggregate(Credit.Score ~ Loan.Status, df , min),
             aggregate(Credit.Score~ Loan.Status, df , max)[,2],
             aggregate(Credit.Score~ Loan.Status, df , mean)[,2])

names(agg2) <- c("Loan.Status","min_Credit.Score","max_Credit.Score","mean_Credit.Score")
agg2

library(ggplot2)
qplot(Loan.Status, Credit.Score, data = df, 
      geom="boxplot", fill = Loan.Status)
# Changing histogram plot fill colors by Loan.Status and usinging semi-transparent fill
p<-ggplot(df, aes(x=Credit.Score, fill=Loan.Status, color=Loan.Status)) +
  geom_histogram(position="identity", alpha=0.5)
p
# Add mean lines
library(plyr)
mu <- ddply(df, "Loan.Status", summarise, grp.mean=mean(Credit.Score,na.rm=T))
head(mu)
p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Loan.Status),
                linetype="dashed")
p

#Add density
p<-ggplot(df, aes(x=Credit.Score, fill=Loan.Status, color=Loan.Status)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.6)
p
# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Loan.Status),
             linetype="dashed")+ theme(legend.position="top")+labs(title="Credit Score histogram plot",x="Credit Score", y = "Density")




#-------------------------------------------
#t-test: 
#-------------------------------------------

#One of the most common tests in statistics is the t-test, used to determine whether the means of two groups are equal to each other. The assumption for the test is that both groups are sampled from normal distributions with equal variances
#The null hypothesis is that the two means are equal, and the alternative is that they are not. 
#There is also a widely used modification of the t-test, known as Welch's t-test that adjusts the number of degrees of freedom when the variances are thought not to be equal to each other.
#By default, R assumes that the variances of two groups are unequal, thus defaulting to Welch's test. To toggle this, we use the flag var.equal=TRUE.


#We are ready to test statistically whether these two samples have a different mean using the T-Test. To do so first, we have to define our Null and Alternate Hypothesis.
#Good:Fully Paid, Bad:Charged Off
#Null Hypothesis: µGood = µBad (the means of both populations are equal)
#Alternate Hypothesis: µGood ??? µBad (the means of both populations are not equal)
t.test(Credit.Score ~ Loan.Status, data=df )#default paired = FALSE, var.equal = FALSE
#But Before the results should be interpreted, the assumptions of the test should be checked.
#Assumption of independent sample t-test:
#1.the two samples are independent(here this assumption is true because each costumer belongs to only one group )
#2.no significant outliers in the two groups
#3.the two groups of samples (A and B), being compared, should be normally distributed.(Rely on the Central Limit Theorem if the sample size is large enough (n>30))
#4.the variances of the two groups should not be significantly different. This assumption is made only by the original Student's t-test. It is relaxed in the Welch's t-test.

#So there is a difference between mean of Fully Paid and Charged Off groups at 5% significant level

#https://statisticsbyjim.com/hypothesis-testing/t-tests-1-sample-2-sample-paired-t-tests/
#https://statisticsbyjim.com/anova/f-tests-anova/


#Segmentation for credit score
#credit scores from 300 to 560 are considered Poor, credit scores from 560 to 660 are considered Fair, credit scores from 660 to 724 are considered good; 725 to 759 are considered very good; and 760 and up are considered excellent.

##########################################################################################

#discritization

##########################################################################################
#make a copy
df_orginal2<-df
#df<-df_orginal2
#discritizing of Credit.Score
names(df)
#Method 1:
#cat1<-rep(NA,nrow(df))
#df<-cbind(df[,1:4],cat1,df[,5:17])
#df$cat1<-df$Credit.Score
#Method 2:
cat1<-df$Credit.Score
df<-cbind(df[,1:4],cat1,df[,5:17])
names(df)

df$cat1[300<=df$cat1 & df$cat1<=559]<-"Poor"
df$cat1[560<=df$cat1 & df$cat1<=659]<-"Fair"
df$cat1[660<=df$cat1 & df$cat1<=724]<-"Good"
df$cat1[725<=df$cat1 & df$cat1<=759]<-"Very Good"
df$cat1[760<=df$cat1 & df$cat1<=850]<-"Excellent"
names(df)[5]<-"Credit.Score.Status"
names(df)
head(df)


#Q10. What is  the credit score  status distribution and is there any association between loan status and credit score status at 5% significant level?
sum(is.na(df$Credit.Score.Status)) 
r1<-which(is.na(df$Credit.Score.Status))
r2<-which(is.na(df$Loan.Status))
dfcat<-df[-c(r1,r2),]

table(dfcat$Credit.Score.Status)


count<-table(dfcat$Credit.Score.Status)
count

# Pie Chart with Percentages
slices <- c(count[1], count[2], count[3])
lbls <- c("Fair", "Good", "Very Good ")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Credit Score Status")


add<-addmargins(xtabs(~ Credit.Score.Status+Loan.Status,data=dfcat))
add
add[,-1]
pro<-prop.table(xtabs(~ Credit.Score.Status+Loan.Status,data=df))
pro
pro[,-1]

#Problem:
#Test the hypothesis whether the Loan.Status is independent of the credit score status at .05 significance level.
# Null hypothesis  Loan status is independent of  credit score status 

#Solution
#We apply the chisq.test function to the contingency table tbl, and found the p-value to be 2.2e-16
tbl <- table(dfcat$Loan.Status,dfcat$Credit.Score.Status)
tbl
tbl <-tbl[-1,]
tbl# the contingency table
chisq.test(tbl)
#Answer:
#As the p-value 2.2e-16 is less than the .05 significance level, we reject the null hypothesis that
#the Loan.Status is independent of the Credit.Score.Status
#so the Loan.Status and Credit.Score.Status are statistically significantly associated (p-value = 0)
assoc(tbl, shade=TRUE)




#Q11.what is the distribution of Current.Loan.Amount and is there any relation between Current.Loan.Amount and target(Loan.Status)?

summary(df$Current.Loan.Amount)
df$Current.Loan.Amount
hist(df$Current.Loan.Amount, breaks = 5, main = "Current.Loan.Amount",col="blue")


# Boxplot of Current.Loan.Amount by Loan.Status
boxplot(Current.Loan.Amount ~ Loan.Status,data=df, main="Current.Loan.Amount",
        xlab="Loan.Status", ylab="Current.Loan.Amount",col="blue")

#it looks like instead of Na  for Current.Loan.Amount in some observation we have 99999999.Let me fix it first.
dfc2<-df[which(df["Current.Loan.Amount"]==99999999),]

nrow(dfc2)


df$Current.Loan.Amount<-ifelse(df$Current.Loan.Amount==99999999, NA, df$Current.Loan.Amount)

#Bivarate Analysis for cointinouse(Current.Loan.Amount ) Vs. categorical (Loan.Status)
# Visulization
summary(df$Current.Loan.Amount)
boxplot(Current.Loan.Amount ~ Loan.Status,data=df, main="Current.Loan.Amount",
        xlab="Loan.Status", ylab="Current.Loan.Amount",col="blue")

library(ggplot2)
qplot(Loan.Status, Current.Loan.Amount, data = df, 
      geom="boxplot", fill = Loan.Status)
# Changing histogram plot fill colors by Loan.Status and using semi-transparent fill
p<-ggplot(df, aes(x=Current.Loan.Amount, fill=Loan.Status, color=Loan.Status)) +
  geom_histogram(position="identity", alpha=0.5)
p
# Add mean lines
library(plyr)
mu <- ddply(df, "Loan.Status", summarise, grp.mean=mean(Current.Loan.Amount,na.rm=T))
head(mu)
p<-p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Loan.Status),
                linetype="dashed")
p
# Add density
p<-ggplot(df, aes(x=Current.Loan.Amount, fill=Loan.Status, color=Loan.Status)) +
  geom_histogram(aes(y=..density..),position="identity", alpha=0.5)+
  geom_density(alpha=0.6)
p
# Add mean lines and Change the legend position
p+geom_vline(data=mu, aes(xintercept=grp.mean, color=Loan.Status),
             linetype="dashed") + theme(legend.position="top")+labs(title="Current Loan Amount histogram plot",x="Credit Score", y = "Density")



#Summarization
names(df)
agg1 <- aggregate(Current.Loan.Amount ~ Loan.Status, df , mean)
agg1
names(agg1) <- c("Loan Status","mean Loan Amount")
agg1

agg2 <- cbind(aggregate(Current.Loan.Amount ~ Loan.Status, df , min),aggregate(Current.Loan.Amount ~ Loan.Status, df , max)[,2],aggregate(Current.Loan.Amount ~ Loan.Status, df , mean)[,2])

names(agg2) <- c("Loan.Status","min_Loan","max_Loan","mean_Loan")
agg2

#Test of independence 
#t-test
#Null Hypothesis: µGood = µBad (the means of both populations are equal)
#Alternate Hypothesis: µGood ??? µBad (the means of both populations are not equal)
t.test(Current.Loan.Amount ~ Loan.Status, data=df )#default paired = FALSE, var.equal = FALSE
# After checking Assumption you can say there is association between mean of Current loan amount and  Loan Status at 5% significant level


#Discritization of Current.Loan.Amount by using "discretize" function

#make a copy
df_org2<-df
#df<-df_org2
#install.packages("infotheo")
library(infotheo)
names(df)
#method1:
#cat<-rep(NA,nrow(df))
#df<-cbind(df[,1:2],cat,df[,3:18])
#x<-df[,2]
#x
# look at the distribution before discretizing
#hist(x, breaks = 5, main = "Current.Loan.Amount",col="blue")

#method2:
cat<-df$Current.Loan.Amount
df<-cbind(df[,1:2],cat,df[,3:18])
# look at the distribution before discretizing
hist(df$cat, breaks = 5, main = "Current.Loan.Amount",col="blue")

df$cat<-discretize(df$cat,disc="equalwidth",nbins=5)#you can discretize by using disc="equalfreq" 
table(df$cat)
summary(df$Current.Loan.Amount)
df$cat[df$cat==1]<-" Very Low"
df$cat[df$cat==2]<-"Low"
df$cat[df$cat==3]<-"Mid"
df$cat[df$cat==4]<-"High"
df$cat[df$cat==5]<-" Very High"
df$cat[df$cat==-2147483647]<-"NA"
names(df)
names(df)[3]<-"Loan.Amount.Levels"
names(df)
class(df$Loan.Amount.Levels)#discretize returns the discretized dataset
head(df$Loan.Amount.Levels)
#dim(df$Loan.Amount.Levels)
df$Loan.Amount.Levels<-df$Loan.Amount.Levels[,1]
class(df$Loan.Amount.Levels)

#Q12. What is the distrbution of Loan.Amount.Levels and is there any association between Loan.Amount.Levels and Loan status?

table(df$Loan.Amount.Levels)
#Bivarate analysis for Loan.Amount.Levels(categorical) Vs. Loan status( categorical)
# summarization Contingency table by using table function or xtabs function
tbl<-addmargins(xtabs(~ Loan.Status+Loan.Amount.Levels,data=df))
tbl
tbl[-1,]
tbl<-prop.table(xtabs(~ Loan.Status+Loan.Amount.Levels,data=df))
tbl
tbl[-1,]
#Test of independence: Chi_square test
#Problem:
#Test the hypothesis whether the Loan.Amount.Levels is independent of Loan Status  at .05 significance level.

tbl = table(df$Loan.Amount.Levels,df$Loan.Status)
tbl   # the contingency table
tbl<-tbl[,-1]
tbl
chisq.test(tbl)

#So there is association between Loan.Amount.Levels and Loan.Status at 5% significant level

#visualization:
# Stacked Bar Plot with Colors and Legend
counts <- table(df$Loan.Amount.Levels, df$Loan.Status)[,-1]#you need 2 columns
barplot(counts, main="Loan.Amount.Levels and Loan.Status",
        xlab="Loan.Amount.Levels", col=c("red","green","darkblue","yellow","lightblue","orange"),
        legend = rownames(counts))




#Q13: Is there any association between Credit.Score.Status and Loan.Amount.Levels

#Summarization:Contingency table

addmargins(xtabs(~ Credit.Score.Status+Loan.Amount.Levels,data=df))
prop.table(xtabs(~ Credit.Score.Status+Loan.Amount.Levels,data=df))

counts <- table(df$Credit.Score.Status, df$Loan.Amount.Levels)#you need 2 columns
counts

counts<-counts[,-6]#If you want to see NAs ignore this line of code
counts
#Visualization
# Stacked Bar Plot with Colors and Legend
barplot(counts, main="Credit.Score.Status and Loan.Amount.Levels",
        xlab="Credit.Score.Status", col=c("darkblue","red","green"),
        legend = rownames(counts))

# Grouped Bar Plot

barplot(counts, main="Credit.Score.Status and Loan.Amount.Levels",
        xlab="Credit.Score.Status", col=c("darkblue","red","green"),
        legend = rownames(counts), beside=TRUE)

#Test of independence: Chi_square test
#Problem:
#Test the hypothesis whether the Loan.Amount.Levels is independent of the Credit.Score  at .05 significance level.

tbl = table(df$Loan.Amount.Levels,df$Credit.Score.Status)
tbl   # the contingency table
chisq.test(tbl)
#So there is association between Loan.Amount.Levels and Loan status at 5% significant level

tbl<-tbl[-6,]
tbl
chisq.test(tbl)


##########################################################################################
#""" Data Cleansing"""
##########################################################################################
#1.Handling duplicate observation
#2. Handling Missing Values
#3.handling Outliers
#make a copy
df_orginal3<-df
#df<-df_orginal3
#save the new dataset to .csv for use in later 
write.csv(df, file = "loan_prediction_before_data_cleaning.csv",row.names = FALSE)
#df<-read.csv("loan_prediction_before_data_cleaning.csv")
names(df)
#dropping 2 categorical columns that I created in discritization step if you want you can remove them later
#df<-df[,-c(3,6)]


##########################################################################################
#Handling Duplicate Data
#########################################################################################

#I drpod duplicated data at the beginning

#One common discussion is that should I handle outliers first or Missing values first 
#By following my approach that I handle them parallel you don't need to worry about it
###########################################################################################
# Handling Outliers
##########################################################################################
# it is better to try find a pattern for outliers if exists like what I did for credit score and for Current loan amount
#For findig pattern of outliers and domain of possible values you should try to undrstand business
#After that you should try to understand having these oultliers is possible or not
# I don't recommend you to drop outliers because this is the chance to have outliers in your future data (test) that you want to predict for it
# What ever you are doing for train data set that you make your model based on it you should d exactly the same for test data
#That's why it is a good approach to combine test and train and clean them together but
#be careful you are not allowed to remove rows from your test data set


# For those columns that I didn't find any pattern for oultliers we can replace them with
#NA and then Handle missing values or you can replace with extreme axceptable values
#In real word it is better to try both and compare your model

#Finding outliers by univariate analysis for numeric columns

#Method 1: Mean and Standard Deviation Method(Univariate analysis)
#For this outlier detection method, the mean and standard deviation of the residuals are 
#calculated and compared.If a value is a certain number of standard deviations away from the 
#mean, that data point is identified as an outlier. The specified number of standard deviations
#is called the threshold. The default value is 3.
#CHEBYCHEV's rule:At least 89% of the data lie within three standard deviations to either side of the mean, that is,  
#between mean - 3s  and   mean + 3s
#If your data is normal based on z-distribution almost 99% of our data are between mean - 3s  and   mean + 3s


str(df)
class(df)

#numeric variables

#Since a data frame is a list we can use the list-apply functions:
nums<-sapply(df, is.numeric)
nums
#or
#nums <- unlist(lapply(df, is.numeric)) 
#Then standard subsetting
df_num=df[ , nums]

names(df_num)
head(df_num)

df_before_removing_outliers<-df

#creating a function to convert outliers based on Mean and Standard Deviation Method to NA
notout<-function(x){
  print("summary before applying this method ")
  print(summary(x))  
  M1<-mean(x,na.rm = TRUE)
  S1<-sd(x,na.rm=TRUE)
  low1<-M1-3*S1
  up1<-M1+3*S1
  x[x<low1]<-NA #Method 2:x[x<low1]<-low1
  x[x>up1]<-NA  #mwthod 2:x[x>up1]<-up1 
  print("summary after applying this method ")
  print(summary(x)) 
  return(x)
}

summary(df)
df$Current.Loan.Amount<-notout(df$Current.Loan.Amount)
#-->There is no (11484 NA's -11484 NA's) Outlier in Current.Loan.Amount column based on Mean and Standard Deviation Method


#notout(df$Credit.Score)
#-->There are 1205 (20359 NA's -19154 NA's )Outliers in Credit.Score column based on Mean and Standard Deviation Method
#I didn't repalce outliers in Credit score because I want to train a model based on them

df$Annual.Income<-notout(df$Annual.Income)
#-->There are 635 Outliers in Annual.Income column based on Mean and Standard Deviation Method

df$Monthly.Debt<-notout(df$Monthly.Debt)
#-->There are 1238 Outliers in Monthly.Debt column based on Mean and Standard Deviation Method


#notout(df$Years.of.Credit.History)
#-->There is 1087 Outliers in Years.of.Credit.History column based on Mean and Standard Deviation Method



#notout(df$Months.since.last.delinquent)
#-->There are 20 Outliers in Months.since.last.delinquent column based on Mean and Standard Deviation Method

#notout(df$Number.of.Open.Accounts)
#-->There are 926 Outlier in Number.of.Open.Accounts column based on Mean and Standard Deviation Method


#notout(df$Number.of.Credit.Problems)
#-->There are 1654 Outliers in Number.of.Credit.Problems column based on Mean and Standard Deviation Method

df$Current.Credit.Balance<-notout(df$Current.Credit.Balance)
#-->There are 1076 Outliers in Current.Credit.Balance column based on Mean and Standard Deviation Method


df$Maximum.Open.Credit<-notout(df$Maximum.Open.Credit)
#-->There are  55 Outliers in Maximum.Open.Credit column based on Mean and Standard Deviation Method


#notout(df$Bankruptcies)
#-->There are 482 Outliers in Bankruptcies column based on Mean and Standard Deviation Method

#notout(df$Tax.Liens)
#-->There are 1696 Outliers in Tax.Liens column based on Mean and Standard Deviation Method
