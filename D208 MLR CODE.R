library(tidyverse)
library(readr)
library(dplyr)
library(assertthat)
library(openxlsx)
library(ggplot2)
library(gridExtra)
churnData <- read.csv(file = "C:/Users/chris/Google Drive/D208 Predictive Modeling/Given Data and Scenerio/churn_clean.csv",header=TRUE,sep=",",stringsAsFactors = TRUE)
str(churnData)
summary(churnData)
#removed demographic columns
churnData=subset(churnData,select= -c(CaseOrder,Customer_id,Interaction,UID,City,State,County,Zip,Lat,Lng,Population,Area,TimeZone))
#checking demographic columns are deleted from the data
summary(churnData)
#removed additional columns
churnData=subset(churnData,select= -c(Job,Marital,PaymentMethod,Outage_sec_perweek,Email,Contacts,Yearly_equip_failure))
#checking additional columns are deleted from the data
summary(churnData)
#churnData$TotalCharges[is.na(churnData$TotalCharges)] <-1 
#omitting any missing data 
churnData = na.omit(churnData)
#removing any missing values
churnData <- churnData[complete.cases(churnData), ]
#churnData 
glimpse(churnData)
#renaming the Item columns and bandwidth gb per year to bandwidth
names(churnData)[names(churnData) == 'Item1'] <- 'TimelyResponse'
names(churnData)[names(churnData) == 'Item2'] <- 'TimelyFixes'
names(churnData)[names(churnData) == 'Item3'] <- 'TimelyReplacements'
names(churnData)[names(churnData) == 'Item4'] <- 'Reliability'
names(churnData)[names(churnData) == 'Item5'] <- 'Options'
names(churnData)[names(churnData) == 'Item6'] <- 'RespectfulResponse'
names(churnData)[names(churnData) == 'Item7'] <- 'CourteousExchange'
names(churnData)[names(churnData) == 'Item8'] <- 'ActiveListening'
names(churnData)[names(churnData) == 'Bandwidth_GB_Year'] <- 'Bandwidth'
names(churnData)[names(churnData) == 'Port_modem'] <- 'PortModem'
summary(churnData)

#recoding Gender to ints 1 = Male, 2 = Female, 3 = Nonbinary unknown = 0
churnData$Gender <- ifelse(churnData$Gender == "Male", 1, ifelse(churnData$Gender == "Female", 2,ifelse(churnData$Gender == "Nonbinary",3,0)))
#checking Gender is now int
summary(churnData$Gender)
#recoding Churn to bits yes = 1 and no/unknown = 0
churnData$Churn <- ifelse(churnData$Churn == "No", 0, ifelse(churnData$Churn == "Yes", 1,0))
#checking Churn is now a bit
summary(churnData$Churn)
#recoding Techie to bits yes = 1 and no/unknown = 0
churnData$Techie <- ifelse(churnData$Techie == "No", 0, ifelse(churnData$Techie == "Yes", 1,0))
#checking Techie is now a bit
summary(churnData$Techie)
#recoding Contract month-to-month = 3, one year = 1, two year = 2, unknown = 0
churnData$Contract <- ifelse(churnData$Contract == "Month-to-Month", 3, ifelse(churnData$Contract == "One Year", 1,ifelse(churnData$Contract == "Two Year",2,0)))
#checking Contract is now a int
summary(churnData$Contract)
#recoding Port Modem to bits yes = 1 and no/unknown = 0
churnData$PortModem <- ifelse(churnData$PortModem == "No", 0, ifelse(churnData$PortModem == "Yes", 1,0))
#checking Port Modem is now a bit
summary(churnData$PortModem)
#recoding Tabket to bits yes = 1 and no/unknown = 0
churnData$Tablet <- ifelse(churnData$Tablet == "No", 0, ifelse(churnData$Tablet == "Yes", 1,0))
#checking Tablet is now a bit
summary(churnData$Tablet)
#recoding Internet Services DSL = 3, Fiber Optic = 2, None = 1,unknown = 0
churnData$InternetService <- ifelse(churnData$InternetService == "DSL", 3, ifelse(churnData$InternetService == "Fiber Optic", 2,ifelse(churnData$InternetService == "None",1,0)))
#checking Internet Services is now a int
summary(churnData$InternetService)
#recoding Phone to bits yes = 1 and no/unknown = 0
churnData$Phone <- ifelse(churnData$Phone == "No", 0, ifelse(churnData$Phone == "Yes", 1,0))
#checking Phone is now a bit
summary(churnData$Phone)
#recoding Multiple bits yes = 1 and no/unknown = 0
churnData$Multiple <- ifelse(churnData$Multiple == "No", 0, ifelse(churnData$Multiple == "Yes", 1,0))
#checking Multiple is now a bit
summary(churnData$Multiple)
#recoding Online Secruity bits yes = 1 and no/unknown = 0
churnData$OnlineSecurity <- ifelse(churnData$OnlineSecurity == "No", 0, ifelse(churnData$OnlineSecurity == "Yes", 1,0))
#checking Online Security is now a bit
summary(churnData$OnlineSecurity)
#recoding Online Backup bits yes = 1 and no/unknown = 0
churnData$OnlineBackup <- ifelse(churnData$OnlineBackup == "No", 0, ifelse(churnData$OnlineBackup == "Yes", 1,0))
#checking Online Backup is now a bit
summary(churnData$OnlineBackup)
#recoding Device Protection bits yes = 1 and no/unknown = 0
churnData$DeviceProtection <- ifelse(churnData$DeviceProtection == "No", 0, ifelse(churnData$DeviceProtection == "Yes", 1,0))
#checking Tech Support is now a bit
summary(churnData$DeviceProtection)
#recoding TechSupport bits yes = 1 and no/unknown = 0
churnData$TechSupport <- ifelse(churnData$TechSupport == "No", 0, ifelse(churnData$TechSupport == "Yes", 1,0))
#checking TechSupport is now a bit
summary(churnData$TechSupport)
#recoding StreamingTV bits yes = 1 and no/unknown = 0
churnData$StreamingTV <- ifelse(churnData$StreamingTV == "No", 0, ifelse(churnData$StreamingTV == "Yes", 1,0))
#checking StreamingTV is now a bit
summary(churnData$StreamingTV)
#recoding StreamingMovies bits yes = 1 and no/unknown = 0
churnData$StreamingMovies <- ifelse(churnData$StreamingMovies == "No", 0, ifelse(churnData$StreamingMovies == "Yes", 1,0))
#checking StreamingMovies is now a bit
summary(churnData$StreamingMovies)
#recoding PaperlessBilling bits yes = 1 and no/unknown = 0
churnData$PaperlessBilling <- ifelse(churnData$PaperlessBilling == "No", 0, ifelse(churnData$PaperlessBilling == "Yes", 1,0))
#checking PaperlessBilling is now a bit
summary(churnData$PaperlessBilling)
summary(churnData)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(churnData))

## set the seed to make your partition reproducible
set.seed(101)
train_level <- sample(seq_len(nrow(churnData)), size = smp_size)

train <- churnData[train_level, ]
test <- churnData[-train_level, ]
glimpse(train)
glimpse(test)
#Exporting all three data sets to xlsx
write.xlsx(churnData, "C:/Users/chris/Google Drive/D208 Predictive Modeling/Given Data and Scenerio/Cleaned Data Set.xlsx")
write.xlsx(train, "C:/Users/chris/Google Drive/D208 Predictive Modeling/Given Data and Scenerio/Train Data set.xlsx")
write.xlsx(test, "C:/Users/chris/Google Drive/D208 Predictive Modeling/Given Data and Scenerio/Test Data Set.xlsx")
#univariate Statistic
childrenGraph <- ggplot(churnData, aes(Children)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Children Distribution",
       x = "Children",
       y = "Count")
AgeGraph <- ggplot(churnData, aes(Age)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Age Distribution",
       x = "Age",
       y = "Count")
IncomeGraph <- ggplot(churnData, aes(Income)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Income Distribution",
       x = "Income",
       y = "Count")
GenderGraph <- ggplot(churnData, aes(Gender)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Gender Distribution",
       x = "Gender",
       y = "Count")
ChurnrGraph <- ggplot(churnData, aes(Churn)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Churn Distribution",
       x = "Churn",
       y = "Count")

TechieGraph <- ggplot(churnData, aes(Techie)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Techie Distribution",
       x = "Techie",
       y = "Count")
ContractGraph <- ggplot(churnData, aes(Contract)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Contract Distribution",
       x = "Contract",
       y = "Count")
PortModemGraph <- ggplot(churnData, aes(PortModem)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Port Modem Distribution",
       x = "Port Modem",
       y = "Count")
TabletGraph <- ggplot(churnData, aes(Tablet)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Tablet Distribution",
       x = "Tablet",
       y = "Count")
InternetServiceGraph <- ggplot(churnData, aes(InternetService)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Internet Service Distribution",
       x = "Internet Service",
       y = "Count")
PhoneGraph <- ggplot(churnData, aes(Phone)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Phone Distribution",
       x = "Phone",
       y = "Count")
MultipleGraph <- ggplot(churnData, aes(Multiple)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Multiple Distribution",
       x = "Multiple",
       y = "Count")
OnlineSecurityGraph <- ggplot(churnData, aes(OnlineSecurity)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Online Security  Distribution",
       x = "Online Security",
       y = "Count")
OnlineBackupGraph <- ggplot(churnData, aes(OnlineBackup)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Online Backup Distribution",
       x = "Online Backup",
       y = "Count")
DeviceProtectionGraph <- ggplot(churnData, aes(DeviceProtection)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer device Protection Distribution",
       x = "Device Protection",
       y = "Count")
TechSupportGraph <- ggplot(churnData, aes(TechSupport)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Tech Support Distribution",
       x = "Tech Support",
       y = "Count")
StreamingTVGraph <- ggplot(churnData, aes(StreamingTV)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Streaming TV Distribution",
       x = "Streaming TV",
       y = "Count")
StreamingMoviesGraph <- ggplot(churnData, aes(StreamingMovies)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Streaming Movies Distribution",
       x = "Streaming Movies",
       y = "Count")
PaperlessBillingGraph <- ggplot(churnData, aes(PaperlessBilling)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Paperless Billing Distribution",
       x = "Paperless Billing",
       y = "Count")
TenureGraph <- ggplot(churnData, aes(Tenure)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Tenure Distribution",
       x = "Tenure",
       y = "Count")
MonthlyChargegrpah<- ggplot(churnData, aes(MonthlyCharge)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Mostly Charge Distribution",
       x = "Monthly Charge",
       y = "Count")
BandwidthGBYearGraph <- ggplot(churnData, aes(Bandwidth)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Bandwidth GB Per Year Distribution",
       x = "Bandwidth GB Year",
       y = "Count")
TimelyResponseGraph <- ggplot(churnData, aes(TimelyResponse)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Timely Responses Distribution",
       x = "Timley Resposne",
       y = "Count")
TimelyFixesGraph <- ggplot(churnData, aes(TimelyFixes)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Timely Fixes Distribution",
       x = "Timely Fixes",
       y = "Count")
TimelyReplacementsGraph <- ggplot(churnData, aes(TimelyReplacements))+ 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Timely Replacements Distribution",
       x = "Timley Replacement",
       y = "Count")
ReliabilityGraph <- ggplot(churnData, aes(Reliability)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Reliability Distribution",
       x = "Reliability",
       y = "Count")
OptionsGraph <- ggplot(churnData, aes(Options)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Options Distribution",
       x = "Options",
       y = "Count")
RespectfulResponseGraph <- ggplot(churnData, aes(RespectfulResponse)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Respectful Response Distribution",
       x = "Respectful Response",
       y = "Count")
CourteousExchangeGraph <- ggplot(churnData, aes(CourteousExchange)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Courteous Exchange Distribution",
       x = "Courteous Exchange",
       y = "Count")
ActiveListeningGraph <- ggplot(churnData, aes(ActiveListening)) + 
  geom_histogram(bins = sqrt(nrow(churnData))) +
  labs(title = "Customer Active Listening Distribution",
       x = "Active Listening",
       y = "Count")
grid.arrange(childrenGraph, AgeGraph, IncomeGraph, GenderGraph,ncol=2)
grid.arrange(ChurnrGraph, TechieGraph, ContractGraph, PortModemGraph,ncol=2)
grid.arrange(TabletGraph, InternetServiceGraph, PhoneGraph, MultipleGraph,ncol=2)
grid.arrange(OnlineSecurityGraph, OnlineBackupGraph, DeviceProtectionGraph, TechSupportGraph,ncol=2)
grid.arrange(StreamingTVGraph,StreamingMoviesGraph,PaperlessBillingGraph, TenureGraph,ncol=2)
grid.arrange(MonthlyChargegrpah, BandwidthGBYearGraph, TimelyResponseGraph, TimelyFixesGraph,ncol=2)
grid.arrange(TimelyReplacementsGraph, ReliabilityGraph, OptionsGraph, RespectfulResponseGraph,ncol=2)
grid.arrange(CourteousExchangeGraph,ActiveListeningGraph,ncol=2)

#Graphs for bivariate data

childrenBV <- ggplot(churnData, aes(x =Children, y = Bandwidth)) +
  geom_point()
AgeBV <- ggplot(churnData, aes(x =Age, y = Bandwidth)) +
  geom_point()

IncomeBV <- ggplot(churnData, aes(x =Income, y = Bandwidth)) +
  geom_point()

GenderBV <- ggplot(churnData, aes(x =Gender, y = Bandwidth)) +
  geom_point()

churnBV <- ggplot(churnData, aes(x =Churn, y = Bandwidth)) +
  geom_point()

TechieBV <- ggplot(churnData, aes(x =Techie, y = Bandwidth)) +
  geom_point()

contractBV <- ggplot(churnData, aes(x =Contract, y = Bandwidth)) +
  geom_point()

PortModemBV <- ggplot(churnData, aes(x =PortModem, y = Bandwidth)) +
  geom_point()

TabletBV <- ggplot(churnData, aes(x =Tablet, y = Bandwidth)) +
  geom_point()

InternetServiceBV <- ggplot(churnData, aes(x =InternetService, y = Bandwidth)) +
  geom_point()

PhoneBV <- ggplot(churnData, aes(x =Phone, y = Bandwidth)) +
  geom_point()

MultipleBV <- ggplot(churnData, aes(x =Multiple, y = Bandwidth)) +
  geom_point()

OnlineSecurityBV <- ggplot(churnData, aes(x =OnlineSecurity, y = Bandwidth)) +
  geom_point()

OnlineBackupBV <- ggplot(churnData, aes(x =OnlineBackup, y = Bandwidth)) +
  geom_point()

DeviceProtectionBV <- ggplot(churnData, aes(x =DeviceProtection, y = Bandwidth)) +
  geom_point()

TechSupportBV <- ggplot(churnData, aes(x =TechSupport, y = Bandwidth)) +
  geom_point()

StreamingTVBV <- ggplot(churnData, aes(x =StreamingTV, y = Bandwidth)) +
  geom_point()

StreamingMoviesBV <- ggplot(churnData, aes(x =StreamingMovies, y = Bandwidth)) +
  geom_point()


PaperlessBillingBV <- ggplot(churnData, aes(x =PaperlessBilling, y = Bandwidth)) +
  geom_point()

TenureBV <- ggplot(churnData, aes(x =Tenure, y = Bandwidth)) +
  geom_point()

MonthlyChargesBV <- ggplot(churnData, aes(x =MonthlyCharge, y = Bandwidth)) +
  geom_point()

TimelyResponseBV <- ggplot(churnData, aes(x =TimelyResponse, y = Bandwidth)) +
  geom_point()

TimelyFixesBV <- ggplot(churnData, aes(x =TimelyFixes, y = Bandwidth)) +
  geom_point()

TimelyReplacementsBV <- ggplot(churnData, aes(x =TimelyReplacements, y = Bandwidth)) +
  geom_point()

ReliabilityBV <- ggplot(churnData, aes(x =Reliability, y = Bandwidth)) +
  geom_point()

OptionsBV <- ggplot(churnData, aes(x =Options, y = Bandwidth)) +
  geom_point()

RespectfulResponseBV <- ggplot(churnData, aes(x =RespectfulResponse, y = Bandwidth)) +
  geom_point()

CourteousExchangeBV <- ggplot(churnData, aes(x =CourteousExchange, y = Bandwidth)) +
  geom_point()

ActiveListeningBV <- ggplot(churnData, aes(x =ActiveListening, y = Bandwidth)) +
  geom_point()
grid.arrange(childrenBV, AgeBV, IncomeBV, GenderBV,ncol=2)
grid.arrange(churnBV, TechieBV, contractBV, PortModemBV,ncol=2)
grid.arrange(TabletBV, InternetServiceBV, PhoneBV, MultipleBV,ncol=2)
grid.arrange(OnlineSecurityBV, OnlineBackupBV, DeviceProtectionBV, TechSupportBV,ncol=2)
grid.arrange(StreamingTVBV, StreamingMoviesBV, PaperlessBillingBV, TenureBV,ncol=2)
grid.arrange(MonthlyChargesBV, TimelyResponseBV, TimelyFixesBV, TimelyReplacementsBV,ncol=2)
grid.arrange(ReliabilityBV, OptionsBV, RespectfulResponseBV, CourteousExchangeBV,ActiveListeningBV,ncol=2)
library(FactoMineR)
library(corrplot)
matrixCor <- cor(train)
corrplot(matrixCor, method="number")
res = RegBest(y=train[,6],x=train[,-6])
res
res$summary
res$best
#liner model all variables
lmAll <- lm(formula = Bandwidth ~., data=train)
print(lmAll)
summary(lmAll)
plot(lmAll)
#linear Model best fit 
lmBest <- lm(Bandwidth~Churn+StreamingMovies+Tenure,data=train)
print(lmBest)
summary(lmBest)
plot(lmBest)