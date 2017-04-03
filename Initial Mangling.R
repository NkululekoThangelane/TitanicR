
#Import Libraries
suppressPackageStartupMessages(library(ggplot2)) # For Plotting 
suppressPackageStartupMessages(library(xgboost)) # Increased computation engine
suppressPackageStartupMessages(library(data.table)) #Deals with data as tables
suppressPackageStartupMessages(library(Matrix))
suppressPackageStartupMessages(library(dplyr)) # Mangle Data FRames
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(party))
suppressPackageStartupMessages(library(randomForest)) # Run Random Forest Classication
suppressPackageStartupMessages(library(rpart))


set.seed(333333)
#Read the csv Files
train <- read.csv('train.csv', stringsAsFactors = F)
test  <- read.csv('test.csv', stringsAsFactors = F)

# For Data Exploration we will combine the two files just for completeness
# dont do this in final training
total  <- bind_rows(train, test)  # Bind rows joins two dataframes together 

#The Titles for all passengers in contained in the name field
#It may be useful to store the title of the passesger as this may give us an indication of a persons social standing
#which may also show that they were important thus important people are priorotised in a disater
#This is a theory for now so we shall have to see in the model.
total$Title <- gsub('(.*, )|(\\..*)', '', total$Name)  # Regular expression that extracts the title from a name and stores the name in a title column
# Get counts of titles
titleCount<-table(total$Title)
#Simple plot of the results 
barplot(titleCount)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
               'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')


#Count the title again and plot a bar plot of the title  again
titleCount<-table(total$Title)
barplot(titleCount)
table(total$Title)

#Now lets count Total Amount of people the embarked from different ports
#The empyt ones will be changes assumed the Embarked im Southampton hence the S
total$Embarked <- ifelse(total$Embarked == "", "S", total$Embarked)


#We shall also get the surname also from the name column so that we have seperate surname columns
#Simplly applys a function to the name Column that strips out the Surname as the surname is written first before the ","
total$Surname <- sapply(total$Name,  
                        function(x) strsplit(x, split = '[,.]')[[1]][1])

#simple table count of the surnames
table(total$Surname)

#WE can also view family size by looking parch and sibsp
# The assumtion here could be you act differently in a disaster if you have a large family and people to think about 
total$Fsize <- total$SibSp + total$Parch


# Create a family variable 
total$Family <- paste(total$Surname, total$Fsize, sep='_')


# Discretize family size
total$FsizeD[total$Fsize == 1] <- 'NoDep'
total$FsizeD[total$Fsize < 5 & total$Fsize > 1] <- 'mDep'
total$FsizeD[total$Fsize >= 5] <- 'MDep'

#The CAbin Type tells us what type of cabin is the passenger in
#the vabin type is in the cabin number
#A cabin number starts with a letter and the Letter is the cabin type 
#These if statements are checking the first character in the string if is A then Cabin A etc
total$CabinType <- ifelse(substring(total$Cabin, 1, 1) == 'A', 'A', 
                          ifelse(substring(total$Cabin, 1, 1) == 'B', 'B', 
                                 ifelse(substring(total$Cabin, 1, 1) == 'C', 'C', 
                                        ifelse(substring(total$Cabin, 1, 1) == 'D', 'D',  
                                               ifelse(substring(total$Cabin, 1, 1) == 'E', 'E', 
                                                      ifelse(substring(total$Cabin, 1, 1) == 'F', 'F',      
                                                             'zzz'))))))
#Missing Ages in the data
#Get a subset data set of all people with missing ages
MissingAge <- total[is.na(total$Age), ]

#Get the average age of people with the title Master
MasterMeanAge <- mean(total[which(total$Title == 'Master'), ]$Age, na.rm = TRUE)

#Get Mean of Master and SEx Male
MasterMeanAgeAdltmale <- mean(total[which(total$Title != 'Master' & total$Sex == 'male'), ]$Age, na.rm = TRUE)


#Get mean age of females  with out title master
MasterMeanAgeAdltfemale <- mean(total[which(total$Title != 'Master' & total$Sex == 'female'), ]$Age, na.rm = TRUE)
summary(MissingAge$Age)

#Replace with the calcuslations
MissingAge$Age <- ifelse(MissingAge$Title != "Master" & MissingAge$Sex == "male", MasterMeanAgeAdltmale,
                         ifelse(MissingAge$Title != "Master" & MissingAge$Sex == "female", MasterMeanAgeAdltfemale,
                                MasterMeanAge))

#Merge all the data
# Merge the data
CleanTotal <- merge(total[ ,c(1:ncol(total))] , MissingAge[ ,c(1, 6)], by = "PassengerId", all.x = TRUE)
#Check both ages age columns for missing age calculations
CleanTotal$Age <- ifelse(is.na(CleanTotal$Age.x), CleanTotal$Age.y, CleanTotal$Age.x)

#Create adult Columns to see if a person is an adult.
#if a person is greater 15 then they are an adult
CleanTotal$Adult <- as.factor(ifelse(CleanTotal$Age > 15, 1, 0)) 

#Replace the missing value in the fare for one passenger with the average for the fair
CleanTotal[which(CleanTotal$PassengerId == 1044), ]$Fare <- median(CleanTotal$Fare, na.rm=TRUE) 


#Fare differ squiet alot so we shall take the log of the fare
CleanTotal$lFare <- log(CleanTotal$Fare + .01)  

#Create a colum for the fare range
CleanTotal$lFareRange <- ifelse(CleanTotal$lFare > 3.344, "High",
                                ifelse(CleanTotal$lFare > 2.851, "Med-High",
                                       ifelse(CleanTotal$lFare > 2.068, "Med", "Low"
                                       )))

#Remove the two unused age fiels
CleanTotal = CleanTotal[ , !names(CleanTotal) %in% c("Age.x","Age.y")]


#PCLASS as factor
CleanTotal$Pclass <- as.factor(CleanTotal$Pclass)