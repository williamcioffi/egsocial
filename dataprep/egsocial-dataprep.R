#######################################
##                                   ##
##   Right Whale Social Analysis     ##
##                                   ##
##          -data prep-              ##
##                                   ##
##   Prep the data recieved from     ## 
##   North Atlantic Right Whale      ##
##   Consortium and get it ready     ##
##   for analysis by tidying it up   ##
##   and assigning agesex classes    ##
##                                   ##
##         Christin Khan             ##
##         November 2017             ##
#######################################


#prep Excel file

   #open data and Save As in Excel file with a new name
   #delete 'Calving Data' worksheet
   #delete 'Code Explanation' worksheet
   #delete top row header that says 'All sightings of identified whales from 1980 through 2016'
   #highlight all and remove shading and borders   
   #highlight top row and replace all spaces with nothing
   #highlight all and replace all spaces with a period
   #highlight all and replace all commas with a period
   #highlight all and replace all double periods with a single period
   #highlight all and replace all blanks with 'NA' values
   #delete column "Association Id"
   #delete column "Association Type"
   #delete column "Singleton"
   #delete column "Observer"
   #delete column "Area Code"
   #delete column "Letter"
   #delete any whales (EGNo) of unknown gender 'X'
   #moved "Latitude" and "Longitude" columns over to the left, and formatted to 5 decimal places
   #insert a RID column for row names
   #save
   #save as a .txt file


######################################

#empty environment before starting

rm(list=ls())

####################################

#load needed libraries 

library(data.table)
library(chron)
library(lubridate)
library(xlsx)


#####################################

#check that you have needed pacakges

require(data.table)
require(chron)
require(lubridate)
require(xlsx)

####################################

#set working directory

# setwd("C:/Users/christin.khan/Documents/Projects/Social-Behavior/2-Data")

#####################################

#load your .txt file into R (which specifies that data has a column header but not row names):

data <- read.table("3-Khan-data-calf-errors-fixed.csv", header = TRUE, sep = ',')

####################################

#look at the first few rows of data to see if it read in correctly

head(data)

###################################

#check out the dimensions of the data frame

dim(data)

###################################

#check out the structure of the data frame

str(data)

###################################

# convert your data frame into a data table
setDT(data)

##################################

#create new columns to assist in defining age class

data$yearminusbirthyear<- (data$Year - data$BirthYear)               #year minus birth year
data$ageknown<-"unknown"                                             #column 'ageknown' with "unknown" values for now
data$yearminusfirstsight<- (data$Year - data$FirstYearSighted)       #year minus first year sighted
data$yearminusfirstcalving<- (data$Year - data$FirstCalvingYear)     #year minus first calving year
data$agemin<-"unknown"                                               #column 'agemin' with "unknown" values for now
data$ageclass<-"unknown"                                             #column 'ageclass' with "unknown" values for now

# check to see if your columns were added

head(data)

##################################
##################################
#   Now assign age-sex classes   #
##################################
##################################

#Calves were excluded from the analysis
#considered calves until December 1st of their birth year
#and thereafter, juveniles. 
#Individuals were considered adults when: 
   #1) nine years or more from known birth year
   #2) eight years or more from date of first sighting when birth year was unknown
   #3) January 1st of the year prior to giving birth to a calf. 
#Whales without a known birth year that had been seen for less than eight years were excluded, as age class was unknown. 
#Females were considered lactating from their first sighting with a dependent calf until December 1st of the calving year (the same time that the calf would be classified as a juvenile). There were 15 cases in which the calf was lost before the normal weaning period; the mother was considered lactating until the last sighting with her calf, and as non-lactating thereafter. 


#set 'agemin' based on time between 'FirstYearSighted' and 'Year' 

setDT(data)[Year - FirstYearSighted >=9, agemin := "A"]        #greater than or equal to 9 = adult
(data)[Year - FirstYearSighted <9, agemin := "U"]              #less than 9 = unknown

#set 'ageknown' based on time between 'BirthYear' and 'Year

setDT(data)[Year - BirthYear >=9, ageknown := "A"]                         #greater than or equal to 9 = adult
(data)[Year - BirthYear <0, ageknown := "C"]                               #calves born Nov or Dec year before "BirthYear"
(data)[(Year == BirthYear) & Month < 12, ageknown := "C"]                  #calves born in "BirthYear" seen Jan-Nov
(data)[(Year == BirthYear) & Month == 12, ageknown := "J"]                 #calves become juveniles on December 1 of  "BirthYear"
(data)[yearminusbirthyear >=1 & yearminusbirthyear <9, ageknown := "J"]    #if Year - BirthYear is between 1 and 8  --> juvenile
(data)[is.na(data$BirthYear), ageknown := "U"]                             #if BirthYear is missing, ageknown is unknown

#make a table and open it to look at results

agecalcs <- data[, .(Month, Year, BirthYear, yearminusbirthyear, ageknown, FirstYearSighted, yearminusfirstsight, agemin)]

#assign ageclass values using ageknown when birth year is known

setDT(data)[ageknown == 'C', ageclass := "C"]                     #if birth year known and ageknown is calf, then calf
(data)[ageknown == 'J', ageclass := "J"]                          #if birth year known and ageknown is juvenile, then juvenile
(data)[ageknown == 'A', ageclass := "A"]                          #if birth year known and ageknown is adult, then adult

#assign ageclass values using agemin when birth year is unknown

setDT(data)[ageknown == 'U' & agemin == 'U', ageclass := "U"]     #if birth year unknown and first sight <9, then unknown
(data)[ageknown == 'U' & agemin == 'A', ageclass := "A"]          #if birth year unknown and first sight >9, then adult

#assign ageclass values for females who have calves under the age of 9 and are then considered adults instead of juvenile/unknown

setDT(data)[ageclass == 'U' & data$Year - data$FirstCalvingYear >= -1, ageclass := "A"]     #if ageclass unknown and year minus first calving year is greater than or equal to minus 1
setDT(data)[ageclass == 'J' & data$Year - data$FirstCalvingYear >= -1, ageclass := "A"]     #if ageclass juvenile and year minus first calving year is greater than or equal to minus 1

#make a table and open it to look at results
agecalv <- data[, .(Month, Year, BirthYear, yearminusbirthyear, ageknown, FirstYearSighted, yearminusfirstsight, agemin, FirstCalvingYear, yearminusfirstcalving, ageclass)]


#merge the Month, Day, Year, and Time columns into one datetime column
data$newdate <- as.character(with(data, paste(Month, Day, Year,sep="/")), "%Y/%m/%d") 
data$newtime <- format(strptime(data$Time, format="%H%M"), format = "%H:%M")
data$datetime <- paste(data$newdate, " ", data$newtime)

#make a table and open it to look at results and make sure your datetime merger went well
Datetime <- data[, .(Month, Day, Year, Time, newdate, newtime, datetime)]

###########################################3

##masterprimary <- data[,.()]

MasterPrimary <- data[, .(datetime, Latitude, Longitude, EGNo, ageclass)]


##############################################
#still need to:
#assign lactation stage
#merge egno with agesex class
#remove blank times
#remove blank lat or long
#remove unknown age or sex