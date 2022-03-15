# Import datasets
# Original dataset
airbnb <- read.csv(file.choose(), header=TRUE)

# LISTING DATA
b_listing <- airbnb

# Checking dataset
summary(b_listing)
str(b_listing)
dim(b_listing)
names(b_listing)
head(b_listing)
tail(b_listing)

# Check for distinct 'id' values in listings
length(unique(b_listing$id))
# 3585 distinct values which equals the number of rows 3585

# Convert 'Price' to numeric and remove $ 
b_listing$price<-gsub("$","",b_listing$price, fixed=TRUE)
b_listing$price <- gsub(",", "", b_listing$price)
b_listing$price <- as.numeric(b_listing$price)
head(b_listing$price)
class(b_listing$price)

# Convert 'host_response_rate' 'host_acceptance_rate' to numeric
b_listing$host_response_rate <- gsub("%","",b_listing$host_response_rate, fixed=TRUE)
b_listing$host_response_rate <- as.numeric(b_listing$host_response_rate)

b_listing$host_acceptance_rate <- gsub("%","",b_listing$host_acceptance_rate, fixed=TRUE)
b_listing$host_acceptance_rate <- as.numeric(b_listing$host_acceptance_rate)

summary(b_listing)
str(b_listing)
# Convert 'host_is_superhost to 1 - t and 0 - f
unique(b_listing$host_is_superhost)
b_listing$host_is_superhost <- ifelse(b_listing$host_is_superhost == 't',1,0)
class(b_listing$host_is_superhost)
b_listing$host_is_superhost <- as.factor(b_listing$host_is_superhost)

# Convert 'host_identity_verified' to 1 -t and 0 -f
unique(b_listing$host_identity_verified)
b_listing$host_identity_verified <- ifelse(b_listing$host_identity_verified == 't',1,0)
b_listing$host_identity_verified <- as.factor(b_listing$host_identity_verified)

# Convert 'zipcode' to numeric
b_listing$zipcode <- as.numeric(b_listing$zipcode)

# Convert 'security_deposit' 'cleaning_fee' 'extra_people' to numeric and remove $
b_listing$security_deposit<-gsub("$","",b_listing$security_deposit, fixed=TRUE)
b_listing$security_deposit <- gsub(",", "", b_listing$security_deposit)
b_listing$security_deposit <- as.numeric(b_listing$security_deposit)

b_listing$cleaning_fee<-gsub("$","",b_listing$cleaning_fee, fixed=TRUE)
b_listing$cleaning_fee <- gsub(",", "", b_listing$cleaning_fee)
b_listing$cleaning_fee <- as.numeric(b_listing$cleaning_fee)

b_listing$extra_people<-gsub("$","",b_listing$extra_people, fixed=TRUE)
b_listing$extra_people <- gsub(",", "", b_listing$extra_people)
b_listing$extra_people <- as.numeric(b_listing$extra_people)

str(b_listing)
summary(b_listing)
# Too many  NAs in security deposit and cleaning fee therefore we will exclude those columns.

# Convert 'host_since' to Date type
b_listing$host_since <-as.Date(b_listing$host_since)
b_listing$host_since
class(b_listing$host_since)


# Convert 'amenities' to dummy vairables

unique(b_listing$amenities)
aggregate(id~ amenities, data=b_listing, FUN=length)

b_listing$amenities

# Install and load 'stringr' package 

install.packages("stringr")
library(stringr)

# Create dummy variables for some amenities

# TV
b_listing$TV <- 0
b_listing$TV <- ifelse(str_detect(b_listing$amenities, "TV"),"1","0")
b_listing$TV <- as.numeric(b_listing$TV) 
sum(b_listing$TV)
# 2725


# Wireless Internet
b_listing$Wireless_Internet <- 0
b_listing$Wireless_Internet <- ifelse(str_detect(b_listing$amenities, "Wireless Internet"),"1","0")

b_listing$Wireless_Internet <- as.numeric(b_listing$Wireless_Internet) 
sum(b_listing$Wireless_Internet)
# 3421

# Pets
b_listing$Pets <- 0
b_listing$Pets <- ifelse(str_detect(b_listing$amenities, "Pets"),"1","0")

b_listing$Pets<- as.numeric(b_listing$Pets) 
sum(b_listing$Pets)
# 806

#Free Parking
b_listing$FreeParking <- 0
b_listing$FreeParking <- ifelse(str_detect(b_listing$amenities, "Free Parking on Premises"),"1","0")

b_listing$FreeParking <- as.numeric(b_listing$FreeParking) 
sum(b_listing$FreeParking)
# 831

# AC
b_listing$AC <- 0
b_listing$AC <- ifelse(str_detect(b_listing$amenities, "Air Conditioning"),"1","0")

b_listing$AC <- as.numeric(b_listing$AC) 
sum(b_listing$AC)
# 2821

# Heating 
b_listing$Heating<- 0
b_listing$Heating <- ifelse(str_detect(b_listing$amenities, "Heating"),"1","0")

b_listing$Heating <- as.numeric(b_listing$Heating) 
sum(b_listing$Heating)
# 3384

# Washer 
b_listing$Washer <- 0
b_listing$Washer <- ifelse(str_detect(b_listing$amenities, "Washer"),"1","0")

b_listing$Washer <- as.numeric(b_listing$Washer) 
sum(b_listing$Washer)
# 2475

# Pool
b_listing$Pool <- 0
b_listing$Pool <- ifelse(str_detect(b_listing$amenities, "Pool"),"1","0")

b_listing$Pool <- as.numeric(b_listing$Pool) 
sum(b_listing$Pool)
# 160

#Kitchen
b_listing$Kitchen <- 0
b_listing$Kitchen <- ifelse(str_detect(b_listing$amenities, "Kitchen"),"1","0")

b_listing$Kitchen <- as.numeric(b_listing$Kitchen) 
sum(b_listing$Kitchen)
# 3284

# Convert 'instant_bookable' to t =1 and f=0

unique(b_listing$instant_bookable)
b_listing$instant_bookable <- ifelse(b_listing$instant_bookable =="t",1,0)
class(b_listing$instant_bookable)
b_listing$instant_bookable <- as.factor(b_listing$instant_bookable)


# Check for correlation
# Availability columns

# Availability_30
cor(b_listing$price, b_listing$availability_30)
# 0.1321001

# Availability_60
cor(b_listing$price, b_listing$availability_60)
# 0.1125769

# Availability_90
cor(b_listing$price, b_listing$availability_90)
# 0.09852607

# Availability 365
cor(b_listing$price, b_listing$availability_365)
#  0.03669411
# Since availability_30 has the highest correlation we include that 


# Remove white spaces from 'neighbourhood_cleansed', 'room_type', 'bed_type', 'host_response_time', 'property_type', 'cancellation_policy' 

b_listing$neighbourhood_cleansed <- gsub(" ","",b_listing$neighbourhood_cleansed)
unique(b_listing_new$neighbourhood_cleansed)

b_listing$property_type <- gsub(" ","",b_listing$property_type)
b_listing$property_type <- gsub("&","",b_listing$property_type)
b_listing$property_type <- gsub("/","",b_listing$property_type)

b_listing$room_type <- gsub(" ","",b_listing$room_type)
b_listing$room_type <- gsub("/","",b_listing$room_type)

b_listing$bed_type <- gsub(" ","",b_listing$bed_type)
b_listing$bed_type <- gsub("-","",b_listing$bed_type)

b_listing$host_response_time <- gsub(" ","",b_listing$host_response_time)

b_listing$cancellation_policy <- gsub(" ","",b_listing$cancellation_policy)


# Adding dummy columns for select variables

# Install and load fastDummies package
install.packages('fastDummies')
library(fastDummies)

names(b_listing)

b_listing_2 <- dummy_cols(b_listing, select_columns = c('neighbourhood_cleansed', 'room_type', 'bed_type', 'host_response_time', 'property_type', 'cancellation_policy'),
                       remove_selected_columns = TRUE)

head(b_listing_2)
str(b_listing_2)
summary(b_listing_2)
names(b_listing_2)

#####


### New dataset with select columns 

b_listing_new <- b_listing_2[,c(1,26,27,28,32,36,42,50:53,56,61:64,67,72,75,85,89,90:132,134:136,138:145)]
dim(b_listing_new)
head(b_listing_new)
names(b_listing_new)
summary(b_listing_new)
str(b_listing_new)

# Clean data by removing NAs

complete_data <- b_listing_new[complete.cases(b_listing_new),]
dim(complete_data)
str(complete_data)
names(complete_data)
# [1] 2751 76

summary(complete_data)
str(complete_data)


# NAs:bathrooms 14, bedrooms 10, beds 9, review_scores_rating 813, review_scores_accuracy 823, review_scores_cleanliness 818,
#review_scores_checkin 820, review_scores_communication 818, review_scores_location 822, review_scores_value 821, reviews_per_month 756.

## Training and testing data ##

install.packages("caret")
library(caret)

index<-createDataPartition(complete_data$price,p=0.8,list=FALSE) 

head(index)
dim(index)

traindata<-complete_data[index,]
testdata<-complete_data[-index,]
dim(traindata)
dim(testdata)
summary(index)
summary(traindata)
summary(testdata)


# Multiple Linear Regression

# Model 1
price_mlr <- lm(price ~ ., data =traindata)
summary(price_mlr)

# Model 2 - removing insignificant variables

price_mlr_2 <- lm(price ~  id + host_acceptance_rate + host_is_superhost + zipcode +  bathrooms + bedrooms + beds + guests_included + minimum_nights + availability_30 
                     + number_of_reviews + review_scores_rating + reviews_per_month + TV + Pets + AC + Washer +  neighbourhood_cleansed_Allston + neighbourhood_cleansed_BackBay + 
                       neighbourhood_cleansed_BayVillage + neighbourhood_cleansed_BeaconHill + neighbourhood_cleansed_Brighton + neighbourhood_cleansed_Charlestown + 
                       + neighbourhood_cleansed_Chinatown + neighbourhood_cleansed_Downtown + neighbourhood_cleansed_EastBoston + neighbourhood_cleansed_Fenway + 
                       neighbourhood_cleansed_JamaicaPlain + neighbourhood_cleansed_LeatherDistrict + neighbourhood_cleansed_LongwoodMedicalArea + neighbourhood_cleansed_MissionHill +
                       neighbourhood_cleansed_NorthEnd + neighbourhood_cleansed_Roxbury + neighbourhood_cleansed_SouthBoston + neighbourhood_cleansed_SouthBostonWaterfront + 
                       neighbourhood_cleansed_SouthEnd + neighbourhood_cleansed_WestEnd  + room_type_Entirehomeapt + host_response_time_withinaday + host_response_time_withinafewhours
                     , data = traindata)

summary(price_mlr_2)


# Model 3 - removing insignificant variables (zipcode, Pets, eighbourhood_cleansed_LeatherDistrict, neighbourhood_cleansed_LongwoodMedicalArea )

price_mlr_3 <- lm(price ~  id + host_acceptance_rate + host_is_superhost + bathrooms + bedrooms + beds + guests_included + minimum_nights + availability_30 
                    + number_of_reviews + review_scores_rating + reviews_per_month + TV  + AC + Washer +  neighbourhood_cleansed_Allston + neighbourhood_cleansed_BackBay + 
                      neighbourhood_cleansed_BayVillage + neighbourhood_cleansed_BeaconHill + neighbourhood_cleansed_Brighton + neighbourhood_cleansed_Charlestown + 
                      + neighbourhood_cleansed_Chinatown + neighbourhood_cleansed_Downtown + neighbourhood_cleansed_EastBoston + neighbourhood_cleansed_Fenway + 
                      neighbourhood_cleansed_JamaicaPlain + neighbourhood_cleansed_MissionHill +
                      neighbourhood_cleansed_NorthEnd + neighbourhood_cleansed_Roxbury + neighbourhood_cleansed_SouthBoston + neighbourhood_cleansed_SouthBostonWaterfront + 
                      neighbourhood_cleansed_SouthEnd + neighbourhood_cleansed_WestEnd  + room_type_Entirehomeapt + host_response_time_withinaday + host_response_time_withinafewhours
                    , data = traindata)

summary(price_mlr_3)

# Model 4 - removing insignificant variables (host_acceptance_rate, reviews_per_month)

price_mlr_4 <- lm(price ~  id + host_is_superhost  +  bathrooms + bedrooms + beds + guests_included  + availability_30 
                      + number_of_reviews + review_scores_rating + TV  + AC + Washer +  neighbourhood_cleansed_Allston + neighbourhood_cleansed_BackBay + 
                        neighbourhood_cleansed_BayVillage + neighbourhood_cleansed_BeaconHill + neighbourhood_cleansed_Brighton + neighbourhood_cleansed_Charlestown + 
                        + neighbourhood_cleansed_Chinatown + neighbourhood_cleansed_Downtown + neighbourhood_cleansed_EastBoston + neighbourhood_cleansed_Fenway + 
                        neighbourhood_cleansed_JamaicaPlain + neighbourhood_cleansed_MissionHill +
                        neighbourhood_cleansed_NorthEnd + neighbourhood_cleansed_Roxbury + neighbourhood_cleansed_SouthBoston + neighbourhood_cleansed_SouthBostonWaterfront + 
                        neighbourhood_cleansed_SouthEnd + neighbourhood_cleansed_WestEnd  + room_type_Entirehomeapt + host_response_time_withinaday + host_response_time_withinafewhours
                      , data = traindata)

summary(price_mlr_4)


# Multicollinearity 

install.packages("car")
library(car)

vif(price_mlr_4)

# All are below 5 therefore all variables are independent


## Random Forest

install.packages("randomForest")
library(randomForest)

price_rf<-randomForest(formula=price~.,
                       data=traindata)
price_rf


price_rf$importance

varImpPlot(price_rf, col="blue")


## Lasso



