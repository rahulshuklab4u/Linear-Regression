setwd("/Users/rahushukla/Desktop/Trainings/Upgrad/11. Assignment- Linear Regression")

library(dplyr)
library(tidyr)
library(MASS)
library(car)
library(corrplot)
library(ggplot2)

# Importing csv with factor True. This helped to tackle each variable one by one during data preparation
cars_df <- read.csv("CarPrice_Assignment.csv")

#check for NAs
sum(is.na(cars_df)) # 0 , no NAs

#check for duplicates
which(duplicated(cars_df)) # 0 no duplicates

# Convert CarName to character and then to lower case for effective split
cars_df$CarName <- as.character(cars_df$CarName)
cars_df$CarName <- tolower(cars_df$CarName)

#Break car name into company and model name.
cars_df <- separate(cars_df,CarName,c("company","model"),sep=" ")
#drop column model as it is not needed for analysis as per the rubic
cars_df <- cars_df[, -4] 


### Data Cleaning
# Find unique car names and rename the ones with spelling mistakes.
unique_car_names <- unique(cars_df$company)
cars_df[which(cars_df$company == "maxda"),]$company <- "mazda"
cars_df[which(cars_df$company == "porcshce"),]$company <- "porsche"
cars_df[which(cars_df$company == "toyouta"),]$company <- "toyota"
cars_df[which(cars_df$company %in% c("vokswagen","vw")),]$company <- "volkswagen"
### Exploratory Data Data Analysis
# Univariate & Segmented Univariate Data Analysis


ggplot(cars_df,aes(x=fueltype))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # Fuel Type Analysis
ggplot(cars_df,aes(x=aspiration))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) #Aspiration Analysis
ggplot(cars_df,aes(x=doornumber))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # doornumber Analysis
ggplot(cars_df,aes(x=carbody))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # carbody Analysis
ggplot(cars_df,aes(x=drivewheel))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # drivewheel Analysis
ggplot(cars_df,aes(x=enginelocation))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # enginelocation Analysis
ggplot(cars_df,aes(x=cylindernumber))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # cylindernumber Analysis
ggplot(cars_df,aes(x=fuelsystem))+geom_bar()+geom_text(stat='count',aes(label=..count..),vjust=-1) # fuelsystem Analysis

ggplot(cars_df,aes(x=car_ID,y=curbweight))+geom_point() # curbweight Analysis

boxplot(cars_df$horsepower)
boxplot(cars_df$compressionratio)

## Analysis summary:
# All bar plots show clean representation of data. Categorical factor variables would have to converted to dummy variables
# Box plot show outliers that need to be handled.


# Bivariate Analysis
ggplot(cars_df,aes(x=curbweight,y=carwidth))+geom_smooth()
ggplot(cars_df,aes(x=curbweight,y=horsepower))+geom_smooth()
ggplot(cars_df,aes(x=curbweight,y=boreratio))+geom_smooth()
ggplot(cars_df,aes(x=curbweight,y=enginesize))+geom_smooth()
ggplot(cars_df,aes(x=curbweight,y=citympg))+geom_smooth()

## Analysis summary:
# All these variables are highly co-related with curbweight. Hence, this need to be taken into consideration during modelling


### Remove outlier by using quantiles. Checked each numeric variable. Only jump beyond 90% is considered.
quantile(cars_df$wheelbase,seq(0,1,0.01))
quantile(cars_df$carlength,seq(0,1,0.01))
quantile(cars_df$carwidth,seq(0,1,0.01))
quantile(cars_df$carheight,seq(0,1,0.01))
quantile(cars_df$curbweight,seq(0,1,0.01))
quantile(cars_df$enginesize,seq(0,1,0.01))
cars_df$horsepower[(which(cars_df$enginesize > 209))] <- 209 # jump from 96% to 97%
quantile(cars_df$boreratio,seq(0,1,0.01))
quantile(cars_df$stroke,seq(0,1,0.01))
quantile(cars_df$horsepower,seq(0,1,0.01))
cars_df$horsepower[(which(cars_df$horsepower > 184))] <- 184 # jump from 97% to 98%
quantile(cars_df$compressionratio,seq(0,1,0.01))
cars_df$compressionratio[(which(cars_df$compressionratio > 10.94))] <- 10.94 # jump from 90% to 91%
quantile(cars_df$peakrpm,seq(0,1,0.01))
quantile(cars_df$citympg,seq(0,1,0.01))
cars_df$compressionratio[(which(cars_df$citympg >38.00))] <- 38.00 # jump from 98% to 99%
quantile(cars_df$highwaympg,seq(0,1,0.01))
cars_df$compressionratio[(which(cars_df$highwaympg >49.88))] <- 49.88 # jump from 99% to 100%


##creating dummy variables for categoricl variable with more than two levels
#Convert company into dummy variable
dummy <- model.matrix(~company,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-3],dummy)

#Convert body into dummy variable
summary(cars_df$carbody)
dummy <- model.matrix(~carbody,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-6],dummy)

#Convert drivewheel into dummy variable
summary(cars_df$drivewheel)
dummy <- model.matrix(~drivewheel,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-6],dummy)


#Convert cylindernumber into dummy variable
summary(cars_df$cylindernumber)
dummy <- model.matrix(~cylindernumber,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-13],dummy)


#Convert enginetype into dummy variable
summary(cars_df$enginetype)
cars_df$enginetype = droplevels(cars_df$enginetype) # Since we converted some factors with typos, they have no observations anymore and need to be dropped before created dummy
dummy <- model.matrix(~enginetype,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-12],dummy)

#Convert enginetype into dummy variable
summary(cars_df$fuelsystem)
dummy <- model.matrix(~fuelsystem,data=cars_df)
dummy<-dummy[,-1]
cars_df<-cbind(cars_df[,-13],dummy)


#Convert categorical variable into numeric value

#Fuel type is gas or diesel. Convert to 1 and 0
summary(cars_df$fueltype)
levels(cars_df$fueltype)<-c(0,1)
cars_df$fueltype <- as.numeric(levels(cars_df$fueltype))[cars_df$fueltype]
names(cars_df)[names(cars_df)=="fueltype"] <- "is_fueltype_gas"

#aspiration is std turbo Convert to 1 and 0
summary(cars_df$aspiration)
levels(cars_df$aspiration)<-c(1,0)
cars_df$aspiration <- as.numeric(levels(cars_df$aspiration))[cars_df$aspiration]
names(cars_df)[names(cars_df)=="aspiration"] <- "is_aspiration_std"

#doornumber is four two Convert to 1 and 0
summary(cars_df$doornumber)
levels(cars_df$doornumber)<-c(1,0)
cars_df$doornumber <- as.numeric(levels(cars_df$doornumber))[cars_df$doornumber]
names(cars_df)[names(cars_df)=="doornumber"] <- "is_doornumber_four"

#enginelocation is front rear Convert to 1 and 0
summary(cars_df$enginelocation)
levels(cars_df$enginelocation)<-c(1,0)
cars_df$enginelocation <- as.numeric(levels(cars_df$enginelocation))[cars_df$enginelocation]
names(cars_df)[names(cars_df)=="enginelocation"] <- "is_enginelocation_front"

#remove Car_ID from analysis
cars_df_original <- cars_df # Will remove the ID column below as not needed for analysis. but needed later for plotting
cars_df <- cars_df[, -1] 

str(cars_df) #verify there are no more factor types in the dataframe

## Data Cleaning and preparation is done. Time for data modelling 

# Divide into training and test data set
set.seed(100)#set the seed to 100 
trainindices= sample(1:nrow(cars_df), 0.7*nrow(cars_df))
# generate the train data set
train = cars_df[trainindices,]
#Similarly store the rest of the observations into an object "test".
test = cars_df_original[-trainindices,]

#Execute the first model_1 multilinear model in the training set. 
model_1 <-lm(price~.,data=train)
summary(model_1) #Multiple R-squared:  0.9814,	Adjusted R-squared:  0.9697  


## Some of the variables have p values as NA. This is bcz they are linearily co-related with other variables. They can be dropped from the model
alias(model_1) # alias will help show which variables they are co-related with.

#first remove all variables that have NAs in p-value
model_1 <- lm(price~. - cylindernumberthree - cylindernumbertwelve - enginetypel - enginetyperotor - fuelsystem4bbl - fuelsystemidi - fuelsystemspfi - enginetypedohcv - enginetypeohcf , data=train)
summary(model_1) #Multiple R-squared:  0.9814,	Adjusted R-squared:  0.9697


#run the stepAIC function in both directions to quickly eliminate variables based on AIC algo
step<-stepAIC(model_1,direction="both")
step

#Use the suggested model stored by stepAIC function
model_2 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                carwidth + curbweight + enginesize + boreratio + stroke + 
                compressionratio + horsepower + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

# Now analyse variables from various angles to understand their impact on the model
# To remove the variable from the model, the underlying idea is that Adjusted R-squared should not significantly drop after removing the variable.

summary(model_2) #Multiple R-squared:  0.9799,	Adjusted R-squared:  0.972
vif(model_2)

## VIF is extremely high for many variables, despite the significance being low. This is bcz they are highly co-related with other variable.
# We have already verified this by bivariate analysis. Finding the co-relation value from below 

cor_mtx <- cor(cars_df %>% dplyr::select(is_aspiration_std , is_enginelocation_front ,
                                                       carwidth , curbweight , enginesize , boreratio , stroke , 
                                                       compressionratio , horsepower , peakrpm , citympg , companybmw , 
                                                       companybuick , companydodge , companyhonda , companyisuzu , 
                                                       companymazda , companymercury , companymitsubishi , companynissan , 
                                                       companypeugeot , companyplymouth , companyporsche , companyrenault , 
                                                       companysaab , companysubaru , companytoyota , companyvolkswagen , 
                                                       companyvolvo , carbodyhardtop , carbodyhatchback , carbodysedan , 
                                                       carbodywagon , drivewheelfwd , cylindernumberfive , cylindernumbertwo , 
                                                       fuelsystem2bbl , fuelsystemmpfi))


?corrplot
corrplot(cor_mtx,method = "number", type = "lower",order = "FPC",number.cex = 0.8,bg="green")

correlation <- cor(cars_df)
View(correlation)

## curbweight is strongly co-related with enginesize (), carwidth(), horsepower(), boreratio()
#  All these variables had very high VIFs as well. Checking impact of dropping all these variables one by one

## Enginesize and curbweight are highly co-related. Let's drop enginesize & see impact


model_3 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                carwidth + curbweight  + boreratio + stroke + 
                compressionratio + horsepower + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

summary(model_3) #Multiple R-squared:  0.9667,	Adjusted R-squared:  0.955
vif(model_3)


## carwidth and curbweight are highly co-related. Let's drop carwidth & see impact
model_4 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                 curbweight  + boreratio + stroke + 
                compressionratio + horsepower + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

summary(model_4) #Multiple R-squared:  0.9628,	Adjusted R-squared:  0.9501
vif(model_4)

## horsepower and curbweight are highly co-related. Let's drop horsepower & see impact


model_5 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight  + boreratio + stroke + 
                compressionratio + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

summary(model_5) #Multiple R-squared:  0.9605,	Adjusted R-squared:  0.9476 
vif(model_5)

## boreratio and curbweight are highly co-related. Let's drop boreratio & see impact


model_6 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

summary(model_6) #Multiple R-squared:  0.9605,	Adjusted R-squared:  0.948 
vif(model_6)

## carbodyhatchback and carbodysedan are highly co-related. Let's drop carbodyhatchback & see impact

model_7 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl + fuelsystemmpfi, data = train)

summary(model_7) #Multiple R-squared:  0.9604,	Adjusted R-squared:  0.9484 
vif(model_7)

## fuelsystemmpfi and curbweight are  co-related. fuelsystemmpfi has a high VIF  7.783153 . Let's drop fuelsystemmpfi & see impact

model_8 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm + citympg + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl , data = train)

summary(model_8) #Multiple R-squared:  0.9593,	Adjusted R-squared:  0.9474 
vif(model_8)

## citympg and curbweight are  co-related. citympg VIF is high. Let's drop & see impact
model_9 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm  + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + drivewheelfwd + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl , data = train)

summary(model_9) #Multiple R-squared:  0.9585,	Adjusted R-squared:  0.947  
vif(model_9)

## drivewheelfwd and curbweight are  co-related. drivewheelfwd VIF is high. Let's drop & see impact

model_10 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm  + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth + companyporsche + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo + carbodyhardtop + carbodyhatchback + 
                carbodywagon + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl , data = train)

summary(model_10) #Multiple R-squared:  0.9539,	Adjusted R-squared:  0.9415
vif(model_10)


## companyporsche and is_enginelocation_front are  co-related. companyporsche VIF is high. Let's drop & see impact

model_11 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                 curbweight + stroke + 
                 compressionratio + peakrpm  + companybmw + 
                 companybuick + companydodge + companyhonda + companyisuzu + 
                 companymazda + companymercury + companymitsubishi + companynissan + 
                 companypeugeot + companyplymouth + companyrenault + 
                 companysaab + companysubaru + companytoyota + companyvolkswagen + 
                 companyvolvo + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + cylindernumberfive + cylindernumbertwo + 
                 fuelsystem2bbl , data = train)

summary(model_11) #Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9399
vif(model_11)


## All cleary visible co-relations from the matrix are now taken care of.

# Now let's look at high p value variables and remove them. ( Considering p values not in category of *,**,***, dot(.))

# carbodyhatchback has highest p value 0.919630. Let's remove it and see impact
model_12 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                 curbweight + stroke + 
                 compressionratio + peakrpm  + companybmw + 
                 companybuick + companydodge + companyhonda + companyisuzu + 
                 companymazda + companymercury + companymitsubishi + companynissan + 
                 companypeugeot + companyplymouth + companyrenault + 
                 companysaab + companysubaru + companytoyota + companyvolkswagen + 
                 companyvolvo + carbodyhardtop  + 
                 carbodywagon + cylindernumberfive + cylindernumbertwo + 
                 fuelsystem2bbl , data = train)

summary(model_12) #Multiple R-squared:  0.9522,	Adjusted R-squared:  0.9404 
vif(model_12)

# carbodyhardtop has higher p value 0.680299. Let's remove it and see impact
model_13 <- lm(formula = price ~ is_aspiration_std + is_enginelocation_front + 
                 curbweight + stroke + 
                 compressionratio + peakrpm  + companybmw + 
                 companybuick + companydodge + companyhonda + companyisuzu + 
                 companymazda + companymercury + companymitsubishi + companynissan + 
                 companypeugeot + companyplymouth + companyrenault + 
                 companysaab + companysubaru + companytoyota + companyvolkswagen + 
                 companyvolvo  + 
                 carbodywagon + cylindernumberfive + cylindernumbertwo + 
                 fuelsystem2bbl , data = train)

summary(model_13) #Multiple R-squared:  0.9521,	Adjusted R-squared:  0.9408 
vif(model_13)

# is_aspiration_std has highest p value. Let's remove it and see impact
model_14 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm  + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive + cylindernumbertwo + 
                fuelsystem2bbl , data = train)

summary(model_14) #Multiple R-squared:  0.952,	Adjusted R-squared:  0.9412 
vif(model_14)

# fuelsystem2bbl has highest p value. Let's remove it and see impact
model_15 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + stroke + 
                compressionratio + peakrpm  + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive + cylindernumbertwo , data = train)

summary(model_15) #Multiple R-squared:  0.9518,	Adjusted R-squared:  0.9416 
vif(model_15)


# cylindernumbertwo has highest p value. Let's remove it and see impact
model_16 <- lm(formula = price ~ is_enginelocation_front + 
                 curbweight + stroke + 
                 compressionratio + peakrpm  + companybmw + 
                 companybuick + companydodge + companyhonda + companyisuzu + 
                 companymazda + companymercury + companymitsubishi + companynissan + 
                 companypeugeot + companyplymouth  + companyrenault + 
                 companysaab + companysubaru + companytoyota + companyvolkswagen + 
                 companyvolvo   + 
                 carbodywagon  + cylindernumberfive , data = train)

summary(model_16) #Multiple R-squared:  0.9518,	Adjusted R-squared:  0.942 
vif(model_16)

# compressionratio has highest p value. Let's remove it and see impact
model_17 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                 peakrpm  + companybmw + 
                companybuick + companydodge + companyhonda + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_17) #Multiple R-squared:  0.9512,	Adjusted R-squared:  0.9423 
vif(model_17)

# All variables are now pvalue 0.05 and below. However some variables have VIF above 2 now as well.
# Good monitoring of impact is required now. Removing variables with VIF above 2 and accessing impact.

# companytoyota has high VIF 3.827474. Removing it. 
model_18 <- lm(formula = price ~ is_enginelocation_front + 
                           curbweight + 
                           peakrpm  + companybmw + 
                           companybuick + companydodge + companyhonda + companyisuzu + 
                           companymazda + companymercury + companymitsubishi + companynissan + 
                           companypeugeot + companyplymouth  + companyrenault + 
                           companysaab + companysubaru  + companyvolkswagen + 
                           companyvolvo   + 
                           carbodywagon  + cylindernumberfive  , data = train)

summary(model_18) #Multiple R-squared:  0.924,	Adjusted R-squared:  0.9108 
vif(model_18)

# Adjusted R square dropped by 3% which seems high. Lets keep companytoyota back and instead drop nxt high VIF  companyhonda. 
model_19 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                peakrpm  + companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_19) #Multiple R-squared:  0.9382,	Adjusted R-squared:  0.9275 
vif(model_19)


##Adjusted R square came down by 2%. And, some variables became less significant. Let's remove then now.

#peakrpm has high p value 0.244226 . Removing it
model_20 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                 companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymazda + companymercury + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_20) #Multiple R-squared:  0.9375,	Adjusted R-squared:  0.9273
vif(model_20)

#companymercury has high p value 0.188890. Removing it
model_21 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymazda  + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysaab + companysubaru + companytoyota + companyvolkswagen + 
                companyvolvo   + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_21) #Multiple R-squared:  0.9366,	Adjusted R-squared:  0.9269 
vif(model_21)

#companysaab high p value 0.113012. Removing it
model_22 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymazda  + companymitsubishi + companynissan + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysubaru + companytoyota + companyvolkswagen + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_22) #Multiple R-squared:  0.9346,	Adjusted R-squared:  0.9257
vif(model_22)

#companynissan high p value 0.067026. Removing it
model_23 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymazda  + companymitsubishi + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysubaru + companytoyota + companyvolkswagen + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_23) #Multiple R-squared:  0.9329,	Adjusted R-squared:  0.9243 
vif(model_23)

#companymazda high p value 0.081125. Removing it
model_24 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysubaru + companytoyota + companyvolkswagen + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_24) #Multiple R-squared:  0.9312,	Adjusted R-squared:  0.9231 
vif(model_24)

#companyvolkswagen high p value 0.078551. Removing it
model_25 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + companydodge  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysubaru + companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_25) #Multiple R-squared:  0.9295,	Adjusted R-squared:  0.9218 
vif(model_25)

#companydodge high p value 0.08785. Removing it
model_26 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + companyplymouth  + companyrenault + 
                companysubaru + companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_26) #Multiple R-squared:  0.9279,	Adjusted R-squared:  0.9206 
vif(model_26)

#companyrenault high p value 0.112403. Removing it
model_27 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + companyplymouth + 
                companysubaru + companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_27) #Multiple R-squared:  0.9264,	Adjusted R-squared:  0.9197 
vif(model_27)

#companysubaru high p value 0.12604. Removing it
model_28 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + companyplymouth + 
                companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_28) #Multiple R-squared:  0.9251,	Adjusted R-squared:  0.9188 
vif(model_28)

#companyplymouth high p value 0.15658. Removing it
model_29 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick  + companyisuzu + 
                companymitsubishi + 
                companypeugeot + 
                companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_29) #Multiple R-squared:  0.9239,	Adjusted R-squared:  0.9182 
vif(model_29)


#companyisuzu high p value 0.07136. Removing it
model_30 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + 
                companymitsubishi + 
                companypeugeot + 
                companytoyota  + 
                carbodywagon  + cylindernumberfive  , data = train)

summary(model_30) #Multiple R-squared:  0.922,	Adjusted R-squared:  0.9168
vif(model_30)


## All variables have p value less than 0.05 and VIF less than 2.
# Removing variables with p value in category of *

#cylindernumberfive has next highest  p value of 0.0337. Removing it
model_31 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + 
                companymitsubishi + 
                companypeugeot + 
                companytoyota  + 
                carbodywagon  , data = train)

summary(model_31) #Multiple R-squared:  0.9193,	Adjusted R-squared:  0.9145 
vif(model_31)


#companymitsubishi has next highest  p value of 0.015646. Removing it
model_32 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + 
                companypeugeot + 
                companytoyota  + 
                carbodywagon  , data = train)

summary(model_32) #Multiple R-squared:  0.9157,	Adjusted R-squared:  0.9114 
vif(model_32)

## All p values are under *** category. However, the p-value of companytoyota is much higher than other variables. 
## So lets drop it and see impact

model_33 <- lm(formula = price ~ is_enginelocation_front + 
                curbweight + 
                companybmw + 
                companybuick + 
                companypeugeot + 
                carbodywagon  , data = train)

summary(model_33) #Multiple R-squared:  0.9082,	Adjusted R-squared:  0.9041
vif(model_33)

model_34 <- lm(formula = price ~ is_enginelocation_front + 
                 curbweight + 
                 companybmw + 
                 companybuick + 
                 carbodywagon  , data = train)

summary(model_34) #Multiple R-squared:  0.8978,	Adjusted R-squared:  0.8941
vif(model_34)

#However, later below when the rsquared was computed, the difference became 6% which is beyond the threshold of 5%
#Hence dropping this model in favor of model_33


## The final model is model_33. The model is now tested against the test dataset
# Predict the car prices in the testing dataset
Predict_1 <- predict(model_33,test[,-19])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared #0.8537533

#Difference between Adjusted R Sqaure and rsquared =0.8941 - 0.8537533 = 0.0403467 = 4.03%

## Computing error field and adding it to the test dataset
test <- test %>% mutate(error_price = price - test_price)

## Plot the error against car_ID
ggplot(test, aes(car_ID, error_price))+geom_point(col="blue")

#The errors (the differences between the actual views and the views predicted by the model) were randomly distributed. 
#What this essentially confirms is that there are no variables that could have helped explain the model better. 


### FINAL SUMMARY OF THE ANALYSIS ###
# Final model to compuate price is
# price = 2.493e+03 +  -1.918e+04 * is_enginelocation_front+ 1.156e+01 *curbweight +  8.265e+03 * companybmw + 7.979e+03 * companybuick + -4.529e+03 * companypeugeot + -2.681e+03 * carbodywagon 
  
# More details on co-efficients and p-values is as follows:

#lm(formula = price ~ is_enginelocation_front + curbweight + companybmw + 
#    companybuick + companypeugeot + carbodywagon, data = train)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7809.5 -1298.8  -200.2  1147.5 11157.7 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              2.493e+03  2.028e+03   1.229 0.221120    
#is_enginelocation_front -1.918e+04  1.491e+03 -12.862  < 2e-16 ***
#  curbweight               1.156e+01  5.081e-01  22.755  < 2e-16 ***
#  companybmw               8.265e+03  1.500e+03   5.510 1.74e-07 ***
#  companybuick             7.979e+03  1.173e+03   6.802 2.99e-10 ***
# companypeugeot          -4.529e+03  1.152e+03  -3.932 0.000134 ***
#  carbodywagon            -2.681e+03  6.461e+02  -4.149 5.86e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 2530 on 136 degrees of freedom
#Multiple R-squared:  0.9082,	Adjusted R-squared:  0.9041 
#F-statistic: 224.2 on 6 and 136 DF,  p-value: < 2.2e-16

#vif(model_33)
#is_enginelocation_front              curbweight              companybmw            companybuick          companypeugeot 
#1.021042                              1.593656                1.032791                1.431651                1.192084 
#carbodywagon 
#1.074967 
 
 # Car variable curbweight places a major role  in the pricing of the car. It is strongly co-related with other variables.
 # Hence, it explains the  behavior of other variables it is co-related to as well.
 # BMW, BUICK are the two cars which positively determine the impact on the pricing.
 # Peugeout has a negative impact on the pricing.
 # Body type wagon and front engine also negatively impact the pricing

  