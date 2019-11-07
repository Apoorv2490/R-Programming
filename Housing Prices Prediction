
######################################### ENVIRONMENT SET UP ################################################################
install.packages("MASS")
library("MASS")
install.packages("rpart")
library("rpart")
install.packages("dplyr")
library("dplyr")


######################################### DATA IMPORT ######################################################################

house_pricing <- read.csv("[Please insert Path of the file here]")

house_pricing_1 <- house_pricing

##Test

#str(house_pricing_1$GarageYrBlt)

###################################### DATA EXPLORATION #####################################################################
########### RUNNING MULTIPLE AGGREGATIONS ###############################

##-- These aggregations are run to understand distribution of data and get an idea of values in different columns.

NROW(subset(aggregate(house_pricing$Id, list(house_pricing$Id),length), x==1)) ### Checking if each there is 1 Id per row.
aggregate(house_pricing_1$Id,list(house_pricing_1$MSSubClass), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$OverallQual), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$MSSubClass), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$OverallCond), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$YearBuilt), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$YearRemodAdd), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$BsmtFullBath), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$BsmtHalfBath), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$FullBath), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$HalfBath), length) 
aggregate(house_pricing_1$Id,list(house_prsicing_1$Bedroom), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$KitchenAbvGr), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$TotRmsAbvGrd), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$Fireplaces), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$FireplaceQu), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$GarageYrBlt), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$GarageCars), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$TotRmsAbvGrd), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$MoSold), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$YrSold), length) 
aggregate(house_pricing_1$Id,list(house_pricing_1$Alley), length)

######################################### DATA PREPARATION ##################################################################


################################### DATA TYPE CONVERSIONS #######################

##---Converting non factor variables to factor variables. This is done to get all the columns to one form of data type such that an algorithm can be finalized, based on whether all variables are categorical or continuous.

house_pricing_1['MSSubClass'] <- as.factor(house_pricing_1$MSSubClass)
house_pricing_1['OverallQual'] <- as.factor(house_pricing_1$OverallQual)
house_pricing_1['OverallCond'] <- as.factor(house_pricing_1$OverallCond)
house_pricing_1['BsmtFullBath'] <- as.factor(house_pricing_1$BsmtFullBath)
house_pricing_1['BsmtHalfBath'] <- as.factor(house_pricing_1$BsmtHalfBath)
house_pricing_1['FullBath'] <- as.factor(house_pricing_1$FullBath)
house_pricing_1['HalfBath'] <- as.factor(house_pricing_1$HalfBath)
house_pricing_1['Bedroom'] <- as.factor(house_pricing_1$Bedroom)
house_pricing_1['KitchenAbvGr'] <- as.factor(house_pricing_1$KitchenAbvGr)
house_pricing_1['TotRmsAbvGrd'] <- as.factor(house_pricing_1$TotRmsAbvGrd)
house_pricing_1['Fireplaces'] <- as.factor(house_pricing_1$Fireplaces)
house_pricing_1['FireplaceQu'] <- as.factor(house_pricing_1$FireplaceQu)
house_pricing_1['GarageCars'] <- as.factor(house_pricing_1$GarageCars)
house_pricing_1['TotRmsAbvGrd'] <- as.factor(house_pricing_1$TotRmsAbvGrd)
house_pricing_1['BedroomAbvGr'] <- as.factor(house_pricing_1$BedroomAbvGr)


## TEST
intgrs<- unlist(lapply(house_pricing_1, is.integer)) #### Picking only factors variable
summary(house_pricing_1[, intgrs]) ### Summary of factprs
#summary(select_if(house_pricing_1,is.integer)) ### See all the columns in integer ####


################## HANDLING MISSING DATA / NAs IN VARIABLES ###################

##-- Both continuous and categorical variables have missing data / NAs. 
##--NAs in the factors / categorical variables need to be converted to "None" as R cannot read NA as a factor level.
##-- NAs in continuous variables are considered as missing valies and have been replaced with '0'

##-- Following code replaces NA's in factors to "None"

colmns<-colnames(select_if(house_pricing_1,is.factor))
for (i in 1:length(colmns)){
##levels <- levels(eval(parse(text=gsub(" ","",paste("house_pricing_1","$",colmns[4])))))
a<-"None"
eval(parse(text=gsub(" ","",paste("levels(",gsub(" ","",paste("house_pricing_1","$",colmns[i])),")","<-","c(levels(",gsub(" ","",paste("house_pricing_1","$",colmns[i])),")",",a)"))))
eval(parse(text=gsub(" ","",paste(gsub(" ","",paste("house_pricing_1","$",colmns[i])),"[","is.na(",gsub(" ","",paste("house_pricing_1","$",colmns[i])),")","]","<-","a"))))
}


##-- Following code replaces NA's in continuous variables to "0"

house_pricing_1$MoSold[is.na(house_pricing_1$MoSold)] <- 0
house_pricing_1$YrSold[is.na(house_pricing_1$YrSold)] <- 0
house_pricing_1$YearBuilt[is.na(house_pricing_1$YearBuilt)] <- 0
house_pricing_1$YearRemodAdd[is.na(house_pricing_1$YearRemodAdd)] <- 0
house_pricing_1$GarageYrBlt[is.na(house_pricing_1$GarageYrBlt)] <- 0
house_pricing_1$LotFrontage[is.na(house_pricing_1$LotFrontage)] <- 0
house_pricing_1$MasVnrArea[is.na(house_pricing_1$MasVnrArea)] <- 0

############ CREATING CATEGORICAL VARIABLES / FACTORS OUT OF CONTINUOUS VARIABLES ############

##--All the continious variables are converted into factors by 
	##--Assigning the values to quartile range in cases where ample data was present. 
	##--Variables where ample data was not present binary flag variable was created where representing data present or not.
##--This is done to get all the variables into on kind of data structure for the ease of running the predictions.

##-- Following code converts the data into ranges and ranks based on quartiles of continuous variables.

##MoSold_Range

MoSold_Range <- case_when(house_pricing_1$MoSold == 0 ~ "none"
											,house_pricing_1$MoSold>=1 & house_pricing_1$MoSold<5  ~ "1-5"
                                            ,house_pricing_1$MoSold>=5 & house_pricing_1$MoSold<6  ~ "5-6"
                                            ,house_pricing_1$MoSold>=6 & house_pricing_1$MoSold<8  ~ "6-8"
                                            ,house_pricing_1$MoSold>=8  ~ ">=8")
house_pricing_1 <- data.frame(house_pricing_1, MoSold_Range)


##YrSold_Range

YrSold_Range <- case_when(house_pricing_1$YrSold == 0 ~ "none"
											,house_pricing_1$YrSold>=2006 & house_pricing_1$YrSold<2007  ~ "2006-2007"
                                            ,house_pricing_1$YrSold>=2007 & house_pricing_1$YrSold<2008  ~ "2007-2008"
                                            ,house_pricing_1$YrSold>=2008 & house_pricing_1$YrSold<2009 ~ "2008-2009"
                                            ,house_pricing_1$YrSold>=2009   ~ ">=2009")
house_pricing_1 <- data.frame(house_pricing_1, YrSold_Range)



##YearBuilt_Range

YearBuilt_Range <- case_when(house_pricing_1$YearBuilt == 0 ~ "none"
											,house_pricing_1$YearBuilt>= 1872 & house_pricing_1$YearBuilt<1954  ~ "1872-1954"
                                            ,house_pricing_1$YearBuilt>=1954 & house_pricing_1$YearBuilt<1973 ~ "1954-1973"
                                            ,house_pricing_1$YearBuilt>=1973 & house_pricing_1$YearBuilt<2000 ~ "1973-2000"
                                            ,house_pricing_1$YearBuilt>=2000   ~ ">=2000")
house_pricing_1 <- data.frame(house_pricing_1, YearBuilt_Range)



##YearRemodAdd_Range

YearRemodAdd_Range <- case_when(house_pricing_1$YearRemodAdd == 0 ~ "none"
											,house_pricing_1$YearRemodAdd>= 1950 & house_pricing_1$YearRemodAdd<1967  ~ "1950-1967"
                                            ,house_pricing_1$YearRemodAdd>=1967 & house_pricing_1$YearRemodAdd<1994  ~ "1967-1994"
                                            ,house_pricing_1$YearRemodAdd>=1994 & house_pricing_1$YearRemodAdd<2004  ~ "1994-2004"
                                            ,house_pricing_1$YearRemodAdd>=2004   ~ ">=2004")
house_pricing_1 <- data.frame(house_pricing_1, YearRemodAdd_Range)



##GarageYrBlt_Range

GarageYrBlt_Range <- case_when(house_pricing_1$GarageYrBlt == 0 ~ "none"
											,house_pricing_1$GarageYrBlt>=1900 & house_pricing_1$GarageYrBlt<1961  ~ "1900-1961"
                                            ,house_pricing_1$GarageYrBlt>=1961 & house_pricing_1$GarageYrBlt<1980  ~ "1961-1980"
                                            ,house_pricing_1$GarageYrBlt>=1980 & house_pricing_1$GarageYrBlt<2002  ~ "1980-2002"
                                            ,house_pricing_1$GarageYrBlt>=2002   ~ ">=2002")
house_pricing_1 <- data.frame(house_pricing_1, GarageYrBlt_Range)


##LotFrontage_Rank

LotFrontage_Rank <- case_when(house_pricing_1$LotFrontage == 0 ~ "0"
											,house_pricing_1$LotFrontage>=21 & house_pricing_1$LotFrontage<59  ~ "1"
                                            ,house_pricing_1$LotFrontage>=59 & house_pricing_1$LotFrontage<69  ~ "2"
                                            ,house_pricing_1$LotFrontage>=69 & house_pricing_1$LotFrontage<80  ~ "3"
                                            ,house_pricing_1$LotFrontage>=80   ~ "4")
house_pricing_1 <- data.frame(house_pricing_1, LotFrontage_Rank)

##TEST

#str(house_pricing_1)

##LotArea_Rank

LotArea_Rank <- case_when(house_pricing_1$LotArea>=1300 & house_pricing_1$LotArea <7554  ~ "1"
                                            ,house_pricing_1$LotArea>=7554 & house_pricing_1$LotArea <9478  ~ "2"
                                            ,house_pricing_1$LotArea>=9478 & house_pricing_1$LotArea <11602  ~"3"
                                            ,house_pricing_1$LotArea>= 11602   ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, LotArea_Rank)

##TEST

##str(house_pricing_1)


##BsmtUnfSF_Rank

BsmtUnfSF_Rank <- case_when(house_pricing_1$BsmtUnfSF>=0 & house_pricing_1$BsmtUnfSF < 223 ~ "1"
                                        ,house_pricing_1$BsmtUnfSF>= 223 & house_pricing_1$BsmtUnfSF <478  ~ "2"
                                       ,house_pricing_1$BsmtUnfSF>=478 & house_pricing_1$BsmtUnfSF <808  ~"3"
                                      ,house_pricing_1$BsmtUnfSF>=808  ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, BsmtUnfSF_Rank)

##TEST

#str(house_pricing_1)


##TotalBsmtSF_Rank

TotalBsmtSF_Rank <- case_when(house_pricing_1$TotalBsmtSF>=0 & house_pricing_1$TotalBsmtSF <796  ~ "1"
                                      ,house_pricing_1$TotalBsmtSF>=796 & house_pricing_1$TotalBsmtSF <992 ~ "2"
                                     ,house_pricing_1$TotalBsmtSF>=992 & house_pricing_1$TotalBsmtSF <1298  ~"3"
                                    ,house_pricing_1$TotalBsmtSF>=1298   ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, TotalBsmtSF_Rank)

##TEST
#str(house_pricing_1)


##X1stFlrSF_Rank

X1stFlrSF_Rank <- case_when(house_pricing_1$X1stFlrSF>=334 & house_pricing_1$X1stFlrSF <882  ~ "1"
                                         ,house_pricing_1$X1stFlrSF>=882 & house_pricing_1$X1stFlrSF <1087 ~ "2"
                                         ,house_pricing_1$X1stFlrSF>=1087 & house_pricing_1$X1stFlrSF <1391  ~"3"
                                        ,house_pricing_1$X1stFlrSF>=1391  ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, X1stFlrSF_Rank)

##TEST
#str(house_pricing_1)



##GrLivArea_Rank

GrLivArea_Rank <- case_when(house_pricing_1$GrLivArea>=334 & house_pricing_1$GrLivArea <1130  ~ "1"
                                         ,house_pricing_1$GrLivArea>=1130 & house_pricing_1$GrLivArea <1464  ~ "2"
                                         ,house_pricing_1$GrLivArea>=1464 & house_pricing_1$GrLivArea <1777 ~"3"
                                        ,house_pricing_1$GrLivArea>=1777   ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, GrLivArea_Rank)

##TEST
#str(house_pricing_1)



##GarageArea_Rank

GarageArea_Rank <- case_when(house_pricing_1$GarageArea>=0 & house_pricing_1$GarageArea <335  ~ "1"
                                        ,house_pricing_1$GarageArea>=335 & house_pricing_1$GarageArea <480  ~ "2"
                                       ,house_pricing_1$GarageArea>=480 & house_pricing_1$GarageArea <576 ~"3"
                                      ,house_pricing_1$GarageArea>=576  ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, GarageArea_Rank)

##TEST
#str(house_pricing_1)



##SalePrice_Rank

SalePrice_Rank <- case_when(house_pricing_1$SalePrice>=34900 & house_pricing_1$SalePrice <129975  ~ "1"
                                ,house_pricing_1$SalePrice>=129975 & house_pricing_1$SalePrice <163000  ~ "2"
                                  ,house_pricing_1$SalePrice>= 163000 & house_pricing_1$SalePrice <214000  ~"3"
                                   ,house_pricing_1$SalePrice>=214000 ~"4"
                                            )
house_pricing_1 <- data.frame(house_pricing_1, SalePrice_Rank)

##TEST
#str(house_pricing_1)

######### HANDLING CONTINUOUS VARIABLES WITH LESS VALUES AND CREATING FACTORS / CATEGORICAL VARIABLES ######

##-- Following code converts the data into flags representing data present or not.

##MasVnrArea_flg 

MasVnrArea_flg <- case_when(house_pricing_1$MasVnrArea == 0  ~ "0"
                                ,house_pricing_1$MasVnrArea > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, MasVnrArea_flg)

## TEST
#str(house_pricing_1)

##BsmtFinSF2_flg 

BsmtFinSF2_flg <- case_when(house_pricing_1$BsmtFinSF2 == 0  ~ "0"
                                ,house_pricing_1$BsmtFinSF2 > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, BsmtFinSF2_flg)

## TEST
#str(house_pricing_1)


##X2ndFlrSF_flg 
X2ndFlrSF_flg <- case_when(house_pricing_1$X2ndFlrSF == 0  ~ "0"
                                ,house_pricing_1$X2ndFlrSF > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, X2ndFlrSF_flg)

## TEST
#str(house_pricing_1)


##LowQualFinSF_flg 

LowQualFinSF_flg <- case_when(house_pricing_1$LowQualFinSF == 0  ~ "0"
                                ,house_pricing_1$LowQualFinSF > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, LowQualFinSF_flg)

## TEST
#str(house_pricing_1)


##WoodDeckSF_flg 

WoodDeckSF_flg <- case_when(house_pricing_1$WoodDeckSF == 0  ~ "0"
                                ,house_pricing_1$WoodDeckSF > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, WoodDeckSF_flg)

## TEST
#str(house_pricing_1)


##OpenPorchSF_flg 
OpenPorchSF_flg <- case_when(house_pricing_1$OpenPorchSF == 0  ~ "0"
                                ,house_pricing_1$OpenPorchSF > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, OpenPorchSF_flg)

## TEST
#str(house_pricing_1)


##EnclosedPorch_flg 

EnclosedPorch_flg <- case_when(house_pricing_1$EnclosedPorch == 0  ~ "0"
                                ,house_pricing_1$EnclosedPorch > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, EnclosedPorch_flg)

## TEST
#str(house_pricing_1)


##X3SsnPorch_flg 
X3SsnPorch_flg <- case_when(house_pricing_1$X3SsnPorch == 0  ~ "0"
                                ,house_pricing_1$X3SsnPorch > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, X3SsnPorch_flg)

## TEST
#str(house_pricing_1)


##ScreenPorch_flg 
ScreenPorch_flg <- case_when(house_pricing_1$ScreenPorch == 0  ~ "0"
                                ,house_pricing_1$ScreenPorch > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, ScreenPorch_flg)

## TEST
#str(house_pricing_1)

##PoolArea_flg 
PoolArea_flg <- case_when(house_pricing_1$PoolArea == 0  ~ "0"
                                ,house_pricing_1$PoolArea > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, PoolArea_flg)

## TEST
#str(house_pricing_1)

##MiscVal_flg 
MiscVal_flg <- case_when(house_pricing_1$MiscVal == 0  ~ "0"
                                ,house_pricing_1$MiscVal > 0 ~ "1"
                                 )
house_pricing_1 <- data.frame(house_pricing_1, MiscVal_flg)

## TEST
#str(house_pricing_1)


######################################### ALGORITHM BUILD ###########################################################


############################### VARIABLE REDUCTION #####################

##--CHI-Square Test is used to fingure out the dependency of factors of the dependent variable.
##--2 types of variables were removed:
	##--Variables that did not produce results in Chi Square Test as they were giving 0 expected frequency in contingency matrix, hence showing 		that the do not explain the dependant variable well.
	##--Variables that have P-value grater than 0.05, as in this case we were unable to rejetct the NULL Hypothesis that the factor is independant 		of the dependent variable

##--Getting factors out of the dataset
pre_algo_housing_prices <- select_if(house_pricing_1,is.factor)

##Test
#str(pre_algo_housing_prices)

##--Following code runs the Chi-Square Test for all the columns against the dependent variable SalePrice_Rank.

colmns_vReduce <- colnames(pre_algo_housing_prices)
for (i in 1:length(colmns_vReduce)){
tbl <- table(eval(parse(text=gsub(" ","",paste("pre_algo_housing_prices","$", colmns_vReduce[i])))),eval(parse(text=gsub(" ","",paste("pre_algo_housing_prices","$", colmns_vReduce[69])))))
print(colmns_vReduce[i])
print(chisq.test(tbl))
}

##TEST
#tbl <- table(eval(parse(text=gsub(" ","",paste("train","$", colmns_vReduce[1])))),eval(parse(text=gsub(" ","",paste("train","$", colmns_vReduce[69])))))
#tbl


########## DATA PREPARATION FOR PREDICTION. ###############
##--The dataset was split into: 
	##--70% Training data which was used to build the model.
	##--30% Testing data which was used to test the prediction.

## GENERATING TRAINING SET
train <- pre_algo_housing_prices[1:(0.70 * NROW(pre_algo_housing_prices)),] 

## GENERATING TESTING SET
testdata<- pre_algo_housing_prices[1:(0.30 * NROW(pre_algo_housing_prices)),] 

################ DECISION TREE #########################

##--Using "class" decision tree as all the columns are factors.
##--Maximum depth was kept at 30 to prevent overfitting.	
##--Variables used in the model are the ones that had produced the Chi-Square results and P-value was less than 0.05 as in this case we were unable to alternet Hypothesis i.e. the independent and the dependent variables are dependent on each other.

model <- rpart(SalePrice_Rank ~  Alley+ MasVnrType+ BsmtQual+ BsmtCond+ BsmtExposure+ BsmtFinType1+ BsmtFinType2
								+Electrical+ FireplaceQu+ GarageType+ GarageFinish+ GarageQual
								+ MoSold_Range+ YearBuilt_Range+ YearRemodAdd_Range+ GarageYrBlt_Range
								+ GarageCond+ Fence+ LotFrontage_Rank+LotArea_Rank
								+BsmtUnfSF_Rank+TotalBsmtSF_Rank+X1stFlrSF_Rank+GrLivArea_Rank+GarageArea_Rank
								+MasVnrArea_flg+ BsmtFinSF2_flg+X2ndFlrSF_flg+LowQualFinSF_flg+WoodDeckSF_flg
								+OpenPorchSF_flg+EnclosedPorch_flg+ScreenPorch_flg
, method = "class", data=train, control=rpart.control(minsplit=1,maxdepth=30),parms=list(split='information'))


################ PREDICTIONS #########################

##-- Predicting future class values for the SalePrice_Rank variable.
predicted_Values <- predict(model,  type="class") ## Using class because we are trying to predict a factor.

##--Following code prepares test data for Checking the predicted values for classification errors

testData_pred<-data.frame(testdata$Alley, testdata$MasVnrType, testdata$BsmtQual, testdata$BsmtCond,testdata$BsmtExposure,testdata$BsmtFinType1,testdata$BsmtFinType2
						,testdata$Electrical,testdata$FireplaceQu,testdata$GarageType,testdata$GarageFinish,testdata$GarageQual
						,testdata$MoSold_Range,testdata$YearBuilt_Range,testdata$YearRemodAdd_Range,testdata$GarageYrBlt_Range
						,testdata$GarageCond,testdata$Fence,testdata$LotFrontage_Rank,testdata$LotArea_Rank
					  ,testdata$BsmtUnfSF_Rank,testdata$TotalBsmtSF_Rank,testdata$X1stFlrSF_Rank,testdata$GrLivArea_Rank,testdata$GarageArea_Rank
						,testdata$MasVnrArea_flg,testdata$BsmtFinSF2_flg,testdata$X2ndFlrSF_flg,testdata$LowQualFinSF_flg,testdata$WoodDeckSF_flg
						,testdata$OpenPorchSF_flg,testdata$EnclosedPorch_flg,testdata$ScreenPorch_flg, testdata$SalePrice_Rank)


colmns_pred <- c("Alley", "MasVnrType","BsmtQual" ,"BsmtCond", "BsmtExposure" ,"BsmtFinType1","BsmtFinType2"
								,"Electrical", "FireplaceQu" ,"GarageType", "GarageFinish", "GarageQual"
								," MoSold_Range","YearBuilt_Range", "YearRemodAdd_Range", "GarageYrBlt_Range"
								,"GarageCond", "Fence", "LotFrontage_Rank","LotArea_Rank"
								,"BsmtUnfSF_Rank","TotalBsmtSF_Rank","X1stFlrSF_Rank","GrLivArea_Rank","GarageArea_Rank"
								,"MasVnrArea_flg"," BsmtFinSF2_flg","X2ndFlrSF_flg","LowQualFinSF_flg","WoodDeckSF_flg"
								,"OpenPorchSF_flg","EnclosedPorch_flg","ScreenPorch_flg", "SalePrice_Rank")

colnames(testData_pred)<-colmns_pred ## Assigning Column Names
									

############## ERROR RATE CALCULATION ###########

##--Following code combines the predicted values of SalePrice_Rank to the test data.

testData_pred_Err <- data.frame(testData_pred,predicted_Values=predicted_Values[1:438])

##--Following code identifies the predicted values that matched or did not match with SalePrice_Rank test data values.

testData_pred_Err$Match_No_Match <- as.factor(case_when(testData_pred_Err$SalePrice_Rank == testData_pred_Err$predicted_Values ~ "Match"
												,testData_pred_Err$SalePrice_Rank != testData_pred_Err$predicted_Values ~ "No Match"))

##--To calculate Error %age

summary(testData_pred_Err$Match_No_Match)

##--No Match %age:
(158/(280+158))*100

##--NOTE: The predictions were made with a 36% error rate.

##--Printing Actual predicted ranges:

SalePrice_ranges_predicted <-case_when(testData_pred_Err$predicted_Values == "1" ~ "34900-129975"
                                 ,testData_pred_Err$predicted_Values == "2" ~ "129975-163000"
                                  ,testData_pred_Err$predicted_Values == "3" ~"163000-214000"
                                  ,testData_pred_Err$predicted_Values == "4" ~">=214000"
                                            )
testData_pred_Err <- data.frame(testData_pred_Err, SalePrice_ranges_predicted)


##Test
#head(testData_pred_Err)

#summary(testData_pred_Err)

























