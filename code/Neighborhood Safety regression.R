install.packages('MASS')
install.packages("corrplot")
install.packages("e1071")
library(MASS)
library(corrplot)
library(e1071) 

#Input data
housing<-read.csv("CT_housing.csv")[-c(1,2,21)]
names(housing)[1]<-'price'
airbnb<-read.csv("CT_airbnb.csv")[-c(1,20)]
#-----------------------Correlation Analysis----------------------------
#Housing data correlation analysis
cor_numVar <- cor(housing)
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))#sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))#select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#Airbnb data correlation analysis
cor_numVar <- cor(airbnb)
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))#sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))#select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
#----------------------------Skewness------------------------------------
#take log for housing data to solve skewness
for(i in 1:ncol(housing)){
  if (abs(skewness(housing[,i]))>0.8){
    print(i)
    print(abs(skewness(housing[,i])))
    housing[,i] <- log(housing[,i] +1)
  }
}
#----------------------stepwise variable selection------------------------
# Fit the full model 
airbnb.model <- lm(price ~., data =airbnb)
housing.model <- lm(price ~., data =housing)
# Stepwise regression model
air_step.model <-stepAIC(airbnb.model, direction = "both", 
                     trace = FALSE)
hou_step.model <-stepAIC(housing.model, direction = "both", 
                         trace = FALSE)
summary(air_step.model)
summary(hou_step.model)
#---------------Selected crime types correlation analysis-----------------
#Airbnb data selected crime types correlation analysis 

airbnb_sel <- airbnb[c('price','Sum_Commer','Sum_Firear','Sum_Search','Sum_Simple','Sum_Vandal')]
cor_numVar <- cor(airbnb_sel)
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))#sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))#select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#Housing data selected crime types correlation analysis 
housing_sel <- housing[c('price','Sum_Assemb','Sum_Bomb_H','Sum_Commer','Sum_Disord','Sum_Drug_V','Sum_Homici','Sum_Offens','Sum_Vandal','Sum_Reside')]
cor_numVar <- cor(housing_sel)
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))#sort on decreasing correlations with SalePrice
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.01)))#select only high corelations
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")



