

#As a statistical consultant working for a real estate investment firm, your task is to
#develop a model to predict the selling price of a given home in Ames, Iowa. 
#Your employer hopes to use this information to help assess whether the asking price of 
#a house is higher or lower than the true value of the house. 
#If the home is undervalued, it may be a good investment for the firm.



load("C:/Users/sanjay/Desktop/Extras/R Capstone/ames_train.Rdata")
attach(ames_train)


library(statsr)
library(dplyr)
library(BAS)
library(MASS)
library(rbokeh)
library(visreg)
library(plotly)
library(sjlabelled)
library(CGPfunctions)
```



ames_train1<-ames_train
z <- lm(log(price) ~ log(area), data = ames_train1)
figure(title = " Area vs Price by Sales Condition", width = 750,
       height = 450, legend_location = "top_left") %>%
  ly_points(log(area), log(price), data=ames_train1,
            color = Sale.Condition, glyph = Sale.Condition,
            hover = list(Neighborhood, price, Overall.Qual))%>%
  ly_abline(z,color = "red",  width = 1, alpha = 0.5) 


#Area size and price are log transformed so as to analyze the relative change 
#between them.

#Graph of log(area) vs log(price) by Sale.Condition is visualized above, it can 
#be clearly figured out that except for Normal Sales other Sales are not significant 
#enough for designing the model. 
#The Normal Sales is well distributed and doesn't possess much of the skewness. 
#Thus filtering out the Normal Sales alone for furthur analysing 


ames_train1 <- ames_train %>% filter(Sale.Condition == "Normal")

#Moving on to the Second important visualization,


housesPrice_lm <- lm(price ~ Bldg.Type + Neighborhood + BsmtFin.Type.1+BsmtFin.Type.2+
                       area + Lot.Area + Total.Bsmt.SF +Bsmt.Qual + Fireplace.Qu+
                       Garage.Area, 
                     data = ames_train1)
visreg(housesPrice_lm, "Bldg.Type", gg = TRUE) +
  scale_y_continuous(label = scales::dollar) +
  labs(title = "Relationship between price and Building Type",
       subtitle = "controlling for lot size, age, land value, bedrooms and bathrooms",
       y = "Home Price",
       x = "")

#As far as Single Family house, Townhouse end unit and Townhouse inside unit the 
#distribution in all the three Bldg.type falls within the the specified price range 
#and doesn't deviate much whereas in terms of Two-family Conversion and Duplex 
#house the distribution seems much deviated from the normal. 

#Hence to avoid complexity the data to be analyzed will be filtered with only 
#1Fam, Twnhs and TwnhsE.  


ames_train1 <- ames_train %>% filter((Bldg.Type == "1Fam")|(Bldg.Type == "Twnhs")|(Bldg.Type == "TwnhsE"))


#Yet another finding in regards with Bldg.type was found, which is, the change in price range of the type of houses sold over the years.


median1<-ames_train1 %>% group_by(Bldg.Type,Yr.Sold) %>% summarise(med = median(price, na.rm = F))
median1$Yr.Sold<-as_factor(median1$Yr.Sold)
newggslopegraph(median1, Yr.Sold,med, Bldg.Type) +
  labs(title="Change in Price over the years", subtitle = "", caption = "") 



#There is been a heavy decline in prices for Townhouse end unit of about *18.3%* decrease over a five-year trend-line and that 
#too it is highly recommended not to invest in TwnhsE as the prices seems to be in downward slope all the while.

#On the other hand Twnhs has seen an increase of *69.6%* in price over the five-trend line but as the price fluctuation of Twnhs is 
#not a steady rise it again is recommended to take careful step before investing in Twnhs.

#Of all the three the *most recommended* will be Single family house, this type of house has not seen any sudden surge or any sudden 
#slump and has always maintained its market position.



## Part 2 - Development and assessment of an initial model, following a semi-guided process of analysis

### Section 2.1 An Initial Model

#Before designing the Initial model the data is better to be cleaned of missing values. Since there are around 
#14 variables like Pool.QC, BsmtFin.Type.1, BsmtFin.Type.2, Garage.Qual and so on with *NA* as one of their levels the system confuses 
#it as missing value and doesn't consider it as a level. Therefore, as a first step all the NA levels of the variables are transformed 
#into a new level termed as *None*. 


levels0 <- levels(ames_train1$Pool.QC)
levels0[length(levels0) + 1] <- "None"
ames_train1$Pool.QC <- factor(ames_train1$Pool.QC, levels = levels0)
ames_train1$Pool.QC[is.na(ames_train1$Pool.QC)] <- "None"

levels1 <- levels(ames_train1$Misc.Feature)
levels1[length(levels1) + 1] <- "None"
ames_train1$Misc.Feature <- factor(ames_train1$Misc.Feature, levels = levels1)
ames_train1$Misc.Feature[is.na(ames_train1$Misc.Feature)] <- "None"

levels2 <- levels(ames_train1$Fence)
levels2[length(levels2) + 1] <- "None"
ames_train1$Fence <- factor(ames_train1$Fence, levels = levels2)
ames_train1$Fence[is.na(ames_train1$Fence)] <- "None"

levels3 <- levels(ames_train1$Garage.Type)
levels3[length(levels3) + 1] <- "None"
ames_train1$Garage.Type <- factor(ames_train1$Garage.Type, levels = levels3)
ames_train1$Garage.Type[is.na(ames_train1$Garage.Type)] <- "None"

levels4 <- levels(ames_train1$Garage.Finish)
levels4[length(levels4) + 1] <- "None"
ames_train1$Garage.Finish <- factor(ames_train1$Garage.Finish, levels = levels4)
ames_train1$Garage.Finish[is.na(ames_train1$Garage.Finish)] <- "None"

levels5 <- levels(ames_train1$Garage.Cond)
levels5[length(levels5) + 1] <- "None"
ames_train1$Garage.Cond <- factor(ames_train1$Garage.Cond, levels = levels5)
ames_train1$Garage.Cond[is.na(ames_train1$Garage.Cond)] <- "None"

levels6 <- levels(ames_train1$Garage.Qual)
levels6[length(levels6) + 1] <- "None"
ames_train1$Garage.Qual <- factor(ames_train1$Garage.Qual, levels = levels6)
ames_train1$Garage.Qual[is.na(ames_train1$Garage.Qual)] <- "None"

levels7 <- levels(ames_train1$Bsmt.Qual)
levels7[length(levels7) + 1] <- "None"
ames_train1$Bsmt.Qual <- factor(ames_train1$Bsmt.Qual, levels = levels7)
ames_train1$Bsmt.Qual[is.na(ames_train1$Bsmt.Qual)] <- "None"

levels8 <- levels(ames_train1$Bsmt.Cond)
levels8[length(levels8) + 1] <- "None"
ames_train1$Bsmt.Cond <- factor(ames_train1$Bsmt.Cond, levels = levels8)
ames_train1$Bsmt.Cond[is.na(ames_train1$Bsmt.Cond)] <- "None"

levels9 <- levels(ames_train1$Bsmt.Exposure)
levels9[length(levels9) + 1] <- "None"
ames_train1$Bsmt.Exposure <- factor(ames_train1$Bsmt.Exposure, levels = levels9)
ames_train1$Bsmt.Exposure[is.na(ames_train1$Bsmt.Exposure)] <- "None"

levels10 <- levels(ames_train1$BsmtFin.Type.1)
levels10[length(levels10) + 1] <- "None"
ames_train1$BsmtFin.Type.1 <- factor(ames_train1$BsmtFin.Type.1, levels = levels10)
ames_train1$BsmtFin.Type.1[is.na(ames_train1$BsmtFin.Type.1)] <- "None"

levels11 <- levels(ames_train1$BsmtFin.Type.2)
levels11[length(levels11) + 1] <- "None"
ames_train1$BsmtFin.Type.2 <- factor(ames_train1$BsmtFin.Type.2, levels = levels11)
ames_train1$BsmtFin.Type.2[is.na(ames_train1$BsmtFin.Type.2)] <- "None"

levels12 <- levels(ames_train1$Fireplace.Qu)
levels12[length(levels12) + 1] <- "None"
ames_train1$Fireplace.Qu <- factor(ames_train1$Fireplace.Qu, levels = levels12)
ames_train1$Fireplace.Qu[is.na(ames_train1$Fireplace.Qu)] <- "None"

levels13 <- levels(ames_train1$Alley)
levels13[length(levels13) + 1] <- "None"
ames_train1$Alley <- factor(ames_train1$Alley, levels = levels13)
ames_train1$Alley[is.na(ames_train1$Alley)] <- "None"

#Once the data is cleaned, we can then proceed to design the initial model without any hassle,


ames_train1<-ames_train1%>%
  mutate(Age = 2020-Year.Built)
ames_train1 <- ames_train1 %>% filter(Sale.Condition == "Normal")
ames_train1 <- ames_train1%>% filter((Bldg.Type == "1Fam")|(Bldg.Type == "Twnhs")|(Bldg.Type == "TwnhsE"))


initial_model <- lm(log(price) ~ Age+ Overall.Qual  + log(area) + Lot.Area+
                      Total.Bsmt.SF + Garage.Area + Yr.Sold+ Neighborhood+ Garage.Qual+BsmtFin.Type.1+BsmtFin.Type.2, data = ames_train1 )
summary(initial_model)


#The initial model designed with the fewer variables considered to be important produces a model with adjusted R square of 0.9112. 
#As the value of adjusted R square is quite high henceforth initial model only can be realized as the best fit for the dataset. 
#To furthur conform the quality of the model StepAIC is performed in the next step.


### Section 2.2 Model Selection


initial_model_BIC <- stepAIC(initial_model, k = log(790))
summary(initial_model_BIC)

#By StepAIC selection we can conclude that the initial model confines to only 8 out of 11 variables and thus excludes 
#Garage.Qual, Yr.Sold and BsmtFin.Type.2 variables. The calibre of the model can be verified by finding their RMSE value which is done in the later steps.

BIC(initial_model)

#The BIC of the initial model is around -893, therefore removing the variables possessing the higher P-values, which are Garage.Qual and BsmtFin.Type.1 
#and thus designed a new initial_model_1 with the remaining independent variables.

initial_model_1 <- lm(log(price) ~ Age+ Overall.Qual  + log(area) + Lot.Area+
                        Total.Bsmt.SF + Garage.Area + Yr.Sold +Neighborhood +BsmtFin.Type.2, data = ames_train1 )
summary(initial_model_1)
BIC(initial_model_1)

#As expected the BIC value got reduced significantly and hence intial_model_1 shall be selected as the intial model.


### Section 2.3 Initial Model Residuals
par(mfrow = c(2,2))
plot(initial_model_1, which = 1, pch = 16, cex = 0.7, col = "coral2")
plot(initial_model_1, which = 2, pch = 16, cex = 0.7, col = "firebrick")
hist(initial_model_1$residuals, col = "royalblue")
plot(initial_model_1$residuals, pch = 16, cex = 0.7, col = "slateblue1")

#All the residual plots be it Normall Q-Q or histogram of initial_model_1 show-case a normal distribution and hence there is no need for any worrying.

predict.full.train <- exp(predict(initial_model_1, ames_train1))
resid.full.train <- ames_train1$price - predict.full.train
rmse.full.train <- sqrt(mean(resid.full.train^2, na.rm = TRUE))
rmse.full.train

load("C:/Users/sanjay/Desktop/Extras/R Capstone/ames_test.Rdata")
ames_test1<-ames_test
ames_test1<-ames_test1%>%
  mutate(Age = 2020-Year.Built)
ames_test1 <- ames_test1 %>% filter(Sale.Condition == "Normal")
ames_test1 <- ames_test1 %>% filter((Bldg.Type == "1Fam")|(Bldg.Type == "Twnhs")|(Bldg.Type == "TwnhsE"))
ames_test1 <- ames_test1 %>% filter(!(Neighborhood == "Landmrk"))

pred_initialmodel_test <- exp(predict(initial_model_1, ames_test1))
resid_initialmodel_test <- ames_test1$price - pred_initialmodel_test
RMSE_initialmodel_test <- sqrt(mean(resid_initialmodel_test^2, na.rm = T))                
RMSE_initialmodel_test

#The RMSE of the initial_model_1 with train set is 21k and RMSE of initial_model_1 with test set is 22k.
#Looking at the RMSE value it is conformed that the initial_model_1 designed is a best fit for both the traina and test data set but since not all the
#important variables are included in the initial model we thus move on for designing the final_model.

final_Model<-lm(log(price) ~ Age + log(area) + Overall.Qual + log(Lot.Area) + Garage.Area + BsmtFin.SF.1  + Year.Remod.Add + Neighborhood+
                  Fireplace.Qu +Overall.Cond + Full.Bath + Neighborhood + Pool.QC +Fence+Garage.Type, data = ames_train1)
summary(final_Model)

finalmodel_BIC <- stepAIC(final_Model, k = log(790))
summary(finalmodel_BIC)

#Upon several removal and addition of variables the model was designed and the final model tends to possess  an higher adjusted R-square, 
#an adjusted R-Square of 0.9295 indicates the goodness-of-fit of the designed final regression model. 
par(mfrow = c(2,2))
plot(finalmodel_BIC, which = 1, pch = 16, cex = 0.7, col = "coral1")
plot(finalmodel_BIC, which = 2, pch = 16, cex = 0.7, col = "firebrick1")
hist(finalmodel_BIC$residuals, col = "royalblue")
plot(finalmodel_BIC$residuals, pch = 16, cex = 0.7, col = "slateblue1")

#The residual plot conforms that the variance is constant to a major portion and also that they are well normally distributed, the normal Q-Q plotted 
#tells that the data is only lightly skewed and that it won't cause any problem for the fit or prediction.

pred_finalmodel_train <- exp(predict(finalmodel_BIC, ames_train1))
resid_finalmodel_train <- ames_train1$price - pred_finalmodel_train
RMSE_finalmodel_train <- sqrt(mean(resid_finalmodel_train^2))                
RMSE_finalmodel_train

#RMSE of training dataset for finalmodel---> 18375

pred_finalmodel_test <- exp(predict(finalmodel_BIC, ames_test1))
resid_finalmodel_test <- ames_test1$price - pred_finalmodel_test
RMSE_finalmodel_test <- sqrt(mean(resid_finalmodel_test^2, na.rm = T))                
RMSE_finalmodel_test

#RMSE of testing dataset for finalmodel---> 20,651

#With lower RMSE than the initial model, the final model seems to be the best fit for testing data as well.

load("C:/Users/sanjay/Desktop/Extras/R Capstone/ames_validation.Rdata")
ames_validation1<-ames_validation%>%
  mutate(Age = 2020-Year.Built)

pred_finalmodel_test <- exp(predict(finalmodel_BIC, ames_validation1))
resid_finalmodel_test <- ames_validation1$price - pred_finalmodel_test
RMSE_finalmodel_test <- sqrt(mean(resid_finalmodel_test^2, na.rm = T))                
RMSE_finalmodel_test

#RMSE value of validation dataset is 19,951.

predict.finalmodel.validation <- exp(predict(finalmodel_BIC, ames_validation1, interval = 'prediction'))
coverage.finalmodel.validation <-
  mean(ames_validation1$price > predict.finalmodel.validation[,'lwr'] &
         ames_validation1$price < predict.finalmodel.validation[,'upr'])
coverage.finalmodel.validation

#With 95.1% coverage, it is hence proved that 95.1% of the time the predicted price falls within the 95% credible interval of the dataset. 

#Question of model's handling of uncertainity should be doubted only if the coverage is below 95%, which is not the case in the final model designed. 
