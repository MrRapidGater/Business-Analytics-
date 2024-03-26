install.packages("dplyr")
library(dplyr)
install.packages("caret")
library(caret)
install.packages("ggplot2")
library(ggplot2)
install.packages("caTools")
library(caTools)
install.packages("Amelia")
library(Amelia)
install.packages("MASS")
library(MASS)

library(caTools)

### LOADING DATASET
initial_data <- read.csv("application_data.csv")
summary(initial_data)

### CLEANING DATASET
clean <- subset(initial_data, select = -c( NONLIVINGAREA_MODE, OWN_CAR_AGE, EXT_SOURCE_1, APARTMENTS_AVG, BASEMENTAREA_AVG, YEARS_BEGINEXPLUATATION_AVG, YEARS_BUILD_AVG, COMMONAREA_AVG, ELEVATORS_AVG, ENTRANCES_AVG, FLOORSMAX_AVG, FLOORSMIN_AVG, LANDAREA_AVG, LIVINGAPARTMENTS_AVG, LIVINGAREA_AVG, NONLIVINGAPARTMENTS_AVG, NONLIVINGAREA_AVG, APARTMENTS_MODE, BASEMENTAREA_MODE, YEARS_BEGINEXPLUATATION_MODE, YEARS_BUILD_MODE, COMMONAREA_MODE, ELEVATORS_MODE, ENTRANCES_MODE, FLOORSMAX_MODE, FLOORSMIN_MODE, LANDAREA_MODE, LIVINGAPARTMENTS_MODE, LIVINGAREA_MODE, NONLIVINGAPARTMENTS_MODE, APARTMENTS_MEDI, BASEMENTAREA_MEDI, YEARS_BEGINEXPLUATATION_MEDI, YEARS_BUILD_MEDI, COMMONAREA_MEDI, ELEVATORS_MEDI, ENTRANCES_MEDI, FLOORSMAX_MEDI, FLOORSMIN_MEDI, LANDAREA_MEDI, LIVINGAPARTMENTS_MEDI, LIVINGAREA_MEDI, NONLIVINGAPARTMENTS_MEDI, NONLIVINGAREA_MEDI, FONDKAPREMONT_MODE, HOUSETYPE_MODE, TOTALAREA_MODE, WALLSMATERIAL_MODE, TOTALAREA_MODE, WALLSMATERIAL_MODE, EMERGENCYSTATE_MODE))
clean <- subset(clean, select = -c( FLAG_DOCUMENT_2, FLAG_DOCUMENT_3, FLAG_DOCUMENT_4, FLAG_DOCUMENT_5, FLAG_DOCUMENT_6, FLAG_DOCUMENT_7, FLAG_DOCUMENT_8, FLAG_DOCUMENT_9, FLAG_DOCUMENT_10, FLAG_DOCUMENT_11, FLAG_DOCUMENT_12, FLAG_DOCUMENT_13, FLAG_DOCUMENT_14, FLAG_DOCUMENT_15, FLAG_DOCUMENT_16, FLAG_DOCUMENT_17, FLAG_DOCUMENT_18, FLAG_DOCUMENT_19, FLAG_DOCUMENT_20, FLAG_DOCUMENT_21))
clean <- subset(clean, select = -c( FLAG_EMP_PHONE, FLAG_WORK_PHONE, FLAG_CONT_MOBILE, FLAG_PHONE, REGION_RATING_CLIENT, REGION_RATING_CLIENT_W_CITY, OBS_30_CNT_SOCIAL_CIRCLE, DEF_30_CNT_SOCIAL_CIRCLE, OBS_60_CNT_SOCIAL_CIRCLE, DEF_60_CNT_SOCIAL_CIRCLE, AMT_REQ_CREDIT_BUREAU_HOUR, AMT_REQ_CREDIT_BUREAU_DAY, AMT_REQ_CREDIT_BUREAU_WEEK, AMT_REQ_CREDIT_BUREAU_MON, AMT_REQ_CREDIT_BUREAU_QRT, AMT_REQ_CREDIT_BUREAU_YEAR, NAME_TYPE_SUITE, REGION_POPULATION_RELATIVE, WEEKDAY_APPR_PROCESS_START, HOUR_APPR_PROCESS_START, REG_REGION_NOT_LIVE_REGION, REG_REGION_NOT_WORK_REGION, LIVE_REGION_NOT_WORK_REGION, REG_CITY_NOT_LIVE_CITY, REG_CITY_NOT_WORK_CITY, LIVE_CITY_NOT_WORK_CITY, SK_ID_CURR))

summary(clean)

str(clean)

clean$DAYS_BIRTH <- abs(clean$DAYS_BIRTH)
clean$DAYS_EMPLOYED <- abs(clean$DAYS_EMPLOYED)
clean$DAYS_REGISTRATION <- abs(clean$DAYS_REGISTRATION)
clean$DAYS_ID_PUBLISH <- abs(clean$DAYS_ID_PUBLISH)
clean$DAYS_LAST_PHONE_CHANGE <- abs(clean$DAYS_LAST_PHONE_CHANGE)

str(clean)

### REMOVING NULL VALUES
clean$ORGANIZATION_TYPE[clean$ORGANIZATION_TYPE == "XNA"] <- "Unknown"
clean$OCCUPATION_TYPE[clean$OCCUPATION_TYPE == ""] <- "UNKNOWN"

clean_data <- clean[!(is.na(clean$AMT_ANNUITY)|is.na(clean$AMT_GOODS_PRICE)|is.na(clean$CNT_FAM_MEMBERS)|is.na(clean$DAYS_LAST_PHONE_CHANGE)|clean$CODE_GENDER == "XNA"),]

summary(clean_data)

### IDENTIFYING OUTLIERS BY BOXPLOTS
boxplot(clean_data$CNT_CHILDREN)
boxplot(clean_data$AMT_INCOME_TOTAL)
boxplot(clean_data$AMT_CREDIT)
boxplot(clean_data$AMT_ANNUITY)
boxplot(clean_data$AMT_GOODS_PRICE)
boxplot(clean_data$DAYS_BIRTH)
boxplot(clean_data$DAYS_EMPLOYED)
boxplot(clean_data$DAYS_REGISTRATION)
boxplot(clean_data$DAYS_ID_PUBLISH)
boxplot(clean_data$DAYS_LAST_PHONE_CHANGE)
boxplot(clean_data$CNT_FAM_MEMBERS)

### REMOVING OUTLIERS
outliers <- boxplot(clean_data$CNT_CHILDREN, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$CNT_CHILDREN %in% outliers),]

outliers <- boxplot(clean_data$AMT_INCOME_TOTAL, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$AMT_INCOME_TOTAL %in% outliers),]

outliers <- boxplot(clean_data$AMT_CREDIT, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$AMT_CREDIT %in% outliers),]

outliers <- boxplot(clean_data$AMT_ANNUITY, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$AMT_ANNUITY %in% outliers),]

outliers <- boxplot(clean_data$AMT_GOODS_PRICE, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$AMT_GOODS_PRICE %in% outliers),]

outliers <- boxplot(clean_data$DAYS_BIRTH, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$DAYS_BIRTH %in% outliers),]

outliers <- boxplot(clean_data$DAYS_EMPLOYED, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$DAYS_EMPLOYED %in% outliers),]

outliers <- boxplot(clean_data$DAYS_REGISTRATION, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$DAYS_REGISTRATION %in% outliers),]

outliers <- boxplot(clean_data$DAYS_ID_PUBLISH, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$DAYS_ID_PUBLISH %in% outliers),]

outliers <- boxplot(clean_data$DAYS_LAST_PHONE_CHANGE, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$DAYS_LAST_PHONE_CHANGE %in% outliers),]

outliers <- boxplot(clean_data$CNT_FAM_MEMBERS, plot = FALSE)$out
clean_data <- clean_data[!(clean_data$CNT_FAM_MEMBERS %in% outliers),]


ggplot(clean_data,aes(x=TARGET,y=NAME_EDUCATION_TYPE)) +
  geom_jitter(colour='blue') 


ggplot(clean_data,aes(x=TARGET,y=NAME_FAMILY_STATUS)) +
  geom_jitter(colour='red')



### DIVIDE THE DATA INTO TRAIN AND TEST SETS
clean_data$TARGET <- as.factor(clean_data$TARGET)
divide = sample.split(clean_data$TARGET, SplitRatio = 0.75)

train = subset(clean_data, divide == TRUE)
test = subset(clean_data, divide == FALSE)

table(clean_data$NAME_INCOME_TYPE)
table(train$NAME_INCOME_TYPE)
table(test$NAME_INCOME_TYPE)

pie(table(clean_data$NAME_INCOME_TYPE))

pie(table(train$NAME_INCOME_TYPE))

pie(table(test$NAME_INCOME_TYPE))

### REMOVING UNNECESSARY DATA
train <- train[!(train$NAME_INCOME_TYPE == "Maternity leave"|train$NAME_INCOME_TYPE == "Businessman"),]
test <- test[!(test$NAME_INCOME_TYPE == "Maternity leave"|test$NAME_INCOME_TYPE == "Businessman"),]

### CHECK MISSING DATA ON TRAIN DATA
library(Amelia)
missmap(train, col = c("red", "blue"), legend = FALSE)

### REMOVING THE MISSING DATA FROM TRAIN DATA
train <- train[complete.cases(train),]
missmap(train, col = c("red", "blue"), legend = FALSE)

### CHECK MISSING DATA ON TEST DATA
missmap(test, col = c("red", "blue"), legend = FALSE)

### REMOVING THE MISSING DATA FROM TEST DATA
test <- test[complete.cases(test),]
missmap(test, col = c("red", "blue"), legend = FALSE)


### RUN THE MODEL
model <- glm(TARGET~., family = "binomial", data = train)
summary(model)

### CHECK THE ACCURACY
pred <- predict(model, newdata = test, type = "response")
glm.pred <- ifelse(pred > 0.5, "Not Paid", "Paid")

### ACCURACY VISUALIZATION
t <- table(glm.pred, test$TARGET)
t

### ACCURACY INCLUDING TYPE 1 & 2 ERRORS
accuracy_1 = (t[2,1] + t[1,2]) / (t[1,1] + t[2,2] + t[2,1] + t[1,2])
accuracy_1 = accuracy_1*100
accuracy_1

### USING MODEL TO PLOT THE GRAPH
predicted.data <- data.frame(prob = model$fitted.values, def = train$TARGET)
predicted.data <- predicted.data[order(predicted.data$prob, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data = predicted.data, aes(x = rank, y = prob)) +
  geom_point(aplha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("ddsa")

                ### DOWNSAMPLING AND MAKING THE MODEL BETTER ###
`%notin%` <- Negate(`%in%`)
options(scipen = 999)

clean_data$TARGET <- as.factor(clean_data$TARGET)
set.seed(100)

#Dividing the data set
library(caret)
trainDataIndex <- createDataPartition(clean_data$TARGET, p = 0.7, list = F)
trainData <- clean_data[trainDataIndex, ]
testData <- clean_data[-trainDataIndex, ]

down_train <- downSample(x = trainData[, colnames(trainData) %notin% "TARGET"], y = trainData$TARGET)

down_train <- down_train[!(down_train$NAME_INCOME_TYPE == "Student"|down_train$NAME_INCOME_TYPE == "Maternity leave"|down_train$NAME_INCOME_TYPE == "Pensioner"|down_train$NAME_INCOME_TYPE == "Businessman"),]
testData <- testData[!(testData$NAME_INCOME_TYPE == "Student"|testData$NAME_INCOME_TYPE == "Maternity leave"|testData$NAME_INCOME_TYPE == "Pensioner"|testData$NAME_INCOME_TYPE == "Businessman"),]

#Removing missing values
down_train <- down_train[complete.cases(down_train),]
testData <- testData[complete.cases(testData),]

#Building & fitting a glm model
down_model <- glm(Class ~ . , family = "binomial", data = down_train)
down_pred <- predict(down_model, newdata = testData, type = "response")
down_glm.pred <- ifelse(down_pred > 0.5, "Not Paid", "Paid")
summary(down_glm.pred)

#Finding accuracy for both errors
dt <- table(down_glm.pred, testData$TARGET)
accuracy_2 = (dt[2,1] + dt[1,2]) / (dt[1,1] + dt[2,2] + dt[2,1] + dt[1,2])
accuracy_2*100

#Constructing Graph
predicted.data <- data.frame(prob = down_model$fitted.values, def = down_train$Class)
predicted.data <- predicted.data[order(predicted.data$prob, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)

ggplot(data = predicted.data, aes(x = rank, y = prob)) +
  geom_point(aplha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("ddsa")

#Finalization
final_model <- stepAIC(down_model, direction = "backward", trace = FALSE)
summary(final_model)

#Testing Model
e <- as.data.frame(exp(coef(final_model)))
final_pred <- predict(final_model, newdata = testData, type = "response")
final_glm.pred <- ifelse(final_pred > 0.5, "Not Paid", "Paid")

#Checking Accuracy
final_t <- table(final_glm.pred, testData$TARGET)
final_t
