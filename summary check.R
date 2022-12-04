
# import data
ped <- read.csv("ped_crashes.csv")

# EDA
library(qacBase)
contents(ped)

# drop and rename variables, remove uncoded values, mutate variable types
library(dplyr)
ped <- ped %>% 
  select(-c("Crash.Day","City.or.Township","Party.Type")) %>% #party type only one value
  rename(year = Crash.Year,
         month = Crash.Month,
         time = Time.of.Day,
         weekday = Day.of.Week,
         intersection = Crash..Intersection,
         hit_run = Crash..Hit.and.Run,
         light = Lighting.Conditions,
         weather = Weather.Conditions..2016..,
         speedLimit = Speed.Limit.at.Crash.Site,
         worstInjury = Worst.Injury.in.Crash,
         age = Person.Age,
         gender = Person.Gender) %>% 
  filter(weather != "Uncoded & errors",
         gender != "Uncoded & errors",
         age != "DOB invalid",
         age != "Less than 1 year old",
         light != "Unknown",
         weather != "Unknown",
         speedLimit != "Uncoded & errors") %>% 
  mutate(speedLimit = as.numeric(speedLimit),
         age = as.numeric(age))
  
# recode time as categorical variable
morning <- c("6:00 AM - 7:00 AM" ,"7:00 AM - 8:00 AM","8:00 AM - 9:00 AM",
             "9:00 AM - 10:00 AM","10:00 AM - 11:00 AM","11:00 AM - 12:00 noon")
afternoon <- c("12:00 noon - 1:00 PM","1:00 PM - 2:00 PM","2:00 PM - 3:00 PM",
               "3:00 PM - 4:00 PM","4:00 PM - 5:00 PM","5:00 PM - 6:00 PM")
night <- c("6:00 PM - 7:00 PM","7:00 PM - 8:00 PM","8:00 PM - 9:00 PM",
           "9:00 PM - 10:00 PM","10:00 PM - 11:00 PM","11:00 PM - 12:00 midnight")
lateNight <- c("12:00 midnight - 1:00 AM","1:00 AM - 2:00 AM","2:00 AM - 3:00 AM",
               "3:00 AM - 4:00 AM","4:00 AM - 5:00 AM","5:00 AM - 6:00 AM")

ped$time <- ifelse(ped$time %in% morning, "morning",
                   ifelse(ped$time %in% afternoon, "afternoon",
                          ifelse(ped$time %in% night, "night", "midnight")))

# recode worst injury
ped$worstInjury <- recode(ped$worstInjury, 
                          "Suspected serious injury (A)" = "4", 
                          "Suspected minor injury (B)" = "3",
                          "Possible injury (C)" = "2",
                          "No injury (O)"  = "1", 
                          "Fatal injury (K)" = "5")

#recode intersection
ped$intersection <- recode(ped$intersection, 
                           "Not intersection crash" = "0", 
                           "Intersection crash" = "1")

#recode hit_run
ped$hit_run <- factor(ifelse(ped$hit_run == "Hit-and-run","yes","no"))

# examine data after management
df_plot(ped)
cor_plot(ped)
barcharts(ped) #unbalanced value for outcome variable hit_run -> need smote, need to adjust ROC cutoff
histograms(ped)


# gradient boost
library(caret)
set.seed(1234)

index <- createDataPartition(ped$hit_run, p=.8, list = FALSE)
train <- ped[index,]
test <- ped[-index,]

train.control <- trainControl(method = "cv",
                              number = 10,
                              summaryFunction = twoClassSummary,
                              classProbs = TRUE,
                              sampling = "smote")

############### stochastic gradient boost ##############
set.seed(1234)
model.gbm <- train(hit_run ~.,
                   data = train,
                   method = "gbm",
                   tuneLength = 10,
                   trControl = train.control,
                   metric = "ROC",
                   verbose = FALSE)



                   #importance = TRUE) - all ROC metric values are missing


model.gbm$finalModel
model.gbm

pred <- predict(model.gbm, test)
gbmConfusionMatrix <- confusionMatrix(pred, test$hit_run, positive = "yes")
gbmConfusionMatrix

# Accuracy: 90.19%
# sensitivity: 8.70%
# sepcificity: 97.93%

#---------------------------
# visualization
#---------------------------

library(gbm)
varImportance <- summary(model.gbm)
rownames(varImportance) <- NULL
varImportance$rel.inf <- round(varImportance$rel.inf, 3)
colnames(varImportance) <- c("variable","rel.inf")

varImportance

train$prob <- predict(model.gbm, train, type="prob")[[2]]
library(plotROC)
ROC.gbm <- ggplot(train, aes(d=hit_run, m=prob)) +
  geom_roc(labelround=2, n.cuts=15, labelsize=3) + 
  style_roc(major.breaks=seq(0, 1, .1),
            minor.breaks=seq(0, 1, .05),
            theme=theme_grey) +
  labs(title="ROC Plot")

ROC.gbm

library(pROC)
auc(train$hit_run, train$prob)
################### extreme gradient boost #####################


set.seed(1234)
model.xgb <- train(hit_run~.,
                   data = train,
                   method = "xgbTree",
                   metric = "ROC",
                   tuneLength = 5,
                   verbose = FALSE,
                   verbosity = 0,
                   trControl = train.control)
                   # importance = TRUE)

model.xgb
#model.xgb$finalModel
model.xgb$bestTune

test$prob <- predict(model.xgb, test, type = "prob")[[2]] #object pred not found??
test$pred <- factor(test$prob > 0.18,
                    levels = c(FALSE, TRUE),
                    labels = c("no","yes"))

pred <- predict(model.xgb, test)
# error message but can still see confusion matrix?
xgbConfusionMatrix <- confusionMatrix(pred, test$hit_run, positive = "yes")
xgbConfusionMatrix

#---------------------------
# visualization
#---------------------------

plot(varImp(model.xgb))

# examine ROC curve

train$prob.xgb <- predict(model.xgb, train, type="prob")[[2]]
ROC.xgb <- ggplot(train, aes(d=hit_run, m=prob.xgb)) +
  geom_roc(labelround=2, n.cuts=15, labelsize=3) + 
  style_roc(major.breaks=seq(0, 1, .1),
            minor.breaks=seq(0, 1, .05),
            theme=theme_grey) +
  labs(title="ROC Plot")

ROC.xgb

# what to do next:
#   manipulate ROC cutoff
#   compare xgb and gbm

################### model comparison ##########
results <- resamples(list(gbm = model.gbm,
                     xgb = model.xgb))
summary(results)
bwplot(results)

# compare different models with others
# after chosen one -> 
#   fit model -> 
#   importance = TRUE, adjust ROC cutoff, check gain and lift charts