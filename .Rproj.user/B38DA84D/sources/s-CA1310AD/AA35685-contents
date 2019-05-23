# Import Library
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(naivebayes)
library(readr)
library(tidyr)
library(caret)
#Import Data

hrd <- read_csv("HR-Employee-Attrition.csv")
#hrd <-HR_Employee_Attrition
dim(hrd)
head(hrd,10)
sum(is.na(hrd))

#index_train <- sample(1:nrow(hrd), 0.7 * nrow(hrd))
index_train <- createDataPartition(y=hrd$Attrition,p=0.9,list=FALSE)
train <- hrd[index_train, ]
test <- hrd[-index_train, ]

# c <- ncol(train)
# pvalues <- numeric(c)
# for (i in 1:c)
# {
#   fit <- lm(train$Attrition ~ train[,i])
#   summ <- summary(fit)
#   pvalues[i] <- summ$coefficients[2,4]
# }

nb <- naive_bayes(Attrition ~ ., data = train)
nb
pred_nb <- predict(nb, as.data.frame(test))
confnb <- table(test$Attrition, pred_nb)
confnb
TPn <- confnb[1, 1]
FNn <- confnb[1, 2]
FPn <- confnb[2, 1]
TNn <- confnb[2, 2]
accnb <- (TPn + TNn)/(TPn + FNn + FPn + TNn)
accnb
#Menghitung Nilai Precision
precnb <- TPn / (TPn + FPn)
precnb
#Menghitung Nilai Recall
recnb <- TPn / (TPn + FNn)
recnb