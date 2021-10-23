install.packages("tidyverse")
install.packages("caret")
install.packages("Rcpp")
install.packages('e1071', dependencies=TRUE)
library(Rcpp)
library(caret)
library(tidyverse)
library(caret)

raw_data <- read.csv("technical_test_data.csv")
x <- raw_data
y <- x %>% 
        group_by(days_from_install) %>% 
        summarise(sum = round(sum(iap_revenue + ad_revenue), 4)) %>% 
        mutate(csum = cumsum(sum)) %>% 
        select(-sum)

validation_index <- createDataPartition(x$iap_revenue+x$ad_revenue, p=0.8, list=FALSE)
validation <- x[-validation_index,]
x <- x[validation_index,]
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
set.seed(7)
reg <- train(iap_revenue + ad_revenue~., data=x, method="lm", metric=metric, trControl=control)
