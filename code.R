library(dplyr)
library(data.table)

setwd("C:/Users/phan3/Desktop/Data/glm")

data_adult <-
read.csv("https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/data_adult.csv")


top_one_percent <- quantile(data_adult$hours.per.week, .99)
top_one_percent

data_adult_drop <-data_adult %>%filter(hours.per.week<top_one_percent)
dim(data_adult_drop)

data_adult_rescale <- data_adult_drop %>%mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)

recast_data <- data_adult_rescale %>% select(-X) %>% mutate(education = factor(
  ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" 
         | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" 
         | education == "9th", "dropout"
         , ifelse(education == "HS-grad", "HighGrad"
                  , ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community"
                           ,ifelse(education == "Bachelors", "Bachelors",
                                   ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))

# Change level marry
recast_data <- recast_data %>% mutate(marital.status = factor(ifelse(marital.status == "Never-married"
| marital.status == "Married-spouse-absent", "Not_married", ifelse(marital.status == "Married-AF-spouse" 
| marital.status == "Married-civ-spouse", "Married", ifelse(marital.status == "Separated" 
| marital.status == "Divorced", "Separated", "Widow")))))

set.seed(1234)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(recast_data, 0.8, train = TRUE)
data_test <- create_train_test(recast_data, 0.8, train = FALSE)
dim(data_train)

formula <- income~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

x<-names(coefficients(logit))
coeff.data<-data.table(names=x,coef = coefficients(logit))

test_results<-data.table(raw_pred=predict(logit,data_test),prob=predict(logit,data_test,type='response'))

fwrite(test_results,"test_results.csv")
fwrite(coeff.data,"coeff.csv")

fwrite(data_test,"data_test.csv")

data_train<-within(data_train, education <- relevel(as.factor(education),
                                         ref = "dropout"))


logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)
coeff.data.relevel<-data.table(names=x,coef = coefficients(logit))

test_results_relevel<-data.table(raw_pred=predict(logit,data_test),prob=predict(logit,data_test,type='response'))

fwrite(test_results_relevel,"test_results_relevel.csv")
fwrite(coeff.data.relevel,"coef_relevel.csv")
