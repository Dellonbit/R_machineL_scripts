
letterdata <- read.csv("Desktop/letterdata.csv") 
str(letterdata)
# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(12345)
letter_rand <- letterdata[order(runif(569)), ]

# split data into training and test set
letter_train <- letter_rand[1:16000, ]
letter_test <- letter_rand[16001:20000, ]


library(C50)
letter_model <- C5.0(letter_train[-1], letter_train$letter)
letter_model
# display detailed information about the tree( confusion matrix)
summary(letter_model)
#plot(wbcd_model)
# create a factor vector of predictions on test data
letter_pred <- predict(letter_model, letter_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(letter_test$letter, letter_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual diagnosis', 'predicted diagnosis'))

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
letter_boost10 <- C5.0(letter_train[-1], letter_train$letter,
                     trials = 10)
letter_boost10
summary(letter_boost10)

letter_boost_pred10 <- predict(letter_boost10, letter_test)
CrossTable(letter_test$letter, letter_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual letter', 'predicted letter'))

# boosted decision tree with 100 trials 
letter_boost100 <- C5.0(letter_train[-1], letter_train$diagnosis,
                      trials = 100)
letter_boost_pred100 <- predict(letter_boost100, letter_test)
CrossTable(letter_test$letter, letter_boost_pred100,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual letter', 'predicted letter'))

#pruning
## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

# apply the cost matrix to the tree
letter_cost <- C5.0(letter_train[-1], letter_train$letter,
                    costs = error_cost)
letter_cost_pred <- predict(letter_cost, letter_test)

CrossTable(letter_test$letter, letter_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual letter', 'predicted letter'))

