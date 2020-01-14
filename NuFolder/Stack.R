library(tidyverse)
Stackoverflow <- read_csv("stackoverflow.csv")
glimpse(Stackoverflow)
class(Stackoverflow)
# First count for Remote
Stackoverflow %>% 
  count(Remote, sort = TRUE)

# then count for Country
Stackoverflow %>% 
  count(Country, sort = TRUE)

#plotting a boxplot to see the relationship btw remote and professional experience
ggplot(Stackoverflow, aes(Remote, YearsCodedJob)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")

stackdata<- read.csv("Stackoverflow.csv")
#Building a simple logistic regression model predicting remote status.
mutate(Remote = factor(Remote, levels = c("Remote", "Not remote")))
simple_glm <- stackdata %>%
  select(-Respondent) %>%
  glm(Remote ~ .,
      family = "binomial",
      data = .)

# Print the summary of the model
summary(simple_glm)

# Load rsample
install.packages("rsample")

library(rsample)
# Create stack_select dataset
stack_select <- stackdata %>%
  select(-Respondent)

# Split the data into training and testing sets
set.seed(1234)
stack_split <- stack_select %>%
  initial_split(p = 0.8,
                strata = "Remote")

stack_train <- training(stack_split)
stack_test <- testing(stack_split)

#Load the caret package
library(caret)
# Create the upsampled training set
up_train <- upSample(x = select(stack_train, -Remote),
                     y = stack_train$Remote,
                     yname = "Remote") %>%
  as_tibble()

# Count the number of each type of Remote employee
up_train %>%
  count(Remote)

install.packages("e1071")
library(e1071)
# Build a logistic regression model
stackGLM <- train(Remote ~ ., method = "glm", family = "binomial",
                   data = stack_train,
                   trControl = trainControl(method = "boot",
                                            sampling = "up"))

# Print the model object
stackGLM

# Build a random forest model
stack_rf <- train(Remote ~ ., method = "rf", 
                  data = stack_train,
                  trControl = trainControl(method = "boot",
                                           sampling = "up"))

# Print the model object
stack_rflp.

#load yardstick package to test for correct model classification using test set
library(yardstick)
# Confusion matrix for logistic regression model
stack_test %>%
  mutate(`Logistic regression` = predict(stackGLM, stack_test)) %>%
  conf_mat(truth = Remote, estimate = "Logistic regression")

# Confusion matrix for random forest model
stack_test %>%
  mutate(`Random forest` = predict(stack_rf, stack_test)) %>%
  conf_mat(truth = Remote, estimate = "Random forest")

#Predict values & classification model matrix
testing_results <- stack_test %>%
  mutate(`Logistic regression` = predict(stackGLM, stack_test),
         `Random forest` = predict(stack_rf, stack_test))

# Calculate accuracy
accuracy(testing_results, truth = Remote, estimate = `Logistic regression`)
accuracy(testing_results, truth = Remote, estimate = `Random forest`)

## Calculate positive predict value
ppv(testing_results, truth = Remote, estimate = `Logistic regression`)
ppv(testing_results, truth = Remote, estimate = `Random forest`)
