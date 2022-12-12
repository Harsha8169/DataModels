# Prasann Keshav Patil, Jonathan Chen, Sree Harsha, Dalton Thompson
# MIS545 Section 1
# FileName: 
# Importing Meta employees dataset containing employees
# who have been laid off and employees who have not been laid off
# and generating logistic regression model

# Loading tidyverse,corrplot,olsrr and smotefamily package
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")

library(tidyverse)
library(corrplot)
library(olsrr)
library(smotefamily)

# Set working directory to where the csv and R file is placed
setwd("C:/Users/ual-laptop/Desktop/Meta")
getwd()

# Reading csv file into a tibble
meta_emp1 <- read_csv(file = "meta_emp.csv",
                     col_types = "lilllcc",
                     col_names =  TRUE)

# Removing Name and Location column from tibble
meta_emp1 <- meta_emp1 %>% select(-Name)
meta_emp1 <- meta_emp1 %>% select(-Location)

#displaying summary in the console
summary(meta_emp1)

# displayAllHistograms function
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x=value,fill=key),
                              color = "black") +
    facet_wrap (~key, scales = "free") +
    theme_minimal()
}

# Calling displayAllHistograms function passing the tibble as a parameter
displayAllHistograms(meta_emp1)

# Display corelation matrix rounded to 2 decimal places
round(cor(meta_emp1),2)

# Display corelation plot
corrplot(cor(meta_emp1),
         method = "number")  

# Using 203 as random seed
set.seed(203)

# Creating vector of 75% randomly sampled rows from original dataset
sampleSet <- sample(nrow(meta_emp1),
                    round(nrow(meta_emp1) * 0.75),
                    replace = FALSE)

# Put these records into mobilePhoneTraining
meta_emp1Training <- meta_emp1[sampleSet, ]

# Put remaining records(25%) into mobilePhoneTesting
meta_emp1Testing  <- meta_emp1[-sampleSet, ]

# Generate the logistic regression model
meta_emp1Model <- glm(data = meta_emp1Training,
                     family = binomial,
                     formula = Laidoff ~.)

# Print the model results on the console
print(meta_emp1Model)

# Calculate the odds ratios for independent variable coefficients
exp(coef(meta_emp1Model)["GenderTRUE"])
exp(coef(meta_emp1Model)["Experience"])
exp(coef(meta_emp1Model)["`H1B Visa`TRUE"])
exp(coef(meta_emp1Model)["ManagerTrue"])

# Display the logistic regression model results using the summary()
summary(meta_emp1Model)

# Using the model to predict outcomes in the testing dataset
meta_emp1Prediction <- predict(meta_emp1Model,
                               meta_emp1Testing,
                              type = "response")

print(meta_emp1Prediction)

# Treat anything below or equal to 0.5 as a 0, anything above 0.5 as a 1
meta_emp1Prediction <- ifelse(
  meta_emp1Prediction >= 0.5, 1, 0)

# Create confusion matrix of predictions
meta_emp1ConfusionMatrix <- table(meta_emp1Testing$Laidoff,
                                  meta_emp1Prediction)

# print the confusion matrix
print(meta_emp1ConfusionMatrix)

# Calculate the false positive rate
meta_emp1ConfusionMatrix[1,2] / 
  (meta_emp1ConfusionMatrix[1,2] + 
     meta_emp1ConfusionMatrix[1,1])

# Calculate the false negative rate
meta_emp1ConfusionMatrix[2,1] / 
  (meta_emp1ConfusionMatrix[2,1] + 
     meta_emp1ConfusionMatrix[2,2])

# Calculate the model prediction accuracy
sum(diag(meta_emp1ConfusionMatrix)) / nrow(meta_emp1Testing)

# ------------------------------------------------------------------

# K Nearest Neighbours
# Generating k-nearest neighbors model and generating kvalue matrix
# to determine best k value for highest accuracy

# Installing the tidyverse package
# install.packages("tidyverse")

# Loading the tidyverse and class libraries
library(tidyverse)
library(class)

# Setting working directory to folder containing csv file
setwd("C:/Users/ual-laptop/Desktop/Meta")
getwd()

# Reading csv file into a tibble
meta_emp <- read_csv(file = "meta_emp.csv",
                     col_types = "lilllcc",
                     col_names =  TRUE)

# Removing Name and Location column from tibble
meta_emp <- meta_emp %>% select(-Name)
meta_emp <- meta_emp %>% select(-Location)

# Creating two tibbles, one with the label and the other
# with the remaining variables
meta_emplabel <- meta_emp %>% select(Laidoff)
meta_emp <- meta_emp %>% select(-Laidoff)

# Using 517 as random seed
set.seed(517)

# Creating vector of 75% randomly sampled rows from original dataset
sampleSet <- sample(nrow(meta_emp),
                    round(nrow(meta_emp) * 0.75),
                    replace = FALSE)

# Put these records into meta_empTraining and meta_empTrainingLabels
meta_empTraining <- meta_emp[sampleSet, ]
meta_empTrainingLabels <- meta_emplabel[sampleSet, ]

# Put remaining records(25%) into meta_empTesting and meta_empTestingLabels
meta_empTesting  <- meta_emp[-sampleSet, ]
meta_empTestingLabels  <- meta_emplabel[-sampleSet, ]

# Generate the k-nearest neighbors model
meta_empPrediction <- knn(train = meta_empTraining,
                          test = meta_empTesting,
                          cl = meta_empTrainingLabels$Laidoff,
                          k = 13)

# Displaying the predictions from the testing dataset on the console
print(meta_empPrediction)

# Evaluating the model by forming a confusion matrix
meta_empConfusionMatrix <- table(meta_empTestingLabels$Laidoff,
                                 meta_empPrediction)

# Displaying the confusion matrix on the console
print(meta_empConfusionMatrix)

# Calculate the false positive rate
meta_empConfusionMatrix[1,2] / 
  (meta_empConfusionMatrix[1,2] + 
     meta_empConfusionMatrix[1,1])

# Calculate the false negative rate
meta_empConfusionMatrix[2,1] / 
  (meta_empConfusionMatrix[2,1] + 
     meta_empConfusionMatrix[2,2])

# Calculating the model predictive accuracy
predictiveAccuracy <- sum(diag(meta_empConfusionMatrix)) /
  nrow(meta_empTesting)

# Displaying the predictive accuracy on the console
print(predictiveAccuracy)

# Creating a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol=2)

# Assigning column names to the matrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Looping and storing k values in matrix
for (kValue in 1:nrow(meta_empTraining)) {
  # Calculate accuracy only if k value is odd 
  if(kValue %% 2 !=0){
    # Generate the model
    meta_empPrediction <- knn(train = meta_empTraining,
                              test = meta_empTesting,
                              cl = meta_empTrainingLabels$Laidoff,
                              k = kValue)
    
    # Generate the confusion matrix
    meta_empConfusionMatrix <- table(meta_empTestingLabels$Laidoff,
                                     meta_empPrediction)
    
    # Calculate predictive accuracy
    predictiveAccuracy <- sum(diag(meta_empConfusionMatrix)) /
      nrow(meta_empTesting)
    
    # Add new row to the matrix
    kValueMatrix <- rbind(kValueMatrix,  c(kValue, predictiveAccuracy))
  }
}

# Displaying the kValueMatrix on the console
print(kValueMatrix)

# optimal k value = 13 with prediction accuracy 70%

# ------------------------------------------------------
# K-Naive Bayes model

#Install tidyverse package
# install.packages("tidyverse")
# install.packages("e1071")

# Load the tidyverse package using library()
library(tidyverse)
library(e1071)

# set working directory to lab folder on my laptop
setwd("C:/Users/ual-laptop/Desktop/Meta")

offType <- read_csv(file = 'meta_emp.csv',
                    col_types = "lilllcc",
                    col_names = TRUE)

# display sedanSize in the console
print(offType)

# show the structure of the sedanSize tibble
str(offType)

# show the summary of the sedanSize tibble
summary(offType)

offType <- offType %>% select(-Name)
offType <- offType %>% select(-Location)

# Randomly split the dataset into mobilePhoneTraining (75% of records) 
#and mobilePhoneTesting (25% of records) using 154 as the random seed
set.seed(154)
sampleset <- sample(nrow(offType), round(nrow(offType) * 0.75), 
                    replace = FALSE)

offTypeTraining <- offType[sampleset, ]
offTypeTesting <- offType[-sampleset, ]

#Generate the Naive Bayes model to predict DwellingType based on the other 
#variables in the dataset.
offmodel= naiveBayes(formula= Laidoff ~.,
                     data= offTypeTraining,
                     laplace=1)

# Build probabilities for each record in the testing dataset and store them 
# in dwellingTypeProbability
# Display dwellingTypeProbability on the console
offTypeProbability= predict(offmodel,
                            offTypeTesting,
                            type="raw")
print(offTypeProbability)

# Predict classes for each record in the testing dataset and store them 
# in dwellingTypePrediction
# Display dwellingTypePrediction on the console
offTypePrediction= predict(offmodel,
                           offTypeTesting,
                           type="class")
print(offTypePrediction)

# Evaluate the model by forming a confusion matrix
# Display the confusion matrix on the console
offConfusion <- table(offTypeTesting$Laidoff,
                      offTypePrediction)
print(offConfusion)

true_positives  <- diag(offConfusion)
false_positives <- colSums(offConfusion) - true_positives
false_negatives <- rowSums(offConfusion) - true_positives
true_negatives  <- sum(offConfusion) - true_positives -
  false_positives - false_negatives

print(false_positives)
print(false_negatives)
print(true_negatives)
print(true_positives)

# Calculate the model predictive accuracy and store it into a variable 
# called predictiveAccuracy
# display predictive Accuracy in the console
predictiveAccuracy <- sum(diag(offConfusion)) / nrow(offTypeTesting)
print(predictiveAccuracy)

# ----------------------------------------------------------------
# Decision Trees

# Install and load tidyverse package
# install.packages("tidyverse")
library(tidyverse)

# Install and load rpart.plot ,rpart package
# install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

# Set working directory to Meta folder
setwd("C:/Users/ual-laptop/Desktop/Meta")

# Read CSV file into a tibble and define column types
employee_Profile <- read_csv(file = "meta_emp.csv",
                             col_types = "lilllcc",
                             col_names = TRUE)

# Display the Employee_Profile tibble
print(employee_Profile)

# Display the structure of the tibble
print(str(employee_Profile))

# Display the summary of the tibble
print(summary(employee_Profile))

# Removing unwanted columns from tibble
employee_Profile <- employee_Profile %>% select(-Name)
employee_Profile <- employee_Profile %>% select(-Location)

# Randomly splitting the data into a smaple set
set.seed(545)
sampleSet <- sample(nrow(employee_Profile),
                    round(nrow(employee_Profile)*.75),
                    replace = FALSE)

# Assigning the testing and training tibble
employee_ProfileTraining <- employee_Profile[sampleSet, ]
employee_ProfileTesting <- employee_Profile[-sampleSet, ]

# Generating Decision tree model for the training dataset
employee_ProfileDecisionTreeModel <- rpart(formula = Laidoff ~ .,
                                           method = "class",
                                           cp = 0.01,
                                           data = employee_ProfileTraining)

# Display the decision tree
rpart.plot(employee_ProfileDecisionTreeModel)

# Predicting classes for each record in the testing dataset and 
# storing them in Employee_ProfilePrediction
employee_ProfilePrediction <- predict(employee_ProfileDecisionTreeModel,
                                      employee_ProfileTesting,
                                      type = 'class')

# Display the prediction
print(employee_ProfilePrediction)

# Evaluating the model by confusion matrix
employee_ProfileConfusionMatrix <- table(employee_ProfileTesting$Laidoff,
                                         employee_ProfilePrediction)

# Display confusion matrix
print(employee_ProfileConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracy <- sum(diag(employee_ProfileConfusionMatrix)) /
  nrow(employee_ProfileTesting)

# Display the predictive accuracy of the model
print(predictiveAccuracy)

# Calculating false positive
employee_ProfileConfusionMatrix[1,2]/
  (employee_ProfileConfusionMatrix[1,2]+employee_ProfileConfusionMatrix[1,1])
# Calculating false negative
employee_ProfileConfusionMatrix[2,1]/
  (employee_ProfileConfusionMatrix[2,1]+employee_ProfileConfusionMatrix[2,2])

# -------------------------------------------------------------

# Neural Network Design

# MIS545 Neural Network

# Installing the tidyverse and neuralnet packages
# install.packages("tidyverse")
# install.packages("neuralnet")

# Loading the libraries
library(tidyverse)
library(neuralnet)

# Set working directory to FinalProject folder
setwd("C:/Users/ual-laptop/Desktop/Meta")

# Creating the tibble
laidOff <- read_csv(file = "meta_emp.csv",
                    col_names = TRUE,
                    col_types = "lilllcc")

# Displaying laidOff in the console
print(laidOff)

# Displaying the structure of laidOff in the console
str(laidOff)

# Displaying the summary of laidOff in the console
summary(laidOff)

laidOff <- laidOff %>% select(-Name)
laidOff <- laidOff %>% select(-Location)

# Scaling the Experience from 0 to 1
laidOff <- laidOff %>% 
  mutate(ExperienceScaled = (Experience - min(Experience)) / 
           (max(Experience) - min(Experience)))

# Using 117 as the random seed
set.seed(117)

# Creating a vector of 75% randomly samples rows from the original dataset
sampleSet <- sample(nrow(laidOff),
                    round(nrow(laidOff) * 0.75),
                    replace = FALSE)

# Putting the records from the 75% randomly sampled records into 
# laidOffTraining
laidOffTraining <- laidOff[sampleSet, ]

# Putting all the other records into laidOffTesting
laidOffTesting <- laidOff[-sampleSet, ]

# Generating the neural network (excluding name and location)
laidOffNeuralNet <- neuralnet(
  formula = Laidoff ~ Gender + ExperienceScaled + H1BVisa + Manager,
  data = laidOffTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE )

# Displaying the neural network results
print(laidOffNeuralNet$result.matrix)

# Visualizing the neural network results
plot(laidOffNeuralNet)

# Using laidOffNeuralNet to generate probabilities on the 
# laidOffTesting data set and store this in laidOffProbability
laidOffProbability <- neuralnet::compute(laidOffNeuralNet,laidOffTesting)

# Displaying the probabilities from the testing dataset on the console
print(laidOffProbability$net.result)

# Converting probability predictions into 0/1 predictions and store this into
# laidOffPrediction
laidOffPrediction <- 
  ifelse(laidOffProbability$net.result > 0.5, 1, 0)

# Displaying the 0/1 predictions on the console
print(laidOffPrediction)

# Evaluating the model by forming a confusion matrix
laidOffConfusionMatrix <- table(laidOffTesting$Laidoff,
                                laidOffPrediction)

# Displaying the confusion matrix
print(laidOffConfusionMatrix)

# Calculating the model predictive accuracy
predictiveAccuracy <- sum(diag(laidOffConfusionMatrix))/
  nrow(laidOffTesting)

# Displaying the predictive accuracy
print(predictiveAccuracy)


# Thank You --------------------------------------------------
