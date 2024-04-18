
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("caretEnsemble")
# install.packages("corrplot")
# install.packages("fastDummies")
# install.packages("e1071")
# install.packages("class")
# install.packages("neuralnet")

# Libraries
library(ggplot2) # plot library
library(tidyverse) # for data manipulation
library(gridExtra) # multiple plots in 1
library(ggrepel) # for graph repel (labels)
library(scales) # for % in density plots
library(fastDummies) # to create dummy variables
library(caret) # for models
library(caretEnsemble) # to create ensembles
library(FactoMineR)
library(factoextra)
library(smotefamily) # also for oversampling
library(e1071)
library(rpart.plot)
library(class)
library(neuralnet)
library(corrplot)

# Predefined personal color schemes
colorThemeCM <-
  c("#F57E00",
    "#FFA90A",
    "#FFCE72",
    "#3AAFF9",
    "#0087DC",
    "#005991")

colorThemePca <-
  c("#BF4402",
    "#94058E",
    "#005DD7",
    "#2690C3",
    "#F5C402",
    "#CE378E")

# Predefined theme
plotTheme <- theme(
  plot.background = element_rect(fill = "grey97",
                                 color = "grey25"),
  panel.background = element_rect(fill = "grey97"),
  panel.grid.major = element_line(colour = "grey87"),
  text = element_text(color = "grey25"),
  plot.title = element_text(size = 18),
  plot.subtitle = element_text(size = 14),
  axis.title = element_text(size = 11),
  legend.box.background = element_rect(color = "grey25",
                                       fill = "grey97",
                                       size = 0.5),
  legend.box.margin = margin(
    t = 5,
    r = 5,
    b = 5,
    l = 5
  )
)

# set working directory
setwd("C:/Users/ual-laptop/Documents/Rworkspace/Project")

# import the data
# col_types = 'icccccccnl'
fraudData <- read_csv(file = 'FraudulentTransactions.csv')

# data preprocessing
# zipcodeOri and zipMerchants column has only one type of value,
# so removing the value
fraudData <- fraudData %>% select(-zipcodeOri,-zipMerchant)

# removing commas and special symbols using regex
fraudData <- fraudData %>%
  mutate(
    customer = gsub("^.|.$", "", customer),
    age = gsub("^.|.$", "", age),
    gender = gsub("^.|.$", "", gender),
    merchant = gsub("^.|.$", "", merchant),
    category = gsub("^.|.$", "", category)
  )
#GSUB() REMOVES ALL THE OCCURENCE
# Both gsub() and sub() take three arguments:
#   
#   the first argument is the pattern to be replaced,
# the second argument is the replacement string,
# # the third argument is the string on which the replacement is to be made.

# ----
# difference between the two is that gsub() is used to replace all occurrences of 
# a specified pattern in a string, while sub() is used to replace only the first 
# occurrence of a specified pattern.

# trim suffix 'es_' from the Category column
fraudData <-
  fraudData %>% mutate(category = sub("es_", "", category))

# removing unknown genders with value 'U'
fraudData <- fraudData %>% filter(gender != "U")

# age ranges from 0-6 but has 'U' for unknown values, hence replacing it with 7
fraudData$age[which(fraudData$age == "U")] <- "7"

# amount is a continuous variable so replacing it with value ranges
fraudData <- fraudData %>%
  mutate(amount_thresh =
           ifelse(
             amount <= 500,
             "0-500",
             ifelse(
               amount <= 1000,
               "500-1000",
               ifelse(
                 amount <= 1500,
                 "1000-1500",
                 ifelse(
                   amount <= 2000,
                   "1500-2000",
                   ifelse(
                     amount <= 2500,
                     "2000-2500",
                     ifelse(amount <= 3000, "2500-3000", ">3000")))))))

# defining draw_confusion_matrix function to plot matrix for every model
# create the matrix and add the predictive accuracy results
draw_confusion_matrix <- function(cm) {
  layout(matrix(c(1, 1, 2)))
  par(mar = c(2, 2, 2, 2))
  plot(
    c(100, 345),
    c(300, 450),
    type = "n",
    xlab = "",
    ylab = "",
    xaxt = 'n',
    yaxt = 'n'
  )
  title('CONFUSION MATRIX', cex.main = 2)
  rect(150, 430, 240, 370, col = colorThemeCM[2])
  text(195, 435, 'Fraud', cex = 1.2)
  rect(250, 430, 340, 370, col = colorThemeCM[4])
  text(295, 435, 'Not Fraud', cex = 1.2)
  text(125,
       370,
       'Predicted',
       cex = 1.3,
       srt = 90,
       font = 2)
  text(245, 450, 'Actual', cex = 1.3, font = 2)
  rect(150, 305, 240, 365, col = colorThemeCM[4])
  rect(250, 305, 340, 365, col = colorThemeCM[2])
  text(140, 400, 'Fraud', cex = 1.2, srt = 90)
  text(140, 335, 'Not Fraud', cex = 1.2, srt = 90)
  res <- as.numeric(cm$table)
  text(195,
       400,
       res[1],
       cex = 1.6,
       font = 2,
       col = 'white')
  text(195,
       335,
       res[2],
       cex = 1.6,
       font = 2,
       col = 'white')
  text(295,
       400,
       res[3],
       cex = 1.6,
       font = 2,
       col = 'white')
  text(295,
       335,
       res[4],
       cex = 1.6,
       font = 2,
       col = 'white')
  plot(
    c(100, 0),
    c(100, 0),
    type = "n",
    xlab = "",
    ylab = "",
    main = "DETAILS",
    xaxt = 'n',
    yaxt = 'n'
  )
  text(10, 85, names(cm$byClass[1]), cex = 1.2, font = 2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex = 1.2)
  text(30, 85, names(cm$byClass[2]), cex = 1.2, font = 2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex = 1.2)
  text(50, 85, names(cm$byClass[5]), cex = 1.2, font = 2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex = 1.2)
  text(70, 85, names(cm$byClass[6]), cex = 1.2, font = 2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex = 1.2)
  text(90, 85, names(cm$byClass[7]), cex = 1.2, font = 2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex = 1.2)
  text(30, 35, names(cm$overall[1]), cex = 1.5, font = 2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex = 1.4)
  text(70, 35, names(cm$overall[2]), cex = 1.5, font = 2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex = 1.4)
}

# Feature engineering
# Create ID and Total Trans for Customer and Merchant
# This returns total number of observation per group 
customer <- fraudData %>%
  group_by(customer) %>%
  summarise(customer_total_trans = n()) %>%
  mutate(customer_ID = seq(1, 4109, 1))

merchant <- fraudData %>%
  group_by(merchant) %>%
  summarise(merchant_total_trans = n()) %>%
  mutate(merchant_ID = seq(1, 50, 1))

category <- fraudData %>%
  group_by(category) %>%
  summarise(category_total_trans = n()) %>%
  mutate(category_ID = seq(1, 15, 1))

amount_thresh <- fraudData %>%
  group_by(amount_thresh) %>%
  summarise(amount_thresh_total_trans = n()) %>%
  mutate(amount_thresh_ID = seq(1, 7, 1))

# add the 4 new modified variables to fraudData tibble
fraudData <- fraudData %>%
  inner_join(customer, by = "customer") %>%
  inner_join(merchant, by = "merchant") %>%
  select(-customer,-merchant)
  
# age type from chr to dbl, gender coding and type change from chr to dbl
fraudData <- fraudData %>%
  mutate(age = as.double(age)) %>%
  mutate(gender = ifelse(gender == "M", 1,
                         ifelse(gender == "F", 2, 3)))
  
# recode category and add total_category_trans column
fraudData <- fraudData %>% 
  inner_join(category, by = "category") %>%
  mutate(category = category_ID) %>%
  select(-category_ID)

fraudDataHistogramTibble <- fraudData
  
# recode amount_thresh
fraudData <- fraudData %>% 
  inner_join(amount_thresh, by = "amount_thresh") %>%
  mutate(amount_thresh = amount_thresh_ID) %>%
  select(-amount_thresh_ID)


# Add also for age and gender total_trans
age <- fraudData %>%
  group_by(age) %>%
  summarise(age_total_trans = n())

gender <- fraudData %>%
  group_by(gender) %>%
  summarise(gender_total_trans = n())

fraudData <- fraudData %>%
  inner_join(age, by = "age") %>%
  inner_join(gender, by = "gender")

# ======== Transform the total_trans numbers into weights ========
# This is done so the numbers will be smaller (except customer_total_trans)
total_freq = 591746

fraudData <- fraudData %>%
  mutate(
    merchant_total_trans = round((merchant_total_trans / total_freq) * 100, 5),
    category_total_trans = round((category_total_trans / total_freq) *
                                   100, 5),
    amount_thresh_total_trans = round((amount_thresh_total_trans /
                                         total_freq) * 100, 5),
    age_total_trans = round((age_total_trans / total_freq) * 100, 5),
    gender_total_trans = round((gender_total_trans / total_freq) *
                                 100, 5)
  )

# Remove Step
fraudData <- fraudData %>% 
  select(-step) %>% 
  select(fraud, everything())


# Create Dummy Variables for Gender, Age, Category and Amount_Thresh 
fraudData <-
  dummy_cols(fraudData,
             
             select_columns = c("age", "gender", "category", "amount_thresh"))


# Recode Fraud column
fraudData <- fraudData %>% 
  mutate(fraud = ifelse(fraud == "1", "F", "NF"))

dim(fraudData)

# Visualize the fraud and non fraud data
pca_object <- prcomp(fraudData[ ,c(2:6)], center = TRUE, scale. = TRUE)

# Eigenvalues for Dimensions variability explained
eigenvalues <- get_eig(pca_object)
eigenvalues

# Keep only first 2 PCs and append target column
pca_data <- pca_object$x[, c(1:2)] %>% 
  as.data.frame() %>% 
  mutate(fraud = fraudData$fraud)

# Visualise Fraud
pca_data %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = fraud, shape = fraud)) +
  plotTheme+
  scale_color_manual(values = c(colorThemePca
                                [1], colorThemePca
                                [4])) +
  labs(x = "PC1", y = "PC2",
       title = "Fraud Spread over the first 2 Dimensionalities",
       subtitle = "frauds & non-frauds have clear different behaviours",
       color = "Fraud", shape = "Fraud")

# Since the fraud data is imbalanced towards non-fraud transactions, 
# we balance the classes
set.seed(123)

# Creating the fraud dataframe(7200 fraud records)
fraudDataframe <- fraudData %>% filter(fraud == "F")
dim(fraudDataframe)

# Creating the non-fraud dataframe(586928 non-fraud records)
non_fraudDataframe <- fraudData %>% filter(fraud == "NF")
dim(non_fraudDataframe)

# Create data Partition
index <- createDataPartition(
  c(non_fraudDataframe$age, 
    non_fraudDataframe$gender, 
    non_fraudDataframe$category, 
    non_fraudDataframe$amount_thresh,
    non_fraudDataframe$merchant_ID),
  p = 0.9834, 
  list = F)

balanced_data <- non_fraudDataframe[-index, ]
dim(balanced_data)

# update the non fraud table with balanced data
non_fraudDataframe <- balanced_data

# Complete fraud and non-fraud data 
undersampling_data <- bind_rows(non_fraudDataframe, fraudDataframe)

# Randomize - because data is chronological
set.seed(123)
undersampling_data <- undersampling_data[sample(1:nrow(undersampling_data)), ]
dim(undersampling_data)

# New tibble for parameter tuning
fraudDataTuning <- undersampling_data

# Prediction models
# Splitting the data into training and testing dataset
set.seed(123)
sampleData <- createDataPartition(undersampling_data$fraud, p = 0.75, list = F)
fraudDataTrain <- undersampling_data[sampleData, ]
fraudDataTest <- undersampling_data[-sampleData, ]

# Split data into Target and Feature variable
X_train <- fraudDataTrain %>% select(-fraud)
y_train <- fraudDataTrain$fraud

X_test <- fraudDataTest %>% select(-fraud)
y_test <- fraudDataTest$fraud

# KFolds
myFolds <- createFolds(y_train, k = 5)

# Train Control variable
trainControl <- trainControl(method = 'cv',
                             number = 5, 
                             index = myFolds,
                             savePredictions = T, 
                             classProbs = T, 
                             verboseIter = T,
                             summaryFunction = twoClassSummary,
                             preProcOptions = c(thresh = 0.8), 
                             allowParallel = T)


# Models Comparative Study
model_list <- caretList(X_train, y_train, trControl = trainControl, 
                        methodList = c("knn", "rpart", "naive_bayes", "nnet"),
                        tuneList = NULL, 
                        continue_on_fail = FALSE, 
                        preProcess = c("zv", "center", "scale"))

 # Model results
resamples <- resamples(model_list)
dotplot(resamples, metric = "Sens")
dotplot(resamples, metric = "Spec")

# Final Predictions on testing data
predictionDecisionTrees <- predict.train(model_list$rpart, newdata = X_test)
predictionNaiveBayes <- predict.train(model_list$naive_bayes, newdata = X_test)
predictionKnn <- predict.train(model_list$knn, newdata = X_test)
predictionNeuralNet <- predict.train(model_list$nnet, newdata = X_test)

# Checking model sensitivity
# Glm = sensitivity(as.factor(predictionGlm), as.factor(y_test))
predictionSensitivity <- data.frame(NaiveBayes = sensitivity(
  as.factor(predictionNaiveBayes), 
  as.factor(y_test)),
  Knn = sensitivity(as.factor(predictionKnn), 
                    as.factor(y_test)),
  NeuralNet = sensitivity(as.factor(predictionNeuralNet),
                          as.factor(y_test)))

print(predictionSensitivity)

# Plot confusion matrix
confusionMatrixDecisionTrees <- confusionMatrix(
  as.factor(predictionDecisionTrees), 
  as.factor(y_test))
confusionMatrixNaiveBayes <- confusionMatrix(
  as.factor(predictionNaiveBayes), 
  as.factor(y_test))
confusionMatrixKnn <- confusionMatrix(
  as.factor(predictionKnn), 
  as.factor(y_test))
confusionMatrixNeuralNetwork <- confusionMatrix(
  as.factor(predictionNeuralNet), 
  as.factor(y_test))

draw_confusion_matrix(confusionMatrixDecisionTrees)
draw_confusion_matrix(confusionMatrixNaiveBayes)
draw_confusion_matrix(confusionMatrixKnn)
draw_confusion_matrix(confusionMatrixNeuralNetwork)

#Run models with varied parameters and parameter tuning
# Generate Regression models
fraudDataTuning <- fraudDataTuning %>%
  mutate(fraud = ifelse(fraud == "F", 1, 0))

# Logistic Regression
# Normalise using min-max
fraudDataTuning <- fraudDataTuning %>%
  mutate(amount = (amount - min(amount))/
           (max(amount) - min(amount)))

fraudDataTuningTrain <- fraudDataTuning[sampleData, ]
fraudDataTuningTest <- fraudDataTuning[-sampleData, ]

fraudDataGlm <- glm(data = fraudDataTuning,
                           family = "binomial",
                           formula = fraud ~ .)

# Naive-Bayes
fraudDataNaiveBayes <- naiveBayes(formula = fraud ~ .,
                                  data = fraudDataTuningTest,
                                  laplace = 1)

# KNN
# Create labels for KNN
fraudDataLabels <- fraudDataTuning %>% select(fraud)

# Split labels in testing and training dataset
fraudDataLabelsTraining <- fraudDataLabels[sampleData, ]
fraudDataLabelsTesting <- fraudDataLabels[-sampleData, ]

fraudDataKNN <- knn(train = fraudDataTuningTrain,
                    test = fraudDataTuningTest,
                    cl = fraudDataLabelsTraining$fraud,
                    k = 9)

# Decision tree
fraudDataDecisionTreeModel <- rpart(formula = fraud ~ .,
                                    method = "class",
                                    cp = 0.05,
                                    data = fraudDataTuningTrain)

# Neural network with 3 nodes
# Corelation plot
corrplot(cor(fraudDataTuning),
         method = "circle",
         type = "lower")

fraudDataNeuralNet <- neuralnet(formula = fraud ~
                                  category + amount + merchant_ID,
                                data = fraudDataTuningTrain,
                                hidden = 1,
                                act.fct = "logistic",
                                linear.output = FALSE)

## Generate prediction for models
# GLM model prediction
fraudDataGlmPrediction <- predict(fraudDataGlm,
                                  fraudDataTuningTest,
                                  type = "response")

fraudDataGlmPrediction <- ifelse(fraudDataGlmPrediction >= 0.4266270, 1, 0)

# Naive-Bayes Prediction
fraudDataNaiveBayesPrediction <- predict(fraudDataNaiveBayes,
                                         fraudDataTuningTest,
                                         type = "class")

# Decision Tree prediction
fraudDataDecisionTreePrediction <- predict(fraudDataDecisionTreeModel,
                                           fraudDataTuningTest,
                                           type = "class")

# Probabilities for Neural Network
fraudDataNeuralNetProbabilities <- compute(fraudDataNeuralNet, fraudDataTest)
summary(fraudDataNeuralNetProbabilities$net.result)
fraudDataNeuralNetPrediction <-
  ifelse(fraudDataNeuralNetProbabilities$net.result > 0.42529, 1, 0)

# Prediction summary
summary(fraudDataGlmPrediction)
summary(fraudDataKNN)
summary(fraudDataNaiveBayesPrediction)
summary(fraudDataDecisionTreePrediction)
print(fraudDataNeuralNetProbabilities$net.result)

## Generate Plots

# Create displayAllHistograms using function
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>% keep(is.numeric) %>% gather() %>% ggplot() +
    geom_histogram(mapping = aes(x = value, fill = key),
                   color = "black") + facet_wrap(~ key, scales = "free") +
    theme_minimal()
}

# Display  plots
fraudDataHistogramTibble <- read_csv(file = "FraudulentTransactions.csv")
displayAllHistograms(fraudDataHistogramTibble)
rpart.plot(fraudDataDecisionTreeModel)
plot(fraudDataNeuralNet)

## Generate confusion matrix & calculate predictive accuracy
# Create a confusion matrix
fraudDataGlmConfusionMatrix <-
  table(fraudDataTuningTest$fraud,
        fraudDataGlmPrediction)

fraudDataKNNConfusionMatrix <-
  table(fraudDataLabelsTesting$fraud, fraudDataKNN)

fraudDataNaiveBayesConfusionMatrix <-
  table(fraudDataTuningTest$fraud,
        fraudDataNaiveBayesPrediction)

fraudDataDecisionTreeConfusionMatrix <-
  table(fraudDataTuningTest$fraud,
        fraudDataDecisionTreePrediction)

fraudDataNeuralNetConfusionMatrix <-
  table(fraudDataTest$fraud,
        fraudDataNeuralNetPrediction)

# Calculate false positive
# Logistic
fraudDataGlmConfusionMatrix[1, 2] /
  (fraudDataGlmConfusionMatrix[1, 1] +
     fraudDataGlmConfusionMatrix[1, 2])

# KNN
fraudDataKNNConfusionMatrix[1, 2] /
  (fraudDataKNNConfusionMatrix[1, 1] +
     fraudDataKNNConfusionMatrix[1, 2])

# Naive Bayes
fraudDataNaiveBayesConfusionMatrix[1, 2] /
  (fraudDataNaiveBayesConfusionMatrix[1, 1] +
     fraudDataNaiveBayesConfusionMatrix[1, 2])

# Decision Tree
fraudDataDecisionTreeConfusionMatrix[1, 2] /
  (fraudDataDecisionTreeConfusionMatrix[1, 1] +
     fraudDataDecisionTreeConfusionMatrix[1, 2])

# Neural Network
fraudDataNeuralNetConfusionMatrix[1, 2] /
  (fraudDataNeuralNetConfusionMatrix[1, 1] +
     fraudDataNeuralNetConfusionMatrix[1, 2])


## Calculate false negative
# Logistic Regression
fraudDataNaiveBayesConfusionMatrix[2, 1] /
  (fraudDataNaiveBayesConfusionMatrix[2, 1] +
     fraudDataNaiveBayesConfusionMatrix[2, 2])

# KNN
fraudDataKNNConfusionMatrix[2, 1] /
  (fraudDataKNNConfusionMatrix[2, 1] +
     fraudDataKNNConfusionMatrix[2, 2])

# Naive Bayes
fraudDataNaiveBayesConfusionMatrix[2, 1] /
  (fraudDataNaiveBayesConfusionMatrix[2, 1] +
     fraudDataNaiveBayesConfusionMatrix[2, 2])

# Decision Tree
fraudDataDecisionTreeConfusionMatrix[2, 1] /
  (fraudDataDecisionTreeConfusionMatrix[2, 1] +
      fraudDataDecisionTreeConfusionMatrix[2, 2])

# Neural Network
fraudDataNeuralNetConfusionMatrix[2, 1] /
  (fraudDataNeuralNetConfusionMatrix[2, 1] +
     fraudDataNeuralNetConfusionMatrix[2, 2])


# Display the confusion matrix
print(fraudDataGlmConfusionMatrix)
print(fraudDataKNNConfusionMatrix)
print(fraudDataNaiveBayesConfusionMatrix)
print(fraudDataDecisionTreeConfusionMatrix)
print(fraudDataNeuralNetConfusionMatrix)

# Predictive accuracy
fraudDataGlmPredictiveAccuracy <-
  sum(diag(fraudDataGlmConfusionMatrix)) /
  nrow(fraudDataTuningTest)

fraudDataKNNPredictiveAccuracy <-
  sum(diag(fraudDataKNNConfusionMatrix)) /
  nrow(fraudDataTuningTest)

fraudDataNaiveBayesPredictiveAccuracy <-
  sum(diag(fraudDataNaiveBayesConfusionMatrix)) /
  nrow(fraudDataTuningTest)

fraudDataDecisionTreePredictiveAccuracy <-
  sum(diag(fraudDataDecisionTreeConfusionMatrix)) /
  nrow(fraudDataTuningTest)

fraudDataNeuralNetPredictiveAccuracy <-
  sum(diag(fraudDataNeuralNetConfusionMatrix)) /
  nrow(fraudDataTuningTest)

# Display predictive accuracy
print(fraudDataGlmPredictiveAccuracy)
print(fraudDataKNNPredictiveAccuracy)
print(fraudDataNaiveBayesPredictiveAccuracy)
print(fraudDataDecisionTreePredictiveAccuracy)
print(fraudDataNeuralNetPredictiveAccuracy)

## Find optimal K Value
# Create a matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to the matrix
colnames(kValueMatrix) <- c("kvalue", "Predictive Accuracy")

# Loop through odd values of k from 1 up to the number of records
# in the training dataset
for (kValue in 1:nrow(fraudDataTuningTrain)) {
  # Only calculate predictive accuracy if the k value is odd
  if (kValue %% 2 != 0) {
    # Generate the model
    fraudDataKNN <- knn(train = fraudDataTuningTrain,
                        test = fraudDataTuningTest,
                        cl = fraudDataLabelsTraining$fraud,
                        k = 9)
    # Generate the confusion matrix
    fraudDataKNNConfusionMatrix <- table(fraudDataLabelsTesting$fraud, 
                                         fraudDataKNN)
    # Calculate predictive accuracy
    fraudDataKNNPredictiveAccuracy <-
      sum(diag(fraudDataKNNConfusionMatrix)) / nrow(fraudDataTuningTest)
    
    # Add a new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix,
                          c(kValue,
                            fraudDataKNNPredictiveAccuracy))
  }
}
# Display k value matrix
print(kValueMatrix)
