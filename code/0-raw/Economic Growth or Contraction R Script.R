###load libraries
if (!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if (!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if (!require(dynlm)) install.packages("dynlm", repos = "http://cran.us.r-project.org")
if (!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(Metrics)) install.packages("Metrics", repos = "http://cran.us.r-project.org")
if (!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if (!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if (!require(latticeExtra)) install.packages("latticeExtra", repos = "http://cran.us.r-project.org")
###

###import data
#import gdp growth data
download.file("https://raw.githubusercontent.com/jbalzani/edx-Economic-Growth-or-Contraction-Project/master/A191RL1Q225SBEA.csv", "A191RL1Q225SBEA.csv")
A191RL1Q225SBEA <- read_csv("A191RL1Q225SBEA.csv")

#import cli data
download.file("https://raw.githubusercontent.com/jbalzani/edx-Economic-Growth-or-Contraction-Project/master/OECDLOLITOAASTSAM.csv", "OECDLOLITOAASTSAM.csv")
OECDLOLITOAASTSAM <- read_csv("OECDLOLITOAASTSAM.csv")
###

###########exploratory data analysis
#check for NAs in gdp data
A191RL1Q225SBEA %>% filter(is.na(DATE))
A191RL1Q225SBEA %>% filter(is.na(A191RL1Q225SBEA))

#check for duplicates gdp
gdp_duplicates <- A191RL1Q225SBEA[duplicated(A191RL1Q225SBEA), ]
gdp_duplicates

#calculate min and max of rating
gdp_min <- min(A191RL1Q225SBEA$A191RL1Q225SBEA)
gdp_max <- max(A191RL1Q225SBEA$A191RL1Q225SBEA)

#boxplot of gdp growth
boxplot(A191RL1Q225SBEA$A191RL1Q225SBEA, main = "Figure 1: Boxplot of GDP Growth")

#check for NAs in cli data
OECDLOLITOAASTSAM %>% filter(is.na(DATE))
OECDLOLITOAASTSAM %>% filter(is.na(OECDLOLITOAASTSAM))

#check for duplicates
cli_duplicates <- OECDLOLITOAASTSAM[duplicated(OECDLOLITOAASTSAM), ]
cli_duplicates

#calculate min and max of CLI
cli_min <- min(OECDLOLITOAASTSAM$OECDLOLITOAASTSAM)
cli_max <- max(OECDLOLITOAASTSAM$OECDLOLITOAASTSAM)

#boxplot of cli
boxplot(OECDLOLITOAASTSAM$OECDLOLITOAASTSAM, main = "Figure 2: Boxplot of CLI Data")

#histogram of cli
hist(OECDLOLITOAASTSAM$OECDLOLITOAASTSAM, main = "Figure 3: Histogram of CLI Data", 
     xlab = "CLI")

#get only the months that have qtly gdp data
CLI_filtered <- OECDLOLITOAASTSAM %>% 
  filter(DATE >= '1982-01-01') %>%
  mutate(MONTH = month(DATE)) %>% #extract month
  filter(MONTH %in% c(1, 4, 7, 10))#filter for months with qtly gdp data

#combine datasets
combined_data <- A191RL1Q225SBEA %>%    #start with gdp growth dataset
  mutate(MONTH = CLI_filtered$MONTH) %>%    #add month variable
  mutate(CLI = CLI_filtered$OECDLOLITOAASTSAM) %>%    #add CLI variable
  mutate(real_gdp_growth = A191RL1Q225SBEA) %>%    #create col with shorter name
  mutate(gdp_impr = factor(ifelse(real_gdp_growth > 0, 1, 0)))   #create y/n dependent var.

#create train and test data
combined_data_train <- combined_data %>% filter(DATE >= "1983-01-01" & DATE < "2008-07-01") 
combined_data_test <- combined_data %>% filter(DATE >= "2008-07-01")

#plot real gdp growth and CLI for train data
combined_data_train %>% ggplot(aes(x = DATE)) +
  geom_line(aes(y = CLI, color = "CLI")) +
  geom_line(aes(y = real_gdp_growth, color = "real_gdp_growth")) +
  ggtitle("Figure 4: Real GDP Growth and CLI 1983-Apr 2008")

#plot gdp improvement vs cli
plot_impr_cli <- xyplot(gdp_impr ~ CLI, data = combined_data_train, 
                        main = "Figure 5: gdp_impr vs. CLI")
plot_impr_cli
##### end eda

#make combined data a ts object, needed for stationarity test
combined_data_train_ts <- combined_data_train %>% ts()

###create gdp and delta gdp lags
#create lags of gdp growth
combined_data <- combined_data %>%
  mutate(real_gdp_growth_lag = lag(real_gdp_growth),
         real_gdp_growth_lag2 = lag(real_gdp_growth, 2),
         real_gdp_growth_lag3 = lag(real_gdp_growth, 3),
         real_gdp_growth_lag4 = lag(real_gdp_growth, 4))

#create first differences of lagged gdp growth
combined_data <- combined_data %>%
  mutate(delta_real_gdp_growth = real_gdp_growth - real_gdp_growth_lag,
         delta_real_gdp_growth_lag = real_gdp_growth_lag - lag(real_gdp_growth_lag),
         delta_real_gdp_growth_lag2 = real_gdp_growth_lag2 - lag(real_gdp_growth_lag2), 
         delta_real_gdp_growth_lag3 = real_gdp_growth_lag3 - lag(real_gdp_growth_lag3),
         delta_real_gdp_growth_lag4 = real_gdp_growth_lag4 - lag(real_gdp_growth_lag4))

#filter to include all lags and up to 2008 recession
combined_data_train <- combined_data %>% filter(DATE >= "1983-01-01" & DATE < "2008-07-01") 
combined_data_test <- combined_data %>% filter(DATE >= "2008-07-01")

#make combined data a ts object, needed for stationarity test
combined_data_train_ts <- combined_data_train %>% ts()
###

#adf test for gdp growth 4 lags
reg_gdp_adf_lags1234 <- dynlm(delta_real_gdp_growth~real_gdp_growth_lag + 
                                delta_real_gdp_growth_lag + delta_real_gdp_growth_lag2 + 
                                delta_real_gdp_growth_lag3 + delta_real_gdp_growth_lag4, 
                              data = combined_data_train_ts)
#create summary
reg_gdp_adf_lags1234summ <- summary(reg_gdp_adf_lags1234)
reg_gdp_adf_lags1234summ

#adf test gdp growth 3 lags
reg_gdp_adf_lags123 <- dynlm(delta_real_gdp_growth~real_gdp_growth_lag + 
                               delta_real_gdp_growth_lag + delta_real_gdp_growth_lag2 + 
                               delta_real_gdp_growth_lag3, data = combined_data_train_ts)
#create summary
reg_gdp_adf_lags123summ <- summary(reg_gdp_adf_lags123)
reg_gdp_adf_lags123summ

#adf test gdp growth 2 lags
reg_gdp_adf_lags12 <- dynlm(delta_real_gdp_growth~real_gdp_growth_lag + 
                              delta_real_gdp_growth_lag + delta_real_gdp_growth_lag2, 
                            data = combined_data_train_ts)
#create summary
reg_gdp_adf_lags12summ <- summary(reg_gdp_adf_lags12)
reg_gdp_adf_lags12summ

#adf test gdp growth 1 lag
reg_gdp_adf_lag1 <- dynlm(delta_real_gdp_growth~real_gdp_growth_lag + 
                            delta_real_gdp_growth_lag, data = combined_data_train_ts)
#create summary
reg_gdp_adf_lag1summ <- summary(reg_gdp_adf_lag1)
reg_gdp_adf_lag1summ

###create cli lags and delta cli lags
#create cli lags
combined_data <- combined_data %>%
  mutate(CLI_lag = lag(CLI),
         CLI_lag2 = lag(CLI, 2),
         CLI_lag3 = lag(CLI, 3),
         CLI_lag4 = lag(CLI, 4))

#create cli first difference lags        
combined_data <- combined_data %>%
  mutate(delta_CLI = CLI - CLI_lag,
         delta_CLI_lag = CLI_lag - CLI_lag2,
         delta_CLI_lag2 = CLI_lag2 - CLI_lag3,
         delta_CLI_lag3 = CLI_lag3 - lag(CLI_lag3),
         delta_CLI_lag4 = CLI_lag4 - lag(CLI_lag4))

#filter to include all lags and up to 2008 recession
combined_data_train <- combined_data %>% filter(DATE >= "1983-01-01" & DATE < "2008-07-01") 
combined_data_test <- combined_data %>% filter(DATE >= "2008-07-01")

#make combined data a ts object, needed for stationarity test
combined_data_train_ts <- combined_data_train %>% ts()
###

#adf test cli 4 lags
reg_CLI_lags1234 <- dynlm(delta_CLI~CLI_lag + delta_CLI_lag + delta_CLI_lag2 + 
                            delta_CLI_lag3 + delta_CLI_lag4, data = combined_data_train_ts)
#create summary
reg_CLI_lags1234summ <- summary(reg_CLI_lags1234)
reg_CLI_lags1234summ

###create cli and delta cli lags 5-8
#create cli lags 5-8
combined_data <- combined_data %>%
  mutate(CLI_lag5 = lag(CLI, 5),
         CLI_lag6 = lag(CLI, 6),
         CLI_lag7 = lag(CLI, 7),
         CLI_lag8 = lag(CLI, 8))

#create cli first difference lags 5-8         
combined_data <- combined_data %>%
  mutate(delta_CLI_lag5 = CLI_lag5 - lag(CLI_lag5),
         delta_CLI_lag6 = CLI_lag6 - lag(CLI_lag6),
         delta_CLI_lag7 = CLI_lag7 - lag(CLI_lag7),
         delta_CLI_lag8 = CLI_lag8 - lag(CLI_lag8))

#filter to include all lags and up to 2008 recession
combined_data_train <- combined_data %>% filter(DATE >= "1983-01-01" & DATE < "2008-07-01") 
combined_data_test <- combined_data %>% filter(DATE >= "2008-07-01")

#make combined data a ts object, needed for stationarity test
combined_data_train_ts <- combined_data_train %>% ts()
###

#adf test cli 8 lags
reg_CLI_8lags <- dynlm(delta_CLI~CLI_lag + delta_CLI_lag + delta_CLI_lag2 + 
                         delta_CLI_lag3 + delta_CLI_lag4 + delta_CLI_lag5 + 
                         delta_CLI_lag6 + delta_CLI_lag7 + delta_CLI_lag8, 
                       data = combined_data_train_ts)
#create summary
reg_CLI_8lagssumm <- summary(reg_CLI_8lags)
reg_CLI_8lagssumm

#adf test cli 7 lags
reg_CLI_7lags <- dynlm(delta_CLI~CLI_lag + delta_CLI_lag + delta_CLI_lag2 + 
                         delta_CLI_lag3 + delta_CLI_lag4 + delta_CLI_lag5 + 
                         delta_CLI_lag6 + delta_CLI_lag7, data = combined_data_train_ts)
#create summary
reg_CLI_7lagssumm <- summary(reg_CLI_7lags)
reg_CLI_7lagssumm

#adf test cli 6 lags
reg_CLI_6lags <- dynlm(delta_CLI~CLI_lag + delta_CLI_lag + delta_CLI_lag2 + 
                         delta_CLI_lag3 + delta_CLI_lag4 + delta_CLI_lag5 + 
                         delta_CLI_lag6, data = combined_data_train_ts)
#create summary
reg_CLI_6lagssumm <- summary(reg_CLI_6lags)
reg_CLI_6lagssumm

#adf test cli 5 lags
reg_CLI_5lags <- dynlm(delta_CLI~CLI_lag + delta_CLI_lag + delta_CLI_lag2 + 
                         delta_CLI_lag3 + delta_CLI_lag4 + delta_CLI_lag5, 
                       data = combined_data_train_ts)
#create summary
reg_CLI_5lagssumm <- summary(reg_CLI_5lags)
reg_CLI_5lagssumm

#logistic regression model
logistic_model_all <- glm(formula = gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + 
                            real_gdp_growth_lag + real_gdp_growth_lag3, 
                          family = binomial(link = "logit"), 
                          data = combined_data_train, maxit = 1000000)

###knn model
#train knn algorithm on training set
set.seed(1)
#values of k to test
k <- seq(2, 25, 1)

#calculate accuracies for different k values
train_accuracy <- sapply(k, function(ks) {
  knn_model <- knn3(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + real_gdp_growth_lag + 
                      real_gdp_growth_lag2 + real_gdp_growth_lag3, 
                    data = combined_data_train, k = ks)
  mean(knn_model$learn$y == combined_data_train$gdp_impr)
})

best_k <- k[which.max(train_accuracy)]

#knn model for optimal k
knn_model <- knn3(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + real_gdp_growth_lag + 
                    real_gdp_growth_lag2 + real_gdp_growth_lag3, 
                  data = combined_data_train, k = best_k)
###

### initial random forest model
set.seed(1)
#random forest initial model
rf_model_initial <- randomForest(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + 
                                   real_gdp_growth_lag + real_gdp_growth_lag2 + 
                                   real_gdp_growth_lag3, data = combined_data_train)

#to see how the model varies with a different number of trees
plot(rf_model_initial, main = "Figure 6: Initial RF Model Error vs. Number of Trees")
###

###optimized random forest model
#values of mtry to test
m <- seq(1, 6, 1)

#accuracies for different mtry parameter values
set.seed(1)
accuracy <- sapply(m, function(m) {
  rf_model <- randomForest(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + 
                             real_gdp_growth_lag + real_gdp_growth_lag2 + 
                             real_gdp_growth_lag3, data = combined_data_train, 
                           mtry = m, ntree = 100)
  mean(rf_model$y == combined_data_train$gdp_impr)
})
best_m <- m[which.max(accuracy)]

#rf model with optimal mtry parameter
rf_model <- randomForest(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + real_gdp_growth_lag + 
                           real_gdp_growth_lag2 + real_gdp_growth_lag3, 
                         data = combined_data_train, mtry = 1, ntree = 25)
###

#logit model test set predictions
#predicted economic growth probabilities on test set
p_hat_logistic_all <- predict(logistic_model_all, newdata = combined_data_test, 
                              type = "response")
#test set predictions
y_hat_logistic_all <- ifelse(p_hat_logistic_all > 0.5, 1, 0)

#knn model test set predictions
combined_data_test$gdp_impr <- as.factor(combined_data_test$gdp_impr)
y_hat_knn <- predict(knn_model, newdata = combined_data_test, type = "class")

#rf model test set predictions
y_hat_rf_optimized <- predict(rf_model, newdata = combined_data_test)

###ensemble predictions
predictions_tbl <- data.frame(logit = as.numeric(y_hat_logistic_all),
                              knn = as.numeric(y_hat_knn),
                              rf = as.numeric(y_hat_rf_optimized))

#map factor values to numeric values
predictions_tbl <- predictions_tbl %>%
  mutate(logit = logit,
         knn = ifelse(knn == 2, 1, 0),
         rf = ifelse(rf == 2, 1, 0))

#generate ensemble predictions
y_hat_ensemble <- rep(NA, nrow(predictions_tbl))
for (i in 1:nrow(predictions_tbl)) {
  y_hat_ensemble[i] <- ifelse(sum(predictions_tbl[i,]) >= 2, 1, 0)
}

#add ensemble predictions to table of predictions
predictions_tbl <- predictions_tbl %>%
  mutate(ensemble = y_hat_ensemble)
###

#confusion matrix logistic model
confusionMatrix(factor(y_hat_logistic_all), reference = factor(combined_data_test$gdp_impr))
#calculate f1 score
f1_logit <- F_meas(factor(y_hat_logistic_all), factor(combined_data_test$gdp_impr))

#confusion matrix knn model
confusionMatrix(factor(y_hat_knn), reference = factor(combined_data_test$gdp_impr))
#calculate f1 score - code won't run since score is 0, because no correct positive predictions
#f1_knn <- F_meas(factor(y_hat_knn), factor(combined_data_test$gdp_impr))

#confusion matrix rf model
confusionMatrix(factor(y_hat_rf_optimized), reference = factor(combined_data_test$gdp_impr))
#calculate F1 score - code won't run since score is 0, because no correct positive predictions
#f1_rf <- F_meas(factor(y_hat_rf_optimized), factor(combined_data_test$gdp_impr))

#confusion matrix ensemble prediction
confusionMatrix(factor(y_hat_ensemble), reference = factor(combined_data_test$gdp_impr))
#calculate f1 score
#f1_ensemble <- F_meas(factor(y_hat_ensemble), factor(combined_data_test$gdp_impr))

#add predictions to test set
combined_data_test <- combined_data_test %>%
  mutate(logit = factor(y_hat_logistic_all),
         knn = y_hat_knn,
         rf = y_hat_rf_optimized,
         ensemble = factor(y_hat_ensemble))

#plot logit model vs real outcomes
combined_data_test %>% ggplot(aes(x = DATE)) +
  geom_point(aes(y = gdp_impr, color = "gdp_impr")) +
  geom_point(aes(y = logit, color = "logit")) +
  ggtitle("Figure 7: Logit Model Predictions vs. Real Economic Outcomes", 
          subtitle = "Red Indicates a Miss")

#plot knn model vs real outcomes
combined_data_test %>% ggplot(aes(x = DATE)) +
  geom_point(aes(y = gdp_impr, color = "gdp_impr")) +
  geom_point(aes(y = knn, color = "knn")) +
  ggtitle("Figure 8: KNN Model Predictions vs. Real Economic Outcomes", 
          subtitle = "Red Indicates a Miss")

#plot rf model vs real outcomes
combined_data_test %>% ggplot(aes(x = DATE)) +
  geom_point(aes(y = gdp_impr, color = "gdp_impr")) +
  geom_point(aes(y = rf, color = "rf")) +
  ggtitle("Figure 9: Random Forest Model Predictions vs. Real Economic Outcomes", 
          subtitle = "Red Indicates a Miss")

#plot ensemble model vs real outcomes
combined_data_test %>% ggplot(aes(x = DATE)) +
  geom_point(aes(y = gdp_impr, color = gdp_impr)) +
  geom_point(aes(y = ensemble, color = "ensemble")) +
  ggtitle("Figure 10: Ensemble Model Predictions vs. Real Economic Outcomes", 
          subtitle = "Red Indicates a Miss")

#calculate mean gdp_impr for baseline comparison
combined_data_test <- combined_data_test %>%
  mutate(mean_test_gdp_impr = mean(as.numeric(gdp_impr)))



