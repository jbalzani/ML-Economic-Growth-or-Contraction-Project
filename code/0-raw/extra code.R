#extra code

#``` {r}
#logistic_model_cli <- glm(formula = gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3, family = #binomial(link = "logit"), data = combined_data_train)
#```


#```{r}
#logistic_model_caret_all <- train(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + #real_gdp_growth_lag + real_gdp_growth_lag2 + real_gdp_growth_lag3, method = "glm", data = #combined_data_train)
#```


#sample size min for probit model
k <- 2 #number of independent variables
p <- sum(combined_data_train$gdp_impr == 0)/nrow(combined_data_train)
min_n <- 10*k/p


```{r}
p_hat_logistic_cli <- predict(logistic_model_cli, newdata = combined_data_test, type = "response")
y_hat_logistic_cli <- ifelse(p_hat_logistic_cli >= 0.5, 1, 0)
confusionMatrix(factor(y_hat_logistic_cli), reference = factor(combined_data_test$gdp_impr))
```

```{r make knn model}
#train knn algorithm on training set
k <- seq(3, 15, 1)
train_accuracy <- sapply(k, function(ks) {
  knn_model <- knn3(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + real_gdp_growth_lag + real_gdp_growth_lag2 + real_gdp_growth_lag3, data = combined_data_train, k = ks)
  y_hat_knn <- predict(knn_model, newdata = combined_data_train, type = "class")
  confusionMatrix(factor(y_hat_knn), reference = factor(combined_data_train$gdp_impr))$overall["Accuracy"]
})

k[which.max(train_accuracy)]
knn_model <- knn3(gdp_impr ~ CLI_lag + CLI_lag2 + CLI_lag3 + real_gdp_growth_lag + real_gdp_growth_lag2 + real_gdp_growth_lag3, data = combined_data_train, k = 3)


```

```{r}
y_hat_rf_initial <- predict(rf_model_initial, newdata = combined_data_test)
confusionMatrix(factor(y_hat_rf_initial), reference = factor(combined_data_test$gdp_impr))
```

p_hat_logistic_all_train <- predict(logistic_model_all, newdata = combined_data_train, type = "response")
y_hat_logistic_all_train <- ifelse(p_hat_logistic_all_train >= 0.5, 1, 0)

```{r}
combined_data_train$gdp_impr <- as.factor(combined_data_train$gdp_impr)
```

y_hat_knn_train <- knn_model$learn$y

y_hat_rf_optimized_train <- rf_model$y
mean(y_hat_ensemble == combined_data_test$gdp_impr)