library(caret)
library(pROC)
library(MASS)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(xgboost)


# Small dataset
set.seed(123)
n <- 200
p <- 20
prob <- 0.10

x <- mvrnorm(n, mu = rep(0, p), Sigma = diag(p))
y <- factor(rbinom(n, 1, prob = prob), levels = c(0,1))
data <- data.frame(x)
data$y <- y

# Function to calculate mean AUC across folds for one seed
run_cv_seed <- function(seed) {
  set.seed(seed)
  folds <- createFolds(data$y, k = 5, returnTrain = TRUE)
  
  glm_aucs <- c()
  rf_aucs <- c()
  xgb_aucs <- c()   # <- new vector
  
  for (i in seq_along(folds)) {
    train_idx <- folds[[i]]
    test_idx <- setdiff(seq_len(nrow(data)), train_idx)
    train <- data[train_idx, ]
    test <- data[test_idx, ]
    
    # Logistic regression
    glm_model <- glm(y ~ ., data = train, family = binomial)
    glm_prob <- predict(glm_model, newdata = test, type = "response")
    glm_aucs[i] <- auc(test$y, glm_prob)
    
    # Random forest
    rf_model <- randomForest(y ~ ., data = train)
    rf_prob <- predict(rf_model, newdata = test, type = "prob")[,2]
    rf_aucs[i] <- auc(test$y, rf_prob)
    
    # XGBoost
    xgb_train <- xgb.DMatrix(data = as.matrix(train[,-ncol(train)]), label = as.numeric(train$y) - 1)
    xgb_test  <- xgb.DMatrix(data = as.matrix(test[,-ncol(test)]), label = as.numeric(test$y) - 1)
    xgb_model <- xgboost(data = xgb_train, objective = "binary:logistic", nrounds = 50, verbose = 0)
    xgb_prob  <- predict(xgb_model, xgb_test)
    xgb_aucs[i] <- auc(test$y, xgb_prob)
  }
  
  data.frame(
    CV_Seed = as.factor(seed),
    Model = c("Logistic Regression", "Random Forest", "XGBoost"),
    AUC = c(mean(glm_aucs), mean(rf_aucs), mean(xgb_aucs))
  )
}

# Run for multiple seeds (10 different partition sets)
seeds <- 1:10
results_list <- lapply(seeds, run_cv_seed)
results_all <- do.call(rbind, results_list)

subset_results <- results_all %>%
  filter(CV_Seed %in% c("1", "6"))


subset_results_fixedc <- data.frame(
  CV_Seed = factor(c("1", "1","2", "2"), levels = as.character(1:10)),
  Model = c("Logistic regression", "Random forest", 
            "Logistic regression", "Random forest"),
  AUC = c(0.638, 0.535, 0.562, 0.650)  
)


subset_results_fixeda <- data.frame(
  CV_Seed = factor(c("1", "1","1","2", "2", "2"), levels = as.character(1:10)),
  Model = c("Logistic regression", "Random forest", "XGboost",
            "Logistic regression", "Random forest", "XGboost"),
  AUC = c(0.638, 0.535, 0.65, 0.562, 0.650, 0.587)  
)

subset_results_fixedb <- data.frame(
  CV_Seed = factor(c("1", "1","1","2", "2", "2"), levels = as.character(1:10)),
  Model = c("Logistic regression", "Random forest", "XGboost",
            "Logistic regression", "Random forest", "XGboost"),
  AUC = c(0.638, 0.535, 0.587, 0.562, 0.650, 0.65)  
)



p2 <- ggplot(subset_results_fixedc, aes(x = CV_Seed, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Logistic regression" = "#b1dbd3", 
                               "Random forest" = "#c9d380")) +
  labs(
    x = "Cross-validation seed",
    y = "Average AUC"
  ) +
  th

p3 <- ggplot(subset_results_fixeda, aes(x = CV_Seed, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Logistic regression" = "#b1dbd3", 
                               "Random forest" = "#c9d380",
                               "XGboost" = "#ee6c87")) +
  labs(
    x = "Cross-validation seed",
    y = "Average AUC"
  ) +
  th

p4 <- ggplot(subset_results_fixedb, aes(x = CV_Seed, y = AUC, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Logistic regression" = "#b1dbd3", 
                               "Random forest" = "#c9d380",
                               "XGboost" = "#ee6c87")) +
  labs(
    x = "Cross-validation seed",
    y = "Average AUC"
  ) +
  th

combined_plot <- ggpubr::ggarrange(
  p2, p3, p4,
  ncol = 3,  # 1 column
  nrow = 1,  # 2 rows
  legend = "bottom"
)

ggsave("fig3bV5.pdf", plot = combined_plot, width = 15, height = 4, dpi = 100)


