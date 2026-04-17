library(BenchHub)
library(limma)
library(dplyr)
library(randomForestSRC)
library(survival)

# TrioB1

# Calculate the accuracy of survival model prediction

# - Here, we use the survival data from survBenchmark-figShare. 
# - This survival data is clinical (lung). 
# - The Figshare ID is [26142922/47361079](https://figshare.com/articles/dataset/SurvBenchmark_comprehensive_benchmarking_study_of_survival_analysis_methods_using_both_omics_data_and_clinical_data/26142922?file=47361079) 

testCache <- tempdir()
trioB1 <- Trio$new("figshare:26142922/47361079", cachePath = testCache)
data <- trioB1 |> purrr::pluck("data")

# Preprocessing: Feature selection of omics data

data_filtered <- data[, !grepl("\\.\\.\\.1|time", colnames(data))]
data_filtered$status <- factor(data_filtered$status, levels = c(0, 1))
design <- model.matrix(~ 0 + data_filtered$status)
colnames(design) <- c("Group0", "Group1")

expression_data <- t(data_filtered[, sapply(data_filtered, is.numeric) & colnames(data_filtered) != "status"])

fit <- lmFit(expression_data, design)
contrast_matrix <- makeContrasts(Group1 - Group0, levels = design)
fit2 <- contrasts.fit(fit, contrast_matrix)
fit2 <- eBayes(fit2)

top_genes <- topTable(fit2, number = 100, adjust = "BH", sort.by = "P")

top_gene_names <- rownames(top_genes)
data_new <- data[, c(top_gene_names, "time", "status")]

trioB1$data <- data_new


# Prepare survival model 

# In this example, we use Cox proportional hazards regression model. 
# Since we use c-index as evaluation metric, we here usethe Cross-validation folds for an example. 

# Preapre function
# - surv_risk_pred: risk prediction/expectation
# - surv_obj: survival object, this is for Gold Standard

surv_risk_pred <- function(model, data) {
  if (is.null(data)) {
    return(unname(stats::predict(model, type = "risk")))
  } else {
    return(unname(stats::predict(model, newdata = data, type = "risk")))
  }
}
surv_obj <- function(data) {
  sub_data_test <- data[data[,"index"] == "test", ]
  sub_data_train <- data[data[,"index"] == "train", ]
  test_surv_obj <- with(sub_data_test, survival::Surv(time, status))
  train_surv_obj <- with(sub_data_train, survival::Surv(time, status))
  return(list(train_surv_obj, test_surv_obj))
}

harrelCIndexMetric <- function(to_eval, gs) {
  harrelC1 <- Hmisc::rcorr.cens(-to_eval[[2]], gs[[2]])
  return(harrelC1["C Index"])
}

beggCIndexMetric <- function(to_eval, gs) {
  begg_cindex <- survAUC::BeggC(gs[[1]], gs[[2]], to_eval[[1]], to_eval[[2]])
  return(begg_cindex)
}

brierScoreMetric <- function(to_eval, gs) {
  time <- gs[[1]][, "time"]
  return(survAUC::predErr(
    gs[[1]], gs[[2]], to_eval[[1]], to_eval[[2]],
    times = time, type = "brier", int.type = "unweighted"
  )$error)
}

unoCIndexMetric <- function(to_eval, gs) {
  
  return(survAUC::UnoC(Surv.rsp = gs[[2]], Surv.rsp.new = gs[[2]], 
                       lpnew = to_eval[[2]]))
}


# Add the metric and AuxData into the trioB1

trioB1$addMetric("Harrel C-Index", harrelCIndexMetric)
trioB1$addMetric("uno C-Index", unoCIndexMetric)
trioB1$addAuxData("surv_obj", surv_obj, c("Harrel C-Index", "uno C-Index"))

set.seed(132)

trioB1$split(y = data$status, n_fold = 5, n_repeat = 10)
cv_ind = trioB1$splitIndices
cv_res_harrel = c()
cv_res_uno = c()
cv_res_harrel2 = c()
cv_res_uno2 = c()

for (i in 1:length(cv_ind)) {
  
  data_woindex <- trioB1 |> purrr::pluck("data")
  data_windex  <- trioB1 |> purrr::pluck("data")
  train_id = cv_ind[[i]]
  test_id <- setdiff(seq_len(nrow(data_woindex)), train_id)
  
  train = data_woindex[train_id, ]
  test = data_woindex[-train_id, ]
  
  fit <- survival::coxph(survival::Surv(time, status) ~ ., data = train)
  
  fit2 <- rfsrc(Surv(time, status) ~ ., data = train, ntree = 100)
  
  data_windex$index <- ifelse(1:nrow(data) %in% test_id, "test", "train")
  
  predicted <- list(
    ifelse(is.infinite(surv_risk_pred(fit, NULL)), 0, surv_risk_pred(fit, NULL)),
    ifelse(is.infinite(surv_risk_pred(fit, test)), 0, surv_risk_pred(fit, test))
  )
  
  predicted2 <- list(
    train = predict(fit2)$predicted,
    test  = predict(fit2, test)$predicted
  )
  
  trioB1$data <- data_windex
  
  evaluation <- trioB1$evaluate(list(surv_obj = predicted))
  evaluation2 <- trioB1$evaluate(list(surv_obj = predicted2))
  
  cv_res_harrel[i] <- evaluation$result[evaluation$metric == "Harrel C-Index"]
  cv_res_uno[i] <- evaluation$result[evaluation$metric == "uno C-Index"]
  
  cv_res_harrel2[i] <- evaluation2$result[evaluation2$metric == "Harrel C-Index"]
  cv_res_uno2[i] <- evaluation2$result[evaluation2$metric == "uno C-Index"]
  
  trioB1$data <- data_woindex
}



# Visualization
cindex_results <- data.frame(
  metric = rep(c("harrel", "uno"), each = length(cv_res_harrel) * 2 / 2),
  result = c(cv_res_harrel, cv_res_uno, cv_res_harrel2, cv_res_uno2),
  model = rep(c("cox", "rsf"), each = length(cv_res_harrel) * 2 )
)

cindex_results <- cindex_results[cindex_results$result != 0, ]

th <-   theme(text=element_text(size=12 ),
              axis.text.x = element_text(angle = 45, hjust = 1),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(colour = "black", size=0.2, fill=NA) )

p <- ggplot(cindex_results, aes(x = model, y = result, fill = model)) +
  geom_boxplot() +
  facet_wrap(~ metric, ncol = 1) +  
  labs(
    x = "model",
    y = "C-index",
    fill = "model"
  ) +
  th

ggsave("cindex_boxplot.pdf", plot = p, width = 4, height = 6, dpi = 300)
