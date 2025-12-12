
data <- read.csv("~/Downloads/add.csv")
data <- rename(data, "height"="X0")
data <- rename(data, "width" ="X1")
data <- rename(data, "ratio"="X2")
data <- rename(data, "status" ="X1558")

data$height <- ifelse(trimws(data$height) == "?", NA, data$height)
data$width  <- ifelse(trimws(data$width) == "?", NA, data$width)
data$ratio  <- ifelse(trimws(data$ratio) == "?", NA, data$ratio)

data$status <- ifelse(data$status == "ad.", 1, 0)
data$height <- as.numeric(data$height)
data$width  <- as.numeric(data$width)
data$ratio  <- as.numeric(data$ratio)


data$height[is.na(data$height)] <- median(data$height, na.rm = TRUE)
data$width[is.na(data$width)]   <- median(data$width, na.rm = TRUE)

update_ratio <- function(df) {
  df$ratio <- df$width / df$height
  
  return(df)
}
data <- update_ratio(data)

remove_outliers_by_group <- function(df) {
  df_clean <- data.frame()
  
  for (label in unique(df$status)) {
    group_data <- subset(df, status == label)
    
    h_q1  <- quantile(group_data$height, 0.25, na.rm = TRUE)
    h_q3  <- quantile(group_data$height, 0.75, na.rm = TRUE)
    h_iqr <- h_q3 - h_q1
    
    w_q1  <- quantile(group_data$width, 0.25, na.rm = TRUE)
    w_q3  <- quantile(group_data$width, 0.75, na.rm = TRUE)
    w_iqr <- w_q3 - w_q1
    
    h_outlier <- group_data$height < (h_q1 - 1.5 * h_iqr) | group_data$height > (h_q3 + 1.5 * h_iqr)
    w_outlier <- group_data$width  < (w_q1 - 1.5 * w_iqr) | group_data$width  > (w_q3 + 1.5 * w_iqr)
    
    group_data_clean <- group_data[!(h_outlier | w_outlier), ]
    df_clean <- rbind(df_clean, group_data_clean)
  }
  return(df_clean)
}

data <- remove_outliers_by_group(data)

# Chia bộ dữ liệu thành train (70%) và test (30%)
sample_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

train_data$status <- factor(train_data$status)
test_data$status <- factor(test_data$status)

library(randomForest)
library(caret)
set.seed(8)

rf_model <- randomForest(
  status ~ .,
  data = train_data,
  ntree = 500,
  mtry = 2,
  importance = TRUE,
  proximity = FALSE
)

print(rf_model)


rf_pred_class <- predict(rf_model, newdata = test_data)
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "1"]

# Ma trận nhầm lẫn + độ đo tổng hợp
rf_cm <- confusionMatrix(rf_pred_class, test_data$status, positive = "1")

rf_accuracy <- rf_cm$overall["Accuracy"]
rf_sensitivity <- rf_cm$byClass["Sensitivity"]
rf_specificity <- rf_cm$byClass["Specificity"]

cat("Accuracy: ", rf_accuracy, "\n")
cat("Sensitivity: ", rf_sensitivity, "\n")
cat("Specificity: ", rf_specificity, "\n")

# ROC + AUC cho Random Forest và so sánh với logistic (ratio)
rf_roc <- roc(test_data$status, rf_prob)
rf_auc <- auc(rf_roc)
cat("AUC Random Forest: ", rf_auc, "\n")


plot(roc_ratio, main = "So sánh ROC: Logistic (ratio) vs Random Forest",
     col = "steelblue", lwd = 2)
lines(rf_roc, col = "firebrick", lwd = 2)
legend("bottomright",
       legend = c(paste0("Logistic (AUC = ", round(auc(roc_ratio), 3), ")"),
                  paste0("Random Forest (AUC = ", round(rf_auc, 3), ")")),
       col = c("steelblue", "firebrick"),
       lwd = 2)


View(data)