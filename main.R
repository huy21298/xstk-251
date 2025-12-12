## TIEN XU LY DU LIEU

library(dplyr)
library(pROC)
library(randomForest)
library(caret)

# Đọc dữ liệu từ file CSV


data <- read.csv("~/Downloads/add.csv")

data <- rename(data, "height" = "X0")
data <- rename(data, "width" = "X1")
data <- rename(data, "ratio" = "X2")
data <- rename(data, "status"  = "X1558")

# Thay thế tất cả các biến thể của dấu '?' thành NA
# Sử dụng trimws để loại bỏ khoảng trắng thừa trước khi so sánh cho chính xác hơn
data$height <- ifelse(trimws(data$height) == "?", NA, data$height)
data$width  <- ifelse(trimws(data$width) == "?", NA, data$width)
data$ratio  <- ifelse(trimws(data$ratio) == "?", NA, data$ratio)

# Ép kiểu về number
data$height <- as.numeric(data$height)
data$width  <- as.numeric(data$width)
data$ratio  <- as.numeric(data$ratio)

# Đổi "ad." = 1, "nonad." = 0 và ép kiểu factor
data$status <- ifelse(data$status == "ad.", 1, 0)
str(data)
cat("\n--- Kích thước và dữ liệu mẫu sau khi chuẩn hóa ban đầu ---\n")
dim(data)   ### ---> (Xem số dòng/cột ban đầu)
head(data)  ### ---> (Xem dữ liệu có đúng kiểu số chưa)
View(data)

# Thống kê số lượng giá trị NA theo từng cột
na_counts <- colSums(is.na(data))

# Tính tỷ lệ phần trăm NA theo cột
na_percentage <- colMeans(is.na(data)) * 100

# Tạo bảng thống kê NA cho mỗi cột và in kết quả
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percentage = round(na_percentage, 2)
)
print(na_summary)
# --- XỬ LÝ MISSING VALUE (NA) ---

# Thay thế giá trị NA trong 'height' và 'width' bằng Median
data$height[is.na(data$height)] <- median(data$height, na.rm = TRUE)
data$width[is.na(data$width)]   <- median(data$width, na.rm = TRUE)
head(data)

# Tính lại ratio cho các dòng bị thiếu bằng công thức width/height
update_ratio <- function(df) {
  df$ratio <- df$width / df$height
  
  return(df)
}
data <- update_ratio(data)
print(data[, c('height', 'width', 'ratio', 'status')])

na_counts_after_median <- colSums(is.na(data))
print(sum(na_counts))

View(data)


# XỬ LÝ OUTLIER 
# Sử dụng phương pháp lọc theo từng nhóm (Target 0 và 1)
remove_outliers_by_group <- function(df) {
  df_clean <- data.frame()
  
  # Duyệt qua từng nhóm status (0 và 1)
  for (label in unique(df$status)) {
    group_data <- subset(df, status == label)
    
    # Tính IQR cho Height
    h_q1  <- quantile(group_data$height, 0.25, na.rm = TRUE)
    h_q3  <- quantile(group_data$height, 0.75, na.rm = TRUE)
    h_iqr <- h_q3 - h_q1
    
    # Tính IQR cho Width
    w_q1  <- quantile(group_data$width, 0.25, na.rm = TRUE)
    w_q3  <- quantile(group_data$width, 0.75, na.rm = TRUE)
    w_iqr <- w_q3 - w_q1
    
    # Xác định outlier
    h_outlier <- group_data$height < (h_q1 - 1.5 * h_iqr) | group_data$height > (h_q3 + 1.5 * h_iqr)
    w_outlier <- group_data$width  < (w_q1 - 1.5 * w_iqr) | group_data$width  > (w_q3 + 1.5 * w_iqr)
    
    # Giữ lại các dòng KHÔNG phải là outlier
    group_data_clean <- group_data[!(h_outlier | w_outlier), ]
    
    # Gộp lại vào dataframe kết quả
df_clean <- rbind(df_clean, group_data_clean)
  }
  return(df_clean)
}

# Áp dụng hàm lọc outlier
data <- remove_outliers_by_group(data)
# Kiểm tra lại số lượng dòng sau khi lọc
cat("Số dòng dữ liệu còn lại:", nrow(data), "\n")
head(data)

cons_data <- data[, c("height", "width", "ratio")]
# Tính toán thống kê
xtb  <- apply(cons_data, 2, mean)
sd   <- apply(cons_data, 2, sd)
med  <- apply(cons_data, 2, median)
Q1   <- apply(cons_data, 2, quantile, probs = 0.25)
Q3   <- apply(cons_data, 2, quantile, probs = 0.75)
GTNN <- apply(cons_data, 2, min)
GTLN <- apply(cons_data, 2, max)
# 3. Tạo bảng kết quả và chuyển vị
result_table <- t(data.frame(xtb, sd, med, Q1, Q3, GTNN, GTLN))
# In kết quả
print(result_table)

# Thống kê số lượng của biến phân loại theo status
table(data$status)
# Đếm số lượng từng loại trong 'status' (0 và 1)
status_count <- table(data$status)
#Tinh ty le phan tram tuong ung
status_percent <- prop.table(status_count) * 100
print(status_percent)
# Vẽ biểu đồ từ bảng thống kê 'status_count'
barplot(status_count, main = "Barplot of Ad", ylab = "Frequency", col = c("grey", "orange"), names.arg = c("Non-Ad", "Ad"), 
border = "black")
# VẼ BIỂU ĐỒ
# Vẽ Histogram cho Height
hist(data$height,
     main   = "Histogram of Height",
     xlab   = "Height",
     col    = "skyblue", 
     border = "black") 

# Vẽ Histogram cho 'width'
hist(data$width,
     main = "Histogram of Width",
     xlab = "Width",
     col = "salmon",
     border = "black")

# Vẽ Histogram cho 'ratio'
hist(data$ratio,
     main = "Histogram of Ratio",
     xlab = "Ratio",
     col = "lightgreen",
     border = "black")


# Vẽ Boxplot cho 'height' theo 'Status'
boxplot(height ~ status, data = data,
        main = "Boxplot of Height by Status",
        xlab = "Status", ylab = "Height",
        col = c("lightgray", "lightblue"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'width' theo 'Status'
boxplot(width ~ status, data = data,
        main = "Boxplot of Width by Status",
        xlab = "Status", ylab = "Width",
        col = c("lightgray", "lightgreen"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'ratio' theo 'Status'
boxplot(ratio ~ status, data = data,
        main = "Boxplot of Ratio by Status",
        xlab = "Status", ylab = "Ratio",
        col = c("lightgray", "lightpink"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Mô hình hồi quy logistic theo 'height'
model_height <- glm(status ~ height, data = data, family = "binomial")
# Thống kê số lượng của biến phân loại theo status
table(data$status)
# Đếm số lượng từng loại trong 'status' (0 và 1)
status_count <- table(data$status)

# Vẽ đồ thị phân tán giữa 'height' và 'status', sau đó vẽ đường cong logistic
plot(data$height, data$status,
     xlab = "height", ylab = "status",
     main = "Logistic Regression: Status vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Đồ thị phân tán

curve(predict(model_height, newdata = data.frame(height = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)  # Đường cong logistic

model_width <- glm(status ~ width, data = data, family = "binomial")

plot(data$width, data$status,
     xlab = "width", ylab = "status",
     main = "Logistic Regression: Status vs Width",
     pch = 16, col = rgb(0, 0.5, 0, 0.3))

curve(predict(model_width, newdata = data.frame(width = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Mô hình hồi quy logistic theo 'ratio'
model_ratio <- glm(status ~ ratio, data = data, family = "binomial")

plot(data$ratio, data$status,
     xlab = "ratio", ylab = "status",
     main = "Logistic Regression: Status vs Ratio",
     pch = 16, col = rgb(0.7, 0, 0, 0.3))

curve(predict(model_ratio, newdata = data.frame(ratio = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Tính ma trận tương quan giữa các biến liên tục 'height', 'width', 'ratio'
library(corrplot)
cor_matrix <- cor(data[, c("height", "width", "ratio")], use = "complete.obs")

# Vẽ biểu đồ ma trận tương quan
corrplot(cor_matrix, method = "circle", type = "upper",
         addCoef.col = "black", tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix", mar = c(0, 0, 1, 0))

## THỐNG KÊ SUY DIỄN:
# Thiết lập lại ngẫu nhiên với set.seed để tái lập kết quả
set.seed(123)

# Chia bộ dữ liệu thành train (70%) và test (30%)
sample_index <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[sample_index, ]
test_data <- data[-sample_index, ]

train_data$status <- factor(train_data$status)
test_data$status <- factor(test_data$status)

calculate_best_threshold <- function(model, data) {
  # Dự báo xác suất
  pred_prob <- as.vector(predict(model, newdata = data, type = "response"))
  
  # Tạo đối tượng ROC (Sửa Target -> status)
  roc_obj <- roc(data$status, pred_prob, quiet = TRUE)
  
  # Tìm ngưỡng Youden tốt nhất
  best_thres <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
  
  # Lấy giá trị đầu tiên (nếu có nhiều ngưỡng) và chuyển về số
  best_thres <- as.numeric(best_thres[1])
  return(best_thres)
}

# --- HUẤN LUYỆN CÁC MÔ HÌNH ---

# Model HW: Height + Width
model_hw <- glm(status ~ height + width, data = train_data, family = binomial)
summary(model_hw)

# Model 1: Width Only 
model_w <- glm(status ~ width, data = train_data, family = binomial)
best_threshold_w <- calculate_best_threshold(model_w, train_data)
summary(model_w)

# Model 2: Ratio Only
model_ratio <- glm(status ~ ratio, data = train_data, family = binomial)
best_threshold_ratio <- calculate_best_threshold(model_ratio, train_data)
summary(model_ratio)

# --- 3. HÀM ĐÁNH GIÁ MÔ HÌNH ---
evaluate_model <- function(model, best_threshold, data, label) {
  # Dự báo trên dữ liệu mới (Test)
  pred_prob <- predict(model, newdata = data, type = "response")

  # Phân loại dựa trên ngưỡng Youden
  pred_class <- ifelse(pred_prob > best_threshold, 1, 0)
  
  # Sửa Target -> status
  actual <- data$status
  
  # Tính độ chính xác
  accuracy <- mean(pred_class == actual)
  
  # In kết quả
  cat(paste0(">>> ", label, ":\n"))
  cat("Best Threshold (Youden): ", round(best_threshold, 4), "\n")
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Confusion Matrix:\n")
  # Actual dòng, Predicted cột
  print(table(Actual = actual, Predicted = pred_class))
  cat("\n------------------------------------------------\n")
}
# Đánh giá mô hình
evaluate_model(model_w, best_threshold_w, test_data, "Model 1 (Width)")
evaluate_model(model_ratio, best_threshold_ratio, test_data, "Model 2 (Ratio)")
# Vẽ biểu đồ Roc curve model 1
prob_w <- predict(model_w, newdata = test_data, type = "response")
roc_w <- roc(test_data$status, prob_w)
plot(roc_w, col = "blue", lwd = 2, main = "ROC Curve Model 1")
cat("AUC Model 1 (width): ", auc(roc_w), "\n")

#Vẽ biểu đồ Roc curve mode 2
prob_ratio <- predict(model_ratio, newdata = test_data, type = "response")
roc_ratio <- roc(test_data$status, prob_ratio)
plot(roc_ratio, col = "orange", lwd = 2, main = "ROC Curve Model 2")
cat("AUC Model 2 (ratio): ", auc(roc_ratio), "\n")

# In ra AIC của các mô hình
cat("AIC Model 1 ( width): ", AIC(model_w), "\n")
cat("AIC Model 2 (ratio): ", AIC(model_ratio), "\n")

# Vẽ biểu đồ so sánh
plot(roc_w, col = "blue", lwd = 2, main = "ROC Curve Comparison")
lines(roc_ratio, col = "orange", lwd = 2)
legend("bottomright", legend = c("Model 1: width", "Model 2: ratio"),
       col = c("blue", "orange"), lwd = 2)

## RANDOM FOREST
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
print(rf_cm)

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
     col = "blue")
lines(rf_roc, col = "red")
legend("bottomright",
       legend = c(paste0("Logistic (AUC = ", round(auc(roc_ratio), 3), ")"),
                  paste0("Random Forest (AUC = ", round(rf_auc, 3), ")")),
       col = c("blue", "red"),
       lwd = 2)

var_importance <- importance(rf_model)
varImpPlot(rf_model,
           main = "ĐỘ QUAN TRỌNG CỦA BIẾN - RANDOM FOREST",
           col = "darkblue")

y_true <- ifelse(test_data$status == "1", 1, 0)
mae <- mean(abs(y_true - rf_prob))
rmse <- sqrt(mean((y_true - rf_prob)^2))

print(paste("MAE:", round(mae, 4)))
print(paste("RMSE:", round(rmse, 4)))