## TIEN XU LY DU LIEU

# Đọc dữ liệu từ file CSV
raw_data <- read.csv("~/Downloads/add.csv")
# View(raw_data)

# # Loại bỏ cột ID đầu tiên
# data <- raw_data[, -1]
# # Xóa các cột từ thứ 4 đến thứ 1558, giữ lại cột cuối cùng chứa ad. , nonad.
# data <- data[, -c(4:1558)]

# Đổi tên cột: 'height', 'width', 'ratio', 'status'
colnames(data) <- c("height", "width", "ratio", "status")
head(data)
dim(data)

# Thay thế tất cả dấu '?' trong dữ liệu thành NA
data[data == "?" | data == "   ?" | data == "     ?"] <- NA

# ép kiểu về number
data$height <- as.numeric(data$height)
data$width <- as.numeric(data$width)
data$ratio <- as.numeric(data$ratio)

# đổi "ad." =  1, "nonad." = 0
data$status <- ifelse(data$status == "ad.", 1, 0)
# ép kiểu factor cho biến phân loại status
data$status <- factor(data$status)

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

# Thay thế giá trị NA trong 'height' và 'width' bằng giá trị trung vị (median) của từng cột
data$height[is.na(data$height)] <- median(data$height, na.rm = TRUE)
data$width[is.na(data$width)] <- median(data$width, na.rm = TRUE)
head(data)

# tính lại NA ratio bằng công thức height / width
missing_ratio <- is.na(data$ratio) & !is.na(data$height) & !is.na(data$width) # tạo 1 vector logic true or false cho từng dòng
data$ratio[missing_ratio] <- data$height[missing_ratio] / data$width[missing_ratio]

# Thống kê lại số lượng và tỷ lệ NA sau khi thay thế
na_counts <- colSums(is.na(data))
na_percentage <- colMeans(is.na(data)) * 100

# In kết quả thống kê lại NA
na_summary <- data.frame(
  NA_Count = na_counts,
  NA_Percentage = round(na_percentage, 2)
)
print(na_summary)

# xử lý outlier bằng phương pháp IQR
# Tính Q1, Q3, IQR cho height
h_q1 <- quantile(data$height, 0.25, na.rm = TRUE)
h_q3 <- quantile(data$height, 0.75, na.rm = TRUE)
h_iqr <- h_q3 - h_q1


# Tính Q1, Q3, IQR cho width
w_q1 <- quantile(data$width, 0.25, na.rm = TRUE)
w_q3 <- quantile(data$width, 0.75, na.rm = TRUE)
w_iqr <- w_q3 - w_q1

# Tạo ngưỡng dưới và trên cho height
h_lower <- h_q1 - 1.5 * h_iqr
h_upper <- h_q3 + 1.5 * h_iqr

# Tạo ngưỡng dưới và trên cho width
w_lower <- w_q1 - 1.5 * w_iqr
w_upper <- w_q3 + 1.5 * w_iqr

# Xác định outlier cho cả height và width
is_outlier <- (data$height < h_lower | data$height > h_upper) |
              (data$width  < w_lower  | data$width  > w_upper)

# Lọc bỏ các outlier khỏi dữ liệu
# data[row_index, column_index], vector 2 chiều
data <- data[!is_outlier, ]
head(data)

## THỐNG KÊ MÔ TẢ

# Sử dụng thư viện 'psych' để thống kê mô tả các biến liên tục 'height', 'width', 'ratio'
library(psych)
# describe nằm trong thư viện psych
describe(data[, c("height", "width", "ratio")], fast = TRUE)

# Vẽ Histogram cho 'height'
hist(data$height,
     main = "Histogram of Height",
     xlab = "Height",
     col = "skyblue",  # Màu sắc
     border = "white")  # Viền trắng

# Vẽ Histogram cho 'width'
hist(data$width,
     main = "Histogram of Width",
     xlab = "Width",
     col = "salmon",
     border = "white")

# Vẽ Histogram cho 'ratio'
hist(data$ratio,
     main = "Histogram of Ratio",
     xlab = "Ratio",
     col = "lightgreen",
     border = "white")

# Thống kê số lượng của biến phân loại theo status
table(data$status)
# Đếm số lượng từng loại trong 'status' (0 và 1)
status_count <- table(data$status)

# Vẽ Barplot cho 'status'
barplot(status_count,
        main = "Barplot of Status",
        names.arg = c("Non-Ad", "Ad"),
        col = c("gray", "orange"),
        ylab = "Frequency")

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

# Vẽ đồ thị phân tán giữa 'height' và 'status', sau đó vẽ đường cong logistic
plot(data$height, data$status,
     xlab = "Height", ylab = "Status",
     main = "Logistic Regression: Status vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Đồ thị phân tán

curve(predict(model_height, newdata = data.frame(height = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)  # Đường cong logistic

# Mô hình hồi quy logistic theo 'width'
model_width <- glm(status ~ width, data = data, family = "binomial")

plot(data$width, data$status,
     xlab = "Width", ylab = "Status",
     main = "Logistic Regression: Status vs Width",
     pch = 16, col = rgb(0, 0.5, 0, 0.3))

curve(predict(model_width, newdata = data.frame(width = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Mô hình hồi quy logistic theo 'ratio'
model_ratio <- glm(status ~ ratio, data = data, family = "binomial")

plot(data$ratio, data$status,
     xlab = "Ratio", ylab = "Status",
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

# Xây dựng mô hình logistic với 'height' và 'width'
model_hw <- glm(status ~ height + width, data = train_data, family = binomial)
summary(model_hw)

# Mô hình logistic với 'width' duy nhất
model_w <- glm(status ~ width, data = train_data, family = binomial)
summary(model_w)

# Mô hình logistic với 'ratio'
model_ratio <- glm(status ~ ratio, data = train_data, family = binomial)
summary(model_ratio)

# Hàm đánh giá mô hình
evaluate_model <- function(model, data, label) {
  pred_prob <- predict(model, newdata = data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  actual <- data$status
  accuracy <- mean(pred_class == actual)
  cat(paste0(label, ":\n"))
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Confusion Matrix:\n")
  print(table(Predicted = pred_class, Actual = actual))
  cat("\n")
}

# Đánh giá mô hình
evaluate_model(model_w, test_data, "Model width only")
evaluate_model(model_ratio, test_data, "Model ratio only")

# In ra AIC của các mô hình
cat("AIC Model 1 ( width): ", AIC(model_w), "\n")
cat("AIC Model 2 (ratio): ", AIC(model_ratio), "\n")

library(pROC)
# Dự đoán xác suất trên tập test
prob_w <- predict(model_w, newdata = test_data, type = "response")
prob_ratio <- predict(model_ratio, newdata = test_data, type = "response")

# Vẽ ROC
roc_hw <- roc(test_data$status, prob_w)
roc_ratio <- roc(test_data$status, prob_ratio)

# Vẽ biểu đồ so sánh
plot(roc_hw, col = "blue", lwd = 2, main = "ROC Curve Comparison")
lines(roc_ratio, col = "red", lwd = 2)
legend("bottomright", legend = c("Model 1: width", "Model 2: ratio"),
       col = c("blue", "red"), lwd = 2)

# In AUC
cat("AUC Model 1 (width): ", auc(roc_hw), "\n")
cat("AUC Model 2 (ratio): ", auc(roc_ratio), "\n")

#------------------------------------------
# RANDOM FOREST PHÂN LOẠI (phiên bản nâng cao)
#------------------------------------------
library(randomForest)
library(caret)
set.seed(8)

# Chọn mtry dựa trên số biến giải thích
mtry_val <- max(1, floor(sqrt(ncol(train_data) - 1)))

rf_model <- randomForest(
  status ~ .,
  data = train_data,
  ntree = 500,
  mtry = mtry_val,
  importance = TRUE,
  proximity = FALSE
)

cat("Random Forest summary:\n")
print(rf_model)

# Dự đoán lớp và xác suất trên tập test
rf_pred_class <- predict(rf_model, newdata = test_data)
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "1"]

# Ma trận nhầm lẫn + độ đo tổng hợp
rf_cm <- confusionMatrix(rf_pred_class, test_data$status, positive = "1")
cat("Confusion matrix (positive = 1):\n")
print(rf_cm)

rf_accuracy <- rf_cm$overall["Accuracy"]
rf_sensitivity <- rf_cm$byClass["Sensitivity"]
rf_specificity <- rf_cm$byClass["Specificity"]

cat("Accuracy: ", rf_accuracy, "\n")
cat("Sensitivity (Recall for class 1): ", rf_sensitivity, "\n")
cat("Specificity: ", rf_specificity, "\n")

# ROC + AUC cho Random Forest và so sánh với logistic (ratio)
rf_roc <- roc(test_data$status, rf_prob, levels = c("0", "1"), direction = "<")
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

# Độ quan trọng của biến
var_importance <- importance(rf_model)
varImpPlot(rf_model,
           main = "Độ quan trọng biến - Random Forest",
           col = "darkblue")

cat("Độ quan trọng của biến:\n")
print(var_importance)

# Sai số dự báo xác suất (đơn vị xác suất 0-1)
y_true <- as.numeric(as.character(test_data$status))
rf_mae <- mean(abs(y_true - rf_prob))
rf_rmse <- sqrt(mean((y_true - rf_prob)^2))
cat("MAE: ", rf_mae, "\n")
cat("RMSE: ", rf_rmse, "\n")
