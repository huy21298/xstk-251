## TIEN XU LY DU LIEU

# Đọc dữ liệu từ file CSV
raw_data <- read.csv("~/Downloads/add.csv")
View(raw_data)

# Loại bỏ cột ID đầu tiên
data <- raw_data[, -1]
# Xóa các cột từ thứ 4 đến thứ 1558, giữ lại cột cuối cùng chứa ad. , nonad.
data <- data[, -c(4:1558)]

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

## THỐNG KÊ MÔ TẢ

# Sử dụng thư viện 'psych' để thống kê mô tả các biến liên tục 'height', 'width', 'ratio'
library(psych)
# describe nằm trong thư viện psych
describe(add_data[, c("height", "width", "ratio")], fast = TRUE)


# Vẽ Histogram cho 'height'
hist(add_data$height,
     main = "Histogram of Height",
     xlab = "Height",
     col = "skyblue",  # Màu sắc
     border = "white")  # Viền trắng

# Vẽ Histogram cho 'width'
hist(add_data$width,
     main = "Histogram of Width",
     xlab = "Width",
     col = "salmon",
     border = "white")

# Vẽ Histogram cho 'ratio'
hist(add_data$ratio,
     main = "Histogram of Ratio",
     xlab = "Ratio",
     col = "lightgreen",
     border = "white")

# Thống kê số lượng của biến phân loại 'target'
table(add_data$target)
# Đếm số lượng từng loại trong 'target' (0 và 1)
target_counts <- table(add_data$target)

# Vẽ Barplot cho 'target'
barplot(target_counts,
        main = "Barplot of Target",
        names.arg = c("Non-Ad", "Ad"),
        col = c("gray", "orange"),
        ylab = "Frequency")

# Vẽ Boxplot cho 'height' theo 'target'
boxplot(height ~ target, data = add_data,
        main = "Boxplot of Height by Target",
        xlab = "Target", ylab = "Height",
        col = c("lightgray", "lightblue"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'width' theo 'target'
boxplot(width ~ target, data = add_data,
        main = "Boxplot of Width by Target",
        xlab = "Target", ylab = "Width",
        col = c("lightgray", "lightgreen"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Vẽ Boxplot cho 'ratio' theo 'target'
boxplot(ratio ~ target, data = add_data,
        main = "Boxplot of Ratio by Target",
        xlab = "Target", ylab = "Ratio",
        col = c("lightgray", "lightpink"),
        names = c("Non-Ad (0)", "Ad (1)"))

# Mô hình hồi quy logistic theo 'height'
model_height <- glm(target ~ height, data = add_data, family = "binomial")

# Vẽ đồ thị phân tán giữa 'height' và 'target', sau đó vẽ đường cong logistic
plot(add_data$height, add_data$target,
     xlab = "Height", ylab = "Target",
     main = "Logistic Regression: Target vs Height",
     pch = 16, col = rgb(0, 0, 1, 0.3))  # Đồ thị phân tán

curve(predict(model_height, newdata = data.frame(height = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)  # Đường cong logistic

# Mô hình hồi quy logistic theo 'width'
model_width <- glm(target ~ width, data = add_data, family = "binomial")

plot(add_data$width, add_data$target,
     xlab = "Width", ylab = "Target",
     main = "Logistic Regression: Target vs Width",
     pch = 16, col = rgb(0, 0.5, 0, 0.3))

curve(predict(model_width, newdata = data.frame(width = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)

# Mô hình hồi quy logistic theo 'ratio'
model_ratio <- glm(target ~ ratio, data = add_data, family = "binomial")

plot(add_data$ratio, add_data$target,
     xlab = "Ratio", ylab = "Target",
     main = "Logistic Regression: Target vs Ratio",
     pch = 16, col = rgb(0.7, 0, 0, 0.3))

curve(predict(model_ratio, newdata = data.frame(ratio = x), type = "response"),
      add = TRUE, col = "red", lwd = 2)


# Tính ma trận tương quan giữa các biến liên tục 'height', 'width', 'ratio'
library(corrplot)
cor_matrix <- cor(add_data[, c("height", "width", "ratio")], use = "complete.obs")

# Vẽ biểu đồ ma trận tương quan
corrplot(cor_matrix, method = "circle", type = "upper",
         addCoef.col = "black", tl.col = "black", number.cex = 0.8,
         title = "Correlation Matrix", mar = c(0,0,1,0))


## THỐNG KÊ SUY DIỄN:
# Thiết lập lại ngẫu nhiên với set.seed để tái lập kết quả
set.seed(123)

# Chia bộ dữ liệu thành train (70%) và test (30%)
sample_index <- sample(1:nrow(add_data), size = 0.7 * nrow(add_data))
train_data <- add_data[sample_index, ]
test_data <- add_data[-sample_index, ]

# Xây dựng mô hình logistic với 'height' và 'width'
model_hw <- glm(target ~ height + width, data = train_data, family = binomial)
summary(model_hw)

# Mô hình logistic với 'width' duy nhất
model_hw2 <- glm(target ~ width, data = train_data, family = binomial)
summary(model_hw2)

# Mô hình logistic với 'ratio'
model_ratio <- glm(target ~ ratio, data = train_data, family = binomial)
summary(model_ratio)

# Hàm đánh giá mô hình
evaluate_model <- function(model, data, label) {
  pred_prob <- predict(model, newdata = data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  actual <- data$target
  accuracy <- mean(pred_class == actual)
  
  cat(paste0(label, ":\n"))
  cat("Accuracy: ", round(accuracy, 4), "\n")
  cat("Confusion Matrix:\n")
  print(table(Predicted = pred_class, Actual = actual))
  cat("\n")
}

# Đánh giá mô hình
evaluate_model(model_hw2, test_data, "Model 1 (width)")
evaluate_model(model_ratio, test_data, "Model 2 (ratio)")

# In ra AIC của các mô hình
cat("AIC Model 1 ( width): ", AIC(model_hw2), "\n")
cat("AIC Model 2 (ratio): ", AIC(model_ratio), "\n")

library(pROC)
# Dự đoán xác suất trên tập test
prob_hw <- predict(model_hw2, newdata = test_data, type = "response")
prob_ratio <- predict(model_ratio, newdata = test_data, type = "response")

# Vẽ ROC
roc_hw <- roc(test_data$target, prob_hw)
roc_ratio <- roc(test_data$target, prob_ratio)

# Vẽ biểu đồ so sánh
plot(roc_hw, col = "blue", lwd = 2, main = "ROC Curve Comparison")
lines(roc_ratio, col = "red", lwd = 2)
legend("bottomright", legend = c("Model 1: width", "Model 2: ratio"),
       col = c("blue", "red"), lwd = 2)

# In AUC
cat("AUC Model 1 (width): ", auc(roc_hw), "\n")
cat("AUC Model 2 (ratio): ", auc(roc_ratio), "\n")
