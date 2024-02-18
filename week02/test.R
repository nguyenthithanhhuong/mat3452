# HỒI QUY TUYẾN TÍNH ĐƠN: y = ax + b
## Đưa ra các ước lượng của hệ số a và b
## Bài toán kiểm định hệ số: H0: a = 0 / H0: b = 0
## Phần dư: e = y - y_h: Kiểm định xem e có tuân theo pp chuẩn với giá trị trung bình bằng 0 không?

# install.packages("MASS") # cài đặt thư viện

library(MASS) # gọi thư viện
names(Boston) # đưa ra tên các biến

# cách 1
lm.fit = lm(medv ~ lstat, data = Boston) # HQTT: medv = a + b * lstat
lm.fit
# or cách 2
lm(Boston$medv ~ Boston$lstat)
# or cách 3
attach(Boston)
lm(medv ~ lstat)
# Output: PT đường thẳng hồi quy tuyến tính: medv = 34.55 - 0.95 * lstat

summary(lm.fit) # tóm tắt kết quả

# Biểu diễn tuyến tính của biến medv theo lstat trong bộ dữ liệu Boston
# Phần dư: e = y - y_h: Min, 1Q, Median, 3Q, Max; y là medv thực trong bộ dữ liệu, y_h là medv được dự đoán theo mô hình
lm.fit$residuals
length(lm.fit$residuals)

# Bài toán kiểm định: Phần dư có tuân theo pp chuẩn với giá trị trung bình bằng 0 không?
names(lm.fit)
?shapiro.test
shapiro.test(lm.fit$residuals) #Kiểm tra pp chuẩn của phần dư
# Do p_value < 2.2 * 10^(-16) nên bác bỏ H0. Vậy phần dư sẽ không tuân theo phân phối chuẩn
t.test(lm.fit$residuals, mu = 0) # kiểm định về giá trị trung bình nếu chuẩn: H0: E_e = 0
wilcox.test(lm.fit$residuals) # kiểm định nếu không chuẩn, do p_value = 0.0002031 < 0.05 nên bác bỏ H0

# Hệ số ước lượng, sai số chuẩn, test thống kê T (t value), p-value (Pr (> |t|))
# Xây dựng mô hình: medv = a + b * lstat
## a_h = 34.55, b_h = -0.95
## Bài toán kiểm định hệ số a: H0: a = 0 vs H1: a != 0
## Tương tự, có bài toán kiểm định hệ số b: H0: b = 0, H1: b != 0
### Do p-value < 2.10^-16 < 0.05 nên bác bỏ H0. Với mức ý nghĩa 5%, có cơ sở để nói a != 0
### Tương tự, b != 0
coeff_test <- coef(summary(lm.fit))
p_values <- coeff_test[, "Pr(>|t|)"]
significant_coeffs <- p_values < 0.05
coeff_test
p_values
significant_coeffs
# Hệ số xác định chính là R-squared: 0.5441

### H0: Phần dư tuân theo phân phối chuẩn, H1: Phần dư không tuân theo phân phối chuẩn
#### Do p-value < 2.10^(-16) nên bác bỏ H0, cơ sở để nói phần dư không tuân theo phân phối chuẩn
t.test(lm.fit$residuals) # nếu e chuẩn
wilcox.test(lm.fit$residuals) # nếu e không chuẩn

### H0: EX = 0 vs H1: EX != 0
lm.fit = lm(medv ~ lstat)
lm.fit
confint(lm.fit) # Khoảng tin cậy 95% cho các hệ số trong mô hình hồi quy
confint(lm.fit, level = 0.99)
?confint

predict(lm.fit, data.frame(lstat =  ((c(5, 10, 15)))),
        interval = "confidence") # Đưa ra giá trị dự đoán của medv khi biết lstat
# 34.5538409 - 0.9500494 * 5
dev.off()
plot(lstat, medv)
abline(lm.fit)
lm.fit$coefficients
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = ".")
plot(1:20, 1:20, pch = 1:20)

par(mfrow = c(2, 2)) # Chia khung vẽ đồ thị thành 2 hàng và 2 cột
plot(lm.fit)
dev.off()
plot(predict(lm.fit), residuals(lm.fit))

