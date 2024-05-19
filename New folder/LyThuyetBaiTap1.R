# HỒI QUY TUYẾN TÍNH ĐƠN y = a + bx
## Đưa ra các ước lượng của các hệ số a và b
## Bài toán kiểm định hệ số: H0: a = 0 / H0: b = 0
## Phần dư: e = y - y_h: Kiểm định xem e có tuân theo pp chuẩn với giá trị trung bình bằng 0 không?
?Boston
install.packages("MASS")  # Cài đặt thư viện chứa dataset Boston
library(MASS) # Gọi thư viện
names(Boston) # Đưa ra tên các biến

# Hồi quy tuyến tính
## Cách 1:
lm.fit = lm(medv ~ lstat, data = Boston)   # HQTT: medv = a + b * lstat

## Cách 2:
lm(Boston$medv ~ Boston$lstat)

# Cách 3: Tách dữ liệu
attach(Boston)
lm(medv ~ lstat)

# Cách dùng hàm như nào
?lm


# Phương trình đường thẳng hồi quy tuyến tính: medv = 34.55 - 0.95 * lstat
summary(lm.fit)   # Tóm tắt kết quả

## Biểu diễn tuyến tính của medc theo lstat trong bọ dữ liệu Boston
## Phần dư: e = y - y_h: Min, 1Q, Median, 3Q, Max
lm.fit$residuals # Phần dư e = y - y_h, y là medv thực trong bộ dữ liệu, y_h là medv được dự đoán theo mô hình
## Bài toán kiểm định: Phần dư có tuân theo pp chuẩn với GTTB bằng 0 không?
?shapiro.test
shapiro.test(lm.fit$residuals)  # Kiểm tra pp chuẩn
## Bài toán: H0: lm.fit$residuals tuân theo phân phối chuẩn và H1: lm.fit$residuals không tuân theo phân phối chuẩn
### Do p_value < 2.2 * 10^(-16) nếu bác bỏ H0 --> phần dư không theo pp chuẩn
t.test(lm.fit$residuals, mu = 0)        # Kiểm định về GTTB nếu chuẩn: H0: E_e = 0
wilcox.test(lm.fit$residuals)     # Nếu không chuẩn

# Hệ số ước lượng, sai số chuẩn, test thông kê (t value), p-value (Pr(>|t|))
# Xây dựng mô hình: medv = a + b * lstat
## a_ h = 34.55, b_h = -0.95
## Bài toán kiểm định hệ số a: H0: a = 0 vs H1: a != 0
### Do p-value < 2. 10^(-16) < 0.05 nên bác bỏ H0. Với mức ý nghĩa 5%, có cơ sở để nói a != 0

### H0: EX = 0 và EX != 0
lm.fit = lm(medv ~ lstat)
confint(lm.fit)   # KTC 95% cho các hệ số trong mô hình hồi quy
confint(lm.fit, level = 0.99)

predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "confidence") # Đưa ra giá trị dữ đoán medv khi biết lstat
# 34.5538409 = 0.9500494 * 5
dev.off()
plot(lstat, medv) # Biểu đò tán xạ
abline(lm.fit)
lm.fit$coefficients
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red") # Biểu đò tán xạ
plot(lstat, medv, pch = 20) # Biểu đò tán xạ
plot(lstat, medv, pch = ".") # Biểu đò tán xạ (pch là kiểu điểm)
plot(1:20, 1:20, pch = 1:20) # Biểu đò tán xạ

# Hiển thị nhiều đồ thị
par(mfrow = c(2,2)) # Chia khung vẽ đồ thị thành 2 hàng, 2 cột
plot(lm.fit)
dev.off()
# plot(predict(lm.fit), residuals(lm.fit))
# plot(predict(lm.fit), rstudent(lm.fit))
