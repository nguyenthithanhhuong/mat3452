# Chương 9: 
univ <- lm(formula = mpg ~ cyl + wt + am + carb, data = mtcars)
fit <- univ$fitted.values  # Giá trị mpg được tính theo mô hình
names(univ)
fit   # Giá trị y_mu = beta0_mu + beta1_mu*y1 + ... + betan_mu*yn
resid <- univ$residuals # Phần dư = mpg_thực - fit = y - y_h


# Kiểm định phần dư có tương quan nhau không
## H0: phần dư không có tương quan
## H1: phần dư có tương quan
### So sánh với p-value
library(car)  # Cách 1
durbinWatsonTest(univ)  

library(lmtest) # Cách 2
dwtest(univ)


### Các hệ số
s = summary(univ)
s
# 1. Giải thích kết quả thu được
# 2. Đưa ra phương trình hồi quy
# 3. Các hệ số trong mô hình có thực sự khác 0 không?
# 4. Phần dư có tuân theo phân phối chuẩn với GTTB = 0 không?

# 1. Giải thích kết quả thu được
## Biểu diễn hồi quy tuyến tính của biến mpg theo các biến cyl, wt, am, carb
## Phần dư: min = -4.5451, 1Q = -1.2184, Media = -0.3739, 3Q = 1.4699, Max = 5.3528 
## Hệ số: ước lượng, sai số chuẩn, test thống kê T (t value) và pvalue (Pr(>|t|))
## Hệ số xác định R2 = 0.8502, hệ số xác định R hiệu chỉnh là 0.828
## --> 82.8% sự biến thiên của mpg có thể được giải thích bởi các biến cyl, wt, am, carb
## Test thống kê: F = 38.3 với bậc tự do (4,27)
## p-value = 9.255e-11 dùng để kiểm định bài toán H0: Mô hình không có ý nghĩa thống kê, H1: Mô hình có ý nghĩa thống kê

# 2. Phương trình hồi quy: mpg=36.8503−1.1968×cyl−2.4785×wt+1.7801×am−0.7480×carb

# 3. Các hệ số trong mô hình có thực sự khác 0 không?
## Giả sử mô hình mpg = a0 + a1*cyl + a2*wt + a3*am + a4*carb
## Có 5 bài toán kiểm định hệ số: H0: a_i = 0, H1: a_i != 0 (i=1,2,3,4)
## Đối với hệ số a3 của biến am và a4 của biến carb, ta thấy p-value > 0.05 nên chấp nhận H0 --> hai biến am và carb không có ý nghĩa trong mô hình
## Đối với hệ số a0, a1 của biến cyl và a2 của biến wt, ta thấy p-value < 0.05 nên bác bỏ H0 --> hai biến cyl và wt có ý nghĩa trong mô hình
## So sánh dựa vào Pr(>|t|)

# 4. Phần dư có tuân theo pp chuẩn với gttb = 0
shapiro.test(s$residuals) # Kiểm tra tính chuẩn của phần dư
## H0: Phần dư tuân theo phân phối chuẩn, H1: phần dư không tuân theo phân phối chuẩn
t.test(s$residuals) # Kiểm định GTTB của phần dư có bằng 0 không?

## H0: GTTB của phần dư bằng 0, H1: GTTB của phần dư khác không
lm(mpg ~ 0 + cyl + wt, data = mtcars)
summary(lm(mpg ~ cyl + wt - 1, data = mtcars))


# Một mô hình theo bao nhiêu biến là tốt nhất sẽ tùy theo tiêu chuẩn đang xét
# => giới thiệu về việc chọn mô hình theo tiêu chuẩn AIC
?step
names(mtcars)
# Xây dựng mô hình đơn giản và mô hình phức tạp nhất để tìm ra mô hình biểu diễn mpg theo các biến phù hợp nhất theo tiêu chuẩn AIC
attach(mtcars)
only = lm(mpg ~ 1, data = mtcars) # Mô hình đơn giản
all = lm(mpg ~ ., data = mtcars) # Mô hình phức tạp nhất
## mpg = a0 + ai*xi (i=1,2,...,10) với xi là các biến còn lại trong data


# Phân tích theo hướng forward/backward/both
## forward: xét từ mô hình only đến mô hình all
fw = step(only, formula(all), direction = "forward", trace = 0)
fw$anova
anova(fw)
### Bắt đầu xét từ mô hình only: mpg = a0
### Thêm 1 trong 10 biến vào mô hình only được mô hình có dạng y = a0 + a1*Xi (i=1,...,10)
### Trong 10 mô hình này, chọn ra mô hình có AIC nhỏ nhất. Ở đây, thu được mô hình mpg = a0 + a1*wt với AIC = 115.94
### Bây giờ, thêm 1 trong 9 biến còn lại vào mô hình mpg = a0 + a1*wt để được mô hình có dạng mpg = a0 + a1*wt + a2*Xj (j=2,...,10)
### Trong 9 mô hình này, chọn ra mô hình có AIC nhỏ nhất. Ở đây, thu được mô hình mpg = a0 + a1*wt + a2*cyl với AIC = 73.22
### Tiếp tục thực hiện đến khi chỉ số AIC thu được không giảm nữa thì dừng lại

## backward: từ mô hình all đến only
backward = step(all, formula(all), direction = "backward", trace = 0)
backward$anova

## both 
both = step(all, formula(all), direction = "both", trace = 0)
both
### VD:
three = lm(mpg ~ cyl + disp + hp)
both = step(three, formula(all), direction = "both", trace = 0)
both$anova
summary(both)


## Dự đoán mô hình
summary(fw)
# Cách 1:
predict(fw, newdata = data.frame(wt = 2, cyl = 3, hp = 5)) # Đưa ra dự đoán về mpg theo mô hình đã xây dựng cho dữ liệu đầu vào là wt = 2, cyl = 3, hp = 5

# Cách 2:
fw$coefficients
fw$coefficients %*% (c(1,2,3,5))

