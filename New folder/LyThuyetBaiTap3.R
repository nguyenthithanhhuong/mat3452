# Phân bố chuẩn
# 1. Hàm mật độ dnorm(x, mean, sd)
## Tham số x là bắt buộc, nếu ko đưa thì mặc định mean = 10, sd = 1 --> chuẩn tắc
dnorm(5)    # hàm mật độ f(x) = f(5) khi X ~ N(0, 1)
dnorm(5, 2, 1.2)    # X ~ N(2, 1.2^2)
## Vẽ hàm mật độ
x <- (-50:50)/2
plot(x, dnorm(x), type = "l")   # Phân bố chuẩn tắc
plot(x, dnorm(x), type = "l", xlim = c(-4.5, 4.5), ylim = c(0, 0.45), xlab = "Giá trị của x", ylab = "Hàm mật độ", main = "Hàm mật độ của x")


# 2. Hàm phân phối tích lũy pnorm(q, mean, sd) = P(X < q) biết X ~ N(mean, sd^2)
pnorm(0.9) # P(X < 0.9) biết X ~ N(0, 1)
pnorm(12, 15, 2) # P(X < 12) khi X ~ N(15, 2^2)
x = (-10:10) # X ~ N(0, 1)
## Vẽ hàm phân phối của X, đặt tên đồ thị, trên trục Ox, Oy với độ dài Ox, Oy phù hợp
plot(x, pnorm(x), type = "l", xlim = c(-10, 10), ylim = c(0, 1), xlab = "Giá trị của x", ylab = "Hàm mật độ", main = "Hàm phân phối P(X < a) của x", col = "red")


# 3. Hàm phân vị: qnorm(p, mean, sd) = a tức P(X < a) = p, X ~ N(mean, sd^2)
qnorm(0.9)  # Tìm a biết P(X < a) = 0.9, X ~ N(0, 1)
qnorm(0.95)  # Tìm a biết P(X < a) = 0.95, X ~ N(0, 1)

# 4. Hàm sinh ngẫu nhiên rnorm(n, mean, sd) --> sinh ngẫu nhiên n giá trị của X ~ N(mean, sd^2)
rnorm(5)  # Sinh ngẫu nhiên 5 giá trị của X
rnorm(5, 170, sqrt(10))   # Sinh ngẫu nhiên 5 giá trị của X biết X ~ N(170, 10)


### Quan sát dữ liệu
library(MASS)
names(Cars93)
head(Cars93)  # Tách dữ liệu để lấy tên cột
data = Cars93[, c("Min.Price", "Price", "Max.Price", "MPG.highway", "Horsepower", "RPM")]
hist(data$Price, freq = TRUE)  # tổ chức đồ (mặc định là False)
hist(data$Price, freq = FALSE)  # không theo tần suất
par(mfrow = c(3,2))   # Khung vẽ đồ thi chia làm 3 hàng 2 cột 
sapply(colnames(data), function(x) {
  hist(data[[x]], main = x)  # tổ chức đồ
})

### Kiểm tra đồ thị bằng QQ-plot (trực quan - nên dùng shapiro.test)
#### (x_i, h_i) - qqnorm
#### + x_i là giá trị thực được sắp xếp x_1 <= x_2 <= ... <= x_n)
#### + h_i là các giá trị phân vị mong muốn: 
# x_1_h = phi^(-1) (1/(n+1)) tức P(X < x_1_h) = 1 /(n+1)
# x_2_h = phi^(-1) (2/(n+1)), ..., x_n_h = phi^(-1) (n/(n+1))
qqnorm(data$Price)
qqline(data$Price)

# Cách 1: Nhìn đồ thị
par(mfrow = c(3,2))   # Khung vẽ đồ thi chia làm 3 hàng 2 cột 
sapply(colnames(data), function(x) {
  qqnorm(data[[x]], main = x)
  qqline(data[[x]])
})

# Cách 2: shapiro.test
sapply(colnames(data), function(x) {
  shapiro.test(data[[x]])$p.value
}) # --> gần đến 0 là phân bố ko chuẩn (<= 0.05 là không chuẩn, > 0.05 là chuẩn)


# BÀI TOÁN KIỂM ĐỊNH
## H0: X tuân theo phân phối chuẩn vs H1: X không tuân theo phân phối chuẩn
install.packages("fBasics")
library("fBasics")

ksnormTest((data$Min.Price - mean(data$Min.Price))/ sd(data$Min.Price))
ks.test(data$Min.Price, "pnorm")

## Một số đặc trưng
sample = rnorm(5)
mean(sample) # GTTB - kỳ vọng
var(sample)  # phương sai
sd(sample)   # độ lệch chuẩn
t.test(sample, mu = 2, alt = "less", conf.level = 0.99) # Kiểm định xen mẫu có GTTB <= 2 không?
### alternative hypothesis là H1 (alt theo H1, conf.level là khoảng tin cậy)


t.test(sample, conf.level = 0.99)$conf.int
# H0: EX = 0 và H1: EX != 0
# Do pvalue > 0.05 --> chấp nhận H0 và ngược lại 


# So sánh với 2 mẫu
sample1 = rnorm(10, 3, 1.2)
sample2 = rnorm(15, 2.8, 1.4)
t.test(sample1, sample2)
# H0: Es1 = Es2 và H1: Es1 != Es2
# 95% confidence interval: Es1 - Es2 (-0.3564534  1.9262418)

# 
sample1 = rnorm(10, 3, 1.2)
sample2 = rnorm(10, 2.8, 1.4)
dt = cbind(sample1, sample2)
dt
colMeans(dt)  # GTTB theo cột
cov(dt)   # ma trận hiệp phương sai

var(dt)
var(dt[, "sample1"])

cor(dt) # ma trận tương quan mẫu
cor(sample1, sample2)

data("mtcars")
cov(mtcars)
a = eigen(cor(mtcars))
a
a$vectors
a$values
