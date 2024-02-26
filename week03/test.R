# PHÂN BỐ CHUẨN 1 CHIỀU
# 1. Một số hàm phân phối chuẩn
## Hàm mật độ: dnorm(x, mean, sd)
## x: bắt buộc
## mean: default = 0
## sd: default = 1
## --> Xây dựng hàm mật độ của biến khi biến tuân theo phân phối chuẩn tắc
## Example: drorm(5, 2, 4) --> X ~ N(2, 4^2)

## Example
x <- (-50:50)/12
# Vẽ đồ thị hàm mật độ pb chuẩn tắc
plot(
  x,
  dnorm(x),
  type = "l"
)

plot(
  x,
  dnorm(x),
  type = "l", # "o", "b", 
  xlim = c(-4.5, 4.5),
  ylim = c(0, 0.45),
  xlab = "Giá trị của x",
  ylab = "Hàm mật độ",
  main = "Hàm mật độ của x",
  col = "red"
)

## Hàm phân phối tích lũy
## pnorm(q, mean, sd) = P(X < q), biết X ~ N(mean, sd^2)
## Example:
pnorm(0.9) # = P(X < 0.9) biêt X ~ N(0, 1)

pnorm(12, 15, 2) # = P(X < 12) biết X ~ N(15, 2^2)

x <- (-10:10)
# Vẽ đồ thị hàm phân phối của x, đặt tên đồ thị, tên trục Ox, Oy,; độ dài trục Ox, Oy phù hợp
# X ~ N(0, 1)
plot(
  x,
  pnorm(x),
  main = "Hàm phân phối tích lũy của X",
  xlab = "Giá trị của X",
  ylab = "Hàm phân phối",
  xlim = c(-10, 10),
  ylim = c(0, 1),
  col = "blue",
  type = "l"
)

# Hàm phân vị
# qnorm(p, mean, sd) = a, tức P(X < a) = p, X ~ mean, sd^2)
qnorm(0.9) # Tìm a biết P(X < a) = 0.9, X ~ N(0, 1)
qnorm(0.95)
# P(X < a) = phi(a)

# Hàm sinh ngẫu nhiên/ Hàm mô phỏng: rnorm(n. mean, sd): sinh ngẫu nhiên n giá trị của biến X ~ N(mean, sd^2)
# Example:
rnorm(5) # Sinh ngẫu nhiên 5 giá trị của X biết X ~ N(0, 1)
rnorm(5, 170, sqrt(10)) # Sinh ngẫu nhiên 5 giá trị của X biết X ~ N(170, 10)


################################################################################################ 

library(MASS)
names(Cars93)
data <- Cars93[, c("Min.Price", "Price", "Max.Price", "MPG.highway", "Horsepower", "RPM")]
data

head(Cars93) # 6 mẫu quan sát đầu tiên của Cars93
head(data)

# Dùng tổ chức đồ: hist
hist(data$Price)
par(mfrow = c(3, 2))
sapply(colnames(data), function(x) {
  hist(data[[x]], main = x)
})

# The QQ Plot
# x_i là các giá trị thực được sắp xếp: x_1 <= x_2 <= ... <= x_n
# x_i_h là các giá trị/ phân vị mong muốn: x_1_h = phi^(-1) (1/(n+1)), tức P(X < x_1_h) = 1/(n+1)
# x_2_h = phi^(-1) (2/(n+1)); ...
# x_n_h = phi^(-1) (n/(n+1))

# qqline: đường nằm giữa Q1 và Q3 của dữ liệu được quan sát

qqnorm(data$Price)
qqline(data$Price)

sapply(
  colnames(data),
  function(x) {
    shapiro.test(data[[x]])
  }
)

# install.packages("fBasics")

# H0: X tuân theo pp chuẩn
# H1: X không tuân theo pp chuẩn
# p-value > 0.05 thì chấp nhận H0
library(fBasics)
ksnormTest(
  (data$Min.Price - mean(data$Min.Price)) / sd(data$Min.Price)
)
ks.test(
  (data$Min.Price - mean(data$Min.Price)) / sd(data$Min.Price),
  "pnorm"
)

# Một số đặc trưng
sample = rnorm(5)
mean(sample)
var(sample)
sd(sample)
t.test(
  sample,
  mu = 2, # default = 0
  alt = "less",
  conf.level = 0.99
) # Kiểm tra xem mẫu đang test có giá trị trung bình nhỏ hơn 2 hay không

## H0: EX = 2 vs H1: EX != 2


sample1 = rnorm(10, 3, 1.2)
sample2 = rnorm(15, 2.8, 1.4)
t.test(sample1, sample2)

# H0: Es1 == Es2 vs H1: Es1 != Es2
# 95 percent confidence interval: Es1 - Es2:
# (-0.4224899  1.6641452)

dt = cbind(sample1, sample2)
dt

colMeans(dt)

cov(dt) # Ma trận hiệp phương sai
var(dt)
var(dt[, "sample1"])

cor(dt) # Ma trận tương quan mẫu
