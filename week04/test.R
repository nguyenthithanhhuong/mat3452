# PHÂN BỐ CHUẨN NHIỀU CHIỀU
## 1. Sinh mẫu ngẫu nhiên nhiều chiều
## 2. So sánh giá trị trung bình của mẫu ngẫu nhiên nhiều chiều
## 3. Kiểm tra tính phân phối chuẩn của biến ngẫu nhiên nhiều chiều

#install.packages('mvtnorm')
library(mvtnorm)
x = rmvnorm(5, mean = c(0, 0), sigma = matrix(c(1,.8,.8,1), 2, 2))
x
set.seed(1)
# Tìm giá trị trung bình mẫu
colMeans(x)

# Ma trận hiệp phương sai mẫu
cov(x)

# Ma trận tương quan mẫu
cor(x)


# NHẬN DẠNG PHÂN BỐ CHUẨN NHIỀU CHIỀU
## X = (X1, X2, ..., Xk)^T
## Y = (Y1, Y2, ..., Yk)^T
## => Xi và Yi là tên biến
## H0: EXi = EYi với i từ 1 --> k và H1: Ngược lại
#install.packages('ICSNP')
library(ICSNP)
data("LASERI")  # Phản ứng của tim mạch khi nghiêng đầu
View(LASERI)

help(package = ICSNP)
#?HotellingsT2

# Test thống kê trong phân phối chuẩn 1 chiều
## 1. Kiểm định xem 1 giá trị trung bình của một biến có bằng 1 giá trị cho trước không
### 1.1. Kiểm định xem giá trị trung bình của biến HRT1 trong LASERI có bằng 70 cho trước không
#?t.test()
t.test(LASERI$HRT1, mu = 70)
## H0: mu_HRT1 = 70 và H1: mu_HRT1 != 70
## KTC 95% cho GTTB của HRT1 là [63.19398, 65.71185]
## Ước lượng điểm X_ngang = 64.45291
## Do p-value = 8.418*10^(-16) < 0.05 nên bác bỏ H0
## Với mức ý nghĩa 5%, có cơ sở để nói GTTB của HRT1 != 70

attach(LASERI) # Tách dữ liệu
mean(HRT1)

### 1.2. Kiểm định xem HRT1 có lớn hơn 64 không (với MYN 1%)
t.test(LASERI$HRT1, mu = 64, alternative = "greater", conf.level = 0.99)

## 1.3. Kiểm định xem HRT1 có nhỏ hơn 65 không (với MYN 10%)
t.test(LASERI$HRT1, mu = 65, alternative = "less", conf.level = 0.9)
### Nếu pvalue < mức ý nghĩa thì bác bỏ H0


## 2. Kiểm định xem GTTB của 2 mẫu có sự khác biệt không
## Kiểm định xem GTTB của HRT1 và HRT2 có thực sự khác nhau không
t.test(HRT1, HRT2)
### H0: mu_HRT1 = mu_HRT2 và H1: khác nhau
### do p-value < 2.2*10^(-16) < 0.05 => bác bỏ H0
### Với myn 5% thì có cơ sở để nói có sự khác biệt giữa GTTB của HRT1 và HRT2
### Khoảng tin cậy của EX - EY = [-14.99750, -11.14599]
### ULD chi HRT1 là 64.45291 và HRT2 là 77.52466

### Kiểm định GTTB của HRT1 có thực sự nhỏ hơn HRT2 không với myn 2%
t.test(HRT1, HRT2, alternative = "less", conf.level = 0.98)
### do p-value < 2.2*10^(-16) < 0.02 => bác bỏ H0
### H0: mu_HRT1 = mu_HRT2 và H1: HRT1 < HRT2


## Nhiều chiều
### Tại thời điểm T1 thì X = (HRT1, COT1, SVRIT1, PWVT1)
### Tại thời điểm T4 thì X = (HRT4, COT4, SVRIT4, PWVT4)
### Bài toán H0: EX = EY
# E(HRT1) = E(HRT4)
# E(COT1) = E(COT4)
# E(SVRIT1) = E(SVRIT4)
# E(PWVT1) = E(PWVT4)

names(LASERI[, 25:28])  # Lấy các biến từ 25 --> 28
colMeans(LASERI[, 25:28])
cov(LASERI[, 25:28])
cor(LASERI[, 25:28])
HotellingsT2(LASERI[, 25:28])
#?HotellingsT2

# Dùng hàm HotellingsT2 để kiểm định GTTB của (HRT1T4, COT1T4, SVRIT1T4, PWVT1T4) có thực sự khác 0 = (0,0,0,0)
HotellingsT2(data.frame(LASERI$HRT1T4, LASERI$COT1T4, LASERI$SVRIT1T4, LASERI$PWVT1T4), mu = c(0, 0, 0, 0))

# Dùng hàm HotellingsT2 để kiểm định GTTB của (HRT1, COT1, SVRIT1, PWVT1) có thực sự khác ((HRT4, COT4, SVRIT4, PWVT4))
L1 <- c(LASERI$HRT1, LASERI$COT1, LASERI$SVRIT1, LASERI$PWVT1)
L2 <- c(LASERI$HRT4, LASERI$COT4, LASERI$SVRIT4, LASERI$PWVT4)
for (i in c(1:4)) {
  t.test(L1[i], L2[i])
}
