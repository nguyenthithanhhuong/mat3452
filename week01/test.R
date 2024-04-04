# Đưa dữ liệu tù file csv vào R
# Cách 1
data <- read.csv("D:\\Study\\[MAT3452]_PhanTichThongKeNhieuChieu\\week1\\Australian Institute of Sport.csv")
# Cách 2
data <- read.csv("D:/Study/[MAT3452]_PhanTichThongKeNhieuChieu/week1/Australian Institute of Sport.csv")
# Đưa ra 6 quan sát đầu tiên của bộ dữ liệu
head(data)
# Để dòng đầu tiên của bộ dữ liệu là tên biến
# Sử dụng header = TRUE
data <- read.csv("D:/Study/[MAT3452]_PhanTichThongKeNhieuChieu/week1/Australian Institute of Sport.csv", header = TRUE)

# Vector
x <- 1:3
x
x <- c(1:3)
x
x <- seq(1, 3, 1) # từ 1 -> 3, hơn kém nhau 1
x
seq(2, 11, length.out = 4) # length.out ~ số phần tử giữa 2 - 11
x[1] # lấy phần tử t1
x[-1] # lấy các phần tử, trừ phần tử t1
x[c(1:3)]
x[-c(1:2)]
x <- (1:3)
y <- (4:6)
x + y
x
3*x

# Ma trận
# Tạo ma trận
# default: Dữ liệu được sắp xếp theo cột từ trái sang phải
A <- matrix(c(1:8), 2) # 2 ~ số hàng
A
# Sử dụng byrow = TRUE
# Để dữ liệu sắp xếp theo hàng từ trên xuống dưới
A <- matrix(c(2,3,4,5,6,8,9,7,1), 3, byrow = TRUE)
A
# ma trận chuyển vị
t(A)
# ma trận nghịch đảo
solve(A)
# định thức 
det(A)
B <- matrix(c(2,1,3,4,2,6,8,2,4), 3)
B
# Nhân 2 ma trận
A %*% B
# Đặt tên cho các các cột của ma trận A
colnames(A) <- c("C1", "C2", "C3")
A
# Đặt tên cho các hàng của ma trận A
rownames(A) <- c("R1", "R2", "R3")
A
# Giá trị riêng, vector riêng
eigen(A)
# Giá trị riêng
eigen(A)$values
# Vector riêng
eigen(A)$vectors