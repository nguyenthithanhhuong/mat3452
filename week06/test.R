# Đặt tên các hàng chính là "Manager"
# Xóa cột "Manager" ra khỏi bộ dữ liệu
# Đối với các bộ dữ liệu mà các biến có cùng thang đo, có thể phân tích thành phần chính trên ma trận hiệp phương sai
# Đối với bộ dữ liệu mà các biến không cùng thang đo, nên phân tích thành phần chính trên ma trận tương quan
# Đưa dữ liệu đuôi txt vào R: read.table()

invest <- read.table(
  "D:/Code/r/mat3452/week06/data/investment.txt",
  header = T
)
head(invest)

rownames(invest) = invest[, 1] # Đổi tên các hàng
invest <- invest[, -1] # Xóa cột đầu tiên
head(invest, 3)

pc <- princomp(invest)
summary(pc) # Tóm tắt kết quả phân tích thành phần chính
pc$loadings

# Standard deviation: ĐỘ lệch tiêu chuẩn của các tpc trên ma trận hiệp phương sai
pc$sdev
(pc$sdev)^2 # là phương sai của TPC D(Y_i) với Y_i là TPC thứ i: Y_i = e_i' x
eigen(cov(invest))$values # Giá trị riêng của ma trận hiệp phương sai của invest

# Proportion of Variance: Tỷ lệ biến sai tổng cộng: D(Y_i) / sum(D(Y_i))
# Cumulative Proportion: dùng để tìm ra số thành phần chính cần thiết khi đề bài yêu cầu hiểu 7% thông tin về bộ dữ liệu ban đầu
pc$loadings # Các hệ số tải l_ij

# Dựa vào ma trận hệ số tải trọng, biểu diễn các thành phần chính theo các biến ban đầu
# PC1 = 0.706 * S.US + 0.114 * B.US - 0.109 * B.Non.US - 0.686 * A.native
# Mỗi cột hệ số tải chính là vector riêng tương ứng của ma trận hiệp phương sai
eigen(cov(invest))$vectors[, 1]

# Nếu các biến trong bộ dữ liệu không cùng thang đo, ta sẽ phân tích thành phần chính trên ma trận tương quan mẫu
pcacor <- princomp(invest, cor = TRUE)
summary(pcacor)

# Standard deviation^2 = D(Y_i) = lambda_i là giá trị riêng thứ i của ma trận tương quan
# Để có được 85% thông tin về bộ dữ liệu ban đầu, ta cần 5 thành phần chính.
# TÌm sai số: sai số = (lambda_6 + lambda_7 + lambda_8) / sum(lambda) = 1 - 0.86364897
100* (1- 0.86364897)

pc = princomp(invest)
screeplot(
  pc, 
  col = "red", 
  pch = 16,
  type = "lines",
  cex = 2,
  lwd = 2,
  main = ""
)


pc <- princomp(invest)
biplot(
  pc, 
  col = c(2, 3), 
  cex = c(.4, 1),
  xlim = c( -.45, .45),
  xlab = "First principal component",
  ylab = "Second principal component",
  main = "Biplot for investment allocations"
)

hemangioma <- read.table(
  "G:\\study\\r\\hus-mat3452\\hw06\\theory\\hemangioma.txt",
  header = T
)
head(hemangioma)
factanal(hemangioma, factors = 3) # Thực hiện phân tích với 3 nhân tố 
# X_i = l_i1 * F1 + l_i2 * F2 + l_i3 * F3 + psi
# spi được gọi là nhiễu 
# Dựa vào ma trận tải trọng:
## (1) CÓ thể biểu diễn các biến x_i theo các nhân tố nào? Mỗi một nhân tố ở F1, F2, F3 chi phối những biễn nào?
## (2) Có thể chỉ ra các biến X_i bị chi phối bởi những nhân tố nào? Mỗi nhân tố F1, F2, F3 chi phối những biến nào?
# Xét một bài toán kiểm định: H0: 3 nhân tố là đủ vs H1: 3 nhân tố là không đủ
# Do p-value = 0.33 > 0.05 nên chấp nhận H0
# Kết luận: Sử dụng 3 nhân tố là đủ
