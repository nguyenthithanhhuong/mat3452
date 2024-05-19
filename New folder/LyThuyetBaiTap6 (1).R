invest = read.table("D:\\MAT3452_ThongKeNhieuChieu\\Buoi06\\dataset\\investment.txt", header = T)
View(invest)

# Đặt tên các hàng chính là "Manager"
rownames(invest) = invest[,1]

# Xóa cột "Manager" ra khỏi bộ dữ liệu
invest <- invest[, -1]

head(invest, 3) # Lấy ra 3 hàng đầu tiên

# Phân tích thành phần chính
pc = princomp(invest)

summary(pc) # Tóm tắt kết quả phân tích thành phần chính
## Standard deviation: độ lệch tiêu chuẩn của các thành phần chính
## Proportion of Variance: tỷ lệ biến sai tổng cộng: D(Y_i) / sum(D(Y_j))
## Cumulative Proportion: tìm ra số TPC cần thiết khi đề bài yêu cầu hiểu ?% thông tin về bộ dữ liệu ban đầu


# Các thành phần chính khi phân tích trên ma trận hiệp phương sai
pc = princomp(covmat = cov(invest))
summary(pc)
pc$sdev   # Độ lệch tiêu chuẩn của các TPC khi phân tích trên ma trận hiệp phương sai
(pc$sdev)^2 # Phương sai của các TPC: D(Y_i), với Y_i là TPC thứ i: Y_i = e_i'X
eigen(cov(invest))$values # Giá trị riêng của ma trận hiệp phương sai của invest
# Vì D(Y_i) = lambda_i

# Dựa vào ma trận hệ số tải trọng, biểu diễn các thành phần chính theo các biến ban đầu
pc$loadings   # Các hệ số tải l_ij
## PC1 = 0.706 * S.US + 0.114 * B.US - 0.109 * B.Non.US - 0.686 * A.native
## Mỗi cột hệ số tải chính là vecto riêng tương ứng của ma trận hiệp phương sai
eigen(cov(invest))$vectors[, 1]  # Vecto riêng ứng vs PC1



# Nếu các biến trong bộ dữ liệu khác nhau về thang đo, ta sẽ phân tích thành phần chính trên ma trận tương quan mẫu
pcacor = princomp(invest, cor = TRUE)
summary(pcacor)
pcacor$loadings
## Standard devitation^2 = D(Y_i) = lambda_i là giá trị riêng thứ i của ma trận tương quan

# Để có được 85% thông tin về bộ dữ liệu ban đầu, ta cần 5 thành phần chính. Tìm sai số:
## Sai số = (lambda_6 + lambda_7 + lambda_8)/tổng(lambda) = 1 - 0.86364897 = 1 - lambda_5 (vì lambda_5 gần với sai số nhất)


# Vẽ biểu đồ thể hiện phương sai của từng TPC (lambda)
screeplot(pc, col = "red", pch = 16,
          type = "lines", cex = 2, lwd = 2, main = "")

# Vẽ biểu đồ phân bổ dữ liệu
pc <- princomp(invest)
biplot(pc, col = c(2, 3), cex = c(.5, 1.25), # (độ co của , độ co của thành phần chính)
       xlim = c( -.45, .45),
       xlab = "First principal component",
       ylab = "Second principal component",
       main = "Biplot for investment allocations")

pc$loadings  

# --> Đối với các bộ dữ liệu mà các biến có cùng thang đo, có thể phân tích thành phần chính trên ma trận hiệp phương sai
# --> Đối với bộ dữ liệu mà các biến không cùng thang đo, nên phân tích thành phần chính trên ma trận tương quan
