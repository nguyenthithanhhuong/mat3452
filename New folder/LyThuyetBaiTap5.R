# VẼ BIỂU ĐỒ TÁN XẠ, BIỂU ĐỒ NHIỆT
# Kiểm định tính pp chuẩn nhiều chiều
names(LASERI)
pairs(LASERI[, 25:28], lwd = 1, pch = 16, cex = .5, col = "red", gap = 0, xaxt = "n", yaxt = "n")
# Ma trận biểu đồ tán xạ biểu diễn các cặp điểm của 2 biến tương ứng, là quan sát trực quan về tính tương quan giữa 2 biến, chỉ nên sử dụng cho số lượng biến < 10

# Nếu số lượng biến lớn thì dùng biểu đồ nhiệt


# BIỂU ĐỒ NHIỆT VỚI SỐ LƯỢNG BIẾN LỚN
install.packages("reshape2")
library(reshape2)
# Vẽ biểu đồ nhiệt cho ma trận hệ số tương quan giữa các biến 
cor_df = round(cor(LASERI[, 22:28]), 2)
cor_df

# melt the dataframe --> dữ nguyên từng biến và ghép lại với nhau (hệ số tương quan giữa các cặp biến)
melted_cor <- melt(cor_df)
head(melted_cor)
dim(melted_cor)

library(ggplot2)
ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + # khung vẽ có màu xanh
geom_text(aes(Var2, Var1, label = value), size = 3) + # giá trị tương quan mẫu trong từng ô
scale_fill_gradient2(low="blue", high="red", limit=c(-1,1), name="Correlation") + # đổi màu ô và thay đổi độ dài trục chú thích
theme(axis.title.x = element_blank(), 
      axis.title.y = element_blank(),
      panel.background = element_blank())
# ô có màu đỏ càng đậm thì hệ số tương quan giữa 2 biến tương ứng càng lớn 
# ô có màu xanh càng đậm thì hệ số tương quan giữa 2 biến tương ứng càng về -1
# giá trị tuyệt đối của HSTQ giữa 2 biến càng tiến về 1 thì 2 biến có tương quan càng mạnh


# Ví dụ: không thêm hệ số tương quan vào đồ thị
df.cor = round(cor(LASERI[, 10:28]), 2)
melt_df_cor = melt(df.cor)

ggplot(data = melt_df_cor, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + # khung vẽ có màu xanh
  scale_fill_gradient2(low="blue", high="red", limit=c(-1,1), name="Correlation") + # đổi màu ô và thay đổi độ dài trục chú thích
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        panel.background = element_blank())
# Kiểm tra tính tương quan chặt với biến nào?


# PHÂN PHỐI CHUẨN NHIỀU CHIỀU
# X = (X1, X2, ..., Xk)^T có phân bố chuẩn k-chiều khi
# +) X1, X2, ..., Xk có phân bố chuẩn 1 chiều
# +) X có phân bố chuẩn nhiều chiều => sd khoảng cách (x-muy)^T SigmaTong^{-1} (x-muy) -> khoảng cách Mahobaton
candy = read.table("D:\\MAT3452_ThongKeNhieuChieu\\Buoi05\\dataset\\data.txt", header = T)
candy = read.csv("D:\\MAT3452_ThongKeNhieuChieu\\Buoi05\\dataset\\data.csv")
head(candy)
candy = candy[,-1]
mah <- mahalanobis(candy, colMeans(candy), var(candy))
dim(candy)
candy
mah
length(mah)
colnames(candy)
# Giá trị thứ i của mah đo khoảng cách giữa quan sát thứ i với gttb Y_h = colMeans(candy)
qqplot(qchisq((1:nrow(candy) - 1/2) / nrow(candy), df = 6), (mah))
qqplot(qchisq((1:nrow(candy) - 1/2) / nrow(candy), df = 6), sort(mah))
abline(a = 0, b = 1, col = "red")
shapiro.test(qnorm(pchisq(mah, 6))) # kiểm tra tính chuẩn nhiều chiều (pchisq là chi bình phương)
# chấp nhận do p-value > 0.05 --> tính chuẩn nhiều chiều được chấp nhận

# KIỂM TRA TÍNH CHUẨN 1 CHIỀU CỦA TỪNG BIẾN TRONG 6 BIẾN ĐANG XÉT
# Để kiểm tra 1 vector nn X có tuân theo pp chuẩn k chiều không thì làm theo 2 bước:
## B1: Kiểm tra tính chuẩn 1 chiều của từng biến. Nếu có 1 biến không tuân theo => ko tuân theo
## B2: Kiểm tra tính chuẩn nhiều chiều shapiro.test(qnorm(pchisq(mah, 6)))
### --> B2 chính là kiểm tra tính chuẩn của phân vị pp chuẩn tắc tương ứng với CS của pp khi bình phương
# xét trên dữ liệu là mah với 6 bậc tự do
