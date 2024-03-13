library(ICSNP)
data("LASERI")

cor(LASERI[, 25:28])
pairs(
  LASERI[, 25:28],
  lwd = 1,
  pch = 16,
  cex = .5,
  col = "red",
  gap = 0,
  xaxt = "n",
  yaxt = "n",
  panel = panel.smooth,
  col.smooth = "blue"
)

# Ma trận biều đồ tán xạ biểu diễn các cặp điểm của 2 biến tương ứng, là quan sát trực quan về tính tương quan giữa 2 biến, chỉ nên sử dụng cho số lượng biến nhỏ hơn 10

# Khi số lượng biến  lớn hơn 10, thì người ta dùng biểu đồ nhiệt để quan sát tính tương quan giữa các biến

# Biểu đồ nhiệt
install.packages("reshape2")

library(reshape2)
cor_df <- round(cor(LASERI[, 10:28]), 2)
cor_df # 7 x 7 

melted_cor <- melt(cor_df)
melted_cor # 49 x 3
#install.packages("ggplot2")
library(ggplot2)

ggplot(
  data = melted_cor,
  aes(x = Var1, y = Var2, fill = value)
) + # data + khung vẽ
geom_tile() + # khung có màu xanh nhiều mức
geom_text(
  aes(Var2, Var1, label = value), 
  size = 1 # size của giá trị trong ô
) + # giá trị tương quan trong từng ô
scale_fill_gradient2(
  low = "blue", 
  high = "red", 
  limit= c(-1, 1), 
  name = "Correlation"
) + # đổi màu ô và thay đổi độ dài trục chú thích
theme(
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(),
  panel.background = element_blank()
) 

# Ô có màu đỏ càng đậm thì hệ số tương quan giữa hai biến tương ứng càng lớn
# Ô có màu xanh càng đậm thì hệ số tương quan giữa 2 biến càng tiến gần -1
# Giá trị tuyệt đối của hệ số tương quan giữa 2 biến càng tiến gần về 1 thì 2 biến có tương quan càng mạnh


## PHÂN PHỐI CHUẨN NHIỀU CHIỀU

## Lấy dữ liệu từ Table 7.4 trang 197 Book vào R
candy <- read.csv("G:\\study\\r\\hus-mat3452\\hw05\\theory\\data.csv")
candy

## Nếu đọc file txt, dùng read.table

candy = candy[, -1]
candy
candy = na.omit(candy)
candy
mah <- mahalanobis(candy, colMeans(candy), var(candy))

qqplot(qchisq((1:nrow(candy) - 1/2) / nrow(candy), df = 6), (mah))
qqplot(qchisq((1:nrow(candy) - 1/2) / nrow(candy), df = 6), sort(mah))
abline(a = 0, b = 1, col = "red")
shapiro.test(qnorm(pchisq(mah, 6)))

## TÍnh chuẩn nhiều chiều được chấp nhận
# KIểm tra tính chuẩn 1-chiều của từng biến trong 6 biến đang xét 
# Để kiểm tra một vector nn X có tuân theo phân phối chuẩn k-chiều không thì làm theo 2 bước

## B1: Kiểm tra tính chuẩn 1-chiều của từng biến trong X
## Nếu tồn tại ít nhất 1 biến không chuẩn thì kết luận luôn là X không tuân theo phân phối chuẩn k-chiều
## Nếu tất cả các biến của X đều tuân theo pp chuẩn 1-chiều thì thực hiện bước 2
## B2: Kiểm tra tính chuẩn nhiều chiều shapiro.test(qnorm(pchisq(mah,6)))
# Bước 2 chính là kiểm tra tính chuẩn của phân vị pp chuẩn tắc tương ứng với xs của pp khi-bình phương xét trên dữ liệu là mah với 6 bậc tự do

