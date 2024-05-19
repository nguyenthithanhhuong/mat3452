#Hồi quy tuyến tính bội
?aov
#Kiểm định sự phục thuộc của từng biến Pr(>F) = pvalue (<0.05 thì bác bỏ H0)
univ.mpg <- aov(mpg ~ cyl + disp + am + carb, data = mtcars)
summary(univ.mpg)
# pValue xét H0: mpg và X_i có độc lập, H1: mpg và X_i phụ thuộc

#Độ tương quan
attach(mtcars)
cor(mpg, cyl)
cor(mpg, disp)
cor(mpg, am)
cor(mpg, carb)
summary(lm(aov(mpg ~ cyl + disp + am + carb, data = mtcars)))

#Chaỵ 4 mô hình lm theo dữ liệu trang 238
univ.mpg <- aov(mpg ~ cyl + disp + am + carb , data = mtcars)
summary(univ.mpg)
univ.hp <- aov(hp ~ cyl + drat + am + gear + carb, data = mtcars)
summary(univ.hp)
univ.wt <- aov(wt ~ cyl + disp + drat + am + carb, data = mtcars)
summary(univ.wt)
univ.qsec <- aov(qsec ~ cyl + disp + drat + vs + am + gear, data = mtcars)
summary(univ.qsec)

#Pr:(>F): Hai biến Y và Xi độc lập/ ko có sự khác biệt về mpg giữa các quan sát có cyl khác nhau
lmfit =(lm(aov(mpg ~ cyl + disp + am + carb, data = mtcars)))
#1. Các biến X_i và mpg có mối tương quan ntn> Mạnh hay yếu
#Xét hệ số tương quan giữa các cặp biến (mpg, X_i)
cor(mpg,cyl)
#Hệ số tương quan r(mpg, cyl) = -0.85, nên có thể thấy mpg và cyl có tương quan chặt
#Xét tương tự với các cặp biến (mpg, X_i) khác.
cor(mpg, disp)
cor(mpg, am)
cor(mpg, carb)
## Mức độ mạnh yếu dựa vào độ lớn

#2. Tìm các ước lượng và KTC 90% cho các hệ số mô hình.
univ.mpg <- lm(mpg ~ cyl + disp + am + carb, data = mtcars)
summary(univ.mpg)
## Nếu F càng cao thì tác động càng mạnh

#3. Các biến có ý nghĩa thống kê tác động ntn đến mpg?
summary(lmfit)$coefficients
#xác định các biến có ý nghĩa thống kê chính là xét bài toán kiểm định cá hệ số trong mô hình.
#H0: b_i = 0 vs H1: b_i != 0 (i = 0,1,...,4)
#Nếu bác bỏ H0 thì biến X_i có ý nghĩa thóng kê
#Sau đó, xem xét việc tăng/giảm của các biến đó anh hưởng ntn đến biến mpg?
#do pvalue tương ứng với b0, b3,b4 deeif nhỏ hơn 0.05 nên bác bỏ H0 hay các hệ số đó khác 0. Tức là, biếm am và carb có ý nghãi thống kê.
#Tương tự,  xem xét cá biến còn lại.
#Khi làm am tăng thì mpg tăng và ngược lại.

#4. Kiểm định phần dư.
shapiro.test(lmfit$residuals)#H0: phần dư tuân theo pp chuẩn vs H1: phần dư ko tuân theo phân phối chuẩn
#Do pvalue..


#5. Hệ số xác định bằng bao nhiêu? Nó thể hiện điều gì?
summary(lmfit) #R2 hiệu chỉnh = 8.93%, tức là mô hình sẽ giải thích được 80.93% biến thiên của mpg.
#6. cho 1 bộ giá trị bất kì của x. đưa ra dự báo về giá trị mg theo mô hình và khoảng dự đoán 90% tương ứng.
dt = data.frame(cyl = 4, disp = 400, am =1 , carb = 5)
predict(lmfit, newdata = data.frame(cyl = 4, disp = 400, am =1 , carb = 5), type = "response")
#Đưa ra dự đoám về mpg của 1 xe có cyl = 4, disp = 400, am =1 , carb = 5
predict(lmfit, newdata = data.frame(cyl = 4, disp = 400, am =1 , carb = 5), interval = "prediction")
#Đưa ra khaonrg dư đoán về giá trị mpg về giá trị mpg của 1 xe có cyl = 4, disp = 400, am =1 , carb = 5
predict(lmfit, newdata = data.frame(cyl = 4, disp = 400, am =1 , carb = 5), interval = "prediction", level = 0.9)

#chạy 4 đoạn lệnh trong sách trang 238
univ.mpg <- aov(mpg ~ cyl + disp + am + carb , data = mtcars)
summary(univ.mpg)
univ.hp <- aov( hp ~ cyl + drat + am + gear + carb, data = mtcars)
summary(univ.hp)
univ.wt <- aov( wt ~ cyl + disp + drat + am + carb, data = mtcars)
summary(univ.wt)
univ.qsec <- aov( qsec ~ cyl + disp + drat + vs + am + gear, data = mtcars)
summary(univ.qsec)

car.res <- cbind( univ.mpg$residuals, univ.hp$residuals,
                  +
                    univ.wt$residuals, univ.qsec$residuals)
colnames(car.res)<- c("mpg_res", "hp_res", "wt_res", "qsec_res")
car.res <- data.frame(car.res)
print(car.res, digits=2)

library(mvnormtest)
mshapiro.test(t(car.res))
