hemangioma = read.table("D:\\MAT3452_ThongKeNhieuChieu\\Buoi06\\dataset\\hemangioma.txt", header = T)
View(hemangioma)
head(hemangioma, 3)

# Thực hiện phân tích với 3 nhân tố
factanal(hemangioma, factor = 3)
# Uniquenesses: sai số epsilon
# Loading: tỷ lệ chi phối của từng Factor 
# Dựa vào ma trận tải trọng:
## (1) Có thể biểu diễn các biến X_i theo các nhân tố F1, F2, F3
## (2) có thể chỉ ra các biến X_i bị chi phối bởi những nhân tố nào? (mỗi nhân tố F1, F2, F3 chi phối những biến nào)

# Xét bài toán kiểm định: H0: 3 nhân tố vs H1: 3 nhân tố là không đủ (dựa trên p_value)
## Do p_value = 0.33 > 0.05 nên chấp nhận H0
## KL: sử dụng 3 nhân tố là đủ
