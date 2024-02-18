# Bài tập vector, ma trận
# Bài 1
# Nhập vector X
X <- c(1:9)
# Nhập vector Y
Y <- c(1.5, 2.3, 3.2, 4.6, 5.4, 6.6, 7.6, 8.6, 9.1)
X
Y
# Bài 1.a
norm(X, type="2")
norm(Y, type="2")
# Bài 1.b
X[2]
Y[1]
Y[4]
Y[7]
# Bài 1.c
X[4] <- 215
X
Y[1] <- 99
Y[5] <- 199
Y
# Bài 1.d
# Làm tròn lên
ceiling(Y)
# Làm tròn xuống
floor(Y)
# Bài 1.e
X + Y
5*Y
# Bài 2
# Nhập A
A <- matrix(c(1, 2, 1, 3, 3, 6, 5, 4, 2, 4, 3, 2, 8, 7, 5, 1), 4)
A
# Nhập B
B <- matrix(c(1, 2, 3, 4, 4, 1, 5, 3, 1, 7, 8, 9, 4, 6, 3, 7), 4)
B
# Bài 2.a
rownames(A) <- c("X", "Y", "Z", "T")
A
# Bài 2.b
rowSums(B)
colSums(B)
# Bài 2.c
rowMeans(A)
colMeans(A)
# Bài 2.d
A[2,3]
# Bài 2.e
D <- matrix(c(A[1,], A[2,]), 2, byrow = TRUE)
D
# Bài 2.f
E <- matrix(c(B[,-c(3)]), 4)
E
# Bài 2.g
t(A)
solve(A)
# Bài 2.h
eigen(B)$values
eigen(B)$vectors
# Bài 2.i
det(B)
# Bài 2.j
A %*% B
# Bài 2.k
Z <- A %*% solve(B)
Z