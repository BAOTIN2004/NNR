# set work direction:  thiet lap thu muc lam viec hien thoi 
setwd("E:/NNR/Dataset/Dataset")
BFCases<- read.table(file="BirdFluCases.txt", header = TRUE)
# liet ke cac ten truong du lieu trong dataset
names(BFcase)
#xem xet cac kieu du lieu tuong ung voi cac truong
str(BFcases)
# tinh tong cac ca mac benh cac nam cua cac nuoc tu cot 2 den cot 16
Cases<- rowSums(BFCases[, 2:16])
# liet ke cac nam (cot 1 )
names(Cases)<- BFCases[, 1] 
Cases
#install.packages("plotrix")
# ve bieu do 
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
# ve bieu do tron voi phan tram tuong ung cac nam, main: dat ten tieu de cho bieu do
pie(Cases, main = "Ordinary pie chart") #A
# tao bieu do tron ,col chon mau xam tu 0.4 den 1 voi 6 cap do bang ham seq() 
pie(Cases, col = gray(seq(0.4, 1.0, length = 6)),clockwise = TRUE, main = "Grey colours") #B
# tao bieu do tron ,mau sac cau vong, clockwise : huong ve theo chieu kim dong ho
pie(Cases, col = rainbow(6), clockwise = TRUE,main = "Rainbow colours") #C
#khai bao thu vien 
library(plotrix) # goi tao ra nhieu bieu do phuc tap
pie3D(Cases, labels = names(Cases), explode = 0.1,main = "3D pie chart", labelcex = 0.6) #D
#The Bar Chart and Strip Chart : bieu do cot va bieu do dải  
# khoi phuc lai cac thong so ban dau truoc khi ve bieu dodo
pie3D(Cases, labels = names(Cases), explode = 0.1,main = "3D pie chart", labelcex = 0.6)
#par(op)

setwd("E:/NNR/Dataset/Dataset")
# doc du lieu tu filfile
BFDeaths <- read.table(file = "Birdfludeaths.txt",header = TRUE)
# tinh tong cac cot tu cot 2 den cot 1616
Deaths <- rowSums(BFDeaths[, 2:16])
names(Deaths) <- BFDeaths[, 1]
Deaths
par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
# hàm barplot tao bieu do cot voi tieu de main:
barplot(Cases , main = "Bird flu cases") 
# tao ra ma tran bang cach ket hop cot case va deaths de tao bieu do cot phan bo so ca tu vong theo nam
Counts <- cbind(Cases, Deaths)
barplot(Counts) #B
# tạo biểu đồ cột ngang bằng cách sử dụng t(Counts) để chuyển vị ma trận Counts, sau đó sử dụng barplot để tạo biểu đồ cột với các màu xám độ sáng khác nhau.
barplot(t(Counts), col = gray(c(0.5, 1))) 
# t(Counts) de chuyen vi ma tran counts, sau do su dung barplot de tao bieu do cot , voi cac cot canh bang nhau 
barplot(t(Counts), beside = TRUE) 
Counts 
t(Counts)
# doc du lieu tu file
Benthic <- read.table(file = "RIKZ2.txt", header = TRUE)
# tính toán giá trị trung bình của độ giàu sinh vật biển trên mỗi bãi biển
Bent.M <- tapply(Benthic$Richness,INDEX = Benthic$Beach, FUN = mean)
# tinh toan do lech chuan cua do giau sinh vat bien tren moi bai bien
Bent.sd <- tapply(Benthic$Richness,INDEX = Benthic$Beach, FUN = sd)
# tao ra ma tran chua do giau sv bien va do lech chuan
MSD <- cbind(Bent.M, Bent.sd)
# thiet lap kich thuoc do thi phu hop voi kh gian hien thi 
par(mar = c(5, 4, 4, 4))
barplot(Bent.M, xlab = "Beach", ylim = c(0, 20), ylab = "Richness", col = rainbow(9))

