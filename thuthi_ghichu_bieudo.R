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
# ve do thi bieu dien gia tri trung binh do giau sinh vat tren moi bai bien, tieu de truc x, y 
barplot(Bent.M, xlab = "Beach", ylim = c(0, 20), ylab = "Richness", col = rainbow(9))
bp <- barplot(Bent.M, xlab = "Beach", ylim = c(0,20),ylab = "Richness", col = rainbow(9))
# dung ham row de chi ra do lech chuan
arrows(bp, Bent.M, bp, Bent.M + Bent.sd, lwd = 1.5,angle = 90, length = 0.1)
# dung box() de dong khung
box()

# Boxplot
# dat lai thu muc lam viec 
setwd("E:/NNR/Dataset/Dataset")
# doc file 
wls <- read.table(file = "Owls.txt", header = TRUE)
# dieu chinh kich thuoc bieu do
par(mar = c(2,3, 2,1))
# ve bieu do 
boxplot(Owls$NegPerChick)
# tao cua so do thi voi 2 hang 2 cot 
par(mfrow = c(2,2), mar = c(3, 3, 2, 1))
# tao bieu do hop ,  ~ : bieu dien su phu thuoc giua cac bien, du lieu duoc lay tu tap du lieu Owls duoc doc vao trong bien wls
boxplot(NegPerChick ~ SexParent, data = Owls)
boxplot(NegPerChick ~ FoodTreatment, data = Owls)
# *: phan tach cac nhom du lieu 
boxplot(NegPerChick ~ SexParent * FoodTreatment,data = Owls)
boxplot(NegPerChick ~ SexParent * FoodTreatment,names = c("F/Dep", "M/Dep", "F/Sat", "M/Sat"),data = Owls)
boxplot(NegPerChick ~ Nest, data = Owls)

Benthic <- read.table(file = "RIKZ2.txt",header= TRUE)
#tinh so luong gia tri trong cot Richeness tuong ung voi moi bien trong bech 
Bentic.n <- tapply(Benthic$Richness, Benthic$Beach,FUN = length)
# ve bieu do hop cho bien Richness ,boxplot được tô màu xám và trục x và trục y được đặt tên là "Beach" và "Richness"                 
boxplot(Richness ~ Beach, data = Benthic,col = "grey", xlab = "Beach", ylab = "Richness")
#Biến BP.info lưu trữ thông tin về các thành phần của biểu đồ hộp, bao gồm giá trị của median, các khoảng dữ liệu, các giá trị ngoại lệ và tên của các nhóm trên trục x
BP.info <- boxplot(Richness ~ Beach, data = Benthic,col = "grey", xlab = "Beach",ylab = "Richness")

setwd("E:/NNR/Dataset/Dataset")
#doc du lieu 
Deer <- read.table("Deer.txt", header = TRUE)
par(mar = c(4,5, 1,2))
#Sử dụng hàm dotchart() để vẽ biểu đồ chấm cho dữ liệu chiều dài thân hình (LCT) của nai. Tham số Deer$LCT chính là dữ liệu chiều dài thân hình của nai được lấy từ cột "LCT" của Deer
dotchart(Deer$LCT, xlab = "Length (cm)",ylab = "Observation number")
# tao tieu de cho bieu do 
title("Bird abundance", cex.main = 2,family = "serif", font.main = 1)

setwd("E:/NNR/Dataset/Dataset")
Whales <- read.table(file="TeethNitrogen.txt",header = TRUE)
N.Moby <- Whales$X15N[Whales$Tooth == "Moby"]
Age.Moby <- Whales$Age[Whales$Tooth == "Moby"]
# sử dụng hàm plot để tạo biểu đồ Scatter plot, với trục hoành là tuổi của răng và trục tung là lượng đồng vị Nitơ-15. Cụ thể, x = Age.Moby và y = N.Moby được đưa vào hàm plot để chỉ định dữ liệu cho trục hoành và trục tung, tương ứng.
plot(x = Age.Moby, y = N.Moby, xlab = "Age",ylab = expression(paste(delta^{15}, "N")))
# plot.new: Mở khung đồ họa mới, giống như frame()
# win.graph: Mở cửa sổ đồ họa thứ hai. Bạn có thể thiết lập chiều rộng và chiều cao của màn hình
# windows: Tương tự như win.graph nhưng với nhiều tùy chọn hơn
# savePlot: Lưu đồ họa hiện tại dưới dạng ("wmf", "emf", "png", "jpeg", "jpg", "bmp", "ps", "eps", hoặc "pdf")
# locator: Ghi lại vị trí của con trỏ bằng cách nhấp chuột trái; dừng bằng cách nhấp chuột phải
# range: Trả về một vector chứa giá trị tối thiểu và tối đa của tất cả các đối số đã cho; hữu ích để thiết lập giới hạn x hoặc y
# matplot: Vẽ các cột của một ma trận so với các cột của ma trận khác; đặc biệt hữu ích khi có nhiều cột Y và một cột X. Xem thêm matlines và matpoints để thêm đường và điểm, tương ứng
# persp: Vẽ biểu đồ hình chiếu của các bề mặt trên một mặt phẳng x-y
# cut: Chuyển đổi một biến số số thành một yếu tố
# split: Chia một vector hoặc khung dữ liệu với các giá trị số thành các nhóm riêng biệt.
setwd("E:/NNR/Dataset/Dataset")
#hàm pairs() được sử dụng để tạo ra một ma trận các biểu đồ phân tán. Các cột thứ hai đến thứ chín của bộ dữ liệu Benthic được truyền vào hàm để tạo các biểu đồ phân tán cho các cặp cột tương ứng
Benthic <- read.table(file = "RIKZ2.txt",header = TRUE)
pairs(Benthic[, 2:9])
#coplot()" tạo ra một biểu đồ phân tán có điều kiện, trong đó mỗi điểm trên biểu đồ tương ứng với một quan sát của "Richness" và "NAP", và màu sắc của điểm được sử dụng để chỉ ra giá trị của "grainsize". Biểu đồ này cho phép bạn xem xét mối quan hệ giữa "Richness" và "NAP" trong các phân loại khác nhau của "grainsize".
coplot(Richness ~ NAP | grainsize, pch=19,data = Benthic)
setwd("E:/NNR/Dataset/Dataset")
pHEire <- read.table(file = "SDI2003.txt",header = TRUE)
#hêm vào data frame "pHEire" bằng cách lấy logarit cơ số 10 của cột "Altitude"
pHEire$LOGAlt <- log10(pHEire$Altitude)
#biến "Forested" được chuyển đổi thành một factor và lưu vào cột mới "fForested"
pHEire$fForested <- factor(pHEire$Forested)
#"coplot()" được sử dụng để tạo biểu đồ phân tán có điều kiện của biến "pH" và "SDI" với sự ảnh hưởng của biến "LOGAlt" và "fForested"
coplot(pH ~ SDI | LOGAlt * fForested,panel = panel.lm, data = pHEire)
coplot(pH ~ SDI | LOGAlt * fForested,panel = panel.lm, data = pHEire, number = 2)
# giá trị của dữ liệu được chia làm 2 nhóm 
cut(pHEire$Temperature, breaks = 2)
# Biến pH và SDI được sử dụng để vẽ đồ thị, trong đó pH là biến phụ thuộc và SDI là biến độc lập.
# Dữ liệu được lấy từ bộ dữ liệu pHEire.
# Đồ thị được chia thành các panel dọc theo giá trị LOGAlt và fForested.
# Mỗi panel được vẽ với hàm panel.lm, biểu thị một mô hình tuyến tính giữa pH và SDI trên panel đó.
# Tổng số panel được hiển thị là 3.
# Kích thước chữ được đặt là 1.5 và kí hiệu được sử dụng là 19.
# Màu sắc được chọn dựa trên biến Temp2.num từ bộ dữ liệu pHEire, với mỗi giá trị được ánh xạ vào một mức xám khác nhau.
coplot(pH ~ SDI | LOGAlt * fForested,panel = panel.lm, data = pHEire,number = 3, cex = 1.5, pch = 19,col = gray(pHEire$Temp2.num / 3))
#install.packages("lattice")
# tạo ra một ma trận có kích thước 2x2 với các phần tử được sắp xếp theo thứ tự từ trái qua phải và từ trên xuống dưới là 2, 0, 1, 3.
MyLayOut <- matrix(c(2, 0, 1, 3), nrow = 2, ncol=2,byrow = TRUE)
# layout(mat = MyLayOut, widths = c(3, 1), heights = c(1, 3), respect = TRUE) sử dụng hàm layout để xác định bố cục của lưới đồ thị, trong đó:

# mat = MyLayOut cho biết ma trận MyLayOut được sử dụng để định hình bố cục của lưới đồ thị.

# widths = c(3, 1) cho biết chiều rộng của mỗi cột trong lưới đồ thị, với cột đầu tiên chiếm 3 phần tư (75%) chiều rộng của lưới, cột thứ hai chiếm 1 phần tư (25%) chiều rộng của lưới.

# heights = c(1, 3) cho biết chiều cao của mỗi hàng trong lưới đồ thị, với hàng đầu tiên chiếm 1 phần tư (25%) chiều cao của lưới, hàng thứ hai chiếm 3 phần tư (75%) chiều cao của lưới.

# respect = TRUE cho biết kích thước của các phần tử của lưới phải tương ứng với tỷ lệ chiều rộng và chiều cao của lưới.

# Biến nf lưu trữ thông tin về kích thước của mỗi phần tử trong lưới đồ thị, dưới dạng một danh sách gồm 5 phần tử, bao gồm:
  
#   nf$ là một ma trận 2x2 chứa thông tin về số hàng và số cột của lưới đồ thị.

# nf$widths là một vectơ 2 phần tử chứa thông tin về chiều rộng của mỗi cột trong lưới.

# nf$heights là một vectơ 2 phần tử chứa thông tin về chiều cao của mỗi hàng trong lưới.

# nf$respect cho biết liệu kích thước của các phần tử của lưới có tương ứng với tỷ lệ chiều rộng và chiều cao của lưới hay không.

# nf$layout là một vectơ 4 phần tử chứa thông tin về kích thước của từng phần tử của lưới đồ thị, được đánh số từ 1 đến 4 theo thứ tự từ trái qua phải và từ trên xuống dưới.
nf <- layout(mat = MyLayOut, widths = c(3, 1),heights = c(1, 3), respect = TRUE)
layout.show(nf)
 
# Đoạn code trên sử dụng ma trận MyLayOut để định hình bố cục của một lưới đồ thị và lưu trữ thông tin về kích thước của mỗi phần tử của lưới trong biến nf.

# layout(mat = MyLayOut, widths = c(3, 1), heights = c(1, 3), respect = TRUE) sử dụng hàm layout để xác định bố cục của lưới đồ thị, trong đó:

# mat = MyLayOut cho biết ma trận MyLayOut được sử dụng để định hình bố cục của lưới đồ thị.

# widths = c(3, 1) cho biết chiều rộng của mỗi cột trong lưới đồ thị, với cột đầu tiên chiếm 3 phần tư (75%) chiều rộng của lưới, cột thứ hai chiếm 1 phần tư (25%) chiều rộng của lưới.

# heights = c(1, 3) cho biết chiều cao của mỗi hàng trong lưới đồ thị, với hàng đầu tiên chiếm 1 phần tư (25%) chiều cao của lưới, hàng thứ hai chiếm 3 phần tư (75%) chiều cao của lưới.

# respect = TRUE cho biết kích thước của các phần tử của lưới phải tương ứng với tỷ lệ chiều rộng và chiều cao của lưới.

# Biến nf lưu trữ thông tin về kích thước của mỗi phần tử trong lưới đồ thị, dưới dạng một danh sách gồm 5 phần tử, bao gồm:

# nf$ là một ma trận 2x2 chứa thông tin về số hàng và số cột của lưới đồ thị.

# nf$widths là một vectơ 2 phần tử chứa thông tin về chiều rộng của mỗi cột trong lưới.

# nf$heights là một vectơ 2 phần tử chứa thông tin về chiều cao của mỗi hàng trong lưới.

# nf$respect cho biết liệu kích thước của các phần tử của lưới có tương ứng với tỷ lệ chiều rộng và chiều cao của lưới hay không.

# nf$layout là một vectơ 4 phần tử chứa thông tin về kích thước của từng phần tử của lưới đồ thị, được đánh số từ 1 đến 4 theo thứ tự từ trái qua phải và từ trên xuống dưới.
# title(MyText): Thêm tiêu đề vào đồ thị bằng cách sử dụng MyText để định nghĩa nội dung của tiêu đề.

# expression: Cho phép sử dụng các ký hiệu đặc biệt trong đồ thị, ví dụ như sử dụng delta15N để biểu diễn ký hiệu delta với số nguyên 15 và chữ cái N, bằng cách sử dụng ylab = expression(paste(delta_{15}, "N")).

# pairs(X): Vẽ biểu đồ scatterplot (biểu đồ phân tán) cho các biến trong ma trận X.

# coplot(y~x|z): Vẽ biểu đồ scatterplot cho hai biến y và x với điều kiện phụ thuộc vào biến z.

# layout(mat, widths, heights): Thiết lập bố cục đồ thị, cho phép vẽ nhiều đồ thị trong cùng một cửa sổ.

# plot(x): Vẽ đồ thị tương ứng với dữ liệu số liệu x.

# plot(y): Vẽ đồ thị tương ứng với dữ liệu số liệu y.

