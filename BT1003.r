getwd()
setwd("E:/NNR/Dataset/Dataset")

# nhiet do lon nhat theo thang
tapply(Temperature$Temperature, Temperature$Month, max, na.rm = TRUE)
# nhiet do nho nhat theo thang
tapply(Temperature$Temperature, Temperature$Month, min, na.rm = TRUE)
# nhiet do trung binh theo thang 
tapply(Temperature$Temperature, Temperature$Month, mean, na.rm = TRUE)
# trung vi nhiet do theo thang
tapply(Temperature$Temperature, Temperature$Month, median, na.rm = TRUE)
# tong nhiet do nho nhat theo thang
tapply(Temperature$Temperature, Temperature$Month, sum, na.rm = TRUE)
# tinh nhiet do trung binh ket hop 2 yeu to mua, nam
tapply(Temperature$Temperature, interaction(Temperature$Season , Temperature$Year), mean, na.rm = TRUE)

#  do man  lon nhat theo thang
tapply(Temperature$Salinity, Temperature$Month, max, na.rm = TRUE)
#  do man nho nhat theo thang
tapply(Temperature$Salinity, Temperature$Month, min, na.rm = TRUE)
# do man  trung binh theo thang 
tapply(Temperature$Salinity, Temperature$Month, mean, na.rm = TRUE)
# do man trung vi nhiet do theo thang
tapply(Temperature$Salinity, Temperature$Month, median, na.rm = TRUE)
# tong do man  theo thang
tapply(Temperature$Salinity, Temperature$Month, sum, na.rm = TRUE)
# tinh do man  trung binh ket hop 2 yeu to mua, nam
tapply(Temperature$Salinity, interaction(Temperature$Season , Temperature$Year), mean, na.rm = TRUE)

