#测试文件
source("E:\\R-workspace\\Test4\\semblance.R")

data1<-c(1,2,3)
data2<-c(5,6,7)
data3<-c(8,9,10)

list1<-list(data1)

list1<-c(list1,list(data2))

list1<-c(list1,list(data3))



data4 = as.vector(unlist(list1[2]))



print(data4)



