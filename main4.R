
#**********************************
#统计出某个文档的降维后的单词种类、单词个数、每个单词的tf

path<-"E:\\R-workspace\\Test4\\end2\\(国际)(1)探访日本大型太阳能电池试验场.txt"
con <- file(path, "r")

wordNum<-0
num<-0
noNum<-0
line=readLines(con,n=1)

data<-c();
value<-c();
tf<-c();

#读取文本，并且获得每个单词的出现频率
while( length(line) != 0 ) {
  
  line=readLines(con,n=1)
  line <- unlist(strsplit(line,","))
 
  #频率大于等于2次才留下（降低维度）
  tempLine2<-as.numeric(line[2])

  if(identical(tempLine2,2) |identical(tempLine2,3) | identical(tempLine2,4) | identical(tempLine2,5) |
     identical(tempLine2,6) | identical(tempLine2,7) | identical(tempLine2,8) |identical(tempLine2,9) |
     identical(tempLine2,10)){
    
    data<-c(data,line[1])
    value<-c(value,line[2])
    
    wordNum<-wordNum+1
    num<-num+as.numeric(line[2])
    
  }else{
    noNum<-noNum+as.numeric(line[2])
  }
  
}

#*******************************************************
print(paste("单词种类",wordNum))
print(paste("单词个数",num))

#遍历每个单词向量，计算tf向量
for(i in 1:wordNum){

  #通过as.numeric把value[i]转化成数值类型
  tempTf<-as.numeric(value[i])/num
  tf<-c(tf,tempTf)
  
}

print(data)
print(value)
print(tf)

close(con)

#*************************************************************



