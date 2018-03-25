
#**********************************
#开始秋某个文档的tf
path<-"E:\\R-workspace\\Test4\\end2\\(国际)(1)探访日本大型太阳能电池试验场.txt"
con <- file(path, "r")

num<-0
line=readLines(con,n=1)

data<-c();
value<-c();
tf<-c();

#读取文本，并且获得每个单词的出现频率
while( length(line) != 0 ) {
  
  line=readLines(con,n=1)
  line <- unlist(strsplit(line,","))
 
  data<-c(data,line[1])
  value<-c(value,line[2])
  
  num<-num+1
  
}

num=num-1   #还要减掉第一行标题
print(num)

#遍历每个单词向量，计算tf向量
for(i in 1:num){

  #通过as.numeric把value[i]转化成数值类型
  tempTf<-as.numeric(value[i])/num
  tf<-c(tf,tempTf)
  
}

print(data)
print(value)
print(tf)
close(con)

#*************************************************************



