#**********************************
#统计出某个文档的降维后的单词种类、单词个数、每个单词的tf

#首先引入semblance.R，它用来比较相似度
source("E:\\R-workspace\\Test4\\semblance.R")

startTime=Sys.time();#记录开始时间

path <- "E:\\R-workspace\\Test4\\end3" ##文件目录
fileNames <- dir(path)  ##获取该路径下的文件名
for(x in 1:length(fileNames)){
  path0<-paste0("E:\\R-workspace\\Test4\\end3\\",fileNames[x])

#*********************************************************************************************

c_word1<-c()
c_word2<-c()
c_tfidf1<-c()
c_tfidf2<-c()

path<-path0
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
  
  if(wordNum >= 10)
    break
  
  line=readLines(con,n=1)
  line <- unlist(strsplit(line,","))
 
  #频率大于等于2次才留下（降低维度）（最多留下10个）
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
#遍历每个单词向量，计算tf向量

# print(paste("单词种类",wordNum))
# print(paste("单词个数",num))

for(i in 1:wordNum){

  #通过as.numeric把value[i]转化成数值类型
  tempTf<-as.numeric(value[i])/num
  tf<-c(tf,tempTf)
  
}

# print(data)
# print(value)
# print(tf)

close(con)

#*************************************************************
#开始统计降维后的每种单词的idf

idf<-c()
for(i in 1:wordNum){
   
  tempData<-data[i]
  df<-0 
  
  #开始遍历全部文件，求取该单词对应的idf
  path <- "E:\\R-workspace\\Test4\\end3" #文件目录
  fileNames <- dir(path)  ##获取该路径下的文件名
  for(j in 1:length(fileNames)){
    
    path<-paste0("E:\\R-workspace\\Test4\\end3\\",fileNames[j])
#    print(j)
#    print(path)
    con <- file(path, "r")
    #开始读取扫描到的这个文件
    line=readLines(con,n=1)
    while( length(line) != 0 ) {
      
      if(tempData == line[1]){
        df<-df+1
      }
      
      line=readLines(con,n=1)
      line <- unlist(strsplit(line,","))
    
    }
    close(con)
  }
  
#  print(paste("含有该词语的文档一共有",df))
  tempIdf<-log(5343/df+1)
#  print(paste("该词语的idf为",tempIdf))
  
  idf<-c(idf,tempIdf)
  
}

tfidf=tf*idf

# print(data)
# print(value)
# print(tf)
# print(idf)
# print(tfidf)
#print(paste("运行时间",runningTime))

c_word1<-data
c_tfidf1<-tfidf

#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#

for(y in x:length(fileNames)){
  path2<-paste0("E:\\R-workspace\\Test4\\end3\\",fileNames[y])

path<-path2
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
  
  if(wordNum >= 10)
    break
  
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
#遍历每个单词向量，计算tf向量

# print(paste("单词种类",wordNum))
# print(paste("单词个数",num))

for(i in 1:wordNum){
  
  #通过as.numeric把value[i]转化成数值类型
  tempTf<-as.numeric(value[i])/num
  tf<-c(tf,tempTf)
  
}

# print(data)
# print(value)
# print(tf)

close(con)

#*************************************************************
#开始统计降维后的每种单词的idf

idf<-c()
for(i in 1:wordNum){
  
  tempData<-data[i]
  df<-0 
  
  #开始遍历全部文件，求取该单词对应的idf
  path <- "E:\\R-workspace\\Test4\\end3" ##文件目录
  fileNames <- dir(path)  ##获取该路径下的文件名
  for(j in 1:length(fileNames)){
    
    path<-paste0("E:\\R-workspace\\Test4\\end3\\",fileNames[j])
    #    print(j)
    #    print(path)
    con <- file(path, "r")
    #开始读取扫描到的这个文件
    line=readLines(con,n=1)
    while( length(line) != 0 ) {
      
      if(tempData == line[1]){
        df<-df+1
      }
      
      line=readLines(con,n=1)
      line <- unlist(strsplit(line,","))
      
    }
    close(con)
  }
  
#  print(paste("含有该词语的文档一共有",df))
  tempIdf<-log(5343/df+1)
#  print(paste("该词语的idf为",tempIdf))
  
  idf<-c(idf,tempIdf)
  
}

tfidf=tf*idf

# print(data)
# print(value)
# print(tf)
# print(idf)
# print(tfidf)
#print(paste("运行时间",runningTime))

c_word2<-data
c_tfidf2<-tfidf


#根据参数计算最终结果
result<-semblance(c_word1,c_word2,c_tfidf1,c_tfidf2)


#print(paste("最终结果是",result))

#********************************************************************************************************
#如果最终相似度大于了0.8,就输出这个文件的名字
 if(semblance(c_word1,c_word2,c_tfidf1,c_tfidf2) == 1){
   print("以下两个文件的相似度大于了0.8")
   print(fileNames[x])
   print(fileNames[y])
 }

print(paste(x,y))

#下面是内层文件遍历的括号
}

#下面的是最外层循环的括号
}

endTime=Sys.time()#结束时间
runningTime=endTime-startTime
print(paste("运行时间为：",runningTime))

