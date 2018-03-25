#**********************************
#统计出某个文档的降维后的单词种类、单词个数、每个单词的tf

startTime=Sys.time();

c_word1<-c()
c_word2<-c()
c_tfidf1<-c()
c_tfidf2<-c()

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
  
  if(wordNum >= 4)
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

print(paste("单词种类",wordNum))
print(paste("单词个数",num))

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
#开始统计降维后的每种单词的idf

idf<-c()
for(i in 1:wordNum){
   
  tempData<-data[i]
  df<-0 
  
  #开始遍历全部文件，求取该单词对应的idf
  path <- "E:\\R-workspace\\Test4\\end2" ##文件目录
  fileNames <- dir(path)  ##获取该路径下的文件名
  for(j in 1:length(fileNames)){
    
    path<-paste0("E:\\R-workspace\\Test4\\end2\\",fileNames[j])
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
  
  print(paste("含有该词语的文档一共有",df))
  tempIdf<-log(5343/df+1)
  print(paste("该词语的idf为",tempIdf))
  
  idf<-c(idf,tempIdf)
  
}

endTime=Sys.time();
runningTime=endTime-startTime;

tfidf=tf*idf

print(data)
print(value)
print(tf)
print(idf)
print(tfidf)
print(paste("运行时间",runningTime))

c_word1<-data
c_tfidf1<-tfidf

#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#*********************************************************************************************
#
startTime=Sys.time();

path<-"E:\\R-workspace\\Test4\\end2\\“白宫书记”案为何怕见阳光.txt"
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
  
  if(wordNum >= 4)
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

print(paste("单词种类",wordNum))
print(paste("单词个数",num))

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
#开始统计降维后的每种单词的idf

idf<-c()
for(i in 1:wordNum){
  
  tempData<-data[i]
  df<-0 
  
  #开始遍历全部文件，求取该单词对应的idf
  path <- "E:\\R-workspace\\Test4\\end2" ##文件目录
  fileNames <- dir(path)  ##获取该路径下的文件名
  for(j in 1:length(fileNames)){
    
    path<-paste0("E:\\R-workspace\\Test4\\end2\\",fileNames[j])
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
  
  print(paste("含有该词语的文档一共有",df))
  tempIdf<-log(5343/df+1)
  print(paste("该词语的idf为",tempIdf))
  
  idf<-c(idf,tempIdf)
  
}

endTime=Sys.time();
runningTime=endTime-startTime;

tfidf=tf*idf

print(data)
print(value)
print(tf)
print(idf)
print(tfidf)
print(paste("运行时间",runningTime))

c_word2<-data
c_tfidf2<-tfidf

#根据参数计算最终结果
print(paste("最终结果是",semblance(c_word1,c_word2,c_tfidf1,c_tfidf2)))






