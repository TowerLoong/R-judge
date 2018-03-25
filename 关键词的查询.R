#**********************************
#统计出某个文档的降维后的单词种类、单词个数、每个单词的tf

#首先引入semblance.R，它用来比较相似度
source("E:\\R-workspace\\Test4\\semblance.R")

list_word<-list()
list_tfidf<-list()

path <- "E:\\R-workspace\\Test4\\end3" ##文件目录
fileNames <- dir(path)  ##获取该路径下的文件名
for(x in 1:length(fileNames)){
  path0<-paste0("E:\\R-workspace\\Test4\\end3\\",fileNames[x])

#*********************************************************************************************

c_word1<-c()
c_word2<-c()
c_tfidf1<-c()
c_tfidf2<-c()
c_txt<-c()

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
for(i in 1:wordNum){

  #通过as.numeric把value[i]转化成数值类型
  tempTf<-as.numeric(value[i])/num
  tf<-c(tf,tempTf)
  
}

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
  
  tempIdf<-log(5343/df+1)
  
  idf<-c(idf,tempIdf)
  
}

tfidf=tf*idf

c_word1<-data
c_tfidf1<-tfidf

#把每一个文档的特征单词和tfidf存入list,比较的时候再取出来
list_word<-c(list_word,list(data))
list_tfidf<-c(list_tfidf,list(tfidf))
c_txt<-c(c_txt,fileNames[x])

print(x)
#下面的是最外层循环的括号
}


#************************************************************************************************
#开始进行查询，把关键词依次与list中存储的每个word向量比较，若找到相同的，则输出对应的文档

startTime=Sys.time();#记录查询开始时间

searchWord<-"奥运会"
for(x in 1:length(fileNames)){
  c_word<-as.vector(unlist(list_word[x]))
  
  for(y in 1:length(c_word)){
    #print(c_word[y])
    if(searchWord == c_word[y]){
      print(paste("查询出的文档有：",fileNames[x]))
    }
  }
}

endTime=Sys.time()#记录查询结束时间
runningTime=endTime-startTime
print(paste("查询时间为：",runningTime))

