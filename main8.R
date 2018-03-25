#**********************************
#统计出某个文档的降维后的单词种类、单词个数、每个单词的tf

#首先引入semblance.R，它用来比较相似度
source("E:\\R-workspace\\Test4\\semblance.R")

list_word<-list()
list_tfidf<-list()

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

print(x)
#下面的是最外层循环的括号
}

#print(list_word)
# data3 = as.vector(unlist(list_word[[3]]))
# print(data3)
# data4 = as.vector(unlist(list_tfidf[[2]]))
# print(data4)


#************************************************************************************************
#开始把list中的数据取出来进行向量比较

for(x in 1:length(fileNames)){
  for(y in x:length(fileNames)){
    c_word1<-as.vector(unlist(list_word[x]))
    c_word2<-as.vector(unlist(list_word[y]))
    c_tfidf1<-as.vector(unlist(list_tfidf[x]))
    c_tfidf2<-as.vector(unlist(list_tfidf[y]))

    # print(x)
    # print(y)
    # print(c_word1)
    # print(c_tfidf1)
    # print(c_word2)
    # print(c_tfidf2)
    
    #根据向量余弦判断相似性
    if(semblance(c_word1,c_word2,c_tfidf1,c_tfidf2) == 1){
      print("以下两个文件的相似度大于了0.8")
      print(fileNames[x])
      print(fileNames[y])
    }
}
}


endTime=Sys.time()#结束时间
runningTime=endTime-startTime
print(paste("运行时间为：",runningTime))

