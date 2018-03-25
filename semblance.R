#****************************************************
#比较相似度的函数，返回值为1表示这两个文档相似度大于0.8，不然就返回0
#c_word1,c_word2,c_tfidf1,c_tfidf2依次是
#第一个文档的单词，第二个文档的单词，第一个文档的tfidf，第二个文档的tfdif

semblance<-function(c_word1,c_word2,c_tfidf1,c_tfidf2){
  
  c_wordAll<-c()
  c_wordEnd1<-c()
  c_wordEnd2<-c()
  c_tfidfEnd1<-c()
  c_tfidfEnd2<-c()
  
  #*************************************************
  #把两个向量合并成c_wordAll
  
  for(k in 1:length(c_word1)){
    c_wordAll<-c(c_wordAll,c_word1[k])
  }
  
  for(k in 1:length(c_word2)){
    
    flag=0;
    
    for(p in 1:length(c_wordAll)){
      if(c_wordAll[p] == c_word2[k])
      {
        flag=1
        break
      }
    }
    
    if(flag == 0){
      c_wordAll<-c(c_wordAll,c_word2[k])

    }
    
  }
  
#*******************************************  
#用合并好后的向量给那两个输入的东西进行赋值  
 
  c_wordEnd1<-c_wordAll
  c_wordEnd2<-c_wordAll
  
  for(i in 1:length(c_wordAll)){
    c_tfidfEnd1<-c(c_tfidfEnd1,0)
    c_tfidfEnd2<-c(c_tfidfEnd2,0)
  }
  
  #计算第一个的
  for(i in 1:length(c_word1)){
    for(j in 1:length(c_wordEnd1)){
      
      if(c_word1[i] == c_wordEnd1[j]){
        c_tfidfEnd1[j]<-c_tfidf1[i]
        break
      }
    }
  }
  
  #计算第二个的
  for(i in 1:length(c_word2)){
 
    for(j in 1:length(c_wordEnd2)){
      
      if(c_word2[i] == c_wordEnd2[j]){
        c_tfidfEnd2[j]<-c_tfidf2[i]
        break
      }
    }
  }
  
#*************************************************************************
#开始计算余弦相似度
  a<-0
  b<-0
  c<-0
  for(i in 1:length(c_tfidfEnd1)){
    a<-a+c_tfidfEnd1[i]*c_tfidfEnd2[i]
    b<-b+c_tfidfEnd1[i]*c_tfidfEnd1[i]
    c<-c+c_tfidfEnd2[i]*c_tfidfEnd2[i]
  }

  b<-sqrt(b)
  c<-sqrt(c)
  
  result<-0.001
  result<-a/(b*c)

  #print(paste("semblance:",result))
  
  resultPoint<-0
  if(result>0.8)
    resultPoint=1

  return (resultPoint)

}



