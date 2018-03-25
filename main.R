library(rJava);
library(Rwordseg);

#********************************************
#对文档进行分词操作

#选择文本进行读入
path <- "E:\\R-workspace\\Test4\\word" ##文件目录
fileNames <- dir(path)  ##获取该路径下的文件名

for(i in 1:length(fileNames))
{
  path<-paste0("E:\\R-workspace\\Test4\\word\\",fileNames[i])

lecture=read.table(path,sep="\t",header=F)

print(i)
# 获取数据集长度  
n=length(lecture[,1]);  
print(n)

# == 文本预处理  
res=lecture[lecture!=" "];  

#用正则表达式剔除特殊词
res=gsub(pattern="[我|你|他|她|的|了|是|的|最|这|对|又]","",res);       

#分词、频数统计  
#unlist函数的作用是把list类型的数据转化为vector
#words=unlist(lapply(X=res, FUN=segmentCN));  
words=lapply(X=res, FUN=segmentCN)
#输出分词的总运行时间

word=lapply(X=words, FUN=strsplit, " ");  
v=table(unlist(word));

# == 输出结果  

outpath<-paste0("E:\\R-workspace\\Test4\\end2\\",fileNames[i])
write.csv(v, file=outpath, row.names = F, quote = F)
print(fileNames[i])

}

print("over")




