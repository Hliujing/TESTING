#需要两个文件"species_50%.csv"，"clinical_adonis.csv"-前两列均为SampleID/Group,后面为变量
library(vegan)
library(dplyr)

data_clinical=read.csv("clinical_adonis.csv",na.strings = "")
data_omic=read.csv("species_50%.csv",row.names = 1)[,-1]
#data=left_join(data_clinical,data_omic,by="SampleID") 确认SampleID都匹配上了

#获取所有变量的名字
names=colnames(data_clinical)[-1]
#创建空数据框用于存储Adonis结果
out_put=data.frame(Parameters= character(), R2= numeric(), PValue= numeric(), stringsAsFactors=FALSE)
n=1

for (i in names) {
  
  #删除含空值的行Sample
  data_names=na.omit(data_clinical,cols="i")
  Sample=data_names$SampleID
  data_omics=data_omic[which(rownames(data_omic) %in% Sample),]
  
  #adonis中无法使用代词，导致循环无法进行，于是重新定义数据框
  k=data_names[i]
  colnames(k)=c("k")
  #用数据data_clinical$group解释丰度矩阵data_omic
  set.seed(123)
  ads=adonis(data_omics~ k,data=k,permutations = 999,method="bray")
  
  ads
  R2=round(ads$aov.tab[1,5],4)     #R2
  pvalue=round(ads$aov.tab[1,6],3) #P值
  out_put[n,]=c(i,R2,pvalue)
  n=n+1
}

#根据R2的值排序
out_put=arrange(out_put,desc(R2))

out_put$Parameters=factor(out_put$Parameters,levels = out_put$Parameters)

library(ggplot2)
ggplot(data=out_put,mapping=aes(x=Parameters,y=R2,fill=Parameters,group=factor(1)))+
  geom_bar(stat="identity", color="black", width = 0.9)+ #柱边上色-black
  theme_classic()+
  theme(axis.text.x = element_text(angle=90,hjust = 1))


