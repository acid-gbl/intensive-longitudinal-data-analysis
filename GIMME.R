#install.packages("DiagrammeR")
#install.packages("gimme")
library(DiagrammeR)#画流程图
library(gimme)
#library(brms)#贝叶斯模型
library(mvtnorm)
#library(bayesplot)
library(ggplot2)
library(dplyr)
#library(lme4)#混合线性模型
library(xlsx)
#1、数据预处理
wd<-"D:/study/current/组会/暂且完结/密集追踪/GIMME/simul" # 设置文件夹路径
setwd(wd)
dat<- read.csv("Two-Level Data with Trend.csv",header=F) # 读取数据文件
colnames(dat)<-c("URGE", "DEP", "JS", "HS", "PERSON", "TIME") # 指定各变量名，其中URGE指吸烟欲望，DEP指抑郁水平
head(dat)
datawd<-paste0(wd,"/data")
dir.create(datawd)
setwd(datawd)
for(i in 1:length(unique(dat$PERSON))){
  # 选取个体i的数据进行预处理
  person_dat<-dat[which(dat$PERSON==i),c("URGE","DEP","TIME")] 
  
  #分别去除变量URGE和DEP随时间变化的整体趋势(detrend)
  temp = lm(person_dat$URGE~person_dat$TIME)
  person_dat$urge<-residuals(temp)
  temp2 = lm(person_dat$DEP~person_dat$TIME)
  person_dat$dep<-residuals(temp2)
  
  #将个体i的数据单独写入excel文档"data i.csv"
  write.csv(person_dat[,c("urge","dep")],
            paste("data",i,".csv"), row.names = FALSE)
  ##画图
  #setwd("D:/study/current/组会/暂且完结/密集追踪/投稿心理科学进展/图文件")
  #p1<-ggplot(person_dat,aes(y=URGE,x=TIME))
  #p1+geom_point()+geom_line()+stat_smooth(method=lm)
  #p2<-ggplot(person_dat,aes(y=urge,x=TIME))
  #p2+geom_point()+geom_line()+stat_smooth(method=lm)
  #p1<-ggplot(person_dat,aes(y=DEP,x=TIME))
  #p1+geom_point()+geom_line()+stat_smooth(method=lm)
  #p2<-ggplot(person_dat,aes(y=dep,x=TIME))
  #p2+geom_point()+geom_line()+stat_smooth(method=lm)
}

#paths <- 'urge~deplag'
fit<-gimmeSEM(                  
  data = datawd,  # 存放数据的文件夹路径 
  out = paste0(wd,'/output3'),  # 存放输出结果的文件夹路径
  sep = ",",  # 声明数据分隔符，这里csv格式的数据分隔符为逗号
  header = T,  # 声明数据中是否包括变量名
  ar = TRUE,  # 指定模型中是否包括自回归，默认为TRUE
  plot = TRUE,  # 指定结果是否输出模型图，默认为TRUE
  subgroup = T,      # TRUE or FALSE (default), cluster individuals based on similarities in effects
  groupcutoff = .75,  # 路径可以被纳入群体模型的临界值(显著改善个体模型的最小比例，默认为75%)
  subcutoff = .5      # the proportion that is considered the majority at the subgroup level
)   

fit2<-aggSEM(                  
  data = datawd,  # 存放数据的文件夹路径 
  out = paste0(wd,'/output3'),  # 存放输出结果的文件夹路径   
  sep = ",",         
  header = T)   

solution.tree(fit2,#ms.fit,
              level = c("group", "individual"),
              cols =  c("stage", "pruned", "rmsea", "nnfi", "cfi","srmr", "grp_sol", "bic",
           "modularity"),
              ids = "all",
              plot.tree = F)
