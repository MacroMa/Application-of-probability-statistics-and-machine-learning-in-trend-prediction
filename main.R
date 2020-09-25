library(e1071);library(ggplot2);library(grid);library(MASS) 
library(forecast);library(neuralnet);library(ggpubr)
# arima start	    ——line58
# svr start	    	——line98
# bp start	    	——line244
# arima-svr start	——line374
# arima-bp start	——line510
#获取工作空间，请将数据放在该代码输出的地址路径中
rm(list = ls())
getwd()
alldata=read.csv("data.csv")

data=alldata[,-2]
data=data[,-2]
data=data[,-2]

data$Time=as.character(data$Time)
# data$Time=as.Date(data$Time)
# as.Date("2004/01/30")
tsdata=ts(data[,2], start = c(2004, 1), frequency = 12)
# tsdata
# 
# plot.ts(tsdata,xlab="",ylab="")

#plot1#
ggseasonplot(tsdata,ylab = "Mortality",xlab = '月份',main="")+geom_point()+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
ggsubseriesplot(tsdata,ylab = "季节效应",xlab = '月份',main="")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

bstl=stl(tsdata,"per")


#plot2#
tu2=bstl$time.series
par(mfrow=c(2,2)) 
#时序图
plot.ts(tsdata,xlab="时序图",ylab="")
#趋势性
plot(tu2[,2],xlab="趋势性",ylab="")
#季节性
plot(tu2[,1],xlab="季节性",ylab="")
#随机性
plot(tu2[,3],xlab="随机性",ylab="")
par(mfrow=c(1,3)) 

#plot3#
diffdata=diff(tsdata)
diffdata=diff(diffdata,12)
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot.ts(diffdata,xlab="年份",ylab="")
acf(diffdata,main='',xlab='Lag',ylab='ACF',las=1)               
pacf(diffdata,main='',xlab='Lag',ylab='PACF',las=1)    

# arima start
## 选择最合适的arima模型
arima1<-auto.arima(tsdata,trace=T)
fit1 <- Arima(tsdata, order=c(1,1,0),seasonal=c(2,1,2))
summary(fit1)
test=fit1$model
"P值";test$P[1:3,1]
"T值";test$T[1:3,1]



# fit1 <- Arima(diffdata, order=c(2,0,0),seasonal=c(2,0,1))
# summary(fit1)

par(mfrow=c(1,1))
# plot(fit1$fitted,lty=2,col="red",lwd=2,xlab="ARIMA模型拟合图 ",ylab="")
# lines(tsdata,col="blue",lwd=2)
fitdata=data.frame(matrix(ncol = 3,nrow = 180*2))
colnames(fitdata)=c("date","value","类型")
fitdata[1:180,1]=as.character(alldata$Time)
fitdata[181:360,1]=as.character(alldata$Time)
fitdata[1:180,2]=fit1$fitted
fitdata[181:360,2]=tsdata

fitdata[1:180,3]="预测值"
fitdata[181:360,3]="真实值"
fitdata$`类型`=as.factor(fitdata$`类型`)
#plot4#
ggplot(data = fitdata,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("时间")+ylab("Mortality")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

# View(test)
# plot(fit1$fitted)
# plot(tsdata)
# predict(fit1)

# svr start
svrdata=data

svrdata$Mortality=(svrdata$Mortality-min(svrdata$Mortality))/(max(svrdata$Mortality)-min(svrdata$Mortality))

test=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(test)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    test$x1[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j]
    test$x2[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+12]
    test$x3[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+24]
    test$y[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+36]
  }
}


## prepare data
svrdata_train=test[1:(144-12),]
svrdata_test=test[(144-12+1):144,]
## create model【test】
mysvr=svm(y~x1+x2+x3,data=svrdata_train,cost=1e04,gamma=0)
svrdata_test$pred_y=predict(mysvr,svrdata_test)
svrdata_test$error2=(svrdata_test$pred_y-svrdata_test$y)*(svrdata_test$pred_y-svrdata_test$y)
## caculate MSE【test】
mse=mean(svrdata_test$error2)
mse


svr_result=data.frame(matrix(ncol = 6,nrow = 220))
colnames(svr_result)=c("basenumber_of_cost","index_of_cost","gamma","mse","mape","epsilon")
svr_result$epsilon=0.1
row=0
for (basenumber_of_cost in c(1,2,4,6,8)) {
  for (index_of_cost in 3:6) {
    for (gamma in 0:10) {
      gamma=gamma/100
      row=row+1
      ## prepare data
      svrdata_train=test[1:(144-12),]
      svrdata_test=test[(144-12+1):144,]
      ## create model
      mysvr=svm(y~x1+x2+x3,data=svrdata_train,
                cost=as.numeric(paste(basenumber_of_cost,"e",index_of_cost,sep = "")),
                gamma=gamma)
      svrdata_test$pred_y=predict(mysvr,svrdata_test)
      ## caculate MSE
      svrdata_test$error2=(svrdata_test$pred_y-svrdata_test$y)*(svrdata_test$pred_y-svrdata_test$y)
      mse=mean(svrdata_test$error2)
      ## caculate MAPE
      svrdata_test$error=abs((svrdata_test$pred_y-svrdata_test$y)/svrdata_test$y)
      mape=mean(svrdata_test$error)
      
      svr_result$basenumber_of_cost[row]=basenumber_of_cost
      svr_result$index_of_cost[row]=index_of_cost
      svr_result$gamma[row]=gamma
      svr_result$mse[row]=mse
      svr_result$mape[row]=mape
    }
  }
  print(basenumber_of_cost)
}


svr_result$cost=as.numeric(paste(svr_result$basenumber_of_cost,"e",svr_result$index_of_cost,sep = ""))


plot_data=svr_result
plot_data$gamma=as.factor(plot_data$gamma)
plot_data$cost=as.factor(plot_data$cost)
plot_data$mape_group=NA
for (i in 1:nrow(plot_data)) {
  if (plot_data$mape[i]<=0.09634) {
    plot_data$mape_group[i]="[0,0.09634]"
  }
  if (plot_data$mape[i]>0.09634 && plot_data$mape[i]<=0.11518) {
    plot_data$mape_group[i]="(0.09634,0.11518]"
  }
  if (plot_data$mape[i]>0.11518 && plot_data$mape[i]<=0.13077) {
    plot_data$mape_group[i]="(0.11518,0.13077]"
  }
  if (plot_data$mape[i]>0.13077) {
    plot_data$mape_group[i]="(0.13077,1.30327]"
  }  
}

plot_data$mape_group=factor(plot_data$mape_group,
                            order=TRUE,levels=c("(0.13077,1.30327]",
                                                "(0.11518,0.13077]",
                                                "(0.09634,0.11518]",
                                                "[0,0.09634]"))

#plot5#
ggplot(plot_data, aes(gamma,cost))+geom_tile(aes(fill = mape_group)) + 
  scale_fill_brewer(name="MAPE")+ggtitle("不同参数组合下的预测错误率")+
  theme(plot.title = element_text(hjust = 0.5))


minmse=1
min_mse_row=0
for (i in 1:nrow(svr_result)) {
  if (svr_result$mse[i]<minmse) {
    minmse=svr_result$mse[i]
    min_mse_row=i
  }
}
svr_result[min_mse_row,3:7]

final_svrdata=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(final_svrdata)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    final_svrdata$x1[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j]
    final_svrdata$x2[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+12]
    final_svrdata$x3[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+24]
    final_svrdata$y[(i-1)*12+j]=svrdata$Mortality[(i-1)*12+j+36]
  }
}

mysvr=svm(y~x1+x2+x3,data=final_svrdata,gamma=0.05,cost=6e05)
final_svrdata$pred_y=predict(mysvr)


fitdata2=data.frame(matrix(ncol = 3,nrow = 144*2))
colnames(fitdata2)=c("date","value","类型")

fitdata2[1:144,1]=as.character(alldata$Time[37:180])
fitdata2[145:288,1]=as.character(alldata$Time[37:180])
fitdata2[1:144,2]=final_svrdata$pred_y
fitdata2[145:288,2]=final_svrdata$y

fitdata2[1:144,3]="预测值"
fitdata2[145:288,3]="真实值"
fitdata2$`类型`=as.factor(fitdata2$`类型`)

#plot6#
ggplot(data = fitdata2,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("时间")+ylab("Mortality")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
# svr over

# bp start
bpdata=data

bpdata$Mortality=(bpdata$Mortality-min(bpdata$Mortality))/(max(bpdata$Mortality)-min(bpdata$Mortality))

test=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(test)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    test$x1[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j]
    test$x2[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+12]
    test$x3[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+24]
    test$y[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+36]
  }
}



## prepare data
bpdata_train=test[1:(144-12),]
bpdata_test=test[(144-12+1):144,]
# practice

mybp <- neuralnet(y~x1+x2+x3,bpdata_train,
                  hidden=20, threshold=0.005,
                  learningrate = 0.1, algorithm = "rprop+",
                  err.fct = "sse", act.fct = "logistic") 
#建立神经网络模型解决回归预测，三个输入，一个输出，
#隐藏层20个神经元，阈值为0.005，学习率为0.1，
#选用rprop+方法进行参数优化，损失函数SSE，激活函数logistic 


bpdata_test$pred_y=predict(mybp,bpdata_test)
bpdata_test$error2=(bpdata_test$pred_y-bpdata_test$y)*(bpdata_test$pred_y-bpdata_test$y)
## caculate MSE
mse=mean(bpdata_test$error2)
mse
# main

bp_result=data.frame(matrix(ncol = 2,nrow = 9))
colnames(bp_result)=c("BP net","MSE")
set.seed(11)
for (M in 1:9) {
  mybp <- neuralnet(y~x1+x2+x3,bpdata_train,
                    hidden=M, threshold=0.005,
                    learningrate = 0.1, algorithm = "rprop+",
                    err.fct = "sse", act.fct = "logistic") 
  #建立神经网络模型解决回归预测，三个输入，一个输出，
  #隐藏层20个神经元，阈值为0.005，学习率为0.1，
  #选用rprop+方法进行参数优化，损失函数SSE，激活函数logistic 
  
  
  bpdata_test$pred_y=predict(mybp,bpdata_test)
  bpdata_test$error2=(bpdata_test$pred_y-bpdata_test$y)*(bpdata_test$pred_y-bpdata_test$y)
  ## caculate MSE
  mse=mean(bpdata_test$error2)
  bp_result$`BP net`[M]=paste("3-",M,"-1",sep = "")
  bp_result$MSE[M]=mse
}
#plot7#
bp_result$MSE=round(bp_result$MSE,6)
ggplot(bp_result,aes(x=`BP net`,y=MSE))+geom_bar(stat = "identity",colour="black",fill="grey")+
  geom_text(aes(label=MSE, y=MSE+0.00005), position=position_dodge(0.9), vjust=0)+
  xlab("BP net")+ylab("MSE")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

final_bpdata=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(final_bpdata)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    final_bpdata$x1[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j]
    final_bpdata$x2[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+12]
    final_bpdata$x3[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+24]
    final_bpdata$y[(i-1)*12+j]=bpdata$Mortality[(i-1)*12+j+36]
  }
}

mybp= neuralnet(y~x1+x2+x3,final_bpdata,
                hidden=2, threshold=0.005,
                learningrate = 0.1, algorithm = "rprop+",
                err.fct = "sse", act.fct = "logistic")
final_bpdata$pred_y=predict(mybp,final_bpdata)



fitdata3=data.frame(matrix(ncol = 3,nrow = 144*2))
colnames(fitdata3)=c("date","value","类型")

fitdata3[1:144,1]=as.character(alldata$Time[37:180])
fitdata3[145:288,1]=as.character(alldata$Time[37:180])
fitdata3[1:144,2]=final_bpdata$pred_y
fitdata3[145:288,2]=final_bpdata$y

fitdata3$type=NA
fitdata3[1:144,3]="BPANN拟合值"
fitdata3[145:288,3]="真实值"
fitdata3$`类型`=as.factor(fitdata3$`类型`)

#plot8#
ggplot(data = fitdata3,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("时间")+ylab("Mortality")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
# bp over



# Residual decomposition

data_all=alldata$Mortality
tsdata=ts(data[,2], start = c(2004, 1), frequency = 12)
bstl=stl(tsdata,"per")
decompose_model=bstl$time.series

data_main=decompose_model[,1]+decompose_model[,2]
data_residual=decompose_model[,3]

# Residual decomposition over

# the common arima of two algorithm
arima_main_data=data_main
fit2 <- Arima(arima_main_data, order=c(10,1,1),seasonal=c(0,1,1))
arima_main_result=fit2$fit

# arima-svr start
suanfa_residual_data=data_residual

test2=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(test2)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    test2$x1[(i-1)*12+j]=suanfa_residual_data[(i-1)*12+j]
    test2$x2[(i-1)*12+j]=suanfa_residual_data[(i-1)*12+j+12]
    test2$x3[(i-1)*12+j]=suanfa_residual_data[(i-1)*12+j+24]
    test2$y[(i-1)*12+j]=suanfa_residual_data[(i-1)*12+j+36]
  }
}

suanfadata_train=test2[1:(144-12),]
suanfadata_test=test2[(144-12+1):144,]

suanfa_result=data.frame(matrix(ncol = 6,nrow = 220))
colnames(suanfa_result)=c("basenumber_of_cost","index_of_cost","gamma","mse","mape","epsilon")
suanfa_result$epsilon=0.1
row=0
for (basenumber_of_cost in c(1,2,4,6,8)) {
  for (index_of_cost in 3:6) {
    for (gamma in 0:10) {
      gamma=gamma/100
      row=row+1
      ## prepare data
      suanfadata_train=test2[1:(144-12),]
      suanfadata_test=test2[(144-12+1):144,]
      ## create model
      mysuanfa=svm(y~x1+x2+x3,data=suanfadata_train,
                   cost=as.numeric(paste(basenumber_of_cost,"e",index_of_cost,sep = "")),
                   gamma=gamma)
      suanfadata_test$pred_y=predict(mysuanfa,suanfadata_test)
      ## caculate MSE
      suanfadata_test$error2=(suanfadata_test$pred_y-suanfadata_test$y)*(suanfadata_test$pred_y-suanfadata_test$y)
      mse=mean(suanfadata_test$error2)
      ## caculate MAPE
      suanfadata_test$error=abs((suanfadata_test$pred_y-suanfadata_test$y)/suanfadata_test$y)
      mape=mean(suanfadata_test$error)
      
      suanfa_result$basenumber_of_cost[row]=basenumber_of_cost
      suanfa_result$index_of_cost[row]=index_of_cost
      suanfa_result$gamma[row]=gamma
      suanfa_result$mse[row]=mse
      suanfa_result$mape[row]=mape
    }
  }
  print(basenumber_of_cost)
}

suanfa_result$cost=as.numeric(paste(suanfa_result$basenumber_of_cost,"e",suanfa_result$index_of_cost,sep = ""))

plot_data=suanfa_result
plot_data$gamma=as.factor(plot_data$gamma)
plot_data$cost=as.factor(plot_data$cost)
plot_data$mape_group=NA
for (i in 1:nrow(plot_data)) {
  if (plot_data$mape[i]<=0.8152) {
    plot_data$mape_group[i]="[0,0.8152]"
  }
  if (plot_data$mape[i]>0.8152 && plot_data$mape[i]<=0.9831) {
    plot_data$mape_group[i]="(0.8152,0.9831]"
  }
  if (plot_data$mape[i]>0.9831 && plot_data$mape[i]<=1.0930) {
    plot_data$mape_group[i]="(0.9831,1.0930]"
  }
  if (plot_data$mape[i]>1.0930) {
    plot_data$mape_group[i]="(1.0930,4.3221]"
  }  
}

plot_data$mape_group=factor(plot_data$mape_group,
                            order=TRUE,levels=c("(1.0930,4.3221]",
                                                "(0.9831,1.0930]",
                                                "(0.8152,0.9831]",
                                                "[0,0.8152]"))

#plot9#
ggplot(plot_data, aes(gamma,cost))+geom_tile(aes(fill = mape_group)) + 
  scale_fill_brewer(name="MAPE")+ggtitle("不同参数组合下的预测错误率")+
  theme(plot.title = element_text(hjust = 0.5))





minmse=1
min_mse_row=0
for (i in 1:nrow(suanfa_result)) {
  if (suanfa_result$mse[i]<minmse) {
    minmse=suanfa_result$mse[i]
    min_mse_row=i
  }
}
suanfa_result[min_mse_row,3:7]

final_suanfadata=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(final_suanfadata)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    final_suanfadata$x1[(i-1)*12+j]=data_residual[(i-1)*12+j]
    final_suanfadata$x2[(i-1)*12+j]=data_residual[(i-1)*12+j+12]
    final_suanfadata$x3[(i-1)*12+j]=data_residual[(i-1)*12+j+24]
    final_suanfadata$y[(i-1)*12+j]=data_residual[(i-1)*12+j+36]
  }
}

mysuanfa=svm(y~x1+x2+x3,data=final_suanfadata,gamma=0.01,cost=4e+05)
final_suanfadata$pred_y=predict(mysuanfa)


fitdata4=data.frame(matrix(ncol = 3,nrow = 144*2))
colnames(fitdata4)=c("date","value","类型")

fitdata4[1:144,1]=as.character(alldata$Time[37:180])
fitdata4[145:288,1]=as.character(alldata$Time[37:180])
fitdata4[1:144,2]=final_suanfadata$pred_y+arima_main_result[37:180]
fitdata4[145:288,2]=final_suanfadata$y+arima_main_data[37:180]

fitdata4[1:144,3]="预测值"
fitdata4[145:288,3]="真实值"
fitdata4$`类型`=as.factor(fitdata4$`类型`)

#plot10#
ggplot(data = fitdata4,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("时间")+ylab("Mortality")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))
# arima-svr over


# arima-bp start
bpdata_train=test2[1:(144-12),]
bpdata_test=test2[(144-12+1):144,]


bp_result=data.frame(matrix(ncol = 2,nrow = 9))
colnames(bp_result)=c("BP net","MSE")
set.seed(11)
for (M in 1:9) {
  mybp <- neuralnet(y~x1+x2+x3,bpdata_train,
                    hidden=M, threshold=0.005,
                    learningrate = 0.1, algorithm = "rprop+",
                    err.fct = "sse", act.fct = "logistic") 
  #建立神经网络模型解决回归预测，三个输入，一个输出，
  #隐藏层20个神经元，阈值为0.005，学习率为0.1，
  #选用rprop+方法进行参数优化，损失函数SSE，激活函数logistic 
  
  
  bpdata_test$pred_y=predict(mybp,bpdata_test)
  bpdata_test$error2=(bpdata_test$pred_y-bpdata_test$y)*(bpdata_test$pred_y-bpdata_test$y)
  ## caculate MSE
  mse=mean(bpdata_test$error2)
  bp_result$`BP net`[M]=paste("3-",M,"-1",sep = "")
  bp_result$MSE[M]=mse
  print(M)
}
#plot 11#
bp_result$MSE=bp_result$MSE*1e06
bp_result$MSE=round(bp_result$MSE,6)

ggplot(bp_result,aes(x=`BP net`,y=MSE))+geom_bar(stat = "identity",colour="black",fill="grey")+
  geom_text(aes(label=MSE, y=MSE+0.05), position=position_dodge(0.9), vjust=0)+
  xlab("BP net")+ylab("MSE(1e-6)")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

final_bpdata=data.frame(matrix(ncol = 4,nrow = 12*12))
colnames(final_bpdata)=c("y","x1","x2","x3")
for (i in 1:12) {
  for (j in 1:12) {
    
    final_bpdata$x1[(i-1)*12+j]=data_residual[(i-1)*12+j]
    final_bpdata$x2[(i-1)*12+j]=data_residual[(i-1)*12+j+12]
    final_bpdata$x3[(i-1)*12+j]=data_residual[(i-1)*12+j+24]
    final_bpdata$y[(i-1)*12+j]=data_residual[(i-1)*12+j+36]
  }
}
set.seed(1)
myarbp= neuralnet(y~x1+x2+x3,final_bpdata,
                  hidden=8, threshold=0.005,
                  learningrate = 0.1, algorithm = "rprop+",
                  err.fct = "sse", act.fct = "logistic")
final_bpdata$pred_y=predict(myarbp,final_bpdata)



fitdata5=data.frame(matrix(ncol = 3,nrow = 144*2))
colnames(fitdata5)=c("date","value","类型")

fitdata5[1:144,1]=as.character(alldata$Time[37:180])
fitdata5[145:288,1]=as.character(alldata$Time[37:180])
fitdata5[1:144,2]=final_bpdata$pred_y+arima_main_result[37:180]
fitdata5[145:288,2]=final_bpdata$y++arima_main_data[37:180]

fitdata5$type=NA
fitdata5[1:144,3]="BPANN拟合值"
fitdata5[145:288,3]="真实值"
fitdata5$`类型`=as.factor(fitdata5$`类型`)

#plot12#
ggplot(data = fitdata5,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("时间")+ylab("Mortality")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

# arima-bp over

alldata[,4]=alldata[,5]
final_train_data=data.frame(matrix(nrow = 12,ncol = 4))
final_train_data[,1]=alldata[169:180,4]
final_train_data[,2]=alldata[133:144,4]
final_train_data[,3]=alldata[145:156,4]
final_train_data[,4]=alldata[157:159,4]
colnames(final_train_data)=c("y","x1","x2","x3")

final_result=data.frame(matrix(nrow = 12,ncol = 7))
colnames(final_result)=c("日期","实际值","ARIMA","SVR","BPANN","ARIMA-SVR","ARIMA-BPANN")
final_result$日期=alldata[169:180,1]

final_result$实际值=alldata[169:180,4]
final_result$ARIMA=fit1$fitted[169:180]

mysvr=svm(y~x1+x2+x3,data=final_train_data,gamma=0.05,cost=6e05)
final_result$SVR=predict(mysvr,final_train_data)

mybp= neuralnet(y~x1+x2+x3,final_train_data,
                hidden=8, threshold=0.005,
                learningrate = 0.1, algorithm = "rprop+",
                err.fct = "sse", act.fct = "logistic")
final_result$BPANN=predict(mybp,final_train_data)

final_result$`ARIMA-SVR`=fitdata4[133:144,2]
final_result$`ARIMA-BPANN`=fitdata5[133:144,2]
final_result

date_char=c("M1","M2","M3","M4","M5","M6",
            "M7","M8","M9","M10","M11","M12")
final_plot_data=data.frame(matrix(ncol = 3,nrow = 12*6))
colnames(final_plot_data)=c("date","value","类型")
final_plot_data[1:12,1]=date_char
final_plot_data[1:12,2]=final_result[,2]
final_plot_data[1:12,3]="实际值"

final_plot_data[13:24,1]=date_char
final_plot_data[13:24,2]=final_result[,3]
final_plot_data[13:24,3]="ARIMA"

final_plot_data[25:36,1]=date_char
final_plot_data[25:36,2]=final_result[,4]
final_plot_data[25:36,3]="SVR"

final_plot_data[37:48,1]=date_char
final_plot_data[37:48,2]=final_result[,5]
final_plot_data[37:48,3]="BPANN"

final_plot_data[49:60,1]=date_char
final_plot_data[49:60,2]=final_result[,6]
final_plot_data[49:60,3]="ARIMA-SVR"

final_plot_data[61:72,1]=date_char
final_plot_data[61:72,2]=final_result[,7]
final_plot_data[61:72,3]="ARIMA-BPANN"

final_plot_data$date=as.factor(final_plot_data$date)
final_plot_data$`类型`=as.factor(final_plot_data$`类型`)

#plot13#

p1=ggplot(data = final_plot_data,aes(x=date,y=value,group=`类型`,color=`类型`))+
  geom_line(lwd=1)+scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  xlab("2018")+ylab("")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))

p2=ggplot(data = final_plot_data,aes(colour=`类型`))+
  geom_boxplot(aes(x=date,y=value,group=`类型`))+
  xlab("")+ylab("Mortality")+
  guides(colour=F)+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),
        axis.text.x = element_blank())

ggarrange(p2, p1,widths = c(1,2.5))

## MSE MAE MAPE
final_analysis=final_result
final_analysis$日期=as.character(final_analysis$日期)
final_analysis

for (i in 3:7) {
  final_analysis[13,i]=mean((final_analysis[1:12,2]-final_analysis[1:12,i])*
                              (final_analysis[1:12,2]-final_analysis[1:12,i]))
}
final_analysis[13,1]='MSE'


for (i in 3:7) {
  final_analysis[14,i]=mean(abs(final_analysis[1:12,2]-final_analysis[1:12,i]))
}
final_analysis[14,1]='MAE'


for (i in 3:7) {
  final_analysis[15,i]=mean(abs((final_analysis[1:12,2]-final_analysis[1:12,i])/
                                  final_analysis[1:12,2]))*100
}
final_analysis[15,1]='MAPE(%)'
View(final_analysis)
