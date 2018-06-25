rm(list=ls())

#-------------------initial parameters--------------------
long_K_para<-0.01#長K實體棒下限定義
short_k_para<-0.005 #短k實體棒上限定義
Vol_multi_para<-1.5 #均量倍數定義
max_K_para<-40      #KD值的K值上限
ma_5day_para<-5      #Price MA的天數定義
Vol_ma_5day_para<-5      #Vol MA的天數定義
ma_10day_para<-10
Vol_ma_10day_para<-10 
ma_20day_para<-20
Vol_ma_20day_para<-20 
transaction_cost<-0.005 # 0.003+0.001425*2*0.3+0.001 # Transaction cost
total=0              #結果數量

min_Vol_mul<-1.2
BBand_volmul<-2
BBand_MA_day<-10
MACD_abs_diff<-0
#-------------------initial parameters--------------------

if (!require(quantmod)) {install.packages('quantmod'); require(quantmod)} 
if (!require(tidyverse)) {install.packages('tidyverse'); require(tidyverse)} 
sp<-read.table(file="~/Final/final7.txt",sep="\t",header=T,stringsAsFactors = F)
colnames(sp)<-c("Code","Name","Date","Open","High","Low","Close","Volume","Trading_Value")
trading_table<-data.frame() #整合買賣交易表
sp$Open<-as.numeric(sp$Open)
sp$High<-as.numeric(sp$High)
sp$Low<-as.numeric(sp$Low)
sp$Close<-as.numeric(sp$Close)
sp$Date<-as.numeric(sp$Date)
sp$Volume<-as.numeric(sp$Volume)
sp$Name<-as.character(sp$Name)
symbol_name<-unique(sp$Name)
symbol_name_list<-data.frame(symbol_name)


code_final<-unique(sp$Code)
for (code_i in code_final)
{
  try({
    stock_now<-filter(sp,Code==code_i)%>%arrange(Date)
    #View(stock_now)
    date<-as.Date(as.character(stock_now$Date),format="%Y%m%d")
    stock_now_xts<-xts::xts(stock_now[4:9],order.by = date)
    x<-stock_now_xts
    stock<-na.omit(x)          #去除有缺值的資料 
    if (nrow(stock)<=200) {next}
    
    stock_data<-stock[which(stock[,5]>0),]
    stock_Close<-stock_data[,4]
    #---------BBAND--------------------
    maType1<-"EMA"
    HLCx<-stock_data[,2:4]
    BBand_x<-BBands(HLCx, n = BBand_MA_day, maType=maType1, sd = 2)  
    stock_data$up<-as.numeric(BBand_x[,3]) #up The upper Bollinger Band
    stock_data$down<-as.numeric(BBand_x[,1])
    #stock_data$signal_BBandup<-as.numeric((stock_data[,4]>=stock_data$up))
    #---------BBAND--------------------
    
    #---------MACD--------------------
    macd <- MACD(stock_data[,4], 12, 26, 9, maType="EMA" )
    stock_data$DIF<-macd$macd
    stock_data$MACD<-macd$signal #或稱DEM
    stock_data$OSC<-stock_data$DIF-stock_data$MACD #柱狀圖
    #stock_data$signal_MACD<-as.numeric((abs(stock_data$DIF)<=MACD_abs_diff)&(abs(stock_data$MACD)<=MACD_abs_diff)&(abs(stock_data$OSC)<=MACD_abs_diff))
    stock_data$signal_MACD<-as.numeric(((stock_data$OSC)>MACD_abs_diff))
    #---------MACD--------------------
    #-----------MA---------------    
    stock_data$MA5<-EMA(stock_Close,5) #引用函數，輸入資料datax(vector)與天數n
    stock_data$MA10<-EMA(stock_Close,10) #引用函數，輸入資料datax(vector)與天數n
    stock_data$MA20<-EMA(stock_Close,20) #引用函數，輸入資料datax(vector)與天數n
    stock_data$VolMA5<-EMA(stock_data$Volume,5) #引用函數，輸入資料datax(vector)與天數n
    #-----------MA----------------
  
    
    #------------KD---------------
    kd<-stoch(HLCx, nFastK = 14, nFastD = 3, nSlowD = 3, maType="EMA")
    stock_data$K<-kd$fastD*100
    K_num<-as.numeric(stock_data$K)
    stock_data$D<-kd$slowD*100
    D_num<-as.numeric(stock_data$D)
    stock_data$numx<-1:nrow(stock_data)
    trading_table<-data.frame()
    Buy_signal<-0
    Sell_signal<-0
    
    First_signal_Ksmall20<-0
    #----------KD--------------------
    #---------進場條件----------------
    #昨天是長度>0.01的黑K
    stock_data$black_K<-(lag(stock_data$Open,1)-lag(stock_data$Close,1))/lag(stock_data$Close,2)
    stock_data$black_K_BL<-(stock_data$black_K>0.01)
    #前天是長度>0.01的黑K
    stock_data$black_K2<-((lag(stock_data$Open,2)-lag(stock_data$Close,2))/lag(stock_data$Close,3)>0.01)
    stock_data$black_K2_BL<-(stock_data$black_K2>0.01)
    #昨天開盤與前天收盤有段跳空
    stock_data$jump<- as.numeric((lag(stock_data$Open,1)-lag(stock_data$Close,2))>0.2*(lag(stock_data$Open,2)-lag(stock_data$Close,2)))
    #Bband
    stock_data$bband1<- as.numeric(lag(stock_data$Close,1)<1.4*lag(stock_data$down,1))
    #MACD
    stock_data$DIF_MACD<-as.numeric(stock_data$DIF>stock_data$MACD)
    
    #---------出場條件------------
    stock_data$signal_MA5up<-as.numeric((stock_data$MA5-lag(stock_data$MA5,1))>0)
    stock_data$signal_MA10up<-as.numeric((stock_data$MA10-lag(stock_data$MA10,1))>0)
  })
  #stock_data<-na.omit(stock_data)
  
  
  buy_temp<-data.frame() #暫存的買賣表，一旦出場就要清掉
  stock_data$day_num<-1:nrow(stock_data)
  #------num-----
  black_K_BL_num<-as.numeric(stock_data$black_K_BL)
  black_K2_BL_num<-as.numeric(stock_data$black_K2_BL)
  jump_num<-as.numeric(stock_data$jump)
  bband1_num<-as.numeric(stock_data$bband1)
  DIF_MACD_num<-as.numeric(stock_data$DIF_MACD)
  signal_MA5up_num<-as.numeric(stock_data$signal_MA5up)
  signal_MA10up_num<-as.numeric(stock_data$signal_MA10up)
  
  x2<-rbind(black_K_BL_num,black_K2_BL_num,bband1_num)
  x2
  for (day_i in (61:(nrow(stock_data)))){
    x2<-rbind(black_K_BL_num,black_K2_BL_num,bband1_num)
    if((black_K_BL_num[day_i]==1)&(black_K2_BL_num[day_i]==1)&(bband1_num[day_i]==1)&(jump_num[day_i]==1))
    {
      #進場
      buy_temp1<-data.frame(Buy_date=index(stock_data)[day_i],Buy_date_num=stock_data$day_num[day_i],Buy_price=stock_data$Close[day_i],Sell_Date=NA,Sell_date_num=NA,Sell_price=NA,Gain=NA)
      colnames(buy_temp1)<-c("Buy_date","Buy_date_num","Buy_price","Sell_Date","Sell_date_num","Sell_price","Gain")
      buy_temp<-bind_rows(buy_temp,buy_temp1)
    }
    
    if (((signal_MA10up_num[day_i]==0)|(day_i==nrow(stock_data)))&(nrow(buy_temp)>0)&((K_num[day_i]<D_num[day_i]) &(K_num[day_i]>70)))
    { #出場條件
      #break
      buy_temp$Sell_Date<-index(stock_data)[day_i]
      buy_temp$Sell_date_num<-as.numeric(stock_data$day_num[day_i])
      buy_temp$Sell_price<-as.numeric(stock_data$Close[day_i])
      buy_temp$Gain<-( buy_temp$Sell_price-buy_temp$Buy_price)/buy_temp$Buy_price-transaction_cost
      trading_table<-bind_rows(trading_table,buy_temp)
      buy_temp<-data.frame() #MA5下彎全出，暫存的買賣表變空
      
    }
   
    
  }
}  
View(stock_data)
trading_table$Holding_day<-trading_table$Sell_date_num-trading_table$Buy_date_num+1
print(mean(trading_table$Gain>0))
print(data.frame(avg_ret=mean(trading_table$Gain),annual_ret=mean(trading_table$Gain)*240/mean(trading_table$Holding_day),holding_days=mean(trading_table$Holding_day),count_n=nrow(trading_table))  )

chartSeries(stock_data,TA=c(addVo(),addBBands(),addMACD( histogram = TRUE)),subset='2006-10-15::2006-11',theme=chartTheme('white'))
matplot(1:50,cbind(as.numeric(stock_data[,4]),MA5,MA10,MA20)[(length(MA5)-49):length(MA5),],type="l",lwd=c(1,2,3,4), lty=c(1,2,3,4), col=c("black","blue","red","green"), xlab="days",ylab="Price")
legend("bottomright", c("price", "MA5", "MA10","MA20"), pch = 17, title = "topright, inset = .02",lwd=c(1,2,3,4), lty=c(1,2,3,4),inset = .02,bty='n', col=c('black','blue','red','green'))

