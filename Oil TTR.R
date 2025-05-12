install.packages("TTR")
library(TTR)

# Time Series Data
data <- read.csv("C:/Users/Admin/Downloads/cleaned_oil.csv")
head(data)
tail(data)

period <- 5
length(data$Close)

# TREND INDICATORS

# 1. Simple Moving Average (SMA)
data$SMA <- SMA(data$Close, n = period)

#Accuracy

for (i in 2:length(data$Change))
  if ((data$Change[i]>0 && data$Change[i-1]<0)|| (data$Change[i]<0 && data$Change[i-1]>0)){
    data$Trend_data[i] = 1
  }else{data$Trend_data[i] = 0}
data$Trend_data

df=data.frame(Close=data$Close,Change=data$Change,Change_Indicator=data$Trend_data)
head(df)
tail(df)
df$Change_Indicator[is.na(df$Change_Indicator)]<-0
df$Change_SMA=rep(0,length(data$SMA))
for(i in 2:length(df$Change_SMA))
  df$Change_SMA[i]=(data$SMA[i]-data$SMA[i-1])

df$Change_SMA[is.na(df$Change_SMA)]<-0

for(i in 2:length(df$Change_SMA))
  if((df$Change_SMA[i]>0 && df$Change_SMA[i-1]<0)||(df$Change_SMA[i]<0 && df$Change_SMA[i-1]>0)){
    df$SMA_Indicator[i]=1
    }else{df$SMA_Indicator[i]=0}

df$SMA_Indicator[is.na(df$SMA_Indicator)]<-0

df$SMA_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$SMA_Indicator[i]==1){
    df$SMA_Acc[i] =1
  }
sum(df$SMA_Acc)
sum(df$Change_Indicator)
SMA_Acc_Score=(sum(df$SMA_Acc)/sum(df$Change_Indicator))*100
SMA_Acc_Score

# Plots

plot(data$Close,type='l',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$SMA,type = 'l',col="red")

# 2. Exponential Moving Average (EMA)
data$EMA <- EMA(data$Close, n = period)

df$Change_EMA=rep(0,length(data$EMA))
for(i in 2:length(df$Change_EMA))
  df$Change_EMA[i]=(data$EMA[i]-data$EMA[i-1])

df$Change_EMA[is.na(df$Change_EMA)]<-0

for(i in 2:length(df$Change_EMA))
  if((df$Change_EMA[i]>0 && df$Change_EMA[i-1]<0)||(df$Change_EMA[i]<0 && df$Change_EMA[i-1]>0)){
    df$EMA_Indicator[i]=1
  }else{df$EMA_Indicator[i]=0}

df$EMA_Indicator[is.na(df$EMA_Indicator)]<-0

tail(df)

df$EMA_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$EMA_Indicator[i]==1){
    df$EMA_Acc[i] =1
  }
EMA_Acc_Score=(sum(df$EMA_Acc)/sum(df$Change_Indicator))*100
EMA_Acc_Score

# Plots

plot(data$Close,type='l',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$EMA,type = 'l',col="Blue")

# 4. Weighted Moving Average (WMA)
data$WMA <- WMA(data$Close, n = period)

df$Change_WMA=rep(0,length(data$WMA))
for(i in 2:length(df$Change_WMA))
  df$Change_WMA[i]=(data$WMA[i]-data$WMA[i-1])

df$Change_WMA[is.na(df$Change_WMA)]<-0

for(i in 2:length(df$Change_WMA))
  if((df$Change_WMA[i]>0 && df$Change_WMA[i-1]<0)||(df$Change_WMA[i]<0 && df$Change_WMA[i-1]>0)){
    df$WMA_Indicator[i]=1
  }else{df$WMA_Indicator[i]=0}

df$WMA_Indicator[is.na(df$WMA_Indicator)]<-0

tail(df)

df$WMA_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$WMA_Indicator[i]==1){
    df$WMA_Acc[i] =1
  }
WMA_Acc_Score=(sum(df$WMA_Acc)/sum(df$Change_Indicator))*100
WMA_Acc_Score

# Plots

plot(data$Close,type='l',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$WMA,type = 'l',col="Yellow")

# 47. McGinley Dynamic 
McGinley <- function(price, n = 14) {
  MD <- numeric(length(price))
  MD[1] <- price[1]
  
  for(i in 2:length(price)) {
    MD[i] <- MD[i-1] + (price[i] - MD[i-1])/(n * (price[i]/MD[i-1])^4)
  }
  
  return(MD)
}

data$McGinley <- runMean(data$Close, n = period)

df$Change_McG=rep(0,length(data$McGinley))
for(i in 2:length(df$Change_McG))
  df$Change_McG[i]=(data$McGinley[i]-data$McGinley[i-1])

df$Change_McG[is.na(df$Change_McG)]<-0

for(i in 2:length(df$Change_McG))
  if((df$Change_McG[i]>0 && df$Change_McG[i-1]<0)||(df$Change_McG[i]<0 && df$Change_McG[i-1]>0)){
    df$McG_Indicator[i]=1
  }else{df$McG_Indicator[i]=0}

df$McG_Indicator[is.na(df$McG_Indicator)]<-0

tail(df)

df$McG_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$McG_Indicator[i]==1){
    df$McG_Acc[i] =1
  }
McG_Acc_Score=(sum(df$McG_Acc)/sum(df$Change_Indicator))*100
McG_Acc_Score

# Plots

plot(data$Close,type='l',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$McGinley,type = 'l',col="Black")

# 55. Adaptive Moving Average 
AMA <- function(price, n = 10, fast = 2, slow = 30) {
  # Kaufman's Adaptive Moving Average
  price <- as.numeric(price)
  AMA <- numeric(length(price))
  ER <- numeric(length(price))  # Efficiency Ratio
  
  # Initialize
  AMA[n] <- mean(price[1:n])
  
  # Calculate Fast and Slow smoothing constants
  fast_sc <- 2/(fast + 1)
  slow_sc <- 2/(slow + 1)
  
  for(i in (n+1):length(price)) {
    # Direction
    direction <- abs(price[i] - price[i-n])
    
    # Volatility
    volatility <- sum(abs(price[i-n+1:n] - price[i-n:1]))
    
    # Efficiency Ratio
    ER[i] <- if(volatility != 0) direction/volatility else 0
    
    # Smoothing Constant
    SC <- (ER[i] * (fast_sc - slow_sc) + slow_sc)^2
    
    # Calculate AMA
    AMA[i] <- AMA[i-1] + SC * (price[i] - AMA[i-1])
  }
  
  return(AMA)
}
data$AMA <- AMA(data$Close, n = period)

df$Change_AMA=rep(0,length(data$AMA))
for(i in 2:length(df$Change_AMA))
  df$Change_AMA[i]=(data$AMA[i]-data$AMA[i-1])

df$Change_AMA[is.na(df$Change_AMA)]<-0

for(i in 2:length(df$Change_AMA))
  if((df$Change_AMA[i]>0 && df$Change_AMA[i-1]<0)||(df$Change_AMA[i]<0 && df$Change_AMA[i-1]>0)){
    df$AMA_Indicator[i]=1
  }else{df$AMA_Indicator[i]=0}

df$AMA_Indicator[is.na(df$AMA_Indicator)]<-0

tail(df)

df$AMA_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$AMA_Indicator[i]==1){
    df$AMA_Acc[i] =1
  }
AMA_Acc_Score=(sum(df$AMA_Acc)/sum(df$Change_Indicator))*100
AMA_Acc_Score

# Plots

plot(data$Close,type='l',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$AMA,type = 'l',col="violet")

# 16. Aroon Oscillator
# Function to calculate Aroon Up and Aroon Down
calculate_aroon <- function(high, low, n) {
  aroon_up <- rep(NA, length(high))
  aroon_down <- rep(NA, length(low))
  
  for (i in n:length(high)) {
    highest_high <- max(high[(i-n+1):i])
    lowest_low <- min(low[(i-n+1):i])
    
    aroon_up[i] <- (n - which.max(high[(i-n+1):i]) + 1) / n * 100
    aroon_down[i] <- (n - which.min(low[(i-n+1):i]) + 1) / n * 100
  }
  
  return(list(up = aroon_up, dn = aroon_down))
}

# Calculate Aroon Up and Aroon Down
aroon_result <- calculate_aroon(data$High, data$Low, period)

# Aroon Oscillator
data$AroonOscillator <- aroon_result$up - aroon_result$dn

aroon_result$up[is.na(aroon_result$up)]<-0
aroon_result$dn[is.na(aroon_result$dn)]<-0

#df$Change_Aroon=rep(0,length(aroon_result$up))
#for(i in 2:length(df$Change_Aroon))
 # df$Change_Aroon[i]=(aroon_result$up[i]-aroon_result$dn[i])

for(i in 2:length(aroon_result$up))
  if((aroon_result$up[i]<aroon_result$dn[i] && aroon_result$up[i-1]>aroon_result$dn[i-1])||(aroon_result$up[i]>aroon_result$dn[i] && aroon_result$up[i-1]<aroon_result$dn[i-1])){
    df$Aroon_Indicator[i]=1
  }else{df$Aroon_Indicator[i]=0}

df$Aroon_Indicator[is.na(df$Aroon_Indicator)]<-0

tail(df)

df$Aroon_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$Aroon_Indicator[i]==1){
    df$Aroon_Acc[i] =1
  }
Aroon_Acc_Score=(sum(df$Aroon_Acc)/sum(df$Change_Indicator))*100
Aroon_Acc_Score

# Plots

plot(data$Close[1:100],type='o',col="Green",xlab = "S.No",ylab = "Closing Price")
plot(data$AroonOscillator[1:100],type = 'h',col="black")
plot(aroon_result$up[1:100],type = 'l',col="violet")
lines(aroon_result$dn[1:100],type = 'l',col="red")

# 17. Parabolic SAR
SAR <- function(high, low, acceleration = 0.02, maximum = 0.2) {
  n <- length(high)
  sar <- numeric(n)
  trend <- numeric(n)  # 1 for uptrend, -1 for downtrend
  af <- numeric(n)     # Acceleration Factor
  ep <- numeric(n)     # Extreme Point
  
  # Initialize
  trend[1] <- 1  # Start with uptrend
  sar[1] <- low[1]
  ep[1] <- high[1]
  af[1] <- acceleration
  
  for(i in 2:n) {
    # Update SAR
    sar[i] <- sar[i-1] + af[i-1] * (ep[i-1] - sar[i-1])
    
    # Update trend
    if(trend[i-1] == 1) {  # Previous uptrend
      if(low[i] < sar[i]) {  # Trend reversal to downtrend
        trend[i] <- -1
        sar[i] <- max(high[i-1], high[i])
        ep[i] <- low[i]
        af[i] <- acceleration
      } else {  # Continue uptrend
        trend[i] <- 1
        if(high[i] > ep[i-1]) {  # New high
          ep[i] <- high[i]
          af[i] <- min(af[i-1] + acceleration, maximum)
        } else {
          ep[i] <- ep[i-1]
          af[i] <- af[i-1]
        }
      }
    } else {  # Previous downtrend
      if(high[i] > sar[i]) {  # Trend reversal to uptrend
        trend[i] <- 1
        sar[i] <- min(low[i-1], low[i])
        ep[i] <- high[i]
        af[i] <- acceleration
      } else {  # Continue downtrend
        trend[i] <- -1
        if(low[i] < ep[i-1]) {  # New low
          ep[i] <- low[i]
          af[i] <- min(af[i-1] + acceleration, maximum)
        } else {
          ep[i] <- ep[i-1]
          af[i] <- af[i-1]
        }
      }
    }
  }
  
  return(sar)
}
data$SAR <- SAR(data$High, data$Low)

df$Change_SAR=rep(0,length(data$SAR))
for(i in 1:length(df$Change_SAR))
  df$Change_SAR[i]=(data$SAR[i]-data$Close[i])

df$Change_SAR[is.na(df$Change_SAR)]<-0

for(i in 2:length(df$Change_SAR))
  if((df$Change_SAR[i]>0 && df$Change_SAR[i-1]<0)||(df$Change_SAR[i]<0 && df$Change_SAR[i-1]>0)){
    df$SAR_Indicator[i]=1
  }else{df$SAR_Indicator[i]=0}

df$SAR_Indicator[is.na(df$SAR_Indicator)]<-0

df$SAR_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$SAR_Indicator[i]==1){
    df$SAR_Acc[i] =1
  }
SAR_Acc_Score=(sum(df$SAR_Acc)/sum(df$Change_Indicator))*100
SAR_Acc_Score

# Plots

lines(data$Close[1:75],type='l',col="Green")
plot(data$SAR[1:75],col="red",,xlab = "S.No",ylab = "Closing Price")

#______________________________________________________________________________________________
# MOMENTUM INDICATORS

# 7. Relative Strength Index (RSI)
data$RSI <- RSI(data$Close, n = period)

data$RSI[is.na(data$RSI)]<-0
for(i in 1:length(data$RSI))
  if(data$RSI[i]>70||data$RSI[i]<30){
    df$RSI_Indicator[i]=1
  }else{df$RSI_Indicator[i]=0}

df$RSI_Indicator[is.na(df$RSI_Indicator)]<-0

tail(df)

df$RSI_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$RSI_Indicator[i]==1){
    df$RSI_Acc[i] =1
  }
RSI_Acc_Score=(sum(df$RSI_Acc)/sum(df$Change_Indicator))*100
RSI_Acc_Score

# Plots

plot(data$RSI[1:75],type = 'l',col="red",,xlab = "S.No",ylab = "Closing Price")
lines(data$Close[1:75],type='l',col="Green")
lines(rep(70,length(data$RSI[1:75])),type='l',col="Black")
lines(rep(30,length(data$RSI[1:75])),type='l',col="Black")

# 9. Moving Average Convergence Divergence (MACD)
macd_result <- MACD(data$Close, nFast = 12, nSlow = 26, nSig = 9, wilder = FALSE)

data$MACD <- macd_result[, "macd"]
data$Signal <- macd_result[, "signal"]

data$MACD[is.na(data$MACD)]<-0
data$Signal[is.na(data$Signal)]<-0
for(i in 2:length(data$MACD))
  if((data$MACD[i]>data$Signal[i]&& data$MACD[i-1]<data$Signal[i-1])||(data$MACD[i]<data$Signal[i]&& data$MACD[i-1]>data$Signal[i-1])){
    df$MACD_Indicator[i]=1
  }else if((data$MACD[i]>0&& data$MACD[i-1]<0)||(data$MACD[i]<0&& data$MACD[i-1]>0)){
    df$MACD_Indicator[i]=1
  }else{df$MACD_Indicator[i]=0}

df$MACD_Indicator[is.na(df$MACD_Indicator)]<-0

tail(df)

df$MACD_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$MACD_Indicator[i]==1){
    df$MACD_Acc[i] =1
  }
MACD_Acc_Score=(sum(df$MACD_Acc)/sum(df$Change_Indicator))*100
MACD_Acc_Score

#Plots

plot(data$MACD,type = 'l',col="red",,xlab = "S.No",ylab = "MACD Index")
lines(data$Signal,type='l',col="Black")

# 54. Stochastic Momentum Index
data$SMI <- 100*((data$Close - (runMax(data$High, n = period) + runMin(data$Low, n = period)) / 2) / 
  ((runMax(data$High, n = period) - runMin(data$Low, n = period))) / 2)
data$SMI_SMA <- SMA(data$SMI, n = period)  # Smoothing

#Plots

plot(data$SMI[100:200],type = 'l',col="red",,xlab = "S.No",ylab = "SMI")
lines(data$SMI_SMA[100:200],type='l',col="Black")

# 48. Know Sure Thing (KST)

# 10 period ROC
data$ROC_10=rep(0,length(data$Close))

for(i in 11:length(data$Close))
  data$ROC_10[i]=data$Close[i]-data$Close[i-10]

# 15 period ROC
data$ROC_15=rep(0,length(data$Close))

for(i in 16:length(data$Close))
  data$ROC_15[i]=data$Close[i]-data$Close[i-15]

# 20 period ROC
data$ROC_20=rep(0,length(data$Close))

for(i in 21:length(data$Close))
  data$ROC_20[i]=data$Close[i]-data$Close[i-20]

# 30 period ROC
data$ROC_30=rep(0,length(data$Close))

for(i in 31:length(data$Close))
  data$ROC_30[i]=data$Close[i]-data$Close[i-30]

data$KST <- SMA(data$ROC_10, n = 10) + (2*SMA(data$ROC_15, n = 10))+(3*SMA(data$ROC_20,n=10))+(4*SMA(data$ROC_30,n=15))
data$KST_Signal<- SMA(data$KST,n=9)

# Accuracy

data$KST[is.na(data$KST)]<-0
data$KST_Signal[is.na(data$KST_Signal)]<-0
for(i in 2:length(data$KST))
  if((data$KST[i]>data$KST_Signal[i]&& data$KST[i-1]<data$KST_Signal[i-1])||(data$KST[i]<data$KST_Signal[i]&& data$KST[i-1]>data$KST_Signal[i-1])){
    df$KST_Indicator[i]=1
  }else if((data$KST[i]>0&& data$KST[i-1]<0)||(data$KST[i]<0&& data$KST[i-1]>0)){
    df$KST_Indicator[i]=1
  }else{df$KST_Indicator[i]=0}

df$KST_Indicator[is.na(df$KST_Indicator)]<-0

tail(df)

df$KST_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$KST_Indicator[i]==1){
    df$KST_Acc[i] =1
  }
KST_Acc_Score=(sum(df$KST_Acc)/sum(df$Change_Indicator))*100
KST_Acc_Score

#Plots

plot(data$KST,type = 'l',col="red",,xlab = "S.No",ylab = "KST")
lines(data$KST_Signal,type='l',col="Black")

# 49. Qstick Indicator 
data$Qstick <- SMA(data$Close - data$Open, n = period)

#Accuracy

data$Qstick[is.na(data$Qstick)]<-0

for(i in 2:length(data$Qstick))
  if((data$Qstick[i]>0&& data$Qstick[i-1]<0)||(data$Qstick[i]<0&& data$Qstick[i-1]>0)){
    df$Qstick_Indicator[i]=1
  }else{df$Qstick_Indicator[i]=0}

df$Qstick_Indicator[is.na(df$Qstick_Indicator)]<-0

tail(df)

df$Qstick_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$Qstick_Indicator[i]==1){
    df$Qstick_Acc[i] =1
  }
Qstick_Acc_Score=(sum(df$Qstick_Acc)/sum(df$Change_Indicator))*100
Qstick_Acc_Score

#Plots

plot(data$Qstick,type = 'l',col="red",,xlab = "S.No",ylab = "Qstick")
lines(data$KST_Signal,type='l',col="Black")

# 51. Fisher Transform 
FisherTransform <- function(price, period = 14, scale = 0.5) {
  n <- length(price)
  fisher <- numeric(n)
  fisher_signal <- numeric(n)
  value <- numeric(n)
  
  # Calculate the normalized price value
  for(i in period:n) {
    price_window <- price[(i-period+1):i]
    max_high <- max(price_window)
    min_low <- min(price_window)
    
    if(max_high != min_low) {
      value[i] <- scale * ((price[i] - min_low) / (max_high - min_low) - 0.5)
    } else {
      value[i] <- 0
    }
  }
  
  # Smooth the value
  smooth_value <- SMA(value, 5)
  
  # Calculate Fisher Transform
  for(i in 2:n) {
    if(!is.na(smooth_value[i])) {
      # Bound the value between -1.5 and 1.5 to prevent numerical issues
      bounded_value <- pmax(pmin(smooth_value[i], 0.999), -0.999)
      fisher[i] <- 0.5 * log((1 + bounded_value) / (1 - bounded_value))
    }
  }
  
  # Calculate Fisher Signal (delayed Fisher)
  fisher_signal <- lag(fisher, 1)
  
  return(list(
    fisher = fisher,
    signal = fisher_signal
  ))
}

fisher_results <- FisherTransform(data$Close, period = period)
data$Fisher <- fisher_results$fisher
data$Fisher_Signal <- fisher_results$signal

# Accuracy

data$Fisher[is.na(data$Fisher)]<-0
data$Fisher_Signal[is.na(data$Fisher_Signal)]<-0
for(i in 2:length(data$Fisher))
  if((data$Fisher[i]>0&& data$Fisher[i-1]<0)||(data$Fisher[i]<0&& data$Fisher[i-1]>0)){
    df$Fisher_Indicator[i]=1
  }else{df$Fisher_Indicator[i]=0}

df$Fisher_Indicator[is.na(df$Fisher_Indicator)]<-0

tail(df)

df$Fisher_Acc =rep(0,length(df$Change_Indicator))
for(i in 1:length(df$Change_Indicator))
  if (df$Change_Indicator[i]==1 && df$Fisher_Indicator[i]==1){
    df$Fisher_Acc[i] =1
  }
Fisher_Acc_Score=(sum(df$Fisher_Acc)/sum(df$Change_Indicator))*100
Fisher_Acc_Score

#Plots

plot(data$Fisher,type = 'l',col="red",,xlab = "S.No",ylab = "Fisher")
lines(data$Fisher_Signal,type='l',col="Black")

# 19. Stochastic Oscillator
stoch <- stoch(cbind(data$High, data$Low, data$Close),
               nFastK = period,
               nFastD = 3,
               nSlowD = 3)

data$FastK <- stoch[, "fastK"]
data$FastD <- stoch[, "fastD"]
data$SlowD <- stoch[, "slowD"]

#______________________________________________________________________________________________
# VOLUME INDICATORS 

# 12. On-Balance Volume (OBV) #
data$OBV <- OBV(data$Close, volume = data$Vol.)

# 46. Elder Ray Index
data$Bull_Power <- data$High - EMA(data$Close, n = period)
data$Bear_Power <- data$Low - EMA(data$Close, n = period)

# 21. Chaikin Oscillator 
# First calculate Accumulation/Distribution Line (ADL)
data$ADL <- with(data, {
  clv <- ((Close - Low) - (High - Close)) / (High - Low)
  adl <- cumsum(clv * Vol.)
})
# Then calculate Chaikin Oscillator as difference between fast and slow EMAs of ADL
data$ChaikinOsc <- EMA(data$ADL, n = 3) - EMA(data$ADL, n = 10)

# 30. Accumulation/Distribution Index (ADI) 
data$ADI <- with(data, cumsum(((Close - Low) - (High - Close)) / (High - Low) * Vol.))
#______________________________________________________________________________________________
# VOLATILITY INDICATORS

# 22. Bollinger Bands
bbands <- BBands(data$Close, n = period, sd = 2)
data$BB_up <- bbands[, "up"]
data$BB_mid <- bbands[, "mavg"]
data$BB_down <- bbands[, "dn"]

# 23. Keltner Channels
data$Keltner_mid <- EMA(data$Close, n = period)
data$ATR_keltner <- ATR(cbind(data$High, data$Low, data$Close), n = period)[, "atr"]
data$Keltner_up <- data$Keltner_mid + (2 * data$ATR_keltner)
data$Keltner_down <- data$Keltner_mid - (2 * data$ATR_keltner)

# 18. Commodity Channel Index (CCI)
CCI <- function(high, low, close, n = 20, constant = 0.015) {
  # Calculate typical price
  tp <- (high + low + close) / 3
  
  # Calculate Simple Moving Average of typical price
  sma_tp <- SMA(tp, n)
  
  # Calculate Mean Deviation
  mad <- numeric(length(tp))
  for(i in n:length(tp)) {
    mad[i] <- mean(abs(tp[(i-n+1):i] - sma_tp[i]))
  }
  
  # Calculate CCI
  cci <- (tp - sma_tp) / (constant * mad)
  
  return(cci)
}
data$CCI <- CCI(data$High, data$Low, data$Close, n = 20)

# 11. Donchian Channels
data$DonchianUpper <- sapply(1:nrow(data), function(i) {
  if (i >= period) max(data$High[(i-period+1):i]) else NA
})

data$DonchianLower <- sapply(1:nrow(data), function(i) {
  if (i >= period) min(data$Low[(i-period+1):i]) else NA
})

data$DonchianMiddle <- (data$DonchianUpper + data$DonchianLower) / 2

# 52. Hilbert Transform (Dominant Cycle) 
# Hilbert Transform Implementation for Financial Time Series
Hilbert <- function(price, period = 20) {
  n <- length(price)
  smooth <- numeric(n)
  detrender <- numeric(n)
  I1 <- numeric(n)
  Q1 <- numeric(n)
  jI <- numeric(n)
  jQ <- numeric(n)
  I2 <- numeric(n)
  Q2 <- numeric(n)
  Re <- numeric(n)
  Im <- numeric(n)
  Period <- numeric(n)
  smoothPeriod <- numeric(n)
  DCPeriod <- numeric(n)
  
  # Smoothing and detrending
  for(i in 7:n) {
    # Price smoothing
    smooth[i] <- (4*price[i] + 3*price[i-1] + 2*price[i-2] + price[i-3])/10
    
    # Detrending
    detrender[i] <- (0.0962*smooth[i] + 0.5769*smooth[i-2] - 0.5769*smooth[i-4] - 0.0962*smooth[i-6])*0.075
    
    # Compute InPhase and Quadrature components
    Q1[i] <- (0.0962*detrender[i] + 0.5769*detrender[i-2] - 0.5769*detrender[i-4] - 0.0962*detrender[i-6])*0.075
    I1[i] <- detrender[i-3]
    
    # Advance the phase of I1 and Q1 by 90 degrees
    jI[i] <- (0.0962*I1[i] + 0.5769*I1[i-2] - 0.5769*I1[i-4] - 0.0962*I1[i-6])*0.075
    jQ[i] <- (0.0962*Q1[i] + 0.5769*Q1[i-2] - 0.5769*Q1[i-4] - 0.0962*Q1[i-6])*0.075
    
    # Phasor addition for 3 bar averaging
    I2[i] <- I1[i] - jQ[i]
    Q2[i] <- Q1[i] + jI[i]
    
    # Smooth the I and Q components before applying the discriminator
    I2[i] <- 0.2*I2[i] + 0.8*I2[i-1]
    Q2[i] <- 0.2*Q2[i] + 0.8*Q2[i-1]
    
    # Calculate the dominant cycle
    Re[i] <- I2[i]*I2[i-1] + Q2[i]*Q2[i-1]
    Im[i] <- I2[i]*Q2[i-1] - Q2[i]*I2[i-1]
    
    if(Im[i] != 0 && Re[i] != 0) {
      Period[i] <- 2*pi/atan(Im[i]/Re[i])
    } else {
      Period[i] <- Period[i-1]
    }
    
    # Constrain the period to reasonable values
    if(Period[i] > 1.5*Period[i-1]) Period[i] <- 1.5*Period[i-1]
    if(Period[i] < 0.67*Period[i-1]) Period[i] <- 0.67*Period[i-1]
    if(Period[i] < 6) Period[i] <- 6
    if(Period[i] > 50) Period[i] <- 50
    
    # Smooth the period
    smoothPeriod[i] <- 0.2*Period[i] + 0.8*smoothPeriod[i-1]
    
    DCPeriod[i] <- smoothPeriod[i]
  }
  
  return(DCPeriod)
}

data$Hilbert <- Hilbert(data$Close)

# ---------------------------------------------------------------
# Plots

plot(data$Close,type='o',col="Green",xlab = "S.No",ylab = "Closing Price")
lines(data$SMA,type = 'l',col="Blue")
