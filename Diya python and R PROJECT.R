
# ============================================
# LOAD LIBRARIES
# ============================================

library(readxl)
library(ggplot2)
library(TTR)
library(forecast)

# ============================================
# IMPORT FILE
# ============================================

tvs_data <- read_excel(file.choose())

# ============================================
# DATE CONVERSION
# ============================================

tvs_data$Date <- as.Date(tvs_data$Date)

# ============================================
# VISUAL 1 : PRICE TREND
# ============================================

p1 <- ggplot(
  tvs_data,
  aes(Date, Price)
)+
  
  geom_line(
    color="blue"
  )+
  
  ggtitle(
    "TVS Stock Price Trend"
  )

p1


# ============================================
# VISUAL 2 : OPEN vs PRICE
# ============================================

p2 <- ggplot(
  tvs_data,
  aes(Open, Price)
)+
  
  geom_point(
    color="red"
  )+
  
  ggtitle(
    "Open vs Closing Price"
  )

p2


# ============================================
# VISUAL 3 : HIGH vs LOW
# ============================================

p3 <- ggplot(
  tvs_data,
  aes(Date)
)+
  
  geom_line(
    aes(
      y=High,
      color="High"
    )
  )+
  
  geom_line(
    aes(
      y=Low,
      color="Low"
    )
  )+
  
  ggtitle(
    "High vs Low Price"
  )

p3


# ============================================
# VISUAL 4 : MOVING AVERAGE
# ============================================

tvs_data$MA20 <-
  SMA(
    tvs_data$Price,
    20
  )

p4 <- ggplot(
  tvs_data,
  aes(Date, Price)
)+
  
  geom_line(
    color="black"
  )+
  
  geom_line(
    aes(y=MA20),
    color="red"
  )+
  
  ggtitle(
    "20 Day Moving Average"
  )

p4


# ============================================
# VISUAL 5 : TREND ANALYSIS
# ============================================

p5 <- ggplot(
  tvs_data,
  aes(Date, Price)
)+
  
  geom_line(
    color="darkgreen"
  )+
  
  geom_smooth(
    method="lm",
    color="red"
  )+
  
  ggtitle(
    "Trend Analysis"
  )

p5


# ============================================
# TIME SERIES
# ============================================

price_ts <- ts(
  tvs_data$Price,
  frequency=30
)

plot(
  price_ts,
  main="Time Series Plot"
)


# ============================================
# SEASONAL DECOMPOSITION
# ============================================

decomp <- decompose(
  price_ts
)

plot(
  decomp
)


# ============================================
# ARIMA MODEL
# ============================================

arima_model <-
  auto.arima(
    price_ts
  )

summary(
  arima_model
)


# ============================================
# FORECAST
# ============================================

forecast_values <-
  forecast(
    arima_model,
    h=30
  )

plot(
  forecast_values
)


# ============================================
# ALGORITHMIC TRADING
# ============================================

tvs_data$SMA20 <-
  SMA(
    tvs_data$Price,
    20
  )

tvs_data$SMA50 <-
  SMA(
    tvs_data$Price,
    50
  )

plot(
  tvs_data$Date,
  
  tvs_data$Price,
  
  type="l",
  
  col="black",
  
  main="Algorithmic Trading",
  
  xlab="Date",
  
  ylab="Price"
)

lines(
  tvs_data$Date,
  
  tvs_data$SMA20,
  
  col="blue",
  
  lwd=2
)

lines(
  tvs_data$Date,
  
  tvs_data$SMA50,
  
  col="red",
  
  lwd=2
)

legend(
  "topleft",
  
  legend=c(
    "Price",
    "SMA20",
    "SMA50"
  ),
  
  col=c(
    "black",
    "blue",
    "red"
  ),
  
  lty=1
)


























p1
p2
p3
p4
p5

price_ts <- ts(
  tvs_data$Price,
  frequency=30
)

plot(
  price_ts,
  main="Time Series Plot"
)


decomp <- decompose(
  price_ts
)

plot(
  decomp
)




arima_model <-
  auto.arima(
    price_ts
  )

summary(
  arima_model
)


# ============================================
# FORECAST
# ============================================

forecast_values <-
  forecast(
    arima_model,
    h=30
  )

plot(
  forecast_values
)





tvs_data$SMA20 <-
  SMA(
    tvs_data$Price,
    20
  )

tvs_data$SMA50 <-
  SMA(
    tvs_data$Price,
    50
  )

plot(
  tvs_data$Date,
  
  tvs_data$Price,
  
  type="l",
  
  col="black",
  
  main="Algorithmic Trading",
  
  xlab="Date",
  
  ylab="Price"
)

lines(
  tvs_data$Date,
  
  tvs_data$SMA20,
  
  col="blue",
  
  lwd=2
)

lines(
  tvs_data$Date,
  
  tvs_data$SMA50,
  
  col="red",
  
  lwd=2
)

legend(
  "topleft",
  
  legend=c(
    "Price",
    "SMA20",
    "SMA50"
  ),
  
  col=c(
    "black",
    "blue",
    "red"
  ),
  
  lty=1
)