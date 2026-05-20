library(ggplot2)
library(TTR)
library(forecast)

# Convert Date

indoco_data$Date <- as.Date(indoco_data$Date)

# GRAPH 1

p1 <- ggplot(
  indoco_data,
  aes(
    Date,
    INDOCO.NS.Close
  )
)+
  geom_line(
    color="blue"
  )+
  ggtitle(
    "Closing Price Trend"
  )

# GRAPH 2

p2 <- ggplot(
  indoco_data,
  aes(
    Date,
    INDOCO.NS.Volume
  )
)+
  geom_bar(
    stat="identity",
    fill="darkgreen"
  )+
  ggtitle(
    "Trading Volume"
  )

# GRAPH 3

p3 <- ggplot(
  indoco_data,
  aes(Date)
)+
  geom_line(
    aes(
      y=INDOCO.NS.High,
      color="High"
    )
  )+
  geom_line(
    aes(
      y=INDOCO.NS.Low,
      color="Low"
    )
  )+
  ggtitle(
    "High vs Low Price"
  )

# GRAPH 4

p4 <- ggplot(
  indoco_data,
  aes(
    INDOCO.NS.Open,
    INDOCO.NS.Close
  )
)+
  geom_point(
    color="red"
  )+
  ggtitle(
    "Open vs Close"
  )

# Moving Average

ma20 <- SMA(
  indoco_data$INDOCO.NS.Close,
  20
)

p5 <- ggplot(
  indoco_data,
  aes(
    Date,
    INDOCO.NS.Close
  )
)+
  geom_line(
    color="black"
  )+
  geom_line(
    aes(
      y=ma20
    ),
    color="red"
  )+
  ggtitle(
    "Moving Average"
  )

# Trend

p6 <- ggplot(
  indoco_data,
  aes(
    Date,
    INDOCO.NS.Close
  )
)+
  geom_line(
    color="blue"
  )+
  geom_smooth(
    method="lm",
    color="red"
  )+
  ggtitle(
    "Trend Analysis"
  )

p1
p2
p3
p4
p5
p6