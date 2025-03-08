library("openxlsx")
library("parallel")
library("rmgarch")
library('lubridate')
library(ggplot2)
library(xts)
library(tidyr)
library(stargazer)
#library(tidyverse)
#install.packages('rmgarch')
#install.packages('GSVA')
#install.packages('Matrix')
################################################
df<-read.xlsx('6sig-Q2.xlsx',detectDates = TRUE)
str(df)

df_new <- subset(df, select = c(Dates,AAPL_Ret,MSFT_Ret,AMZN_Ret,GOOG_Ret,META_Ret,
                                TSLA_Ret,NVDA_Ret,SPX_Ret))
colnames(df_new) <- gsub("_Ret", "", colnames(df_new))
DATE=df_new$Dates;Y = df_new[,2:9]
k = ncol(Y);NAMES = colnames(Y)
Y = na.omit(diff(as.matrix(Y)))
date = DATE[-1];t = nrow(Y)

#Estimate DCC(1,1)-ARMA(1,1)-GARCH(1,1)
garch11.spec = ugarchspec(mean.model=list(armaOrder=c(1,1),include.mean=TRUE), 
                          variance.model=list(garchOrder=c(1,1),model="sGARCH"))
dcc.garch11.spec = dccspec(uspec=multispec(replicate(k, garch11.spec)),
                           dccOrder=c(1,1), distribution="mvt") #distribution.model="t"
dcc.fit = dccfit(dcc.garch11.spec, data=Y)
dcc.test = DCCtest(Y, garchOrder=c(1,1), solver="solnp", n.lags=2)
# plot(dcc.fit)
################################################
library(ggplot2)
library(reshape2)

# extract DDC Correlation Matrix (dimension: k × k × t)
dcc_cor <- rcor(dcc.fit, type="R")

#set SPX in the central
spx_index <- which(NAMES == "SPX")

#set as time series
dates <- as.Date(date) 

# create a dataframe and # add corr into data frame
dcc_data <- data.frame(Date = dates)
for (i in 1:k) {
  if (i != spx_index) {  # exclude SPX itself
    dcc_data[[NAMES[i]]] <- dcc_cor[i, spx_index, ]
  }
}

# to long format
dcc_long <- melt(dcc_data, id.vars = "Date", variable.name = "Stock", value.name = "Correlation")

# plot
ggplot(dcc_long, aes(x = Date, y = Correlation, color = Stock)) +
  geom_line(size=0.75) +
  facet_wrap(~ Stock, ncol = 4) +  # each line 4
  labs(title = "Dynamic Conditional Correlation: SPX & Individual Stocks",
       x = "Date", y = "Correlation") +
  theme_minimal() +
  theme(legend.position = "none")  # hide legend
# ggsave("dcc_correlation_plot.png", width = 12, height = 4, dpi = 200)

################################################

dcc_long <- data.frame()
for (i in 1:k) {
  for (j in 1:k) {
    temp <- data.frame(
      Date = dates,
      Var1 = NAMES[i],
      Var2 = NAMES[j],
      Correlation = dcc_cor[i, j, ]
    )
    dcc_long <- rbind(dcc_long, temp)
  }
}

head(dcc_long)

ggplot(dcc_long, aes(x = Date, y = Correlation, color = Var2)) +
  geom_line() +
  facet_grid(Var1 ~ Var2, scales = "free_y") +
  labs(title = "Dynamic Conditional Correlations",
       x = "Date", y = "Correlation") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")
################################################


