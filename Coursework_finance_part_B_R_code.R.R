


##### Group Coursework - Part B #######

### Victoire Burg, Beatrice Matteo, Paul Couturier and Robin Mathelier ###

library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyquant)
library(zoo) ## for data processing
library(tidyverse) ## for data processing
library(jsonlite)
library(moments)
library(stabledist)
library(fBasics)
library(MASS)
library(VaRES)
library(e1071)
library(xtable)
library(gridExtra)
library(GLDEX)

my_theme <- theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14))

setwd("C:/Users/robin/Dropbox/Applications/Overleaf/Coursework finance part B")

##### read CAC40 #####

CAC40 = read.csv(file = "CAC40.csv")
CAC40$Date = as.Date(CAC40$Date)
CAC40$Close = as.numeric(CAC40$Close)

CAC40 = na.omit(CAC40)

Close_prices = CAC40$Close
CAC40 = CAC40[-1,]
CAC40$return = log(Close_prices[-length(Close_prices)]) - log(Close_prices[-1])
CAC40$return_100 = CAC40$return * 100

##### read bitcoin #####

BITCOIN_JSON <- fromJSON("https://api.coindesk.com/v1/bpi/historical/close.json?start=2011-01-01&end=2020-12-31")

dates <- as.Date(names(BITCOIN_JSON[[1]]))
number_days <- length(dates)
list_days_elapsed <- 1:(number_days-1)
list_bpi <<- c()
sapply(1:(number_days), function(i){list_bpi[i] <<- BITCOIN_JSON$bpi[[i]]})
return <- sapply(1:(number_days-1), function(i){log(list_bpi[i+1])-log(list_bpi[i])})

BITCOIN_TO_PLOT <- data.frame(Date = dates, value = list_bpi)
BITCOIN <- data.frame(number_days_elapsed = (1:(number_days-1)) ,Date = dates[-1], return = return)
BITCOIN$return_100 = BITCOIN$return * 100

##### read Ali Baba ####

ALIBABA <- tq_get("BABA", get = "stock.prices", from ="1990-01-01")

ALIBABA = na.omit(ALIBABA)
Close_prices = ALIBABA$close
ALIBABA = ALIBABA[-1,]
ALIBABA$return = log(Close_prices[-length(Close_prices)]) - log(Close_prices[-1])
ALIBABA$return_100 = ALIBABA$return * 100

##### plot time series ######

png(filename = "plot_time_series.png", width = 1000)

g_BITCOIN = ggplot(data = BITCOIN_TO_PLOT, aes(x = Date, y = value))
g_BITCOIN = g_BITCOIN + geom_line() + my_theme
g_BITCOIN <- g_BITCOIN + labs(x = "Date", y = "Value ($)", title = "BITCOIN value since 2011")
g_BITCOIN

g_CAC40 = ggplot(data = CAC40, aes(x = Date, y = Close))
g_CAC40 = g_CAC40 + geom_line() + my_theme
g_CAC40 = g_CAC40 + labs(x = "Date", y = "Closing price (???)", title = "CAC40 closing price since 2002")
g_CAC40

g_ALIBABA = ggplot(data = ALIBABA, aes(x = date, y = close))
g_ALIBABA = g_ALIBABA + geom_line() + my_theme
g_ALIBABA = g_ALIBABA + labs(x = "Date", y = "Closing price ($)", title = "ALIBABA closing price since 2015")
g_ALIBABA

grid.arrange(g_ALIBABA,g_CAC40,g_BITCOIN,ncol = 3)

dev.off()

##### Question (i) - plot log returns #####

png(filename = "plot_log_returns.png", width = 1000)

g_BITCOIN <- ggplot(data=BITCOIN)
g_BITCOIN <- g_BITCOIN + geom_line(aes(x= Date, y=return_100),size=0.5) 
g_BITCOIN <- g_BITCOIN + labs(x = "Date",y = "Log returns (%)",title = "BITCOIN log returns since 2011")
g_BITCOIN <- g_BITCOIN + my_theme

g_CAC40 <- ggplot(data=CAC40)
g_CAC40 <- g_CAC40 + geom_line(aes(x= Date, y=return_100),size=0.5) 
g_CAC40 <- g_CAC40 + labs(x = "Date",y = "Log returns (%)",title = "CAC40 log returns since 2002")
g_CAC40 <- g_CAC40 + my_theme

g_ALIBABA <- ggplot(data=ALIBABA)
g_ALIBABA <- g_ALIBABA + geom_line(aes(x= date, y=return_100),size=0.5) 
g_ALIBABA <- g_ALIBABA + labs(x = "Date",y = "Log returns (%)",title = "ALIBABA log returns since 2015")
g_ALIBABA <- g_ALIBABA + my_theme

grid.arrange(g_ALIBABA,g_CAC40,g_BITCOIN,ncol = 3)

dev.off()

############ SF 1 #############

mu_ALIBABA = mean(ALIBABA$return_100)
sigma_ALIBABA = sd(ALIBABA$return_100)
ALIBABA$normal = dnorm(ALIBABA$return_100, mean = mu_ALIBABA, sd = sigma_ALIBABA)

mu_CAC40 = mean(CAC40$return_100)
sigma_CAC40 = sd(CAC40$return_100)
CAC40$normal = dnorm(CAC40$return_100, mean = mu_CAC40, sd = sigma_CAC40)

mu_BITCOIN = mean(BITCOIN$return_100)
sigma_BITCOIN = sd(BITCOIN$return_100)
BITCOIN$normal = dnorm(BITCOIN$return_100, mean = mu_BITCOIN, sd = sigma_BITCOIN)

##### plot with normal fitted ######

png(filename = "log_returns_normal.png", width = 1000)

g_CAC40 = ggplot(data = CAC40)
g_CAC40 = g_CAC40 + geom_histogram(aes(x = return_100, y = ..density..), bins = 100, alpha = 0.3, fill = "blue", color = 'lightblue')
g_CAC40 = g_CAC40 + geom_density(aes(x = return_100, color = "Kernel density"), alpha = 0.8, lwd = 0.8)
g_CAC40 = g_CAC40 + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8)
g_CAC40 = g_CAC40 + my_theme
g_CAC40 = g_CAC40 + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Normal' = 'red'))
g_CAC40 = g_CAC40 + labs(x = "log returns (%)", title = "CAC40 log returns")

g_ALIBABA = ggplot(data = ALIBABA)
g_ALIBABA = g_ALIBABA + geom_histogram(aes(x = return_100, y = ..density..), bins = 100, alpha = 0.3, fill = "blue", color = 'lightblue')
g_ALIBABA = g_ALIBABA + geom_density(aes(x = return_100, color = "Kernel density"), alpha = 0.8, lwd = 0.8)
g_ALIBABA = g_ALIBABA + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8)
g_ALIBABA = g_ALIBABA + my_theme
g_ALIBABA = g_ALIBABA + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Normal' = 'red'))
g_ALIBABA = g_ALIBABA + labs(x = "log returns (%)", title = "ALIBABA log returns")

g_BITCOIN = ggplot(data = BITCOIN)
g_BITCOIN = g_BITCOIN + geom_histogram(aes(x = return_100, y = ..density..), bins = 100, alpha = 0.3, fill = "blue", color = 'lightblue')
g_BITCOIN = g_BITCOIN + geom_density(aes(x = return_100, color = "Kernel density"), alpha = 0.8, lwd = 0.8)
g_BITCOIN = g_BITCOIN + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8)
g_BITCOIN = g_BITCOIN + my_theme
g_BITCOIN = g_BITCOIN + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Normal' = 'red'))
g_BITCOIN = g_BITCOIN + labs(x = "log returns (%)", title = "BITCOIN log returns")

ggarrange(g_ALIBABA, g_CAC40, g_BITCOIN, common.legend = TRUE, ncol = 3, legend = "bottom")

dev.off()

##### qqplot #####

png(filename = "qqplots.png", width = 1000)

g_CAC40 = ggplot(data = CAC40)
g_CAC40 = g_CAC40 + stat_qq(aes(sample = scale(return)), color = 'blue') + stat_qq_line(aes(sample = scale(return)), color = 'red')
g_CAC40 = g_CAC40 + my_theme
g_CAC40 = g_CAC40 + labs(y = "standardized residuals", x = "N(0,1) quantile", title = "qqplot of CAC40 returns")

g_ALIBABA = ggplot(data = ALIBABA)
g_ALIBABA = g_ALIBABA + stat_qq(aes(sample = scale(return)), color = 'blue') + stat_qq_line(aes(sample = scale(return)), color = 'red')
g_ALIBABA = g_ALIBABA + my_theme
g_ALIBABA = g_ALIBABA + labs(y = "standardized residuals", x = "N(0,1) quantile", title = "qqplot of ALIBABA returns")

g_BITCOIN = ggplot(data = BITCOIN)
g_BITCOIN = g_BITCOIN + stat_qq(aes(sample = scale(return)), color = 'blue') + stat_qq_line(aes(sample = scale(return)), color = 'red')
g_BITCOIN = g_BITCOIN + my_theme
g_BITCOIN = g_BITCOIN + labs(y = "standardized residuals", x = "N(0,1) quantile", title = "qqplot of BITCOIN returns")

grid.arrange(g_ALIBABA, g_CAC40, g_BITCOIN, ncol = 3)

dev.off()

##### kurtosis and skewness ######


k_CAC40 = kurtosis(CAC40$return_100, method = "moment")
s_CAC40 = skewness(CAC40$return_100)
k_CAC40_normal = kurtosis(CAC40$normal, method = "moment")
s_CAC40_normal = skewness(CAC40$normal)

k_ALIBABA = kurtosis(ALIBABA$return_100, method = "moment")
s_ALIBABA = skewness(ALIBABA$return_100)
k_ALIBABA_normal = kurtosis(ALIBABA$normal, method = "moment")
s_ALIBABA_normal = skewness(ALIBABA$normal)

k_BITCOIN = kurtosis(BITCOIN$return_100, method = "moment")
s_BITCOIN = skewness(BITCOIN$return_100)
k_BITCOIN_normal = kurtosis(BITCOIN$normal, method = "moment")
s_BITCOIN_normal = skewness(BITCOIN$normal)

k = c(k_CAC40, k_ALIBABA, k_BITCOIN)
k_norm = c(k_CAC40_normal, k_ALIBABA_normal, k_BITCOIN_normal)

s = c(s_CAC40, s_ALIBABA, s_BITCOIN)
s_norm = c(s_CAC40_normal, s_ALIBABA_normal, s_BITCOIN_normal)

xtable(data.frame(k, k_norm, row.names = c("CAC40","ALIBABA","BITCOIN")))
xtable(data.frame(s, s_norm, row.names = c("CAC40","ALIBABA","BITCOIN")))

############ SF 2 #############

png(filename = "ACF_plots_SF2.png", width = 1000)

acf_CAC40 = acf(CAC40$return, plot = FALSE, lag = 50)$acf
df_acf_CAC40 = data.frame(abs = 0:(length(acf_CAC40)-1), acf_CAC40)
g_CAC40 = ggplot(data = df_acf_CAC40)
g_CAC40 = g_CAC40 + geom_bar(aes(x = abs, y = acf_CAC40), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_CAC40 = g_CAC40 + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_CAC40 = g_CAC40 + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_CAC40 = g_CAC40 + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for CAC40 returns")

acf_ALIBABA = acf(ALIBABA$return, plot = FALSE, lag = 50)$acf
df_acf_ALIBABA = data.frame(abs = 0:(length(acf_ALIBABA)-1), acf_ALIBABA)
g_ALIBABA = ggplot(data = df_acf_ALIBABA)
g_ALIBABA = g_ALIBABA + geom_bar(aes(x = abs, y = acf_ALIBABA), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_ALIBABA = g_ALIBABA + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_ALIBABA = g_ALIBABA + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_ALIBABA = g_ALIBABA + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for ALIBABA returns")

acf_BITCOIN = acf(BITCOIN$return, plot = FALSE, lag = 50)$acf
df_acf_BITCOIN = data.frame(abs = 0:(length(acf_BITCOIN)-1), acf_BITCOIN)
g_BITCOIN = ggplot(data = df_acf_BITCOIN)
g_BITCOIN = g_BITCOIN + geom_bar(aes(x = abs, y = acf_BITCOIN), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_BITCOIN = g_BITCOIN + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_BITCOIN = g_BITCOIN + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_BITCOIN = g_BITCOIN + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for BITCOIN returns")

grid.arrange(g_ALIBABA, g_CAC40, g_BITCOIN, ncol = 3)

dev.off()

########### SF3 ############


png(filename = "ACF_plots_SF3.png", width = 1000)

acf_CAC40 = acf(abs(CAC40$return), plot = FALSE, lag = 50)$acf
df_acf_CAC40 = data.frame(abs = 0:(length(acf_CAC40)-1), acf_CAC40)
g_CAC40 = ggplot(data = df_acf_CAC40)
g_CAC40 = g_CAC40 + geom_bar(aes(x = abs, y = acf_CAC40), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_CAC40 = g_CAC40 + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_CAC40 = g_CAC40 + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_CAC40 = g_CAC40 + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for CAC40 returns")

acf_ALIBABA = acf(abs(ALIBABA$return), plot = FALSE, lag = 50)$acf
df_acf_ALIBABA = data.frame(abs = 0:(length(acf_ALIBABA)-1), acf_ALIBABA)
g_ALIBABA = ggplot(data = df_acf_ALIBABA)
g_ALIBABA = g_ALIBABA + geom_bar(aes(x = abs, y = acf_ALIBABA), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_ALIBABA = g_ALIBABA + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_ALIBABA = g_ALIBABA + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_ALIBABA = g_ALIBABA + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for ALIBABA returns")

acf_BITCOIN = acf(abs(BITCOIN$return), plot = FALSE, lag = 50)$acf
df_acf_BITCOIN = data.frame(abs = 0:(length(acf_BITCOIN)-1), acf_BITCOIN)
g_BITCOIN = ggplot(data = df_acf_BITCOIN)
g_BITCOIN = g_BITCOIN + geom_bar(aes(x = abs, y = acf_BITCOIN), stat = 'identity', color = 'blue', fill = 'blue', alpha = 0.5)
g_BITCOIN = g_BITCOIN + geom_hline(yintercept = 0.025, color = 'red', lty = 2)
g_BITCOIN = g_BITCOIN + geom_hline(yintercept = -0.025, color = 'red', lty = 2)
g_BITCOIN = g_BITCOIN + my_theme + labs(x = "Lag", y = "Autocorrelation", title = "ACF plot for BITCOIN returns")

grid.arrange(g_ALIBABA, g_CAC40, g_BITCOIN, ncol = 3)

dev.off()


############# part (iii) ##############


######### Student ############


rt_CAC40 = fitdistr(CAC40$return_100, dt, start = list(df = 1, ncp = 0))
df_hat_CAC40 = rt_CAC40$estimate[[1]]
ncp_hat_CAC40 = rt_CAC40$estimate[[2]]
CAC40$t = dt(CAC40$return_100, df = df_hat_CAC40, ncp = ncp_hat_CAC40)

rt_BITCOIN = fitdistr(BITCOIN$return_100, dt, start = list(df = 1, ncp = 0))
df_hat_BITCOIN = rt_BITCOIN$estimate[[1]]
ncp_hat_BITCOIN = rt_BITCOIN$estimate[[2]]
BITCOIN$t = dt(BITCOIN$return_100, df = df_hat_BITCOIN, ncp = ncp_hat_BITCOIN)

rt_ALIBABA = fitdistr(ALIBABA$return_100, dt, start = list(df = 1, ncp = 0))
df_hat_ALIBABA = rt_ALIBABA$estimate[[1]]
ncp_hat_ALIBABA = rt_ALIBABA$estimate[[2]]
ALIBABA$t = dt(ALIBABA$return_100, df = df_hat_ALIBABA, ncp = ncp_hat_ALIBABA)

######### Stable ##########

f <- function(u,a,b,c,d) {
  cat(a,b,c,d,"\n")  # Some logging (it is very slow)
  dstable(CAC40$return_100, 2*exp(a)/(1+exp(a)), 2*exp(b)/(1+exp(b))-1, exp(c), d)
}

# r_stable_CAC40 <- fitdistr(CAC40$return_100, f, list(a=1, b=0, c=log(mad(CAC40$return_100)), d=median(CAC40$return_100)))
# r_stable_CAC40

### gives following values (very long to run)

a_CAC40 = 1.31111593    
b_CAC40 = 0.31231726  
c_CAC40 = -0.32470313  
d_CAC40 = -0.07071168 

CAC40$stable = f(CAC40$return_100, a = a_CAC40, b = b_CAC40, c = c_CAC40, d = d_CAC40)


f <- function(u,a,b,c,d) {
  cat(a,b,c,d,"\n")  # Some logging (it is very slow)
  dstable(BITCOIN$return_100, 2*exp(a)/(1+exp(a)), 2*exp(b)/(1+exp(b))-1, exp(c), d)
}

# r_stable_BITCOIN <- fitdistr(BITCOIN$return_100, f, list(a=1, b=0, c=log(mad(BITCOIN$return_100)), d=median(BITCOIN$return_100)))
# r_stable_BITCOIN

a_BITCOIN = 0.46938594  
b_BITCOIN = 0.17962342
c_BITCOIN = 0.54323727
d_BITCOIN = 0.20952216

BITCOIN$stable = f(BITCOIN$return_100, a = a_BITCOIN, b = b_BITCOIN, c = c_BITCOIN, d = d_BITCOIN)

f <- function(u,a,b,c,d) {
  cat(a,b,c,d,"\n")  # Some logging (it is very slow)
  dstable(ALIBABA$return_100, 2*exp(a)/(1+exp(a)), 2*exp(b)/(1+exp(b))-1, exp(c), d)
}

# r_stable_ALIBABA = fitdistr(ALIBABA$return_100, f, list(a=1, b=0, c=log(mad(ALIBABA$return_100)), d=median(ALIBABA$return_100)))
# r_stable_ALIBABA


a_ALIBABA = 2.29140859  
b_ALIBABA = 0.20163948
c_ALIBABA = 0.28143916
d_ALIBABA = -0.10266055

ALIBABA$stable = f(ALIBABA$return_100, a = a_ALIBABA, b = b_ALIBABA, c = c_ALIBABA, d = d_ALIBABA)


######### generalize lambda distribution ##########

logret_CAC40 = CAC40$return_100[-1]

#Method of moment
wshLambdaMM_CAC40 <- fun.RMFMKL.mm(logret_CAC40)

fittedVal_CAC40 = dgl(CAC40$return_100, lambda1 = wshLambdaMM_CAC40[1], lambda2 = wshLambdaMM_CAC40[2],
                      lambda3 = wshLambdaMM_CAC40[3], lambda4 = wshLambdaMM_CAC40[4],
                param = "fmkl", inverse.eps = 1e-08,
                max.iterations = 500)

CAC40$gld = fittedVal_CAC40


logret_BITCOIN = BITCOIN$return_100[-1]

#Method of moment
wshLambdaMM_BITCOIN <- fun.RMFMKL.mm(logret_BITCOIN)

fittedVal_BITCOIN = dgl(BITCOIN$return_100, lambda1 = wshLambdaMM_BITCOIN[1], lambda2 = wshLambdaMM_BITCOIN[2],
                      lambda3 = wshLambdaMM_BITCOIN[3], lambda4 = wshLambdaMM_BITCOIN[4],
                      param = "fmkl", inverse.eps = 1e-08,
                      max.iterations = 500)

BITCOIN$gld = fittedVal_BITCOIN


logret_ALIBABA = ALIBABA$return_100[-1]

#Method of moment
wshLambdaMM_ALIBABA <- fun.RMFMKL.mm(logret_ALIBABA)

fittedVal_ALIBABA = dgl(ALIBABA$return_100, lambda1 = wshLambdaMM_ALIBABA[1], lambda2 = wshLambdaMM_ALIBABA[2],
                      lambda3 = wshLambdaMM_ALIBABA[3], lambda4 = wshLambdaMM_ALIBABA[4],
                      param = "fmkl", inverse.eps = 1e-08,
                      max.iterations = 500)

ALIBABA$gld = fittedVal_ALIBABA

######## plot ##########

png(filename = "fit_distrib.png", width = 1000)

g_CAC40 = ggplot(data = CAC40)
g_CAC40 = g_CAC40 + geom_histogram(aes(x = return_100, y = ..density..), colour = 'lightblue', fill = "blue", alpha = 0.3)
g_CAC40 = g_CAC40 + geom_density(aes(x = return_100, colour = 'Kernel density'), alpha = 0.8, lwd = 0.8, lty = 1)
g_CAC40 = g_CAC40 + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8, lty = 1)
g_CAC40 = g_CAC40 + geom_line(aes(x = return_100, y = stable, color = 'Stable'), lwd = 0.8, lty = 1)
g_CAC40 = g_CAC40 + geom_line(aes(x = return_100, y = t, color = 'Student'), lwd = 0.8, lty = 1)
g_CAC40 = g_CAC40 + geom_line(aes(x = return_100, y = gld, color = 'Lambda'), lwd = 0.8, lty = 2)
g_CAC40 = g_CAC40 + scale_x_continuous(limits = c(min(CAC40$return_100),max(CAC40$return_100)))
g_CAC40 = g_CAC40 + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Stable' = 'green',
  'Normal' = 'red',
  'Student' = 'yellow',
  'Lambda' = 'black'))
g_CAC40 = g_CAC40 + my_theme + labs(x = "return (%)", title = "CAC40 returns")
g_CAC40 = g_CAC40 + theme(legend.position="bottom")

g_ALIBABA = ggplot(data = ALIBABA)
g_ALIBABA = g_ALIBABA + geom_histogram(aes(x = return_100, y = ..density..), colour = 'lightblue', fill = "blue", alpha = 0.3)
g_ALIBABA = g_ALIBABA + geom_density(aes(x = return_100, colour = 'Kernel density'), alpha = 0.8, lwd = 0.8, lty = 1)
g_ALIBABA = g_ALIBABA + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8, lty = 1)
g_ALIBABA = g_ALIBABA + geom_line(aes(x = return_100, y = stable, color = 'Stable'), lwd = 0.8, lty = 1)
g_ALIBABA = g_ALIBABA + geom_line(aes(x = return_100, y = t, color = 'Student'), lwd = 0.8, lty = 1)
g_ALIBABA = g_ALIBABA + geom_line(aes(x = return_100, y = gld, color = 'Lambda'), lwd = 0.8, lty = 2)
g_ALIBABA = g_ALIBABA + scale_x_continuous(limits = c(min(ALIBABA$return_100),max(ALIBABA$return_100)))
g_ALIBABA = g_ALIBABA + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Stable' = 'green',
  'Normal' = 'red',
  'Student' = 'yellow',
  'Lambda' = 'black'))
g_ALIBABA = g_ALIBABA + my_theme + labs(x = "return (%)", title = "ALIBABA returns")
g_ALIBABA = g_ALIBABA + theme(legend.position="bottom")

g_BITCOIN = ggplot(data = BITCOIN)
g_BITCOIN = g_BITCOIN + geom_histogram(aes(x = return_100, y = ..density..), colour = 'lightblue', fill = "blue", alpha = 0.3)
g_BITCOIN = g_BITCOIN + geom_density(aes(x = return_100, colour = 'Kernel density'), alpha = 0.8, lwd = 0.8, lty = 1)
g_BITCOIN = g_BITCOIN + geom_line(aes(x = return_100, y = normal, color = "Normal"), lwd = 0.8, lty = 1)
g_BITCOIN = g_BITCOIN + geom_line(aes(x = return_100, y = stable, color = 'Stable'), lwd = 0.8, lty = 1)
g_BITCOIN = g_BITCOIN + geom_line(aes(x = return_100, y = t, color = 'Student'), lwd = 0.8, lty = 1)
g_BITCOIN = g_BITCOIN + geom_line(aes(x = return_100, y = gld, color = 'Lambda'), lwd = 0.8, lty = 2)
g_BITCOIN = g_BITCOIN + scale_x_continuous(limits = c(min(BITCOIN$return_100),max(BITCOIN$return_100)))
g_BITCOIN = g_BITCOIN + scale_color_manual(name = "Distribution", values = c(
  "Kernel density" = "blue",
  'Stable' = 'green',
  'Normal' = 'red',
  'Student' = 'yellow',
  'Lambda' = 'black'))
g_BITCOIN = g_BITCOIN + my_theme + labs(x = "return (%)", title = "BITCOIN returns")
g_BITCOIN = g_BITCOIN + theme(legend.position="bottom")


ggarrange(g_ALIBABA, g_CAC40, g_BITCOIN, common.legend = TRUE, ncol = 3, legend = "bottom")

dev.off()

## statistics (mean, sd, skewness, kurtosis) ##

m_CAC40 = mean(CAC40$return_100)
sd_CAC40 = sd(CAC40$return_100)
k_CAC40 = kurtosis(CAC40$return_100, method = "moment")
s_CAC40 = skewness(CAC40$return_100)

##### BOOTSTRAP ######

## Normal ##

m_rep_norm = c()
sd_rep_norm = c()
s_rep_norm = c()
k_rep_norm = c()

replicate(5000,{
  b_data = rnorm(n, mean = mu_CAC40, sd = sigma_CAC40)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_norm <<- c(m_rep_norm, mean(b_data))
  sd_rep_norm <<- c(sd_rep_norm, sd(b_data))
  s_rep_norm <<- c(s_rep_norm, skewness(b_data))
  k_rep_norm <<- c(k_rep_norm, kurtosis(b_data, method = "moment"))
})

m_CAC40_normal = mean(m_rep_norm)
sd_CAC40_normal = mean(sd_rep_norm)
s_CAC40_normal = mean(s_rep_norm)
k_CAC40_normal = mean(k_rep_norm)


## Student ##

m_rep_t = c()
sd_rep_t = c()
s_rep_t = c()
k_rep_t = c()

replicate(5000,{
  b_data = rt(n, df = df_hat_CAC40, ncp = ncp_hat_CAC40)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_t <<- c(m_rep_t, mean(b_data))
  sd_rep_t <<- c(sd_rep_t, sd(b_data))
  s_rep_t <<- c(s_rep_t, skewness(b_data))
  k_rep_t <<- c(k_rep_t, kurtosis(b_data, method = "moment"))
})

m_CAC40_t = mean(m_rep_t)
sd_CAC40_t = mean(sd_rep_t)
s_CAC40_t = mean(s_rep_t)
k_CAC40_t = mean(k_rep_t)


## Stable ##

m_rep_stable = c()
sd_rep_stable = c()
s_rep_stable = c()
k_rep_stable = c()

replicate(5000,{
  b_data = rstable(n, 2*exp(a_CAC40)/(1+exp(a_CAC40)), 2*exp(b_CAC40)/(1+exp(b_CAC40))-1, exp(c_CAC40), d_CAC40)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_stable <<- c(m_rep_stable, mean(b_data))
  sd_rep_stable <<- c(sd_rep_stable, sd(b_data))
  s_rep_stable <<- c(s_rep_stable, skewness(b_data))
  k_rep_stable <<- c(k_rep_stable, kurtosis(b_data, method = "moment"))
})

m_CAC40_stable = mean(m_rep_stable)
sd_CAC40_stable = mean(sd_rep_stable)
s_CAC40_stable = mean(s_rep_stable)
k_CAC40_stable = mean(k_rep_stable)

## GLD ##

m_rep_gld = c()
sd_rep_gld = c()
s_rep_gld = c()
k_rep_gld = c()

replicate(5000,{
  b_data = rgl(n,lambda1 = wshLambdaMM_CAC40[1], lambda2 = wshLambdaMM_CAC40[2],
               lambda3 = wshLambdaMM_CAC40[3], lambda4 = wshLambdaMM_CAC40[4],
               param = "fmkl")
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_gld <<- c(m_rep_gld, mean(b_data))
  sd_rep_gld <<- c(sd_rep_gld, sd(b_data))
  s_rep_gld <<- c(s_rep_gld, skewness(b_data))
  k_rep_gld <<- c(k_rep_gld, kurtosis(b_data, method = "moment"))
})

m_CAC40_gld = mean(m_rep_gld)
sd_CAC40_gld = mean(sd_rep_gld)
s_CAC40_gld = mean(s_rep_gld)
k_CAC40_gld = mean(k_rep_gld)


# ALIBABA


## Normal ##

m_rep_norm = c()
sd_rep_norm = c()
s_rep_norm = c()
k_rep_norm = c()

replicate(5000,{
  b_data = rnorm(n, mean = mu_ALIBABA, sd = sigma_ALIBABA)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_norm <<- c(m_rep_norm, mean(b_data))
  sd_rep_norm <<- c(sd_rep_norm, sd(b_data))
  s_rep_norm <<- c(s_rep_norm, skewness(b_data))
  k_rep_norm <<- c(k_rep_norm, kurtosis(b_data, method = "moment"))
})

m_ALIBABA_normal = mean(m_rep_norm)
sd_ALIBABA_normal = mean(sd_rep_norm)
s_ALIBABA_normal = mean(s_rep_norm)
k_ALIBABA_normal = mean(k_rep_norm)


## Student ##

m_rep_t = c()
sd_rep_t = c()
s_rep_t = c()
k_rep_t = c()

replicate(5000,{
  b_data = rt(n, df = df_hat_ALIBABA, ncp = ncp_hat_ALIBABA)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_t <<- c(m_rep_t, mean(b_data))
  sd_rep_t <<- c(sd_rep_t, sd(b_data))
  s_rep_t <<- c(s_rep_t, skewness(b_data))
  k_rep_t <<- c(k_rep_t, kurtosis(b_data, method = "moment"))
})

m_ALIBABA_t = mean(m_rep_t)
sd_ALIBABA_t = mean(sd_rep_t)
s_ALIBABA_t = mean(s_rep_t)
k_ALIBABA_t = mean(k_rep_t)


## Stable ##

m_rep_stable = c()
sd_rep_stable = c()
s_rep_stable = c()
k_rep_stable = c()

replicate(5000,{
  b_data = rstable(n, 2*exp(a_ALIBABA)/(1+exp(a_ALIBABA)), 2*exp(b_ALIBABA)/(1+exp(b_ALIBABA))-1, exp(c_ALIBABA), d_ALIBABA)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_stable <<- c(m_rep_stable, mean(b_data))
  sd_rep_stable <<- c(sd_rep_stable, sd(b_data))
  s_rep_stable <<- c(s_rep_stable, skewness(b_data))
  k_rep_stable <<- c(k_rep_stable, kurtosis(b_data, method = "moment"))
})

m_ALIBABA_stable = mean(m_rep_stable)
sd_ALIBABA_stable = mean(sd_rep_stable)
s_ALIBABA_stable = mean(s_rep_stable)
k_ALIBABA_stable = mean(k_rep_stable)

## GLD ##

m_rep_gld = c()
sd_rep_gld = c()
s_rep_gld = c()
k_rep_gld = c()

replicate(5000,{
  b_data = rgl(n,lambda1 = wshLambdaMM_ALIBABA[1], lambda2 = wshLambdaMM_ALIBABA[2],
               lambda3 = wshLambdaMM_ALIBABA[3], lambda4 = wshLambdaMM_ALIBABA[4],
               param = "fmkl")
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_gld <<- c(m_rep_gld, mean(b_data))
  sd_rep_gld <<- c(sd_rep_gld, sd(b_data))
  s_rep_gld <<- c(s_rep_gld, skewness(b_data))
  k_rep_gld <<- c(k_rep_gld, kurtosis(b_data, method = "moment"))
})

m_ALIBABA_gld = mean(m_rep_gld)
sd_ALIBABA_gld = mean(sd_rep_gld)
s_ALIBABA_gld = mean(s_rep_gld)
k_ALIBABA_gld = mean(k_rep_gld)


# BITCOIN

## Normal ##

m_rep_norm = c()
sd_rep_norm = c()
s_rep_norm = c()
k_rep_norm = c()

replicate(5000,{
  b_data = rnorm(n, mean = mu_BITCOIN, sd = sigma_BITCOIN)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_norm <<- c(m_rep_norm, mean(b_data))
  sd_rep_norm <<- c(sd_rep_norm, sd(b_data))
  s_rep_norm <<- c(s_rep_norm, skewness(b_data))
  k_rep_norm <<- c(k_rep_norm, kurtosis(b_data, method = "moment"))
})

m_BITCOIN_normal = mean(m_rep_norm)
sd_BITCOIN_normal = mean(sd_rep_norm)
s_BITCOIN_normal = mean(s_rep_norm)
k_BITCOIN_normal = mean(k_rep_norm)


## Student ##

m_rep_t = c()
sd_rep_t = c()
s_rep_t = c()
k_rep_t = c()

replicate(5000,{
  b_data = rt(n, df = df_hat_BITCOIN, ncp = ncp_hat_BITCOIN)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_t <<- c(m_rep_t, mean(b_data))
  sd_rep_t <<- c(sd_rep_t, sd(b_data))
  s_rep_t <<- c(s_rep_t, skewness(b_data))
  k_rep_t <<- c(k_rep_t, kurtosis(b_data, method = "moment"))
})

m_BITCOIN_t = mean(m_rep_t)
sd_BITCOIN_t = mean(sd_rep_t)
s_BITCOIN_t = mean(s_rep_t)
k_BITCOIN_t = mean(k_rep_t)


## Stable ##

m_rep_stable = c()
sd_rep_stable = c()
s_rep_stable = c()
k_rep_stable = c()

replicate(5000,{
  b_data = rstable(n, 2*exp(a_BITCOIN)/(1+exp(a_BITCOIN)), 2*exp(b_BITCOIN)/(1+exp(b_BITCOIN))-1, exp(c_BITCOIN), d_BITCOIN)
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_stable <<- c(m_rep_stable, mean(b_data))
  sd_rep_stable <<- c(sd_rep_stable, sd(b_data))
  s_rep_stable <<- c(s_rep_stable, skewness(b_data))
  k_rep_stable <<- c(k_rep_stable, kurtosis(b_data, method = "moment"))
})

m_BITCOIN_stable = mean(m_rep_stable)
sd_BITCOIN_stable = mean(sd_rep_stable)
s_BITCOIN_stable = mean(s_rep_stable)
k_BITCOIN_stable = mean(k_rep_stable)

## GLD ##

m_rep_gld = c()
sd_rep_gld = c()
s_rep_gld = c()
k_rep_gld = c()

replicate(5000,{
  b_data = rgl(n,lambda1 = wshLambdaMM_BITCOIN[1], lambda2 = wshLambdaMM_BITCOIN[2],
               lambda3 = wshLambdaMM_BITCOIN[3], lambda4 = wshLambdaMM_BITCOIN[4],
               param = "fmkl")
  b_data = b_data[which(b_data < max(return_100) & b_data > min(return_100))]
  m_rep_gld <<- c(m_rep_gld, mean(b_data))
  sd_rep_gld <<- c(sd_rep_gld, sd(b_data))
  s_rep_gld <<- c(s_rep_gld, skewness(b_data))
  k_rep_gld <<- c(k_rep_gld, kurtosis(b_data, method = "moment"))
})

m_BITCOIN_gld = mean(m_rep_gld)
sd_BITCOIN_gld = mean(sd_rep_gld)
s_BITCOIN_gld = mean(s_rep_gld)
k_BITCOIN_gld = mean(k_rep_gld)

## TABLES ##

m = c(m_CAC40, m_ALIBABA, m_BITCOIN)
m_norm = c(m_CAC40_normal, m_ALIBABA_normal, m_BITCOIN_normal)
m_t = c(m_CAC40_t, m_ALIBABA_t, m_BITCOIN_t)
m_stable = c(m_CAC40_stable, m_ALIBABA_stable, m_BITCOIN_stable)
m_gld = c(m_CAC40_gld, m_ALIBABA_gld, m_BITCOIN_gld)

sdev = c(sd_CAC40, sd_ALIBABA, sd_BITCOIN)
sd_norm = c(sd_CAC40_normal, sd_ALIBABA_normal, sd_BITCOIN_normal)
sd_t = c(sd_CAC40_t, sd_ALIBABA_t, sd_BITCOIN_t)
sd_stable = c(sd_CAC40_stable, sd_ALIBABA_stable, sd_BITCOIN_stable)
sd_gld = c(sd_CAC40_gld, sd_ALIBABA_gld, sd_BITCOIN_gld)

k = c(k_CAC40, k_ALIBABA, k_BITCOIN)
k_norm = c(k_CAC40_normal, k_ALIBABA_normal, k_BITCOIN_normal)
k_t = c(k_CAC40_t, k_ALIBABA_t, k_BITCOIN_t)
k_stable = c(k_CAC40_stable, k_ALIBABA_stable, k_BITCOIN_stable)
k_gld = c(k_CAC40_gld, k_ALIBABA_gld, k_BITCOIN_gld)

s = c(s_CAC40, s_ALIBABA, s_BITCOIN)
s_norm = c(s_CAC40_normal, s_ALIBABA_normal, s_BITCOIN_normal)
s_t = c(s_CAC40_t, s_ALIBABA_t, s_BITCOIN_t)
s_stable = c(s_CAC40_stable, s_ALIBABA_stable, s_BITCOIN_stable)
s_gld = c(s_CAC40_gld, s_ALIBABA_gld, s_BITCOIN_gld)


xtable(data.frame(m, m_norm, m_t, m_stable, m_gld, row.names = c("CAC40","ALIBABA","BITCOIN")))
xtable(data.frame(sdev, sd_norm, sd_t, sd_stable, sd_gld, row.names = c("CAC40","ALIBABA","BITCOIN")))
xtable(data.frame(k, k_norm, k_t, k_stable, k_gld, row.names = c("CAC40","ALIBABA","BITCOIN")))
xtable(data.frame(s, s_norm, s_t, s_stable, s_gld, row.names = c("CAC40","ALIBABA","BITCOIN")))


## p-values Kolmogorov-Smirnov ##

# CAC40

pval_CAC40_norm = ks.test(CAC40$return_100, function(x) pnorm(x, mean = mean(CAC40$return_100), sd = sd(CAC40$return_100)), exact = TRUE)$p.value
pval_CAC40_t = ks.test(CAC40$return_100, function(x) pt(x, df = df_hat_CAC40, ncp = ncp_hat_CAC40), exact = TRUE)$p.value
pval_CAC40_stable = ks.test(CAC40$return_100, function(x) pstable(x, 2*exp(a_CAC40)/(1+exp(a_CAC40)), 
                                              2*exp(b_CAC40)/(1+exp(b_CAC40))-1, 
                                              exp(c_CAC40), 
                                              d_CAC40), exact = TRUE)$p.value
pval_CAC40_gld = ks.test(CAC40$return_100, function(x) pgl(x, lambda1 = wshLambdaMM_CAC40[1], lambda2 = wshLambdaMM_CAC40[2],
                                          lambda3 = wshLambdaMM_CAC40[3], lambda4 = wshLambdaMM_CAC40[4],
                                          param = "fmkl"), exact = TRUE)$p.value

# ALIBABA

pval_ALIBABA_norm = ks.test(ALIBABA$return_100, function(x) pnorm(x, mean = mean(ALIBABA$return_100), sd = sd(ALIBABA$return_100)), exact = TRUE)$p.value
pval_ALIBABA_t = ks.test(ALIBABA$return_100, function(x) pt(x, df = df_hat_ALIBABA, ncp = ncp_hat_ALIBABA), exact = TRUE)$p.value
pval_ALIBABA_stable = ks.test(ALIBABA$return_100, function(x) pstable(x, 2*exp(a_ALIBABA)/(1+exp(a_ALIBABA)),
                                                2*exp(b_ALIBABA)/(1+exp(b_ALIBABA))-1, 
                                                exp(c_ALIBABA), 
                                                d_ALIBABA), exact = TRUE)$p.value
pval_ALIBABA_gld = ks.test(ALIBABA$return_100, function(x) pgl(x, lambda1 = wshLambdaMM_ALIBABA[1], lambda2 = wshLambdaMM_ALIBABA[2],
                                          lambda3 = wshLambdaMM_ALIBABA[3], lambda4 = wshLambdaMM_ALIBABA[4],
                                          param = "fmkl"), exact = TRUE)$p.value

# BITCOIN

pval_BITCOIN_norm = ks.test(BITCOIN$return_100, function(x) pnorm(x, mean = mean(BITCOIN$return_100), sd = sd(BITCOIN$return_100)), exact = TRUE)$p.value
pval_BITCOIN_t = ks.test(BITCOIN$return_100, function(x) pt(x, df = df_hat_BITCOIN, ncp = ncp_hat_BITCOIN), exact = TRUE)$p.value
pval_BITCOIN_stable = ks.test(BITCOIN$return_100, function(x) pstable(x, 2*exp(a_BITCOIN)/(1+exp(a_BITCOIN)),
                                                2*exp(b_BITCOIN)/(1+exp(b_BITCOIN))-1,
                                                exp(c_BITCOIN),
                                                d_BITCOIN), exact = TRUE)$p.value
pval_BITCOIN_gld = ks.test(BITCOIN$return_100, function(x) pgl(x, lambda1 = wshLambdaMM_BITCOIN[1], lambda2 = wshLambdaMM_BITCOIN[2],
                                          lambda3 = wshLambdaMM_BITCOIN[3], lambda4 = wshLambdaMM_BITCOIN[4],
                                          param = "fmkl"), exact = TRUE)$p.value

## Tables ##

df_pval = data.frame(norm = c(pval_CAC40_norm, pval_ALIBABA_norm, pval_BITCOIN_norm), 
                     t = c(pval_CAC40_t, pval_ALIBABA_t, pval_BITCOIN_t), 
                     stable = c(pval_CAC40_stable, pval_ALIBABA_stable, pval_BITCOIN_stable), 
                     gld = c(pval_CAC40_gld, pval_ALIBABA_gld, pval_BITCOIN_gld),
                     row.names = c("CAC40", "ALIBABA", "BITCOIN"))

xtable(df_pval, digits = -1)



