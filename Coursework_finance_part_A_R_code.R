


### Introduction to statistical finance - Coursework 1 - Part A ###

setwd("C:/Users/robin/Dropbox/Applications/Overleaf/Coursework finance part A")

library("ggplot2")
library("tidyverse")

my_theme <-
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 14))

mu = 0.005
h = 0.06
r = 0.01
S0 = 100
T_final = 8
p_plus = 0.8
p_minus = 0.1

R = exp(r)
U = exp(mu+h)
M = exp(mu)
D = exp(mu-h)

(U-M)/(R-M)
(D-M)/(M-R)
(D-R)/(D-M) * 1/( (R-M)/(U-M) + (M-R)/(D-M))

q_plus = 2*(R-M)/(U-M)
q_minus = (M-R)/(D-M)
q_plus
q_plus; q_minus; 1 - q_plus - q_minus

gen_q_plus = function(alpha) alpha*(R-M)/(U-M)
gen_q_minus = function(alpha) (1-alpha)*(R-M)/(D-M)
abs = seq(0,15,by = 0.01)
ord_plus = gen_q_plus(abs)
ord_minus = gen_q_minus(abs)
  
png(filename = "q_+_q_-.png", width = 1000)

df_q = data.frame(abs, ord_plus, ord_minus)
q = ggplot(df_q)
q = q + geom_line(aes(x = abs, y = ord_plus, color = "q+"))
q = q + geom_line(aes(x = abs, y = ord_minus, color = "q-"))
q = q + my_theme
q = q + scale_color_manual(values = c("q+" = 'blue',
                            "q-" = 'red'))
q = q + geom_hline(yintercept = 0, lty = 2)
q = q + geom_hline(yintercept = 1, lty = 2)
q = q + labs(title = "q+ and q- for an EMM", x = "value", y = "q")
q

dev.off()

######### creation payoff function ###########

B = function(t) return(exp(r*t))

f = function(s){
  if (s <= 100) return(-2*s + 250)
  if (s > 100 & s <= 150) return(-s + 150)
  if (s > 150 & s <= 200) return(0)
  if (s > 200 & s <= 300) return(s - 200)
  else print("error : spot price is not atteignable")
}

f = Vectorize(f)

abs = seq(0,300,by=1)
ord = f(abs)
df_f = data.frame(abs,ord)

png(filename="f_S_T.png", width = 900)
g = ggplot(df_f, aes(x = abs, y = ord))
g = g + geom_line(color = 'red', lwd = 1.2)
g = g + theme(legend.position="right", text = element_text(size=20))
g = g + xlab(expression(S[T])) + ylab(expression(f(S[T])))
g
dev.off()

######### create tree stock prices #############


tree_St = list()
tree_St[[1]] = S0

for (t in 1:T_final){
  prices_t = c()
  grid = 0:t
  set = expand.grid(grid, grid, grid)
  set = set[sapply(1:dim(set)[1], function(i) sum(set[i,]) == t), ]
  prices = sapply(1:dim(set)[1], function(i) S0*U^(set[i,1])*M^(set[i,2])*D^(set[i,3]))
  prices = unique(sort(prices))
  tree_St[[t+1]] = unique(round(prices,digits=5))
}

sapply(1:length(tree_St), function(i) round(tree_St[[i]],digits = 1))


########## create tree option prices ###########


PI_disc_T = function(s) f(s)/B(T_final)

tree_prices_disc = list()
tree_prices_disc[[T_final+1]] = PI_disc_T(tree_St[[T_final+1]])

for (t in rev(1:T_final)){
  PI_next = tree_prices_disc[[t+1]]
  PI_cur = c()
  for (i in 1:(length(PI_next)-2)){
    price = PI_next[i]*q_minus +
      PI_next[i+1]*(1 - q_plus - q_minus) + 
      PI_next[i+2]*q_plus
    PI_cur = c(PI_cur,price)
  }
  tree_prices_disc[[t]] = PI_cur
}


tree_prices = list()
for (t in 1:length(tree_prices_disc)){
  tree_prices[[t]] = tree_prices_disc[[t]]*B(t-1)
}

tree_prices[[9]]      # need to be equal
f(tree_St[[9]])

sapply(1:length(tree_St), function(i) round(tree_prices[[i]],digits = 1))

############   tree price European call option (K = 200)  #########

B = function(t) return(exp(r*t))

f_call_K_200 = function(s) max(0,s-200)
f_call_K_200 = Vectorize(f_call_K_200)

abs = seq(0,300,by=0.01)
ord = f_call_K_200(abs)
plot(abs,ord, col = 'red')

PI_disc_T_K_200 = function(s) f_call_K_200(s)/B(T_final+1)

tree_prices_disc_call_K_200 = list()
tree_prices_disc_call_K_200[[T_final+1]] = PI_disc_T_K_200(tree_St[[T_final+1]])

for (t in rev(1:T_final)){
  PI_next = tree_prices_disc_call_K_200[[t+1]]
  PI_cur = c()
  for (i in 1:(length(PI_next)-2)){
    price = PI_next[i]*q_minus +
      PI_next[i+1]*(1 - q_plus - q_minus) + 
      PI_next[i+2]*q_plus
    PI_cur = c(PI_cur,price)
  }
  tree_prices_disc_call_K_200[[t]] = PI_cur
}

tree_prices_call_K_200 = list()
for (t in 1:length(tree_prices_disc_call_K_200)){
  tree_prices_call_K_200[[t]] = tree_prices_disc_call_K_200[[t]]*B(t)
}

tree_prices_call_K_200[[9]]      # need to be equal
f_call_K_200(tree_St[[9]])

sapply(1:length(tree_St), function(i) round(tree_prices_call_K_200[[i]],digits = 1))

############   tree price European put option (K = 150)  #########


B = function(t) return(exp(r*t))

f_put_K_150 = function(s) max(0,150-s)
f_put_K_150 = Vectorize(f_put_K_150)

abs = seq(0,300,by=0.01)
ord = f_put_K_150(abs)
plot(abs,ord, col = 'red')

PI_disc_put_K_150 = function(s) f_put_K_150(s)/B(T_final+1)

tree_prices_disc_put_K_150 = list()
tree_prices_disc_put_K_150[[T_final+1]] = PI_disc_put_K_150(tree_St[[T_final+1]])

for (t in rev(1:T_final)){
  PI_next = tree_prices_disc_put_K_150[[t+1]]
  PI_cur = c()
  for (i in 1:(length(PI_next)-2)){
    price = PI_next[i]*q_minus +
      PI_next[i+1]*(1 - q_plus - q_minus) + 
      PI_next[i+2]*q_plus
    PI_cur = c(PI_cur,price)
  }
  tree_prices_disc_put_K_150[[t]] = PI_cur
}

tree_prices_put_K_150 = list()
for (t in 1:length(tree_prices_disc_put_K_150)){
  tree_prices_put_K_150[[t]] = tree_prices_disc_put_K_150[[t]]*B(t)
}

tree_prices_put_K_150[[9]]      # need to be equal
f_put_K_150(tree_St[[9]])

sapply(1:length(tree_St), function(i) round(tree_prices_put_K_150[[i]],digits = 1))

############   tree price European put option (K = 100)  #########

B = function(t) return(exp(r*t))

f_put_K_100 = function(s) max(0,100-s)
f_put_K_100 = Vectorize(f_put_K_100)

abs = seq(0,300,by=0.01)
ord = f_put_K_100(abs)
plot(abs,ord, col = 'red')

PI_disc_put_K_100 = function(s) f_put_K_100(s)/B(T_final+1)

tree_prices_disc_put_K_100 = list()
tree_prices_disc_put_K_100[[T_final+1]] = PI_disc_put_K_100(tree_St[[T_final+1]])

for (t in rev(1:T_final)){
  PI_next = tree_prices_disc_put_K_100[[t+1]]
  PI_cur = c()
  for (i in 1:(length(PI_next)-2)){
    price = PI_next[i]*q_minus +
      PI_next[i+1]*(1 - q_plus - q_minus) + 
      PI_next[i+2]*q_plus
    PI_cur = c(PI_cur,price)
  }
  tree_prices_disc_put_K_100[[t]] = PI_cur
}

tree_prices_put_K_100 = list()
for (t in 1:length(tree_prices_disc_put_K_100)){
  tree_prices_put_K_100[[t]] = tree_prices_disc_put_K_100[[t]]*B(t)
}

tree_prices_put_K_100[[9]]      # need to be equal
f_put_K_100(tree_St[[9]])

sapply(1:length(tree_St), function(i) round(tree_prices_put_K_100[[i]],digits = 1))

############# total value options ################


sum_tree_prices_options = list()
for (t in 1:length(tree_prices_put_K_100)){
  sum_tree_prices_options[[t]] = tree_prices_call_K_200[[t]] + tree_prices_put_K_150[[t]] + tree_prices_put_K_100[[t]]
}

sum_tree_prices_options
tree_prices

data_gg_options = data.frame(S = abs)
data_gg_options$call200 = f_call_K_200(abs)
data_gg_options$put150 = f_put_K_150(abs)
data_gg_options$put100 = f_put_K_100(abs)
data_gg_options$sum_payoffs = f(abs)
data_gg_options = data_gg_options %>%  gather(key = "payoff", value = "value", -S)


png(filename = "options.png", width = 1000)
g = ggplot(data = data_gg_options, aes(x = S, y = value))
g = g + geom_line(aes(color = payoff, lty = payoff), lwd = 1.2)
g = g + theme(legend.position="right", text = element_text(size=20))
g = g + xlab(expression(S[T])) + ylab("payoff")
g
dev.off()



