rm(list=ls())
gc()

country = "BRAZIL"
step <- 1
l <- 6

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- LOADING PACKAGES --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
load_package<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
  }
  require(x,character.only=TRUE)
}

load_package('tseries')
load_package('xlsx')
load_package('QRM')
load_package('openxlsx')
load_package('xts')
load_package('class')
load_package('zoo')
load_package('fBasics')
load_package('qrmtools')
load_package('fDMA')
load_package('TSA')
load_package('roll')
load_package('MTS')
load_package('forecast')
load_package('fGarch')
load_package('rugarch')
load_package('rmgarch')
load_package('imputeTS')
load_package('ggplot2')
load_package('reshape2') 
load_package('quadprog')
load_package('qrmdata')
load_package('mvtnorm')
load_package('graphics')
load_package('dplyr')
load_package('midasr')
load_package('TSP')
load_package('imputeTS')
load_package("MTS")
load_package("quantmod")
load_package("vars")
load_package("stats")
load_package("stargazer")
load_package("corrplot")
load_package("Metrics")
load_package("MLmetrics")
load_package("MCS")
load_package("tsDyn")
load_package("matrixStats")
load_package("car")
load_package("strucchange")
load_package("hrbrthemes")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#-------- IMPORTING DATA ---------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

df <- read.csv("daily_data_all.csv", sep = ",")
df[df==0] <- NA
df$date <- as.Date(df$date)
df_country = na.omit(df[df$country==country,])

add.months <- function(date,n) seq(date, by = paste(n, "months"), length = 2)[2]

market <- read.csv("market.csv", sep = ";", na.strings = "#N/A")
colnames(market) <- c("date", "BRAZIL", "ARGENTINA", "CHINA_S", "JAPAN", "UK", "GERMANY", "SOUTHAFRICA", "USA")
market <- na_locf(market)
market$date <- as.Date(market$date, "%d/%m/%Y")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#----------- VARIABLES -----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

inflation_ps <- na.omit(xts(df_country$annualPS, order.by = df_country$date))
inflation_cpi <- na.omit(xts(df_country$annualCPI, order.by = df_country$date))
market_m_annual <- na.omit(xts(market[country], order.by = market$date) %>% diff.xts(lag = 12, log = TRUE)*100)

min <- max(c(min(time(inflation_ps)), min(time(inflation_cpi))))
max <- min(c(max(time(inflation_ps)), max(time(inflation_cpi))))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- ADJUSTING SAMPLES -------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sample_period <- paste(min+1, max-1, sep = "/")

inflation_cpi_full_d <- inflation_cpi[sample_period]
inflation_ps_full <- inflation_ps[sample_period]
market_m_annual_full <- market_m_annual[sample_period]

date_m <- time(market_m_annual_full)

if (country == "GERMANY"){
  date_m <- date_m[date_m !="2015-03-31"]
  market_m_annual_full[time(market_m_annual_full) != "2015-03-02",]
}

if (country == "UK"){
  date_m <- date_m[date_m !="2015-03-31"]
  market_m_annual_full[time(market_m_annual_full) != "2015-03-02",]
}
  


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---- HARMONIZING DAILY DATA -----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_to_28 <- function(series){
  tt <- time(series)
  last.date.of.month <- as.Date(as.yearmon(tt), frac = 1)
  series[ last.date.of.month - tt < 28 ]
}

inflation_ps_full <- adjust_to_28(inflation_ps_full)
inflation_cpi_full_d <- adjust_to_28(inflation_cpi_full_d)

inflation_ps_agg_full <- matrix(inflation_ps_full, nrow=28) %>% colMeans(na.rm=TRUE) %>% cbind %>% xts(order.by = date_m)
inflation_cpi_full <- matrix(inflation_cpi_full_d, nrow=28) %>% colMeans(na.rm=TRUE) %>% cbind %>% xts(order.by = date_m)

date_d <- time(inflation_ps_full) %>% as.Date
aux <- xts(date_d, order.by = date_d)

inflation_cpi_full_d <- na_locf(inflation_cpi_full_d)
aux <-xts(time(inflation_ps_full), order.by = time(inflation_ps_full))
inflation_cpi_full_d <-merge(aux, inflation_cpi_full_d, all = FALSE)$inflation_cpi_full


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- SOME PLOTS -----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

par(mfrow=c(1,1))

# cpi vs PS - scatter
par(mfrow=c(1,1))
plot.default(coredata(inflation_ps_agg_full),coredata(inflation_cpi_full),
             xlab = 'PS Inflation',
             ylab = 'IPCA Inflation',
             cex.lab=1)
eq = lm(inflation_cpi_full ~ + inflation_ps_agg_full)
abline(coef(eq), col='red')


# cpi vs PS - plot
par(mfrow=c(1,1))
data <- cbind(inflation_ps_full, inflation_cpi_full_d)
plot.zoo(data, 
         plot.type = 'single',
         xlab = "Period",
         ylab = "Annual Inflation Rate (%)",
         lwd = 1,
         col=c(2,1),
         cex.lab=1,
         pch=)
legend("top", c('PS Inflation', 'CPI Inflation'), lty = 1, col=c(2,1), nc=2, cex = 1, bty = "n")




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------ CORRELATION MATRIX -------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aux <- cbind(inflation_cpi_full, inflation_ps_agg_full, market_m_annual_full)
colnames(aux) <- c("CPI Inflation", "PS Inflation", "Market")
cor(na.omit(aux))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- PARAMETERS ----------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n = length(inflation_ps_agg_full)
n_test <- round(0.25*n)
# n_test = 10

# removing the last 10 observations 
# inflation_cpi_full <- inflation_cpi_full[1:72]
# inflation_ps_full <- inflation_ps_full[1:2016]
# market_m_annual_full <- market_m_annual_full[1:72]


n_max <- nrow(inflation_cpi_full)
n_train <- n_max - n_test

inflation_cpi_test <- inflation_cpi_full[(n_max - n_test + step):n_max]

date_m_train <- date_m[1:(n_max - n_test)]
date_m_test <- date_m[(n_max - n_test + 1):(n_max)]
date_d_train <- date_d[1:(n_max*28 - n_test*28)]
date_d_test <-date_d[((n_max*28 - n_test*28) + 1):(n_max*28 - (step - 1)*28)]


data_train <- list(y = as.numeric(inflation_cpi_full[date_m_train]),
                   x = as.numeric(inflation_ps_full[date_d_train]),
                   z = as.numeric(market_m_annual_full[date_m_train]),
                   x_agg = as.numeric(inflation_ps_agg_full[date_m_train]),
                   trend = seq(1:nrow(inflation_cpi_full[date_m_train])))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#---------- IN-SAMPLE ------------

#-------------- ARIMA ---
arima <- Arima(data_train$y, c(1, 1, 0), include.constant = TRUE)
summary(arima)
aux_arima_f <- fitted(arima)

checkresiduals(arima)
adf.test(residuals(arima))
accuracy_arima_in <- rmse(ts(inflation_cpi_full[1:n_train]), aux_arima_f)

#-------------- VAR 1 ---

aux <- cbind(na.omit(data_train$y),
             na.omit(data_train$x_agg),
             na.omit(data_train$z),
             na.omit(data_train$trend))
colnames(aux) <- c("y", "x", "z", "trend")
VARselect(aux)


aux_vec <- ts(cbind(na.omit(data_train$y), na.omit(data_train$x_agg)))
vec <- ca.jo(aux_vec, spec = "transitory")
summary(vec)

var_1 <- VAR(aux, 1)
summary(var_1$varresult$y)
aux_var_f <- fitted(var_1)[,1]

par(mfrow=c(1,1))
plot(residuals(var_1)[,1], type = "l")
par(mfrow=c(1,2))
acf(residuals(var_1)[,1]); pacf(residuals(var_1)[,1])
par(mfrow=c(1,1))
adf.test(residuals(var_1)[,1])

accuracy_var_1_in <- rmse(ts(inflation_cpi_full[2:n_train]), aux_var_f)

#-------- BRIDGE EQUATION ---

# testing exogeneity
eq_exo <- lm(y ~ x_agg, data_train)
durbinWatsonTest(eq_exo)

# testing granger causality (H0: no Granger causality)
grangertest(y ~ x_agg, order = 1,  data = data_train)
grangertest(x_agg ~ y, order = 1,  data = data_train)

eqb_1 <- lm(y ~ trend + mls(y, 1, 1) + 
              mls(z, 1, 1) + 
              mls(x_agg, 0, 1), 
            data = data_train)

summary(eqb_1)
aux_eqb_1_f <- fitted(eqb_1)

checkresiduals(eqb_1)
adf.test(residuals(eqb_1))
accuracy_eqb_1_in <- rmse(inflation_cpi_full[2:n_train], aux_eqb_1_f)

# testing long-term relationships 
linearHypothesis(eqb_1, c("mls(y, 1, 1)=1", "mls(z, 1, 1)=0", "mls(x_agg, 0, 1)=0"))



#-------- MIDAS-DL ---

eqm_u <- lm(y ~ trend +
                   mls(z, 1, 1) +
                   mls(x, 0:l, 28),
                 data = data_train)

summary(eqm_u)
aux_eqm_u_f <- fitted(eqm_u)

checkresiduals(eqm_u)
adf.test(residuals(eqm_u)) 
accuracy_eqm_u_in <- rmse(inflation_cpi_full[2:n_train], aux_eqm_u_f)

linearHypothesis(eqm_u, c("z=0", "x1 + x2 + x3 + x4 + x5 + x6 + x7 = 0"))


#-------- MIDAS-AR(1) ---

eqm_ar1 <- lm(y ~ trend +
                     mls(y, 1, 1) +
                     mls(z, 1, 1) +
                     mls(x, 0:l, 28),
                   data = data_train, start = NULL)

summary(eqm_ar1)
aux_eqm_ar1_f <- fitted(eqm_ar1)

checkresiduals(eqm_ar1)
adf.test(residuals(eqm_ar1))
accuracy_eqm_ar1_in <- rmse(inflation_cpi_full[2:n_train], aux_eqm_ar1_f)

linearHypothesis(eqm_ar1, c("y=1", "z=0", "x1 + x2 + x3 + x4 + x5 + x6 + x7 = 0"))



#-------- MIDAS-AR(1)-R ---

eqm_ar1r <- midas_r(y ~ trend +
                      mls(y, 1, 1) +
                      mls(z, 1, 1) +
                      mls(x, 0:l, 28, nealmon),
                    data = data_train, start = list(x = c(-0.1,0.1)))

summary(eqm_ar1r)
aux_eqm_ar1r_f <- fitted(eqm_ar1r)

checkresiduals(eqm_ar1r)
adf.test(residuals(eqm_ar1r)) 
accuracy_eqm_ar1r_in <- cbind(rmse(inflation_cpi_full[2:n_train], aux_eqm_ar1r_f))

linearHypothesis(eqm_ar1r, c("y=1", "z=0", "x1 + x2 = 0"))



#-------- MIDAS-DL non-parametric---

eqm_np <- midas_r_np(y ~ trend +
                       mls(x, 0:l, 28),
                     data = data_train, lambda = NULL)

summary(eqm_np)
aux_eqm_np_f <- fitted(eqm_np)

checkresiduals(eqm_np)
adf.test(residuals(eqm_np)) 
accuracy_eqm_np_in <- cbind(rmse(inflation_cpi_full[1:n_train], aux_eqm_np_f))


coefs = eqm_np$coefficients
r = sum(coefs[3:(3+l)])/(l*sd(data_train$x)/length(data_train$x))
r

####
accuracy_in <- rbind(accuracy_arima_in,
                     accuracy_var_1_in,
                     accuracy_eqb_1_in,
                     accuracy_eqm_u_in,
                     accuracy_eqm_ar1_in,
                     accuracy_eqm_ar1r_in,
                     accuracy_eqm_np_in)

colnames(accuracy_in) <- c("RMSE")
rownames(accuracy_in) <- c("ARIMA(1,1,0)", "VAR(1)", "Bridge Equation",
                           "MIDAS-DL", "MIDAS-ADL", "MIDAS-ADLr",
                           "MIDAS-DLnp")

assign(paste("accuracy_in", l, sep = "_"),
       matrix(rbind(accuracy_arima_in, accuracy_var_1_in, accuracy_eqb_1_in,
                    accuracy_eqm_u_in, accuracy_eqm_ar1_in, accuracy_eqm_ar1r_in, accuracy_eqm_np_in), ncol = 1, nrow = 7,
              dimnames = list(c("ARIMA(1,1,0)", "VAR(1)", "Bridge Equation",
                                "MIDAS-DL", "MIDAS-ADL", "MIDAS-ADLr",
                                "MIDAS-DLnp"), c("RMSE"))
       )
)


stargazer(eqm_u, eqm_ar1, eqm_ar1, eqm_ar1)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#--- FORECASTING NAIVE MODELS ----
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arima_f <- c()
var_1_f <- c()
eqb_1_f <- c()

aux_var_1_rmse <- c()
resids_pvalues_var_1 <- c()

options(warn = -1)

for (h in 0:(n_test - step)) {
  for (i in 1:28) {
    TT = n_max - n_test + h
    
    aux_4 <- inflation_ps_full[1:(TT * 28)] %>% coredata %>% cbind %>% matrix(nrow = 28) %>% colMeans(na.rm = TRUE)
    
    data_train <- list(
      y = as.numeric(inflation_cpi_full[1:TT]),
      x = as.numeric(xts(aux_4, order.by = date_m[1:TT])),
      z = as.numeric(market_m_annual_full[1:TT]),
      trend = seq(1:TT)
    )
    
    
    aux_1 <- Arima(inflation_ps_full[1:(TT * 28 + i)], order = c(1, 1, 0))
    
    if (28 - i + 28 * (step - 1) != 0) {
      aux_2 <- predict(aux_1, (28 - i + 28 * (step - 1)))$pred %>% xts(order.by = date_d[(TT * 28 + i + 1):((TT + 1) * 28 + 28 * (step - 1))])
    } else {
      aux_2 <- c()
    }
    
    aux_3 <- rbind(inflation_ps_full[(TT * 28 + 1):(TT * 28 + i)], aux_2)
    aux_4 <- aux_3 %>% coredata %>% cbind %>% matrix(nrow = 28) %>% colMeans(na.rm = TRUE)
    
    data_test <- list(
      y = as.numeric(rep(NA, step)),
      x = as.numeric(aux_4),
      z = as.numeric(rep(NA, step)),
      trend = seq(1:step)
    )
    
    #------------- ARIMA -------------
    
    arima <- Arima(data_train$y, c(1, 1, 0), include.constant = TRUE)
    aux_arima_f <- forecast(arima, h = step)$mean[step]
    arima_f <- rbind(arima_f, aux_arima_f)
    
    
    #-------------- VAR(1) --------------
    
    aux <- cbind(
      na.omit(data_train$y),
      na.omit(data_train$x),
      na.omit(data_train$z),
      na.omit(data_train$trend)
    )
    colnames(aux) <- c("y", "x", "z", "trend")
    
    
    var_1 <- VAR(aux, p = 1)
    aux_var_f <- predict(var_1, n.ahead = step)$fcst$y[step, 1]
    var_1_f <- rbind(var_1_f, aux_var_f)
    
    aux_var_1_rmse <- rbind(aux_var_1_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], var_1_f))
    assign(paste("aux_var_1_rmse", step, sep = "_"), aux_var_1_rmse)
    
    residuals = xts(residuals(var_1), order.by=date_d_train[2:TT])$y
    aux <- cbind(adf.test(residuals)$p.value, jarque.bera.test(residuals)$p.value, durbinWatsonTest(c(coredata(residuals))))
    resids_pvalues_var_1 <- rbind(resids_pvalues_var_1, aux)
    assign(paste("resids_pvalues_var_1", step, sep = "_"), resids_pvalues_var_1)
    
    #-------- BRIDGE EQUATION --------
    eqb_1 <- midas_r(y ~ -1 + 
                       mls(y, step, 1) +
                       mls(z, step:(step+1), 1) +
                       mls(x, c(0,3), 1),
                     data = data_train,
                     start = NULL
    )
    
    
    aux_eqb_1_f <- forecast(eqb_1, newdata = data_test)$mean[step]
    eqb_1_f <- rbind(eqb_1_f, aux_eqb_1_f)
  }
}

iaux_var_1_rmse_1 = xts(aux_var_1_rmse_1, order.by = date_d_test)
aux_var_1_rmse_2 = xts(aux_var_1_rmse_2, order.by = date_d_test)
aux_var_1_rmse_3 = xts(aux_var_1_rmse_3, order.by = date_d_test)


options(warn = 1)

# AVERAGE FORECAST

arima_f_avg <- colMeans(matrix(arima_f, 28), na.rm = TRUE)
var_1_f_avg <- colMeans(matrix(var_1_f, 28), na.rm = TRUE)
eqb_1_f_avg <- colMeans(matrix(eqb_1_f, 28), na.rm = TRUE)



accuracy_arima <- c(rmse(inflation_cpi_test, arima_f_avg))
loss_arima <- LossLevel( inflation_cpi_test, arima_f_avg)

accuracy_var_1 <- c(rmse(inflation_cpi_test, var_1_f_avg))
loss_var_1 <- LossLevel(inflation_cpi_test, var_1_f_avg)

accuracy_eqb_1 <- c(rmse(inflation_cpi_test, eqb_1_f_avg))
loss_eqb_1 <- LossLevel(inflation_cpi_test, eqb_1_f_avg)


assign(paste("accuracy_naive", l, sep = "_"),
       matrix(rbind(accuracy_arima, accuracy_var_1, accuracy_eqb_1), ncol = 1, nrow = 3,
              dimnames = list(c("ARIMA", "VAR(1)", "Bridge Equation"), c("RMSFE"))
       )
)


# MOST RECENT FORECAST

arima_f_last <- matrix(arima_f, 28)[28, ]
var_1_f_last <- matrix(var_1_f, 28)[28, ]
eqb_1_f_last <- matrix(eqb_1_f, 28)[28, ]

accuracy_arima <- rmse(inflation_cpi_test, arima_f_last)
accuracy_var_1 <-rmse(inflation_cpi_test, var_1_f_last)
accuracy_eqb_1 <- rmse(inflation_cpi_test, eqb_1_f_last)

assign(paste("accuracy_naive_point", l, sep = "_"), 
       matrix(rbind(accuracy_arima, accuracy_var_1, accuracy_eqb_1), ncol = 1, nrow = 3,
              dimnames = list(c("ARIMA", "VAR(1)", "Bridge Equation"), c("RMSFE"))
       )
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------ FORECASTING MIDAS --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eqm_u_f <- c()
eqm_ar1_f <- c()
eqm_ar1r_f <- c()
eqm_np_f <- c()

aux_eqm_u_rmse <- c()
aux_eqm_ar1_rmse <- c()
aux_eqm_ar1r_rmse <- c()
aux_eqm_np_rmse <- c()

resids_pvalues_ar1 <- c()
resids_pvalues_ar1r <-c()

options(warn = -1)

for (h in 0:(n_test - step)) {
  for (i in 1:28) {
    TT = n_max - n_test + h
    
    data_train <- list(
      y = as.numeric(inflation_cpi_full[1:TT]),
      x = as.numeric(inflation_ps_full[1:(TT * 28)]),
      z = as.numeric(market_m_annual_full[1:TT]),
      trend = seq(1:TT)
    )
    
    
    aux_1 <- Arima(inflation_ps_full[1:(TT * 28 + i)], order = c(1, 1, 0), include.constant = TRUE)
    
    if (28 - i + 28 * (step-1) != 0) { 
      aux_2 <- xts(forecast(aux_1, h = (28-i+28*(step-1)))$mean,
                   order.by = date_d[(TT*28+i+1):((TT+1)*28+28*(step-1))])
    } else {
      aux_2 <- c()
    }
    
    aux_3 <- rbind(inflation_ps_full[(TT*28+1):(TT*28+i)], aux_2)
    
    data_test <- list(
      y = as.numeric(rep(NA, step)),
      x = as.numeric(aux_3),
      z = as.numeric(rep(NA, step)),
      trend = seq(1:step)
    )
    
    
    
    #-------- MIDAS-DL --------
    
    eqm_u <- midas_r(y ~ trend +
                       mls(z, step, 1) +
                       mls(x, 0:l, 28),
                     data = data_train,
                     start = NULL)
    
    aux_eqm_u_f <- forecast(eqm_u, newdata = data_test)$mean[step]
    eqm_u_f <- rbind(eqm_u_f, aux_eqm_u_f)
    
    aux_eqm_u_rmse <- rbind(aux_eqm_u_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_u_f))
    assign(paste("aux_eqm_u_rmse", step, sep = "_"), aux_eqm_ar1_rmse)
    
    
    
    #-------- MIDAS-AR(1) --------
    #l=13
    eqm_ar1 <- midas_r(y ~ trend +
                         mls(y, step, 1) +
                         mls(z, step, 1) +
                         mls(x, 0:l, 28),
                       data = data_train,
                       start = NULL)
    
    aux_eqm_ar1_f <- forecast(eqm_ar1, newdata = data_test)$mean[step]
    eqm_ar1_f <- rbind(eqm_ar1_f, aux_eqm_ar1_f)
    
    aux_eqm_ar1_rmse <- rbind(aux_eqm_ar1_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_ar1_f))
    assign(paste("aux_eqm_ar1_rmse", step, sep = "_"), aux_eqm_ar1_rmse)
    
    aux <- cbind(adf.test(eqm_ar1$residuals)$p.value, jarque.bera.test(eqm_ar1$residuals)$p.value, durbinWatsonTest(eqm_ar1$residuals))
    resids_pvalues_ar1 <- rbind(resids_pvalues_ar1, aux)
    assign(paste("resids_pvalues_ar1", step, sep = "_"), resids_pvalues_ar1)
    
    #-------- MIDAS-AR(1)-R --------
    #l=6
    eqm_ar1r <- midas_r(
      y ~ trend +
        mls(y, step, 1) +
        mls(z, step, 1) +
        mls(x, 0:l, 28, nealmon),
      data = data_train,
      start = list(x = c(0, 0))
    )
    
    aux_eqm_ar1r_f <- forecast(eqm_ar1r, newdata = data_test)$mean[step]
    eqm_ar1r_f <- rbind(eqm_ar1r_f, aux_eqm_ar1r_f)
    
    aux_eqm_ar1r_rmse <- rbind(aux_eqm_ar1r_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], eqm_ar1r_f))
    assign(paste("aux_eqm_ar1r_rmse", step, sep = "_"), aux_eqm_ar1r_rmse)
    
    aux <- cbind(adf.test(eqm_ar1r$residuals)$p.value, jarque.bera.test(eqm_ar1r$residuals)$p.value, durbinWatsonTest(eqm_ar1r$residuals))
    resids_pvalues_ar1r <- rbind(resids_pvalues_ar1r, aux)
    assign(paste("resids_pvalues_ar1r", step, sep = "_"), resids_pvalues_ar1r)
    
    
    #-------- MIDAS-DL non-parametric--------
    
    eqm_np <- midas_r_np(y ~ trend +
                           mls(x, 0:l, 28),
                         data = data_train,
                         lambda = NULL)
    
    aux_eqm_np_f <- forecast(eqm_np, newdata = data_test)$mean[step]
    eqm_np_f <- rbind(eqm_np_f, aux_eqm_np_f)
    
    aux_eqm_np_rmse <- rbind(aux_eqm_np_rmse, rmse(inflation_cpi_full_d[(((n_max-n_test)+step-1)*28+1):((TT+step-1)*28+i)], aux_eqm_np_f))
    assign(paste("aux_eqm_np_rmse", step, sep = "_"), aux_eqm_ar1r_rmse)
    
    
  }
}


aux_eqm_ar1_rmse_1 = xts(aux_eqm_ar1_rmse_1, order.by = date_d_test)
aux_eqm_ar1r_rmse_1 = xts(aux_eqm_ar1r_rmse_1, order.by = date_d_test)

aux_eqm_ar1_rmse_2 = xts(aux_eqm_ar1_rmse_2, order.by = date_d_test)
aux_eqm_ar1r_rmse_2 = xts(aux_eqm_ar1r_rmse_2, order.by = date_d_test)

aux_eqm_ar1_rmse_3 = xts(aux_eqm_ar1_rmse_3, order.by = date_d_test)
aux_eqm_ar1r_rmse_3 = xts(aux_eqm_ar1r_rmse_3, order.by = date_d_test)



options(warn = 1)

# testing for heteroskedasticity
# ar1_resdid_eqm_ar1 <- lm(resids_eqm_ar1[[2]]~ c(NA,resids_eqm_ar1[[2]][-1]))
# bptest(ar1_resdid_eqm_ar1)




# AVERAGE FORECAST

eqm_u_f_avg <- colMeans(matrix(as.numeric(eqm_u_f), 28), na.rm = TRUE)
eqm_ar1_f_avg <- colMeans(matrix(as.numeric(eqm_ar1_f), 28), na.rm = TRUE)
eqm_ar1r_f_avg <- colMeans(matrix(as.numeric(eqm_ar1r_f), 28), na.rm = TRUE)
eqm_np_f_avg <- colMeans(matrix(as.numeric(eqm_np_f), 28), na.rm = TRUE)


accuracy_eqm_u <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_avg)
loss_eqm_u <- LossLevel(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_avg)
assign(paste("loss_eqm_u", l, sep = "_"), loss_eqm_u)

accuracy_eqm_ar1 <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_avg)
loss_eqm_ar1 <- LossLevel(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_avg)
assign(paste("loss_eqm_ar1", l, sep = "_"), loss_eqm_ar1)

accuracy_eqm_ar1r <- rmse(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_ar1r_f_avg)
loss_eqm_ar1r <- LossLevel(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_ar1r_f_avg)
assign(paste("loss_eqm_ar1r", l, sep = "_"), loss_eqm_ar1r)

accuracy_eqm_np <- rmse(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_np_f_avg)
loss_eqm_np <- LossLevel(inflation_cpi_full[( n_max- n_test + step):n_max], eqm_np_f_avg)
assign(paste("loss_eqm_np", l, sep = "_"), loss_eqm_np)


assign(paste("accuracy_midas", l, sep = "_"),
       matrix(rbind(accuracy_eqm_u, accuracy_eqm_ar1, accuracy_eqm_ar1r, accuracy_eqm_np), ncol = 1, nrow = 4,
              dimnames = list(c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP"), c("RMSFE")
              )
       )
)


# MOST RECENT FORECAST
eqm_u_f_last <- matrix(as.numeric(eqm_u_f), 28)[28, ]
eqm_ar1_f_last <- matrix(as.numeric(eqm_ar1_f), 28)[28, ]
eqm_ar1r_f_last <- matrix(as.numeric(eqm_ar1r_f), 28)[28, ]
eqm_np_f_last <- matrix(as.numeric(eqm_np_f), 28)[28, ][1:(n_test - step + 1)]

accuracy_eqm_u <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_u_f_last)
accuracy_eqm_ar1 <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1_f_last)
accuracy_eqm_ar1r <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_ar1r_f_last)
accuracy_eqm_np <- rmse(inflation_cpi_full[(n_max- n_test + step):n_max], eqm_np_f_last)

assign(paste("accuracy_midas_point", l, sep = "_"),
       matrix(rbind(accuracy_eqm_u, accuracy_eqm_ar1, accuracy_eqm_ar1r, accuracy_eqm_np), ncol = 1, nrow = 4, 
              dimnames = list(c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP"),c("RMSFE")
              )
       )
)


# intra-period forecasts
eqm_u_f <- xts(eqm_u_f, order.by = date_d_test)
eqm_ar1_f <- xts(eqm_ar1_f, order.by = date_d_test)
eqm_ar1r_f <- xts(eqm_ar1r_f, order.by = date_d_test)
eqm_np_f <- xts(eqm_np_f, order.by = date_d_test)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#--------- PLOT RMSFE ------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
par(mfrow=c(1,3))
data = c()
data = cbind(aux_var_1_rmse_1, aux_eqm_ar1_rmse_1, aux_eqm_ar1r_rmse_1)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('bottomright', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=1')

data = cbind(aux_var_1_rmse_2, aux_eqm_ar1_rmse_2, aux_eqm_ar1r_rmse_2)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('bottomright', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=2')

data = cbind(aux_var_1_rmse_3, aux_eqm_ar1_rmse_3, aux_eqm_ar1r_rmse_3)
plot.zoo(data, plot.type = 'single', col = c(1,2,4), lwd=1, xlab='Period', ylab='RSMFE')
legend('topleft', c('VAR (1)', 'MIDAS-ADL (l=13)', 'MIDAS-ADLr (l=6)'), lty = 1, col=c(1,2,4), nc=1, cex = 1, bty = "n")
title('h=3')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------- COMPARING MODELS --------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# MODEL CONFIDENCE SET
aux <- cbind(loss_arima, loss_var_1, loss_eqb_1, loss_eqm_u_6, loss_eqm_ar1_6, loss_eqm_ar1r_6, loss_eqm_np_6,
             loss_eqm_u_13, loss_eqm_ar1_13, loss_eqm_ar1r_13, loss_eqm_np_13,  loss_eqm_u_27, loss_eqm_ar1_27, 
             loss_eqm_ar1r_27, loss_eqm_np_27)
colnames(aux) <- cbind('loss_arima', 'loss_var_1', 'loss_eqb_1', 'loss_eqm_u_6', 'loss_eqm_ar1_6', 'loss_eqm_ar1r_6', 'loss_eqm_np_6',
                       'loss_eqm_u_13', 'loss_eqm_ar1_13', 'loss_eqm_ar1r_13', 'loss_eqm_np_13',  'loss_eqm_u_27', 'loss_eqm_ar1_27', 
                       'loss_eqm_ar1r_27', 'loss_eqm_np_27')
MCSprocedure(aux)

# DIEBOLD E MARIANO
error_eqb_1 <- eqb_1_f_avg - inflation_cpi_test
error_var_1 <- var_1_f_avg - inflation_cpi_test
error_eqm_ar1 <- eqm_ar1_f_avg - inflation_cpi_test
error_eqm_ar1r <- eqm_ar1r_f_avg - inflation_cpi_test

dm.test(error_var_1, error_eqm_ar1r)


# intra period forecasts
color = c("orange", "blue", "violetred2", "green", "black")
plot.zoo(cbind(eqm_u_f, eqm_ar1_f, eqm_ar1r_f, eqm_np_f,
               inflation_cpi_full_d[date_d_test]),
         plot.type = "single",
         col = color, lwd = c(1,1,1),
         ylab = "Annual Inflation (%)", xlab = "Period")
legend("topleft", inset=c(0,0), y.intersp = 1,
       legend = c("MIDAS", "MIDAS-AR(1)", "MIDAS-AR(1)R", "MIDAS-NP", "Observed"),
       lty = 1, bty = "n", col = color, cex = 0.8)
title("intra-month forecasts")




par(mfrow=c(1,3))

plot.zoo(cbind(aux_var_1_rmse_1, aux_eqm_ar1_rmse_1), main="1-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)

plot.zoo(cbind(aux_var_1_rmse_2, aux_eqm_ar1_rmse_2), main="2-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)

plot.zoo(cbind(aux_var_1_rmse_3, aux_eqm_ar1_rmse_3), main="3-step ahead", plot.type = "single",
         col =c("dodgerblue", "violetred2"), ylab = "", xlab = "",  cex.axis=1.5, cex.main=1.5)
