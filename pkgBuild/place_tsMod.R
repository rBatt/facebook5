
# =================
# = Load Packages =
# =================
library(data.table)
library(bit64)
library(battFb5)
library(Metrics)
library(rbLib)
library(forecast)
library(WaveletComp)


# ============
# = Get a DT =
# ============
# play_train <- (battFb5::train)
# play_train[,c("Hour","Day"):=list(time%/%60, time%/%1440)]
#
# play_test <- (battFb5::test)
# play_test[,c("Hour","Day"):=list(time%/%60, time%/%1440)]


# =================================
# = Analyze Aggregate Time series =
# =================================
# u_hr <- play_train[,unique(Hour)]
# u_day <- play_train[,unique(Day)]
# play_agg <- play_train[,.N,by=c("Hour")]
#
# # ---- time series ----
# plot(play_agg, type='l')
#
# # ---- wavelet ----
# aw <- analyze.wavelet(play_agg)
# wt.image(aw)
#
# # ---- define msts ----
# play_msts <- msts(play_agg[,N], seasonal.periods=c(24, 24*7, 24*7*4))
#
# # ---- fourier/ tslm ----
# play_four <- fourier(play_msts, K=c(1,6,1))
# play_preds <- cbind(hr=play_agg[,Hour], play_four)
# play_tslm <- tslm(play_msts~play_preds)
# n_fore <- 24*7*4*2
# play_preds_tslm <- cbind(hr=play_test[,sort(unique(Hour))[1:n_fore]]  ,fourier(play_msts, K=c(1,6,1), h=n_fore))
# plot(forecast(play_tslm, data.frame(play_preds_tslm)))
#
# # ---- tbats ----
# play_tbats <- tbats(play_msts)
# plot(play_tbats)
# plot(forecast(play_tbats, 1E3))


# ===================
# = Analyze 1 Place =
# ===================
# play_set <- merge(play_train[place_id=="8772469670"], data.table(Hour=u_hr), by="Hour", all=TRUE)[,list(N=sum(!is.na(x))),by="Hour"]
#
# # ---- Time series ----
# plot(play_set, type='l')
#
# # ---- wavelet ----
# aw_set <- analyze.wavelet(play_set)
# wt.image(aw_set)
#
# # ---- define msts ----
# play_set_msts <- msts(play_set[,N], seasonal.periods=c(24, 24*7, 24*7*4, 24*365))
#
# # ---- fourier/ tslm ----
# play_set_four <- fourier(play_set_msts, K=c(1,6,1,1))
# play_set_preds <- cbind(hr=play_set[,Hour], play_set_four)
# play_set_tslm <- tslm(play_set_msts~play_set_preds)
# n_fore <- 24*7*4*2
# play_set_preds_tslm <- cbind(hr=play_test[,sort(unique(Hour))[1:n_fore]]  ,fourier(play_set_msts, K=c(1,6,1,1), h=n_fore))
# plot(forecast(play_set_tslm, data.frame(play_set_preds_tslm)))
#
# # ---- glm ----
# play_set_glm <- glm(N~., data=data.frame(N=play_set_msts, play_set_preds), family="poisson")
# play_set_preds_glm <- cbind(hr=play_test[,sort(unique(Hour))[1:n_fore]]  ,fourier(play_set_msts, K=c(1,6,1,1), h=n_fore))
# plot(predict(play_set_glm, newdata=data.frame(play_set_preds_glm), type="response"), type='l')
#
#
# # ---- tbats ----
# play_set_tbats <- tbats(play_set_msts)
# plot(play_set_tbats)
# plot(forecast(play_set_tbats, 1E3))



# ==================================
# = Fit an ARIMA to that palce at  =
# ==================================
ts_predict <- function(X, Y, x_n_hour, ts_frac_start=0.5){
	
	# ---- Ensure regular time series via merge ----
	time_skele <- x_n_hour[,list(Hour)]
	
	x_set_full <- merge(X, time_skele, by="Hour", all=TRUE)[,list(N=sum(!is.na(x))),by="Hour"]
	x_set <- x_set_full[Hour>quantile(x_set_full[,Hour],ts_frac_start)]
	x_n_hour2 <- x_n_hour[Hour>=x_set[,min(Hour)]]
	x_set_msts <- msts(x_set[,N], seasonal.periods=c(24, 24*7))
	
	
	y_set <- Y[,list(n_tot=.N),by="Hour"]
	y_hr <- Y[,sort(unique(Hour))]
	n_fore <- length(y_hr)
	
	if(sum(x_set_msts>0)<50){
		return(rep(0, n_fore))
	}
	
	x_set_four <- fourier(x_set_msts, K=c(3,3))
	x_set_preds <- data.table(hr=x_set[,Hour], n_tot=x_n_hour2[,n_tot], x_set_four)	
	
	x_set_mod <- glm(N~., data=data.frame(N=x_set_msts, x_set_preds), family="poisson")
	# x_set_mod <- auto.arima(x_set_msts, xreg=data.frame(x_set_preds), seasonal=FALSE)
	# x_set_mod <- tbats(x_set_msts, use.trend=TRUE, use.damped.trend=TRUE, use.box.cox=TRUE)
	
	x_set_preds_fore <- data.frame(hr=y_set[,Hour], n_tot=y_set[,n_tot], fourier(x_set_msts, K=c(3,3), h=n_fore))
	
	x_forecast <- unname(predict(x_set_mod, newdata=data.frame(x_set_preds_fore), type="response"))
	# x_forecast <- forecast(x_set_mod, xreg=data.frame(x_set_preds_fore))$mean
	
	par(mfrow=c(2,2))
	plot(x_set_full, type='l')
	abline(v=x_set[,min(Hour)], col='blue')
	plot(fitted(x_set_mod), type='l', col='red')
	plot(residuals(x_set_mod), type='l')
	plot(x_forecast, type='l')
	
	return(x_forecast)
	
}

# play_train[,names(sort(table(place_id), dec=T, partial=1:10))[1:10]]
play_train[,names(sort(table(place_id), dec=F, partial=1:1E3))[990:1000]]
# "8772469670" "1623394281" "1308450003" "4823777529" "9586338177" "9129780742" "9544215131" "4638096372" "5351837004" "8610202964" == top 10
# "7445190392" "7498749584" "7663770365" "7863750419" "7933330073" "8216243815" "8328906659" "8341850327" "8365888303" "8410957884" "8628643901" == 990-1000
# 1014661751 1008823061 1054647709 1037530880 2496385414 2260673088
#         50        100        250        500        750       1000

eg_id <- c("8772469670", "1623394281", "1308450003", "4823777529", "9586338177", "9129780742", "9544215131", "4638096372", "5351837004", "8610202964", "7445190392", "7498749584", "7663770365", "7863750419", "7933330073", "8216243815", "8328906659", "8341850327", "8365888303", "8410957884", "8628643901", "1014661751", "1008823061", "1054647709", "1037530880", "2496385414", "2260673088")

# X = play_train[place_id=="8610202964"]
# Y = play_test[,list(row_id,Hour)]
# x_n_hour = play_train[,list(n_tot=.N),by="Hour"]

x_n_hour = play_train[,list(n_tot=.N),by="Hour"]
Y = play_test[,list(row_id,Hour)]
for(eg in 1:length(eg_id)){
	ts_predict(X = play_train[place_id==eg_id[eg]], Y, x_n_hour)
	# readline()
}





