
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
play_train <- (battFb5::train)
play_train[,c("Hour","Day"):=list(time%/%60, time%/%1440)]
setkey(play_train, row_id, place_id, time)

play_test <- (battFb5::test)
play_test[,c("Hour","Day"):=list(time%/%60, time%/%1440)]


# ==================================
# = Fit an ARIMA to that palce at  =
# ==================================
ts_predict <- function(y_set, x_n_hour, time_skele, y_hr, nX){
	
	# ---- Ensure regular time series via merge ----
	# time_skele <- x_n_hour[,list(Hour)]
	# setkey(time_skele)
	# setkey(X, Hour)
	
	# x_set_full <- data.table:::merge.data.table(X, time_skele, by="Hour", all=TRUE)[,list(N=sum(!is.na(x))),by="Hour"]
	# nX <- X[,.N,keyby="Hour"]
	x_set <- nX[time_skele, on="Hour"]
	x_set[is.na(N), N:=0L]
	# x_set_full <- X[time_skele, on=c("Hour")][,list(N=sum(!is.na(x))),by="Hour"]
	# x_set <- x_set_full[Hour>quantile(x_set_full[,Hour],ts_frac_start)]
	# x_n_hour2 <- x_n_hour[Hour>=x_set[,min(Hour)]]
	x_set_msts <- msts(x_set[,N], seasonal.periods=c(24, 24*7))
	
	
	# y_set <- Y[,list(n_tot=.N),by="Hour"]
	# y_hr <- y_set[,sort(unique(Hour))]
	n_fore <- length(y_hr)
	
	if(sum(x_set_msts>0)<50){
		# return(rep(0, n_fore))
		return(list(Hour=y_set[,Hour],ts_forecast=rep(NA, n_fore)))
	}
	
	x_set_four <- fourier(x_set_msts, K=c(3,2))
	x_set_preds <- data.frame(N=x_set_msts, hr=x_set[,Hour], n_tot=x_n_hour[,n_tot], x_set_four)	
	
	x_set_mod <- glm(N~., data=x_set_preds, family="poisson")
	# x_set_mod <- auto.arima(x_set_msts, xreg=data.frame(x_set_preds), seasonal=FALSE)
	# x_set_mod <- tbats(x_set_msts, use.trend=TRUE, use.damped.trend=TRUE, use.box.cox=TRUE)
	
	x_set_preds_fore <- data.frame(hr=y_set[,Hour], n_tot=y_set[,n_tot], fourier(x_set_msts, K=c(3,2), h=n_fore))
	
	x_forecast <- unname(predict(x_set_mod, newdata=x_set_preds_fore, type="response"))
	# x_forecast <- forecast(x_set_mod, xreg=data.frame(x_set_preds_fore))$mean
	
	# par(mfrow=c(2,2))
	# plot(x_set_full, type='l')
	# abline(v=x_set[,min(Hour)], col='blue')
	# plot(fitted(x_set_mod), type='l', col='red')
	# plot(residuals(x_set_mod), type='l')
	# plot(x_forecast, type='l')
	
	# return(x_forecast)
	return(list(Hour=y_set[,Hour], ts_forecast=x_forecast))
	
}

# play_train[,names(sort(table(place_id), dec=T, partial=1:10))[1:10]]
# play_train[,names(sort(table(place_id), dec=F, partial=1:1E3))[990:1000]] 
# "8772469670" "1623394281" "1308450003" "4823777529" "9586338177" "9129780742" "9544215131" "4638096372" "5351837004" "8610202964" == top 10
# "7445190392" "7498749584" "7663770365" "7863750419" "7933330073" "8216243815" "8328906659" "8341850327" "8365888303" "8410957884" "8628643901" == 990-1000
# 1014661751 1008823061 1054647709 1037530880 2496385414 2260673088
#         50        100        250        500        750       1000

eg_id <- c("8772469670", "1623394281", "1308450003", "4823777529", "9586338177", "9129780742", "9544215131", "4638096372", "5351837004", "8610202964", "7445190392", "7498749584", "7663770365", "7863750419", "7933330073", "8216243815", "8328906659", "8341850327", "8365888303", "8410957884", "8628643901", "1014661751", "1008823061", "1054647709", "1037530880", "2496385414", "2260673088")
eg_id <- data.table(place_id=as.integer64(eg_id))

# X = play_train[place_id=="8610202964"]
# Y = play_test[,list(row_id,Hour)]
# x_n_hour = play_train[,list(n_tot=.N),by="Hour"]

# X_play <- play_train[place_id%in%eg_id]
# X_play <- play_train[data.table(place_id=as.integer64(eg_id)), on="place_id"]
X_play <- play_train[eg_id, on="place_id"]

Y_set <- play_test[,list(row_id,Hour)][,list(n_tot=.N),by="Hour"]
x_n_hour <- play_train[,list(n_tot=.N),by="Hour"]
x_n_hour <- x_n_hour[Hour>mean(range(Hour))] # this makes it so only 2nd half of time series is used
time_skele <- x_n_hour[,list(Hour=do.call('seq', as.list(range(Hour))))] #x_n_hour[,list(Hour)]
setkey(time_skele, Hour)
y_hr <- Y_set[,sort(unique(Hour))]


nX_all <- play_train[,.N,keyby=c("place_id","Hour")]
# x_set_full <- nX[, .SD[time_skele, on="Hour"], by="place_id"]
# x_set_full[is.na(N), N:=0L]

doParallel::registerDoParallel(cores=6)
Rprof()
# ts_forecasts <- list()
# for(eg in 1:nrow(eg_id)){
ts_forecasts <- foreach::foreach(eg = 1:nrow(eg_id), .combine=rbind) %dopar% {
	data.table(eg_id[eg], as.data.table(ts_predict(y_set=Y_set, x_n_hour, time_skele, y_hr, nX=nX_all[eg_id[eg], on="place_id"])))
}
	# ts_forecasts[[eg]] <- 
	# readline()
# }
Rprof(NULL)
print(prof.tree::prof.tree(), limit=50)


Rprof()
data.table(place_id=eg_id[eg], as.data.table(ts_predict(y_set=Y_set, x_n_hour, time_skele, y_hr, nX=nX_all[eg_id[eg], on="place_id"])))
Rprof(NULL)
summaryRprof()
print(prof.tree(), limit=50)

