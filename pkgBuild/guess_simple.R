
# =================
# = Load Packages =
# =================
library(data.table)
library(bit64)
library(battFb5)
library(Metrics)
library(rbLib)
library(h2o)
library(caret)
library(xgboost)
library(ks)


# ===========
# = Options =
# ===========
use_old <- TRUE


# =====================
# = Load R Data Files =
# =====================
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")



# =========================
# = Try some other models =
# =========================

# play <- train_full[1:1E5]
# train_ind <- sample(1:nrow(play), 0.75*nrow(play))
# pTrain <- play[train_ind,list(x,y,Day)]
# pTrainTarg <- play[train_ind, place_id]
# pValid <- play[-train_ind, list(x,y,Day)]
# pValidTarg <- play[-train_ind, place_id]

# --- vbmp never even finished
# source("https://bioconductor.org/biocLite.R")
# biocLite("vbmp")
# pTheta <- rep(1, ncol(pTrain))
# vout <- vbmp(pTrain, t.class=pTrainTarg, X.TEST=pValid, t.class.TEST=pValidTarg, theta=pTheta)


# tr_lda <- train(play[,list(x,y)], y=play[,factor(place_id)], method="rFerns", tuneLength=1, trControl=trainControl(method="none",search='random', returnData=FALSE)) # slow, but ran on small set

# xgb_params <- list(
# 	objective="multi:softprob",
# 	num_class=length(unique(pTrainTarg)),
# 	eval_metric = "merror"
# )
#
#
# xgb_scaling <- function(x, y, nK){
#
# 	y <- as.character(y)
#
# 	u_targs <- unique((y))
# 	targ_ind <- u_targs %in% sample(u_targs, nK)
#
# 	xgb_params <- list(
# 		objective="multi:softprob",
# 		num_class=length(u_targs),
# 		eval_metric = "merror"
# 	)
#
# 	y_sub <- as.integer(factor(y[targ_ind])) - 1
# 	x_sub <- x[(targ_ind)]
#
# 	xg_time <- system.time({xg_out <- xgboost(as.matrix(x_sub), y_sub, nrounds=3, params=xgb_params, verbose=0)})
#
# 	return(xg_time[3])
#
# }
#
# xgb_scaling(pTrain, pTrainTarg, 100)
#
# xgb_results <- data.table(nK=seq(10, 500, by=50))
# xgb_results[,xTime:=xgb_scaling(pTrain,pTrainTarg,nK[1]),by="nK"]
#
#
# tr_xgb <- xgboost(as.matrix(pTrain), as.integer(factor(pTrainTarg))-1, nrounds=10, params=xgb_params, verbose=1)
#
# tr_xgb_y_pred <- predict(tr_xgb, as.matrix(test_full[1:100,list(x,y,Day)]))
# tr_xgb_pred_full <- matrix(tr_xgb_y_pred, ncol=length(unique(pTrainTarg)), byrow=TRUE)
# colnames(tr_xgb_pred_full) <- unique(as.character(pTrainTarg))
# top_preds <- function(preds, k=3){
# 	np <- colnames(preds)
# 	nc_x <- ncol(preds)
# 	k <- 3
# 	q_rank <- function(x){np[sapply(sort(as.numeric(x), index.return=TRUE), `[`, nc_x:(nc_x-k+1))[,-1]]}
# 	top_k <- apply(preds, 1, q_rank)
# 	# top_k <- gsub("p", "", top_k)
# 	top_k_form <- apply(top_k, 2, paste, collapse = " ")
# 	return(top_k_form)
# }
# tr_xgb_pred <- top_preds(tr_xgb_pred_full)




# ===============================================
# = Figure out frequency distribution of places =
# ===============================================
# pt <- train_full[,prop.table(table(place_id))]
#
# seq(1E-7,5E-5,length.out=1E3)
# pt_cum <- sapply(seq(1E-7,5E-5,length.out=1E3), function(x)sum(pt>x))


#
#
# # ===============================================
# # = Figure out frequency distribution of places =
# # ===============================================
# pt <- train_full[,prop.table(table(place_id))]
# pt_grad <- seq(1E-7,5E-5,length.out=1E3)
# pt_n <- sapply(pt_grad, function(x)sum(pt>x))
# pt_prop <- sapply(pt_grad, function(x)sum(pt[pt>x]))
#
# plot(pt_grad, pt_n, type="l")
# par(new=TRUE)
# plot(pt_grad, pt_prop, xaxt="n",yaxt="n", xlab="", ylab="", type="l")
# axis(side=4)
#
#
#
# train_full[,c("rg_x","rg_y"):=list(cut(x,2), cut(y,10))]
# train_full[,c("xy_big"):=list(paste(rg_x, rg_y))]
# train_full[,c("rg_x","rg_y"):=NULL]
#
# # n_groups <- train_full[,lu(xy_big)]
# par(mfrow=c(9,9), mar=c(3,3,1,1), cex=1, ps=9, mgp=c(1,0.5,0), tcl=-0.1)
# train_full[,j={
#
# 	pt <- prop.table(table(place_id))
# 	pt_grad <- seq(5E-4,1E-2,length.out=1E3)
# 	pt_n <- sapply(pt_grad, function(x)sum(pt>x))
# 	pt_prop <- sapply(pt_grad, function(x)sum(pt[pt>x]))
#
#
#
# 	plot(pt_grad, pt_n, type="l", main=paste(xy_big[1], Part_of_Day[1],sep=", "))
# 	abline(h=100, col="red", lwd=0.5)
#
# 	par(new=TRUE)
# 	plot(pt_grad, pt_prop, xaxt="n",yaxt="n", xlab="", ylab="", type="l")
# 	axis(side=4)
# 	abline(h=0.75/81, col="blue")
#
#
# },by=c("xy_big","Part_of_Day")]



# # ===============================================
# # = Figure out frequency distribution of places =
# # ===============================================
# pt <- train_full[,prop.table(table(place_id))]
# pt_grad <- seq(1E-7,5E-5,length.out=1E3)
# pt_n <- sapply(pt_grad, function(x)sum(pt>x))
# pt_prop <- sapply(pt_grad, function(x)sum(pt[pt>x]))
#
# plot(pt_grad, pt_n, type="l")
# par(new=TRUE)
# plot(pt_grad, pt_prop, xaxt="n",yaxt="n", xlab="", ylab="", type="l")
# axis(side=4)
#
#
#
# train_full[,c("rg_x","rg_y"):=list(cut(x,4), cut(y,20))]
# train_full[,c("xy_big"):=list(paste(rg_x, rg_y))]
# train_full[,c("rg_x","rg_y"):=NULL]
#
# # n_groups <- train_full[,lu(xy_big)]
# par(mfrow=c(9,9), mar=c(3,3,1,1), cex=1, ps=9, mgp=c(1,0.5,0), tcl=-0.1)
# train_full[Day>0.8,j={
#
# 	pt <- prop.table(table(place_id))
# 	pt_grad <- seq(1E-4,1E-2,length.out=1E3)
# 	pt_n <- sapply(pt_grad, function(x)sum(pt>x))
# 	pt_prop <- sapply(pt_grad, function(x)sum(pt[pt>x]))
#
#
#
# 	# plot(pt_grad, pt_n, type="l", main=paste(xy_big[1], Part_of_Day[1],sep=", "))
# 	# abline(h=100, col="red", lwd=0.5)
# 	#
# 	# par(new=TRUE)
# 	# plot(pt_grad, pt_prop, xaxt="n",yaxt="n", xlab="", ylab="", type="l")
# 	# axis(side=4)
# 	# abline(h=0.75/81, col="blue")
#
# 	plot(pt_prop, pt_n, type="l")
# 	abline(h=200, col="red")
# 	abline(v=0.75, col="blue")
#
#
# },by=c("xy_big")]







# ===========================================================================================
# = Do a K-D Density Estimate to Estimate Probability of Each place_id for Each Observation =
# ===========================================================================================
# tr_xmin <- play[,c(min(x),min(y),min(Day_of_Week),min(Hour_of_Day))]
# tr_xmax <- play[,c(max(x),max(y),max(Day_of_Week),max(Hour_of_Day))]
# tr_xmin <- train_full[,c(min(x),min(y),min(Hour_of_Day))]
# tr_xmax <- train_full[,c(max(x),max(y),max(Hour_of_Day))]
#
# setkey(train_full, place_id, time)
# pop1_ind <- train_full[,place_id==train_full[,names(sort(table(place_id),dec=TRUE))[200]]]
# play_kde <- train_full[pop1_ind, ks::kde(.SD[,list(x,y)], w=acc_score)]
#
# par(mfrow=c(1,1), mar=c(1,1,0.5,0.1), ps=6, mgp=c(0.5,0.1,0), tcl=-0.01, cex=1)
# p1_split <- cut(1:sum(pop1_ind), 300)
# vline <- train_full[pop1_ind, mean(x)]
# hline <- train_full[pop1_ind, mean(y)]
# xlim <- train_full[pop1_ind, quantile(x, c(0.01,0.975))]
# ylim <- train_full[pop1_ind, quantile(y, c(0.01,0.99))]
# for(s in 1:length(unique(p1_split))){
# 	t_ps <- p1_split%in%unique(p1_split)[s]
# 	play_kde <- train_full[pop1_ind][t_ps, ks::kde(.SD[,list(x,y)], w=acc_score)]
# 	if(s==1){
# 		plot(play_kde, xlim=xlim, ylim=ylim, col=zCol(300, train_full[pop1_ind,time])[t_ps])
# 	}else{
# 		plot(play_kde, xlim=xlim, ylim=ylim, add=TRUE, col=zCol(300, train_full[pop1_ind,time])[t_ps])
# 	}
#
# 	Sys.sleep(0.5)
#
# 	abline(v=vline, h=hline, lwd=0.5, col="blue")
# 	mtext(train_full[pop1_ind][t_ps,round(mean(Part_of_Day),2)], adj=1, font=2)
# }
# plot(play_kde, add=F, display="persp", drawpoints=TRUE)
#
# plot(play_kde)
# train_full[pop1_ind]
# train_full[,day0:=Day-min(Day)]
# train_full[pop1_ind, length(zCol(256,accuracy))]
# kde_cols <- train_full[pop1_ind, zCol(length(acc_score),acc_score)]
# train_full[pop1_ind, plot(x,y, col=adjustcolor(kde_cols, 0.5))]




