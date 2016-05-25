
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


# =====================
# = Load R Data Files =
# =====================
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")



time_score <- function(X){
	X[,list(time_wt=0.25^(-(Day-max(Day))))][,time_wt]#[,time_wt/sum(time_wt)]
}

accuracy_score <- function(X){
	X[,list(acc_wt=0.5^pmax(0,-(accuracy-1)))][,acc_wt]#[,acc_wt/sum(acc_wt)]
}
abundance_score <- function(X){
	# X[,1/.N]
	nms <- X[,place_id]
	pt <- X[,cumsum(sort(prop.table(table(place_id)),dec=T))]
	pt[as.character(nms)]
}
pid_ecdf <- function(X, score_cat=c("t_score","acc_score","abu_score")){
	score_cat <- match.arg(score_cat)
	switch(score_cat,
		t_score = X[!duplicated(place_id),ecdf(t_score)](X[,t_score]),
		acc_score = X[!duplicated(place_id),ecdf(acc_score)](X[,acc_score]),
		abu_score = X[!duplicated(place_id),ecdf(abu_score)](X[,abu_score])
	)
}
train_full[,c("big_xy"):=list(paste(cut(x,5), cut(y,20)))]
train_full[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD),abundance_score(.SD)),by=c("big_xy")]

play <- train_full[sample(1:nrow(train_full),1E5)]
#
#
# play[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD),abundance_score(.SD)),by=c("big_xy")]
# play[,c("t_score","acc_score","abu_score"):=list(t_score=sum(t_score),acc_score=sum(acc_score),abu_score=sum(abu_score)),by=c("big_xy","place_id")]
# play[,c("t_score","acc_score","abu_score"):=list(t_score=pid_ecdf(.SD, "t_score"),acc_score=pid_ecdf(.SD,"acc_score"),abu_score=pid_ecdf(.SD,"abu_score")),by=c("big_xy")]
# play[big_xy=="(-1.39,-1.04] (-0.696,-0.522]", .SD[place_id==.SD[which.min(acc_score), place_id],mean(accuracy)]] # .SD[place_id==.SD[which.max(abu_score), place_id]]
#

# train_full[,c("t_score","acc_score","abu_score"):=list(t_score=(t_score),acc_score=(acc_score),abu_score=sum(abu_score)),by=c("big_xy","place_id")]
# train_full[,c("t_score","acc_score","abu_score"):=list(t_score=pid_ecdf(.SD, "t_score"),acc_score=pid_ecdf(.SD,"acc_score"),abu_score=abu_score),by=c("big_xy")]


train_full[abu_score<0.95 & acc_score>0.99 & t_score>0.4,list(N=lu(place_id)),by="big_xy"][,hist(N, main=paste(lu(big_xy), sum(N), max(N)))]


