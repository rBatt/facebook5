
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
# = Load TS Forecasts =
# =====================
load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/ts_forecasts.RData")
setkey(ts_forecasts, place_id, Hour)


# =====================
# = Load R Data Files =
# =====================
# load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
# load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")

train_full <- copy(battFb5::train)
test_full <- copy(battFb5::test)

train_full[,place_id:=as.character(place_id)]

full_xyrid <- rbind(train_full[, list(row_id, x, y, place_id)], test_full[, list(row_id, x, y, place_id=NA_character_)])
full_xyrid[,c("rg_x","rg_y"):=list(cut(x,14), cut(y,100))]
full_xyrid[,c("xy_group"):=list(paste(rg_x, rg_y))]

train_full <- merge(train_full, full_xyrid[!is.na(place_id),list(row_id,rg_x,rg_y,xy_group)], by=c("row_id"), all.x=TRUE, all.y=FALSE)
test_full <- merge(test_full, full_xyrid[is.na(place_id),list(row_id,rg_x,rg_y,xy_group)], by=c("row_id"), all.x=TRUE, all.y=FALSE)

train_full[,c("rg_x","rg_y"):=NULL]
test_full[,c("rg_x","rg_y"):=NULL]

train_full[,place_id:=as.integer64(place_id)]

train_full[,c("Hour"):=list(time%/%60)]
setkey(train_full, row_id, place_id, Hour)

test_full[,c("Hour"):=list(time%/%60)]
setkey(test_full, row_id, Hour)


# ======================
# = Add Scores to Data =
# ======================
# time_score <- function(X){
# 	X[,list(time_wt=0.25^(-(Day-max(Day))))][,time_wt]#[,time_wt/sum(time_wt)]
# }
# accuracy_score <- function(X){
# 	X[,list(acc_wt=0.5^pmax(0,-(accuracy-1)))][,acc_wt]#[,acc_wt/sum(acc_wt)]
# }
# abundance_score <- function(X){
# 	# X[,1/.N]
# 	nms <- X[,place_id]
# 	pt <- X[,cumsum(sort(prop.table(table(place_id)),dec=T))]
# 	pt[as.character(nms)]
# }
# pid_ecdf <- function(X, score_cat=c("t_score","acc_score","abu_score")){
# 	score_cat <- match.arg(score_cat)
# 	switch(score_cat,
# 		t_score = X[!duplicated(place_id),ecdf(t_score)](X[,t_score]),
# 		acc_score = X[!duplicated(place_id),ecdf(acc_score)](X[,acc_score]),
# 		abu_score = X[!duplicated(place_id),ecdf(abu_score)](X[,abu_score])
# 	)
# }


# # =============================
# # = Adjust / Add to Data Sets =
# # =============================
# t_both <- rbind(train_full, test_full, fill=TRUE)
# t_both[,c("big_xy"):=list(paste(cut(x,5), cut(y,20)))]
# train_full <- t_both[!is.na(place_id)]
# test_full <- t_both[is.na(place_id)]
# both_range_x <- t_both[,range(x)]
# both_range_y <- t_both[,range(y)]
#
# # ---- Specify x- and y-range in each xy-group ----
# # xy_range <- t_both[,list(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y)),by=c("xy_group")]
# xy_range <- t_both[,list(xmin=quantile(x, 0.1),xmax=quantile(x, 0.9),ymin=quantile(y, 0.1),ymax=quantile(y, 0.9)),by=c("xy_group")]
# setkey(xy_range, xy_group)
#
# rm(list="t_both")
# gc()
# test_full[,place_id:=NULL]

# train_full[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD),abundance_score(.SD)),by=c("big_xy")]
# test_full[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD)),by=c("big_xy")]


# ================================================
# = Split Into Train, Valid, Test, Add Groupings =
# ================================================
# setkey(train_full, place_id)

# ---- create train and validation sets ----
# if(use_old){
# 	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/train_set.RData")
# 	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/valid_set.RData")
# }else{
# 	pidtbl <- train_full[,table(place_id)]
# 	train_full2 <- train_full[!place_id%in%names(pidtbl)[pidtbl<2]]
# 	train_full2[,place_id:=factor(place_id)]
# 	tv_part <- createDataPartition(train_full2[,place_id], 0.85)
# 	train_set <- train_full2[tv_part[[1]]]
# 	valid_set <- train_full2[-tv_part[[1]]]
#
# 	save(train_set, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/train_set.RData", compress="xz")
# 	save(valid_set, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/valid_set.RData", compress="xz")
# }

# ---- Associated place_id with xy_group ----
xy_pid <- train_full[,list(place_id=as.character(unique(place_id))), by=c("xy_group")] #unique(train_full[,list(xy_group,place_id=as.character(place_id))])
xy_pid[, place_id:=as.integer64(place_id)]
setkey(xy_pid, xy_group, place_id)

# ---- Free Up Memory ----
rm(list=c("train_full"))
gc()

# ---- Add KDE Prediction Group ----
# split_grp <- function(X, group_size){
# 	x <- 1:nrow(X)
# 	vec <- quantile(x, seq(0,1,by=(1/nrow(X))*group_size), names=FALSE)
# 	fi <- findInterval(x, vec)
# 	# X[,group_fact:=fi]
# 	fi
# }

# setkey(train_set, xy_group)
# setkey(valid_set, xy_group)
# setkey(test_full, xy_group)

# train_set[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]
# valid_set[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]
# test_full[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]

# ---- Counters ----
# n_gf_train <- nrow(train_set[,1,by=c("group_fact","xy_group")]) #train_set[,lu(group_fact)]
# n_gf_valid <- nrow(valid_set[,1,by=c("group_fact","xy_group")]) #valid_set[,lu(group_fact)]
# n_gf_test <- nrow(test_full[,1,by=c("group_fact","xy_group")]) #test_full[,lu(group_fact)]
# n_pid_train <- nrow(train_set[,1,by=c("place_id")]) #train_set[,lu(place_id)]
#
# setkey(train_set, xy_group, group_fact, place_id)
# setkey(valid_set, xy_group, group_fact, place_id)
# setkey(test_full, xy_group, group_fact)


# ==============================
# = Try Simple Grid Classifier =
# ==============================
# train_set[1:1E3, list(abu_score=(unique(abu_score))), by=c("xy_group","place_id")] # if precalculated abundance score
# t_score <- train_set[1:1E3, list(place_id=place_id, abu_score=abundance_score(.SD)), by=c("xy_group")] # calculate score on fly
# setkey(t_score, xy_group, place_id)
# t_score <- unique(t_score)
# setorder(t_score, -abu_score)

blah <- test_full[,.N,by=c("xy_group","Hour")]
niter <- nrow(blah)

# ================
# = Test Guesses =
# ================
# pb <- txtProgressBar(min=1, max=niter, initial=1, style=3)
# pb <- txtProgressBar(min=1, max=10, initial=1, style=3)
# iter <- 0

# ugh <- dcast.data.table(ts_forecasts, Hour~place_id, value.var="ts_forecast")
# ugh[,list(Hour, `9999755282`)]

# setorder(ts_forecasts, -ts_forecast, na.last=TRUE)
# ts_forecasts_mini <- ts_forecasts[,j={
# 	# setorder(.SD, -ts_forecast, na.last=TRUE)
# 	head(.SD, 3)
# }, by="Hour"]
#
#  #ts_forecasts[ts_forecast>0.25 & ts_forecast < 500]
# setkey(ts_forecasts_mini, place_id, Hour)

# fore_xy <- ts_forecasts_mini[xy_pid, on="place_id", allow.cartesian=TRUE]
fore_xy <- ts_forecasts[xy_pid, on="place_id", allow.cartesian=TRUE]
fore_xy <- fore_xy[!is.na(Hour) & !is.na(ts_forecast)]

setkey(fore_xy, xy_group, Hour)
setorder(fore_xy, -ts_forecast, na.last=TRUE)
fore_xy_mini <- fore_xy[,j={
	head(.SD, 3)
}, by=c("xy_group","Hour")]


answer <- data.table:::merge.data.table(test_trim, fore_xy_mini, all=TRUE, by=c("xy_group","Hour"), allow.cartesian=TRUE)
answer2 <- answer[!is.na(row_id)]
setorder(answer2, -ts_forecast, na.last=TRUE)
answer3 <- answer2[,list(place_id=paste(place_id[1:3], collapse=" ")),keyby="row_id"]
ts_submission <- answer3

# fore_xy[1:5,j={
# 	# iter <<- iter + 1L
# 	# setTxtProgressBar(pb, iter)
# 	# u_pid <- xy_pid[xy_group, unique(place_id)]
# 	# u_hr <- unique(Hour)
# 	# sub_dt <- data.table(place_id=u_pid, Hour=Hour)
# 	# candidates <- fore_xy[.SD, on=c("xy_group","Hour")] #ts_forecasts_mini[sub_dt, on=c("place_id","Hour")]
# 	# setorder(.SD, -ts_forecast, na.last=TRUE)
# 	print(sort(ts_forecast, decreasing=TRUE, index.return=TRUE))
# 	top3 <- place_id[sort(ts_forecast, decreasing=TRUE, index.return=TRUE)[1:3]]
# 	print(.SD)
# 	# top3_ind <- candidates[,sort(candidates, decreasing=TRUE, index.return=TRUE, na.last=TRUE)]
# 	# top3 <- .SD[1:3,place_id] # candidates[top3_ind[1:3], place_id] #
# 	data.table(k1=top3[1], k2=top3[2], k3=top3[3])
# },by=c("xy_group","Hour")]
#


# merge(ts_forecasts, xy_pid, all.x=TRUE, all.y=FALSE, by=c("place_id"), allow.cartesian=TRUE)
#
# test_trim <- test_full[,list(row_id, xy_group, Hour)]
#
# ugh <- merge(test_trim, ts_forecasts)
#
# Rprof()
# ts_guesses_test <- test_trim[1:10,j={
# 	iter <<- iter + 1L
# 	setTxtProgressBar(pb, iter)
# 	# u_pid <- xy_pid[xy_group, unique(place_id)]
# 	# u_hr <- unique(Hour)
# 	# sub_dt <- data.table(place_id=u_pid, Hour=Hour)
# 	candidates <- fore_xy[.SD, on=c("xy_group","Hour")] #ts_forecasts_mini[sub_dt, on=c("place_id","Hour")]
# 	setorder(candidates, -ts_forecast, na.last=TRUE)
# 	# top3_ind <- candidates[,sort(candidates, decreasing=TRUE, index.return=TRUE, na.last=TRUE)]
# 	top3 <- candidates[1:3,place_id] # candidates[top3_ind[1:3], place_id] #
# 	data.table(.SD[,list(row_id)], k1=top3[1], k2=top3[2], k3=top3[3], N=candidates[,length(unique(place_id))])
# },by=c("xy_group","Hour"), .SDcols=c("row_id","xy_group","Hour")]


# ===============
# = Save Things =
# ===============
save(ts_guesses_test, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/ts_guesses_test.RData", compress="xz")
# ts_submission <- ts_guesses_test[,list(place_id=paste(k1,k2,k3, collapse=" ")), keyby="row_id"]
setkey(ts_submission, row_id)
save(ts_submission, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/submissions/last_submission.RData")
write.csv(ts_submission, file=renameNow("~/Documents/School&Work/kaggle/facebook5/pkgBuild/submissions/ts_submission.csv"), row.names=FALSE, quote=FALSE)



