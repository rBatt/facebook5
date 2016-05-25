
# nohup R CMD BATCH -cwd --no-save pkgBuild/kde_guess.R kde_guess.Rout &

# =================
# = Load Packages =
# =================
library(data.table)
library(bit64)
library(battFb5)
library(Metrics)
library(rbLib)
library(ks)
library(caret) # for data paritition


# ===========
# = Options =
# ===========
set.seed(1337)
use_old <- FALSE


# =====================
# = Load R Data Files =
# =====================
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")


# ======================
# = Add Scores to Data =
# ======================
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


# =============================
# = Adjust / Add to Data Sets =
# =============================
t_both <- rbind(train_full, test_full, fill=TRUE)
t_both[,c("big_xy"):=list(paste(cut(x,5), cut(y,20)))]
train_full <- t_both[!is.na(place_id)]
test_full <- t_both[is.na(place_id)]
both_range_x <- t_both[,range(x)]
both_range_y <- t_both[,range(y)]

# ---- Specify x- and y-range in each xy-group ----
# xy_range <- t_both[,list(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y)),by=c("xy_group")]
xy_range <- t_both[,list(xmin=quantile(x, 0.1),xmax=quantile(x, 0.9),ymin=quantile(y, 0.1),ymax=quantile(y, 0.9)),by=c("xy_group")]
setkey(xy_range, xy_group)

rm(list="t_both")
gc()
test_full[,place_id:=NULL]

train_full[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD),abundance_score(.SD)),by=c("big_xy")]
test_full[,c("t_score","acc_score","abu_score"):=list(time_score(.SD),accuracy_score(.SD)),by=c("big_xy")]


# ================================================
# = Split Into Train, Valid, Test, Add Groupings =
# ================================================
setkey(train_full, place_id)

# ---- create train and validation sets ----
if(use_old){
	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/train_set.RData")
	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/valid_set.RData")
}else{
	pidtbl <- train_full[,table(place_id)]
	train_full2 <- train_full[!place_id%in%names(pidtbl)[pidtbl<2]]
	train_full2[,place_id:=factor(place_id)]
	tv_part <- createDataPartition(train_full2[,place_id], 0.85)
	train_set <- train_full2[tv_part[[1]]]
	valid_set <- train_full2[-tv_part[[1]]]
	
	save(train_set, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/train_set.RData", compress="xz")
	save(valid_set, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/valid_set.RData", compress="xz")
}

# ---- Associated place_id with xy_group ----
xy_pid <- train_full[,list(place_id=as.character(unique(place_id))), by=c("xy_group")] #unique(train_full[,list(xy_group,place_id=as.character(place_id))])
setkey(xy_pid, xy_group, place_id)

# ---- Free Up Memory ----
rm(list=c("train_full","train_full2"))
gc()

# ---- Add KDE Prediction Group ----
split_grp <- function(X, group_size){
	x <- 1:nrow(X)
	vec <- quantile(x, seq(0,1,by=(1/nrow(X))*group_size), names=FALSE)
	fi <- findInterval(x, vec)
	# X[,group_fact:=fi]
	fi
}

setkey(train_set, xy_group)
setkey(valid_set, xy_group)
setkey(test_full, xy_group)

train_set[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]
valid_set[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]
test_full[,group_fact:=split_grp(.SD, 100),by=c("xy_group")]

# ---- Counters ----
n_gf_train <- nrow(train_set[,1,by=c("group_fact","xy_group")]) #train_set[,lu(group_fact)]
n_gf_valid <- nrow(valid_set[,1,by=c("group_fact","xy_group")]) #valid_set[,lu(group_fact)]
n_gf_test <- nrow(test_full[,1,by=c("group_fact","xy_group")]) #test_full[,lu(group_fact)]
n_pid_train <- nrow(train_set[,1,by=c("place_id")]) #train_set[,lu(place_id)]

setkey(train_set, xy_group, group_fact, place_id)
setkey(valid_set, xy_group, group_fact, place_id)
setkey(test_full, xy_group, group_fact)


# =========================
# = Estimate KDE on Train =
# =========================
if(use_old){
	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/kde_list.RData")
}else{
	kde_dropped <- c()
	kde_list <- vector('list', train_set[,n_pid_train])
	names(kde_list) <- train_set[,unique(place_id)]
	ctr <- 0
	v_start <- proc.time()[3]
	train_set[,j={
		
		# get all the xy_groups pertaining to the current place_id
		t_xy_ranges <- xy_range[.SD[,unique(xy_group)]]
		t_xmin <- t_xy_ranges[,c(min(xmin),min(ymin))] # xmin is for kde(); so both x and y
		t_xmax <- t_xy_ranges[,c(max(xmax),max(ymax))] # has x & y b/c is for kde()
		
		acpid <- as.character(place_id[1])
		if(nrow(.SD)<=2){
			kde_list[[acpid]] <<- NULL
			kde_dropped <<- c(kde_dropped, acpid)
		}else{
			kde_list[[acpid]] <<- suppressWarnings(ks::kde(.SD[,list(x,y)], w=acc_score, xmax=t_xmax, xmin=t_xmin, gridsize=rep(151,2)))
			kde_list[[acpid]]$x <<- as.matrix(data.frame(x=0,y=0))
		}
	
		ctr <<- ctr+1L
		if(ctr%%10==0){			
			prop_fin <- ctr/n_pid_train
			elap <- unname(proc.time()[3] - v_start)/60/60
			eta <- round(elap/prop_fin - elap,2)
			cat(paste0("\n Finished KDE List # ",ctr," of ", n_pid_train, " (",round(prop_fin,3)*100,"%, ETA = ", eta, " hrs);", " Elapsed:", round(elap,2), " hrs; Time Stamp: ", Sys.time(),"\n"))
			
			flush.console()
		}		
		NULL
	},by="place_id"]
	save(kde_list, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/kde_list.RData", compress="xz")
}
kde_list <- kde_list[!sapply(kde_list, is.null)]

# ---- Free Memory ----
pid_prop_tbl <- train_set[,table(place_id, xy_group)]
rm(list="train_set")
gc()


# ============================================================
# = Functions Used During Fitting and Prediction and Ranking =
# ============================================================
est_from_kde <- function(kl, X){
	# if(all(is.na(kl))){
	# 	preds <- rep(0,nrow(X))
	# }else{
		preds <- predict(kl, x=as.matrix(X[,list(x,y)]))
	# }
	# names(preds) <- as.character(X[,row_id])
	names(preds) <- X[,row_id]
	return(preds)
}

kde_top_k <- function(preds, k=3){
	np <- colnames(preds)
	x <- preds
	nc_x <- ncol(x)
	k <- 3
	q_rank <- function(x){np[sapply(sort(as.numeric(x), index.return=TRUE, na.last=TRUE, method="radix"), `[`, nc_x:(nc_x-k+1))[,-1]]}
	top_k <- t(apply(x, 1, q_rank))
	colnames(top_k) <- c("k1","k2","k3")
	top_k <- data.table(row_id=rownames(top_k), top_k)
	return(top_k)
}


# # ---- Rescale Kde List ----
# for(i in 1:length(kde_list)){
# 	kde_list[[i]]$estimate[] <- kde_list[[i]]$estimate/max(kde_list[[i]]$estimate[])
# 	print(i)
# }


# ===============================
# = Predict KDE Onto Validation =
# ===============================
# ---- Set Up Validation Data and Counters ----
valid_set[,row_id:=as.character(row_id)]
setkey(valid_set,xy_group,group_fact)
ctr <- 0
v_start <- proc.time()[3]

# ---- Make Validation Predictions ----
kde_guesses_valid <- valid_set[,j={
	
	# Can subset the KDE list to relevant regions
	# saves time by not looping through every single KDE model
	txyg <- unique(xy_group)[1]
	kde_xy_ind <- names(kde_list)%in%(xy_pid[txyg,unique(place_id)])
	if(sum(kde_xy_ind)<3){		
		s_tbl <- sort(pid_prop_tbl[,txyg], dec=TRUE)
		
		# Report Calculation Status
		cat("Using top", pmin(3,length(s_tbl)), "most common PID's\n")
		
		k_in <- data.frame("k1"=NA, "k2"=NA, "k3"=NA)
		k_in[,1:pmin(3,length(s_tbl))] <- names(s_tbl[1:pmin(3,length(s_tbl))])
		ktk <- data.table(row_id=row_id, k_in)
		
	}else{
		# Report Size of Each Calculation
		cat("Applying", sum(kde_xy_ind), "KDE's to", nrow(.SD), "rows\n")
		
		# For each KDE model (in the subset), make prediction
		# Each KDE model corresponds to a place_id
		# So KDE model predictions can be used to rank place_id's
		t_kde_list <- kde_list[kde_xy_ind]
		ests <- sapply(t_kde_list, est_from_kde, .SD)
		ests <- matrix(ests, ncol=sum(kde_xy_ind), nrow=nrow(.SD), dimnames=list(row_id=.SD[,row_id],place_id=names(t_kde_list)))
		ktk <- kde_top_k(ests)
	}
	
	# Report status every so often
	ctr <<- ctr+1L
	if(ctr%%10==0){
		prop_fin <- ctr/n_gf_valid
		elap <- unname(proc.time()[3] - v_start)/60/60
		eta <- round(elap/prop_fin - elap,2)
		cat(paste0("Finished for # ",ctr," of ", n_gf_valid, " (",round(prop_fin,3)*100,"%, ETA = ", eta, " hrs);", " Elapsed:", round(elap,2), " hrs; Time Stamp: ", Sys.time(),"\n\n"))
		flush.console()
	}
	
	# return a data.table with estimates, coordiantes, and true value
	data.table(.SD[,list(place_id,x,y)],ktk)
	
},by=c("xy_group","group_fact")]

# ---- Add precision column to kde validation guesses ----
kde_guesses_valid_pa3 <- kde_guesses_valid[,j={
	pt1.match <- (place_id==k1 & !is.na(k1))
	pt2.match <- (place_id==k2 & !is.na(k2) & !pt1.match)
	pt3.match <- (place_id==k3 & !is.na(k3) & !pt1.match & !pt2.match)
	pt1.match + pt2.match/2 + pt3.match/3
}]
kde_guesses_valid[,pa3:=kde_guesses_valid_pa3]

# ---- Save Validation Predictions ----
save(kde_guesses_valid, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_data/kde_guesses_valid.RData", compress="xz")

# # ---- Function for Visualizing Accuracy of Estimates ----
# show_kde_result <- function(V){
# 	k123 <- V[,c(k1,k2,k3)]
# 	if(V[,place_id]%in%k123){
# 		pt_col <- c("black","red","blue")[which(k123==V[,place_id])]
# 	}else{
# 		pt_col = "green"
# 	}
#
# 	# plot(x=V[,x], V[,y], pch=19, col=pt_col, ylim=V[,y]+c(-0.02,0.02), xlim=V[,x]+c(-0.075,0.075)) # the location
# 	plot(x=V[,x], V[,y], pch=19, col=pt_col, ylim=V[,y]+c(-0.2,0.2)/2, xlim=V[,x]+c(-0.75,0.75)/2) # the location
# 	plot(kde_list[[V[,k1]]], add=T) # kde of the place_id that was guessed 1st (wrong)
#
# 	plot(kde_list[[V[,k2]]],add=T, col='red') # kde of 2nd guess (wrong)
# 	plot(kde_list[[V[,k3]]],add=T, col='blue')
#
# 	if(!V[,place_id]%in%k123){
# 		plot(kde_list[[V[,place_id]]],add=T, col='green')
# 	}
# }
#
# par(mfrow=c(5,5), mar=c(1.5,1.5,0.1,0.1))
# for(v in 1:25){
# 	show_kde_result(kde_guesses_valid[pa3<0.4][(v)])
# }

# ---- Free Memory ----
rm(list=c("kde_guesses_valid","valid_set"))
gc()


# =================================
# = Make KDE Predictions for Test =
# =================================
# ---- Set Up Test Data and Counters ----
test_full[,row_id:=as.character(row_id)]
setkey(test_full,xy_group,group_fact)
ctr <- 0
v_start <- proc.time()[3]

# ---- Begin Test Predictions ----
kde_guesses_test <- test_full[,j={
	
	# subset to relevant part of grid for efficiency
	txyg <- unique(xy_group)[1]
	kde_xy_ind <- names(kde_list)%in%(xy_pid[txyg,unique(place_id)])
	if(sum(kde_xy_ind)<3){
		s_tbl <- sort(pid_prop_tbl[,txyg], dec=TRUE)
		
		# Report Calculation Status
		cat("Using top", pmin(3,length(s_tbl)), "most common PID's\n")
		
		k_in <- data.frame("k1"=NA, "k2"=NA, "k3"=NA)
		k_in[,1:pmin(3,length(s_tbl))] <- names(s_tbl[1:pmin(3,length(s_tbl))])
		ktk <- data.table(row_id=row_id, k_in)
		
	}else{
		# Report Size of Each Calculation
		cat("Applying", sum(kde_xy_ind), "KDE's to", nrow(.SD), "rows\n")
		
		# For each KDE model (in the subset), make prediction
		# Each KDE model corresponds to a place_id
		# So KDE model predictions can be used to rank place_id's
		t_kde_list <- kde_list[kde_xy_ind]
		ests <- sapply(t_kde_list, est_from_kde, .SD)
		ests <- matrix(ests, ncol=sum(kde_xy_ind), nrow=nrow(.SD), dimnames=list(row_id=.SD[,row_id],place_id=names(t_kde_list)))
		ktk <- kde_top_k(ests)
	}
	
	# print status
	ctr <<- ctr+1L
	if(ctr%%10==0){
		prop_fin <- ctr/n_gf_test
		elap <- unname(proc.time()[3] - v_start)/60/60
		eta <- round(elap/prop_fin - elap,2)
		cat(paste0("\n Finished Test # ",ctr," of ", n_gf_test, " (",round(prop_fin,3)*100,"%, ETA = ", eta, " hrs);", " Elapsed:", round(elap,2), " hrs; Time Stamp: ", Sys.time(),"\n"))
		flush.console()
	}
	
	# return dt with coordinates and guesses
	data.table(.SD[,list(x,y)],ktk)
	
},by=c("xy_group","group_fact")]

# ---- Save Test Predictions ----
save(kde_guesses_test, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/kde_preds/kde_guesses_test.RData", compress="xz")


