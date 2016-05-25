
# =================
# = Load Packages =
# =================
library(data.table)
library(bit64)
library(battFb5)
library(Metrics)
library(rbLib)
library(h2o)


# ===========
# = Options =
# ===========
pick_up_from_last <- TRUE


# =====================
# = Load R Data Files =
# =====================
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
load(file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")


# =====================
# = Read in H2O Files =
# =====================
# h2o_conn <- h2o.init(nthreads=6, max_mem_size="24G")
# train_full_h2o <- h2o.importFile(path="Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.csv", destination_frame="train_full_h2o")
# test_full_h2o <- h2o.importFile(path="Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.csv", destination_frame="test_full_h2o")
# train_full_h2o[,"place_id"] <- as.character(train_full_h2o[,"place_id"])

h2o_restart_server <- function(){
	cat("\nH2O restart initialized")
	if(tryCatch(h2o.clusterIsUp(), error=function(cond){FALSE})){
		cat("\nCluster detected, shutting down\n")
		flush.console()
		outcome <- h2o.shutdown(prompt=FALSE)
		Sys.sleep(0.5)
	}
	cat("\nStarting New H2o Server\n")
	flush.console()
	Sys.sleep(0.5)
	assign("h2o_conn", h2o.init(nthreads=6, max_mem_size="24G"), envir=.GlobalEnv)
	Sys.sleep(0.5)
}

h2o_reload_data <- function(){
	cat("\nImporting H2O Data Files\n")
	flush.console()
	if(!h2o.clusterIsUp()){
		h2o_restart_server()
		Sys.sleep(0.5)
	}
	Sys.sleep(0.5)
	train_full_h2o <- h2o.importFile(path="Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.csv", destination_frame="train_full_h2o")
	test_full_h2o <- h2o.importFile(path="Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.csv", destination_frame="test_full_h2o")
	train_full_h2o[,"place_id"] <- as.character(train_full_h2o[,"place_id"])
	assign("train_full_h2o", train_full_h2o, envir=.GlobalEnv)
	assign("test_full_h2o", test_full_h2o, envir=.GlobalEnv)
	cat("\nH2O Data Reloaded\n")
	flush.console()
}

tryCatch(
	{
		blah <- h2o_restart_server()
		h2o_reload_data()
	},
	error=function(cond){
		blah <- h2o_restart_server()
		h2o_reload_data()
	}
)


# ---- Set up Column Indices ----
dep_var <- which(names(train_full_h2o)=="place_id")
ind_var_short <- which(names(train_full_h2o)%in%c("x","y","accuracy","Day","Part_of_Day"))


# =================================================================
# = Create Indices for Subsetting Using the R version of the data =
# =================================================================
xy_ind_train <- list()
xy_ind_test <- list()
u_xy <- train_full[,unique(xy_group)]
xyg_train <- train_full[,.I,by="xy_group"]
xyg_test <- test_full[,.I,by="xy_group"]
for(l in 1:length(u_xy)){
	xy_ind_train[[l]] <- xyg_train[u_xy[l],I]
	xy_ind_test[[l]] <- xyg_test[u_xy[l],I]
	names(xy_ind_train)[l] <- u_xy[l]
	names(xy_ind_test)[l] <- u_xy[l]
}


# ===================
# = Some Cool Plots =
# ===================
# dev.new()
# par(mfrow=c(3,2), mar=c(1,1,0.5,0.5), mgp=c(1,0.1,0), tcl=-0.1, ps=9, cex=1)
# for(r in 1:6){
# 	tr_ind <- xy_ind_train[[u_xy[r]]]
# 	train_full[tr_ind, plot(x,y, col=adjustcolor(zCol(256, as.integer(place_id)), 0.25), cex=0.5, pch=20)]
# }
#
# dev.new()
# par(mfrow=c(3,2), mar=c(1,1,0.5,0.5), mgp=c(1,0.1,0), tcl=-0.1, ps=9, cex=1)
# for(r in 1:6){
# 	te_ind <- xy_ind_test[[u_xy[r]]]
# 	test_full[te_ind, plot(x,y, col=adjustcolor(zCol(256, as.integer(Part_of_Day)), 0.25), cex=0.5, pch=20)]
# }


# ============================================
# = Function to Extract Top k(3) Predictions =
# ============================================
top_preds <- function(preds, k=3){
	p_df <- as.data.frame(preds)
	np <- names(preds)[-1]
	x <- p_df[,-1] #rnorm(10)
	nc_x <- ncol(x)
	k <- 3
	q_rank <- function(x){np[sapply(sort(as.numeric(x), index.return=TRUE), `[`, nc_x:(nc_x-k+1))[,-1]]}
	top_k <- apply(x, 1, q_rank)
	top_k <- gsub("p", "", top_k)
	top_k_form <- apply(top_k, 2, paste, collapse = " ")
	return(top_k_form)
}



# =========================================
# = Load Results from Incomplete Analysis =
# =========================================
if(pick_up_from_last){
	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/h2o_preds/gbm_h2o_pred_l.RData")
	load("~/Documents/School&Work/kaggle/facebook5/pkgBuild/h2o_preds/fit_stats.RData")
	l_start <- length(gbm_h2o_pred_l)# + 1 # 644L
	rm(list=c("train_full","test_full"))
}else{
	gbm_h2o_pred_l <- list()
	l_start <- 1
}


# ================================================
# = Define Functions Needed During Model Fitting =
# ================================================
# ---- Function to Clean Up H2O Objects ----
removeLastValues <- function(conn) {
    df <- h2o.ls()
    keys_to_remove <- grep("^Last\\.value\\.", perl=TRUE, x=df$Key, value=TRUE)
    unique_keys_to_remove = unique(keys_to_remove)
    if (length(unique_keys_to_remove) > 0) {
        h2o.rm(unique_keys_to_remove)
    }
}

# ---- Function to Fit GBM Model in Loop ----
loop_gbm <- function(y, x, frame){
	
	# ---- Split Data and Fit Model ----
	h2o_train_valid <- h2o.splitFrame(frame)
	deep_h2o_mod_l <- h2o.gbm(y=dep_var, x=ind_var_short, training_frame=h2o_train_valid[[1]], validation_frame=h2o_train_valid[[2]], distribution="multinomial", learn_rate=0.01, ntrees=3, keep_cross_validation_predictions=F)
	predict_deep <- h2o.predict(deep_h2o_mod_l, test_full_h2o_l)
	
	# ---- Compute Metrics ----
	print_hist <- h2o.scoreHistory(deep_h2o_mod_l)[,c("timestamp","duration","number_of_trees","training_classification_error","validation_classification_error")]
	print_hit <- h2o.hit_ratio_table(deep_h2o_mod_l, valid=TRUE)[1:5,]
	map3_hat <- data.frame(map3_hat = print_hit[1,2] + (print_hit[2,2] - print_hit[1,2])/2 + (print_hit[3,2] - print_hit[2,2])/3)
	
	# ---- Update Metric DT (external) ----
	fit_stats <<- fit_stats[l, c("hit1","hit2","hit3","map3"):=list(print_hit[1,2], print_hit[2,2], print_hit[3,2], map3_hat[1,1])]
	
	# ---- Print Metrics ----
	print(print_hist)
	print(print_hit)
	print(map3_hat)
	flush.console()
	
	# ---- Create Output DT ----
	out <- data.table(as.data.frame(test_full_h2o_l)[,c("row_id","x","y")], place_id=as.data.frame(predict_deep$predict)[,1], k_place_id=top_preds(predict_deep))
	
	# ---- Clean Up ----
	rm(list=c("deep_h2o_mod_l","h2o_train_valid","predict_deep"))
	removeLastValues(h2o_conn)
	gc()
	
	# ---- Return ----
	return(out)
}


# ==================================
# = Fit Model 1 xy_group at a Time =
# ==================================
l_max <- length(u_xy)
if(!exists("fit_stats")){fit_stats <- data.table(xy_group=u_xy)}

for(l in l_start:l_max){
	
	# ---- Create Training and Validation Subsets ----
	train_full_h2o_l <- train_full_h2o[xy_ind_train[[u_xy[l]]],]
	train_full_h2o_l[,"place_id"] <- as.factor(train_full_h2o_l[,"place_id"])
	test_full_h2o_l <- test_full_h2o[xy_ind_test[[u_xy[l]]],]
	
	# ---- Fit GBM Model ----
	gbm_h2o_pred_l[[l]] <- tryCatch(
			loop_gbm(y=dep_var, x=ind_var_short, frame=train_full_h2o_l),
		error=function(cond){ # if GBM fail, do Deep Learning
				h2o_train_valid <- h2o.splitFrame(train_full_h2o_l)
				deep_h2o_mod_l <- h2o.deeplearning(y=dep_var, x=ind_var_short, training_frame=h2o_train_valid[[1]], validation_frame=h2o_train_valid[[2]], standardize=TRUE, epoch=100, hidden=c(50,50), l1=1E-7, l2=1E-7, activation="Rectifier", max_hit_ratio_k=3, seed=1122)
				predict_deep <- h2o.predict(deep_h2o_mod_l, test_full_h2o_l)
				print_hit <- h2o.hit_ratio_table(deep_h2o_mod_l, valid=TRUE)[1:5,]
				map3_hat <- data.frame(map3_hat = print_hit[1,2] + (print_hit[2,2] - print_hit[1,2])/2 + (print_hit[3,2] - print_hit[2,2])/3)
				fit_stats <<- fit_stats[l, c("hit1","hit2","hit3","map3"):=list(print_hit[1,2], print_hit[2,2], print_hit[3,2], map3_hat[1,1])]
				data.table(as.data.frame(test_full_h2o_l)[,c("row_id","x","y")], place_id=as.data.frame(predict_deep$predict)[,1], k_place_id=top_preds(predict_deep))
		}
	)
	
	# ---- Old Deep Learning Code ----
	# gbm failed in l = 695, but running deep learning worked
	# 900 also failed
	# h2o_train_valid <- h2o.splitFrame(train_full_h2o_l)
	# deep_h2o_mod_l <- h2o.deeplearning(y=dep_var, x=ind_var_short, training_frame=h2o_train_valid[[1]], validation_frame=h2o_train_valid[[2]], standardize=TRUE, epoch=100, hidden=c(50,50), l1=1E-7, l2=1E-7, activation="Rectifier", max_hit_ratio_k=3, seed=1122)
	# predict_deep <- h2o.predict(deep_h2o_mod_l, test_full_h2o_l)
	# print_hit <- h2o.hit_ratio_table(deep_h2o_mod_l, valid=TRUE)[1:5,]
	# map3_hat <- data.frame(map3_hat = print_hit[1,2] + (print_hit[2,2] - print_hit[1,2])/2 + (print_hit[3,2] - print_hit[2,2])/3)
	# fit_stats <<- fit_stats[l, c("hit1","hit2","hit3","map3"):=list(print_hit[1,2], print_hit[2,2], print_hit[3,2], map3_hat[1,1])]
	# gbm_h2o_pred_l[[l]] <- data.table(as.data.frame(test_full_h2o_l)[,c("row_id","x","y")], place_id=as.data.frame(predict_deep$predict)[,1], k_place_id=top_preds(predict_deep))
	
	# ---- Others Plots/ Diagnostics ----
	# plot(deep_h2o_mod_l[[l]])
	# deep_h2o_pred_l[[l]][,plot(x, y, col=zCol(256, as.integer(as.factor(place_id))), pch=20, cex=0.75)]
	# h2o.varimp(deep_h2o_mod_l[[l]])
	
	# ---- Status Message ----
	cat(paste0("\nGBM Completed: ", l, " of ", l_max, " (", round(l/l_max, 2)*100, "%)\n\n"))
	flush.console()
	
	
	# ---- Restart H2O if Needed ----
	if(((l-l_start)%%50)==0){
		tryCatch(
			{
				blah <- h2o_restart_server()
				h2o_reload_data()
			},
			error=function(cond){
				blah <- h2o_restart_server()
				h2o_reload_data()
			}
		)
	}
	
}


# ====================================
# = Save Predictions and Fit Metrics =
# ====================================
save_name_pred <- paste0("~/Documents/School&Work/kaggle/facebook5/pkgBuild/h2o_preds/gbm_h2o_pred_l.RData")
save(gbm_h2o_pred_l, file=save_name_pred)
save(fit_stats, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/h2o_preds/fit_stats.RData")


# =============================
# = Save Formatted Submission =
# =============================
submission <- rbindlist(gbm_h2o_pred_l, fill=TRUE)
submission[,c("x","y","place_id"):=NULL]
setnames(submission, "k_place_id", "place_id")
setkey(submission, row_id)
save(submission, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/submissions/last_submission.RData")
write.csv(submission, file=renameNow("~/Documents/School&Work/kaggle/facebook5/pkgBuild/submissions/submission.csv"), row.names=FALSE, quote=FALSE)


# ================
# = Shutdown H2O =
# ================
# h2o.shutdown(prompt=FALSE)
# sum(sapply(gbm_h2o_pred_l, function(x)all(!is.na(x))))

