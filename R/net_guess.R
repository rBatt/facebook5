#' Make  a Guess Using a Neural Network
#' 
#' Function that uses the neural network to make a guess at the test set
#' 
#' @param train_set data.table of training data
#' @param test_set data.table of test data
#' @param form formula used by \code{\link{nnet}}
#' @param use_map3 Logical, default is FALSE, mean average precision (at 3) is not used as metric during tuning
#' @param ... other arguments to be passed
#' 
#' @details Just learning to use nnet
#' 
#' @export
net_guess <- function(train_set, test_set, form=place_id~x+y+time, use_map3=FALSE, ...){
	requireNamespace("nnet", quietly = TRUE)
	requireNamespace("caret", quietly = TRUE)
	
	
	# ---- Fit N-Net ----
	# net_fit <- nnet::nnet(place_id~x+y+time, data=train_set, size=15, linout=TRUE)
	# net_pred_test <- predict(net_fit, newdata=test_set)
	
	if(use_map3){
		net_fit2 <- caret::train(form, data=train_set, method="nnet", ...)
	}else{
		net_fit2 <- caret::train(form, data=train_set, method="nnet", ...)
	}
	net_pred_test2 <- predict(net_fit2, newdata=test_set)
	
	# comparison <- table(net_pred_test2, test_set[,place_id])
	# correct_guesses <- sum(diag(comparison))
	# incorrect_guesses <- sum(comparison[row(comparison)!=col(comparison)])
	#
	# correct_per_id <- test_set[net_pred_test2==place_id, table(place_id)]
	# incorrect_per_id <- test_set[net_pred_test2!=place_id, table(place_id)]
	
	if("place_id"%in%names(test_set)){
		map3 <- mapk(3, as.list(test_set[,place_id]), as.list(net_pred_test2))
	}else{
		map3 <- NULL
	}
	
	return(mod=net_fit2, map3=map3, pred=net_pred_test2)
		
}

