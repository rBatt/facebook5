#' Scale Predictors
#' 
#' Scales the predictors in the training or test data set
#' 
#' @param dt a data.table (e.g., \link{train} or \link{test})
#' 
#' @details subtracts mean then divides by standard deviation
#' 
#' @return Returns a list of length 3: the data.table with scaled predictors, a vector of the means in \code{dt}, a vector of the standard deviations in \code{dt}
#' 
#' @export
scale_preds <- function(dt){
	mus <- sapply(dt, mean)
	sds <- sapply(dt, sd)
	ms <- function(v){nm <- as.character(substitute(v)); (v-mus[nm])/sds[nm]}
	dt2 <- copy(dt)
	dt2[,c("x","y","accuracy","time"):=list(x=ms(x), y=ms(y), accuracy=ms(accuracy), time=ms(time))]
	
	return(list(dt=dt2, mus=mus, sds=sds))
}

