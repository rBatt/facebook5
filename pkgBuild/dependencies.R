
update_dependencies <- function(){
	devtools::use_package("data.table", type="Depends") # Basis for handling all data sets
	devtools::use_package("bit64", type="Depends") # Basis for handling all data sets
	devtools::use_package("trawlData", type="Depends") # Meets basic requirements for data content and format
	
	devtools::use_package("nnet", type="Suggests")
	devtools::use_package("caret", type="Suggests")
}
