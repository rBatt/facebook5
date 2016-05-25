
# ============
# = Packages =
# ============
library("data.table")
library("trawlData") # for read.zip
library("bit64")

# =============
# = Read Data =
# =============
classes <- c("integer", "numeric", "numeric", "integer", "integer", "integer64")

# ---- Training Data Set ----
train <- read.zip("pkgBuild/data_provided/train.csv.zip", SIMPLIFY=FALSE, colClasses=classes)[[1]]
# train[,row_id:=NULL]
setkey(train, time, place_id)

# ---- Test Data Test ----
test <- read.zip("pkgBuild/data_provided/test.csv.zip", SIMPLIFY=FALSE, colClasses=classes[-6])[[1]]
# test[,row_id:=NULL]
setkey(test, time)

# ========
# = Save =
# ========
save(train, file="data/train.RData", compress="xz")
save(test, file="data/test.RData", compress="xz")
