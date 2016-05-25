# =================
# = Load Packages =
# =================
library(data.table)
library(bit64)
library(battFb5)
library(rbLib)


# ===========
# = Options =
# ===========
mini <- FALSE
use_h2o <- TRUE
prop_train <- 0.7


# =================
# = Full Data Set =
# =================
train_full <- copy(battFb5::train)
test_full <- copy(battFb5::test)
train_full[,place_id:=as.factor(as.character(place_id))]


# ==============
# = Add Groups =
# ==============
full_xyrid <- rbind(train_full[, list(row_id, x, y, place_id)], test_full[, list(row_id, x, y, place_id=NA)])
full_xyrid[,c("rg_x","rg_y"):=list(cut(x,14), cut(y,100))]
full_xyrid[,c("xy_group"):=list(paste(rg_x, rg_y))]

train_full <- merge(train_full, full_xyrid[!is.na(place_id),list(row_id,rg_x,rg_y,xy_group)], by=c("row_id"), all.x=TRUE, all.y=FALSE)
test_full <- merge(test_full, full_xyrid[is.na(place_id),list(row_id,rg_x,rg_y,xy_group)], by=c("row_id"), all.x=TRUE, all.y=FALSE)

train_full[,c("rg_x","rg_y"):=NULL]
test_full[,c("rg_x","rg_y"):=NULL]


# ================
# = Add Features =
# ================
train_full[,c("Hour","Day"):=list(floor(time/60), floor(time/60/24))]
train_full[,c("Hour_of_Day","Day_of_Week"):=list(Hour%%24, (Day%%7))]
# train_full[,c("Hour_of_Day", "Qtr_of_Day"):=list(as.factor(Hour_of_Day), as.factor(Hour_of_Day%/%6))]

test_full[,c("Hour","Day"):=list(floor(time/60), floor(time/60/24))]
test_full[,c("Hour_of_Day","Day_of_Week"):=list(Hour%%24, (Day%%7))]
# test_full[,c("Hour_of_Day", "Qtr_of_Day"):=list(as.factor(Hour_of_Day), as.factor(Hour_of_Day%/%6))]

# day part classification
n_obs_hod <- train_full[,.N,by=c("Hour_of_Day")]
n_place_hod <- train_full[,list(n_places=lu(place_id)),by=c("Hour_of_Day")]
plot(n_obs_hod[,list(Hour_of_Day, N)], type="o")
plot(n_place_hod[,list(Hour_of_Day, n_places)], type="o")
day_part <- c(
	rep(4, 4), # late night (1am to 4am)
	rep(1, 6), # breakfast (5am to 10am)
	rep(2, 6), # lunch (11am to 4pm)
	rep(3, 4), # dinner (5pm to 10pm)
	rep(4, 4)
)
train_full[,c("Part_of_Day"):=list(day_part[Hour_of_Day+1])]
test_full[,c("Part_of_Day"):=list(day_part[Hour_of_Day+1])]

train_full[,c("Hour_of_Day","Part_of_Day"):=list((Hour_of_Day), (Part_of_Day))]
test_full[,c("Hour_of_Day","Part_of_Day"):=list((Hour_of_Day), (Part_of_Day))]


# =========
# = Scale =
# =========
# ---- X and Y ----
all_xy <- c(train_full[,c(x,y)], test_full[,c(x,y)])
mu_xy <- mean(all_xy)
sd_xy <- sd(all_xy)
train_full[,c("x","y"):=list((x-mu_xy)/sd_xy, (y-mu_xy)/sd_xy)]
test_full[,c("x","y"):=list((x-mu_xy)/sd_xy, (y-mu_xy)/sd_xy)]

# ---- accuracy, hour, day, time ----
ms <- function(v){nm <- as.character(substitute(v)); (v-mus[nm])/sds[nm]}
t_combined <- rbind(train_full[,list(accuracy, time, Hour, Day)], test_full[,list(accuracy, time, Hour, Day)])
sds <- sapply(t_combined, sd)
mus <- sapply(t_combined, mean)
train_full[,c("accuracy","time", "Hour", "Day"):=list(accuracy=ms(accuracy), time=ms(time), Hour=ms(Hour), Day=ms(Day))]
test_full[,c("accuracy","time", "Hour", "Day"):=list(accuracy=ms(accuracy), time=ms(time), Hour=ms(Hour), Day=ms(Day))]


# =====================================
# = Add Transforms of Scaled Features =
# =====================================
train_full[,c("x2","y2"):=list(x^2, y^2)]
test_full[,c("x2","y2"):=list(x^2, y^2)]


# ================
# = Key and Save =
# ================
setkey(train_full, xy_group, row_id)
setkey(test_full, xy_group, row_id)
save(train_full, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.RData")
save(test_full, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.RData")


train_full_csv <- copy(train_full)
train_full_csv[,xy_group:=NULL]
test_full_csv <- copy(test_full)
test_full_csv[,xy_group:=NULL]

data.table::fwrite(train_full_csv, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/train_full.csv")
data.table::fwrite(test_full_csv, file="~/Documents/School&Work/kaggle/facebook5/pkgBuild/data/test_full.csv")


