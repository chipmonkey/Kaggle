# Based on Ben Harmer script from Springleaf
# Forked because the prior version was getting messy...
# This is not too special, but adds a custom error function to the xgb.train call
# which I've been meaning to learn how to do. ---Chip


library(xgboost)

set.seed(1729)  # Taxicab Number

# Custom function to make the error results in xgb.train easier to compare to leaderboard
rmspe.xgb <- function(preds, dtrain) {
  target <- getinfo(dtrain, "label")
  predicted <- preds
  x1 <- target - predicted
  x2 <- x1 / target
  x2[target==0] <- 0
  x3 <- x2*x2
  x4 <- sum(x3)
  x5 <- x4/length(target)
  x6 <- sqrt(x5)
  # Yes, this is needlessly complex, but I had debugging issues
  return(list(metric="error", value=x6))
}


cat("reading the train and test data\n")
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
store <- read.csv("../input/store.csv")

test$Date <- as.Date(test$Date)
train$Date <- as.Date(train$Date)

# Dates don't overlap... why bother?

train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
train$dayofyear <- as.POSIXlt(train$Date)$yday
train$daysinperiod <- as.integer(train$Date - min(train$Date))

test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))
test$dayofyear <- as.POSIXlt(test$Date)$yday
test$daysinperiod <- as.integer(test$Date - min(test$Date))

train <- train[,!(names(train) %in% c("Date"))]
test <- test[,!(names(test) %in% c("Date"))]


# What is this field?
train.Customers <- train$Customers
train <- train[,!names(train) %in% c("Customers")]

train.Sales <- train$Sales
train <- train[,!names(train) %in% c("Sales")]



train <- merge(store, train, by = c("Store"))
Id <- test$Id
test <- test[,!names(test) %in% c("Id")]
test <- merge(store, test, by=c("Store"))

gc() ; proc.time()

cat("Names")
names(train)
cat("Structure")
str(train)
cat("Summary")
summary(train)
cat("Dimensions")
dim(train)

feature.names <- names(train)

# Check for coverage:
names(test) %in% names(train)
names(train) %in% names(test)

cat("Examine NAs")
sapply(train, function(x) length(which(is.na(x))))
sapply(test, function(x) length(which(is.na(x))))

cat("replacing missing values with something other than -1...\n")
# usually mean or some standard default
train$CompetitionDistance[is.na(train$CompetitionDistance)] <- 0
test$CompetitionDistance[is.na(test$CompetitionDistance)]   <- 0

# train$CompetitionOpenSinceYear <- pmax(train$CompetitionOpenSinceYear, 1999)
train$CompetitionOpenSinceYear[is.na(train$CompetitionOpenSinceYear)] <- 2009
# test$CompetitionOpenSinceYear <- pmax(test$CompetitionOpenSinceYear, 1999)
test$CompetitionOpenSinceYear[is.na(test$CompetitionOpenSinceYear)] <- 2009

train$CompetitionOpenSinceMonth[is.na(train$CompetitionOpenSinceMonth)] <- 6
test$CompetitionOpenSinceMonth[is.na(test$CompetitionOpenSinceMonth)] <- 6

train$Promo2SinceWeek[is.na(train$Promo2SinceWeek)] <- 26
test$Promo2SinceWeek[is.na(test$Promo2SinceWeek)] <- 26

train$Promo2SinceYear[is.na(train$Promo2SinceYear)] <- 2012
test$Promo2SinceYear[is.na(test$Promo2SinceYear)] <- 2012

cat("test$Open behaves oddly")
test[is.na(test$Open),] # These are weird
test$Open[is.na(test$Open)] <- 1  # Assume these stores are open

cat("Names")
names(train)
cat("Structure")
str(train)
cat("Summary")
summary(train)
cat("Dimensions")
dim(train)

# waste that ram!
alltrain <- train
alltrain.Sales <- train.Sales

h <- sample(nrow(train), nrow(train)*.75)
train <- alltrain[h,]
train.Sales <- alltrain.Sales[h]
valid <- alltrain[-h,]
valid.Sales <- alltrain.Sales[-h]

rm(alltrain)
rm(alltrain.Sales)

gc() ; proc.time()

# feature_names <- names(train)  # play with this later

cat("Making train and validation matrices\n")

dtrain <- xgb.DMatrix(data.matrix(train), label=train.Sales)
dvalid <- xgb.DMatrix(data.matrix(valid), label=valid.Sales)

gc() ; proc.time()

watchlist <- list(eval = dvalid, train = dtrain)

param <- list(  objective           = "reg:linear", 
                # booster = "gblinear",
                eta                 = 0.01,
                max_depth           = 8,  # changed from default of 6
                # subsample           = 0.6,
                # colsample_bytree    = 0.6,
                eval_metric         = rmspe.xgb
                # min_child_weight    = 50
                # alpha = 0.0001, 
                # lambda = 1
)

model_xgb <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 200,
                    verbose             = 1, 
                    early.stop.round    = 30,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    print.every.n       = 10
)

cat("Predicting Sales")

gc() ; proc.time()

xgb.importance(feature_names = names(train), model = model_xgb)
p.train <- predict(model_xgb, data.matrix(train))
rmspe.xgb(p.train, dtrain)

pred <- predict(model_xgb, data.matrix(test))
submission <- data.frame(Id=Id, Sales=pred)

cat("saving the submission file\n")
write.csv(submission, "xgb_script_submission.csv", row.names = FALSE)
