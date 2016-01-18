#
# For xgboost and some other useful things, factor and character fields
# can be replaced with integers; it's rather tricky to get them to match
# between test and train, when the two may have different sets of values
#

featurenames <- intersect(names(train), names(test))
featurenames <- setdiff(featurenames, c('id', 'date_first_booking', 'country_destination'))  # Adjust as needed

for (i in featurenames) {
  if (class(train[,i]) != class(test[,i])) { print("Warning: type mismatch between test and dev") } # We could exit, but meh
  if (class(train[,i]) == 'factor') {             # Convert everything to character first
    train[,i] <- as.character(train[,i])
    test[,i] <- as.character(test[,i])
  }
  
  if (class(train[[i]])=="character") {
    mylevels <- unique(c(train[[i]], test[[i]]))  # build a list of alllll the levels
    colname = paste0(i, '_num', sep='')
    train[[colname]] <- as.integer(factor(train[[i]], levels=mylevels))  # use the same levels list
    test[[colname]]  <- as.integer(factor(test[[i]],  levels=mylevels))  # in test and train
    featurenames <- setdiff(featurenames, i)
    featurenames <- union(featurenames, colname)
  }
}
