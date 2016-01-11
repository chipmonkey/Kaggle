CompareSets <- function(test, train) {
  oldpar <- par( mfcol=c(3,3))
  
  comprows <- ifelse(nrow(train) < nrow(test), nrow(train), nrow(test))
  fields <- intersect(names(train), names(test))
  fields <- setdiff(fields, 'Id')
  fields <- setdiff(fields, names(which(sapply(train, class) =='factor')))
  
  for (name in sort(fields)) {
    plot(density(na.omit(train[1:comprows,name])), col="red", main=name)
    lines(density(na.omit(test[1:comprows,name])), col="blue")
  }
  
  par(oldpar)
}
