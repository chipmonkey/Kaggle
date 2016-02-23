CompareSets <- function(test, train) {
  par( mfcol=c(3,3))
  
  comprows <- ifelse(nrow(train) < nrow(test), nrow(train), nrow(test))
  fields <- intersect(names(train), names(test))
  fields <- setdiff(fields, 'Id')
  fields <- setdiff(fields, names(which(sapply(train, class) =='factor')))
  
  for (name in sort(fields)) {
    print(name)
    if (class(train[[name]]) %in% c('numeric', 'integer')) {
      plot(density(na.omit(train[1:comprows,name])), col=rgb(1,0,0,0.5), main=name)
      lines(density(na.omit(test[1:comprows,name])), col=rgb(0,0,1,0.5))
    }
  }
  
  par( mfcol=c(1,1))
}
