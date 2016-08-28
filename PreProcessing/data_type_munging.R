#
# Converting this to that in useful situations
# Dates, Logical, Character, Integer, Numeric conversions, etc.
#

# Convert TRUE/FALSE to 1/0 (respectively):

    logical_to_bit <- function (x) {
      myData <- copy(x)  # I don't know why... data.tables made this break
      p_logi <- names(people)[which(sapply(myData, is.logical))]
      for (col in p_logi) {
        set(myData, j = col, value = as.numeric(myData[[col]]))  # REQUIRES data.table !
      }
      return(myData)
    }
    
    train <- logical_to_bit(train)  # usage example
    
# 
