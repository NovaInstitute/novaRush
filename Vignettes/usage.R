

#create database
createDb("authority/test")

colNames <- c("mpg", "cyl", "disp")
# create schema
schemaList <- lapply(colNames, function(x) {
  return(list(
    "_id" = '_predicate',
    name = paste("", x, sep = ""),
    type = 'string'
  ))
})


#create predicate
createPredicate("rdataset/mtcars", schemaList)

#insert data
insertData("rdataset/mtcars", dataList = mtcars[, colNames])




