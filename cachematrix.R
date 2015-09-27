## Put comments here that give an overall description of what your

## cacheSolve function checked if there is a cached inverse matrix. If that's the case
## it retreives this value. If it didn't find a cached value it calls a function to calculate
## and then cache it.

##makeCacheMatrix controls the values of the matrix and the inverse matrix as described below

## functions do

## Write a short comment describing this function
##The function creates a list of functions that 
##1. Retreives the value of the matrix
##2. Sets the value of the matrix to a new value and in this case clears the cached inverse
##3. Sets the value of the cache variable holding the inverse of the matrix to the passed variable
##4. Retreives the value of the cached variable hodling the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  get <- function() x
  set <- function(y){
    x <<-y
    invMatrix <<- NULL
  }
  set_Inverse <- function(inv) invMatrix <<- inv
  get_Inverse <- function() invMatrix
  list(get = get, set = set, get_Inverse = get_Inverse, set_Inverse = set_Inverse)
}


## Write a short comment describing this function
## Function tries retreiving the cached Inverse. 
##If it hasn't been calculated yet it calculates it and sets the cached Inverse to the new value

cacheSolve <- function(x, ...) {
  
  invMatrix <- x$get_Inverse()
  
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  message("Calculating...")
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$set_Inverse(invMatrix)
  invMatrix
}
