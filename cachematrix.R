## This file contains two functions
## makeCacheMatrix - store a matrix and its inverse. It porvides method to get/set inverse and matrix
## cacheSolve - It check if the given matrix ( as argument) as a cached inverse or not. If teh cached value
##              of inverse is present it returns the same otherwise it calculates. Matrix and inverse are also set.


# The belwo function creates a object which can hold a  matrix and its inverse
# It also has functions to set/get inverse and set/get matrix it self
# Return - A list containing four functions to set and get the value of the
#     matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # set inverse var 'inv' as NULL
  inv <- NULL
  # Define the set function to set matrix and clear old inverse var
  set <- function(y) {
    x <<- y    
    inv <<- NULL 
  }
  # function to get the value of the matrix
  get <- function() x
  # function to set the inverse. 
  setInverse <- function(inverse) inv <<- inverse
  # function to get the inverse
  getInverse <- function() inv
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This functions check if the inverse of matrix is already calculted or not.
# if its calculated then the cached value is returnd
# otherwise inverse is calcualted and also set in object (cache) to be used later

cacheSolve <- function(x) {
  m <- x$getInverse() # This will get the cached value for the inverse
  # check for null
  if(!is.null(m)) { 
    message("getting cached data") #get value from cache
    return(m)
  }
  # Else set matrix , get inverser and cache value of inverse
  data <- x$get()  
  inv <- solve(data) 
  x$setInverse(inv)  
  inv                
}