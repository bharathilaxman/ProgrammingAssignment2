## makeCacheMatrix: This function creates special "matrix" object that can cache its inverse
## cacheSolve:This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix  
## has not changed), then the cacheSolve will retrieve the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<- NULL
  }
get <- function()x
    setinverse <- function (inverse)inv <<- inverse
    getinverse <- function()inv
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Following function "CacheSolve" will check whether 
## the inverse is computed. If already calculated it will be 
## retrieved from the cache; if not then it will be computed
## and the value will be set in the cache through 
## "setinverse" function.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # if the inverse has already been calculated
  if (!is.null(inv)){
  # gets it from cache and skips the computation
    message("getting cached data")
    return(inv)
  }
  # if not calculates the inverse
  data <- x$get()
  inv <- solve(data, ...)
  # sets the value of inverse in the cache via the setinv function
  x$setinv(inv)
  inv
}

##Solution after running a sample data in R:

## x <- rbind (c(2,4), c(4,2))
## d = makeCacheMatrix(x)
## d$get()
## [,1] [,2]
## [1,]    2    4
## [2,]    4    2
## cacheSolve(d)
##  [,1]       [,2]
## [1,] -0.1666667  0.3333333
## [2,]  0.3333333 -0.1666667

## Second run:
## cacheSolve(d)
## getting cached data
##   [,1]       [,2]
## [1,] -0.1666667  0.3333333
## [2,]  0.3333333 -0.1666667
  
