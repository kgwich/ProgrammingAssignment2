## This program writes a pair of functions that caches the inverse of a matrix.
## It includes makeCacheMatrix and cacheSolve.


## makeCacheMatrix creates a "special" matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## sets the value of inv to NULL to provide a default if cacheSolve has not been used
  inv <- NULL
  ## sets the value of the matrix
  set <- function(y){
    ##caches the inputted matrix so cacheSolve can check for changes
    x <<- y
    ## sets the value of inv for cacheSolve to NULL
    inv <<- NULL
  }
  get <-function() x
  setinv <- function(inverse)
    inv <<- inverse
  getinv <- function()inv
  ## creates list of the four functions
  ## 1. set will set the matrix
  ## 2. get will get the matrix
  ## 3. setinv will set the inverse
  ## 4. getinv will get the inverse
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## cacheSolve computes the inverse of the matrix made by makeCacheMatrix.
## If the inverse has been calculated and the matrix has not changed,
## cacheSolve should retrieve the inverse of the cache.
## Otherwise, it computes the inverse and sets the value in the cache with the
## setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  # if the inverse has already been calculated
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # calculates the inverse if it has not already been calculated
  data <- x$get()
  inv <-solve(data)
  
  #sets the value in the cache through the setinv function
  x$setinv(inv)
  
  return(inv)
}



##Test the program
## x= rbind(c(1,.5), c(.5,1))
## m= makeCacheMatrix(x)
## m$get()

##Expected Results
##     [,1] [,2]
##[1,]  1.0  0.5
##[2,]  0.5  1.0

## cacheSolve(m)
##       [,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333

# run cacheSolve(m) again
## cacheSolve(m)
## getting cached data
##      [,1]       [,2]
##[1,]  1.3333333 -0.6666667
##[2,] -0.6666667  1.3333333


