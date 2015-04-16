## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  x.inv = NULL
  set = function(y) {
    x <<- y
    x.inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) x.inv <<- inverse 
  getinverse = function() x.inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The following function calculates the mean of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  
  ## The following function calculates the mean of the special "matrix" created with the above function. 
  x.inv = x$getinverse()
  
  ## However, it first checks to see if the inverse has already been calculated. 
  if (!is.null(x.inv)){
    ## If so, it gets the inverse from the cache and skips the computation. 
    message("getting cached data")
    return(x.inv)
  }
  
  ## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function.
  mat.data = x$get()
  x.inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinverse(x.inv)
  
  return(x.inv)
}
