## 2 functions as described below
##makeCacheMatrix is used to create a special object that stores a matrix and cacheSolve caches its inverse

## makeCacheMatrix - This function takes a matrix and defines a list of getter and setter functions as listed below
## set(y) - sets the value of the matrix
## get() - gets the value of the matrix
## setinverse(inverse) - sets the inverse of the matrix
## getinverse() - gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve caluclates the inverse of the special matrix created using makeCacheMatrix
## if an inverse has already been calculated and cached, then it returns the inverse from the cache, 
## otherwise it performs the calculation, and adds the inverse to the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
