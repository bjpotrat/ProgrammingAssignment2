## Make getting the inverse of a matrix through using a matrix cache

## Create an Cached Matrix
makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
    set <- function(y) 
    {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(mat) x <<- mat
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }

## Caclulate the inverse of x
cacheSolve <- function(x, ...) 
  {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }
