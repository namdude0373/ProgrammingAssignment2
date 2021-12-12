## Create a matrix object that can cache computations, the current
## implementation only allows for caching of inverse matrices.

makeCacheMatrix <- function(x = matrix()) { # default is 1x1 matrix
  i <- NULL # pre-allocate the inverse otherwise setinverse() will fail
  
  # this is essentially a class and the below functions are methods
  
  set <- function(y){ # add input matrix to object and clear cache
    x <<- y
    i <<- NULL # adding in case matrix is set again
  }
  get <- function () x # return matrix value
  setinverse <- function(inverse) i <<- inverse # set argument to inverse (i)
  getinverse <- function() i # return inverse
  
  # below is a list which ties the functions to the attributes, allowing them to
  # be called by extracting the list element by name (using the $ operator). 
  # the list contains both instance variables and methods.
  
  invisible( # no terminal output is desired
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse))
}



## Compute the inverse of a matrix or return the cached inverse matrix if
## available. Note that makeCacheMatrix.set() clears any existing cache.

cacheSolve <- function(x) {
  m <- x$getinverse() # get cache
  if(!is.null(m)) { # if a inversed matrix is returned/there is a cache
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) # solve for inverse
  x$setinverse(m) # cache solution
  m
}