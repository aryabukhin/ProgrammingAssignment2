## This functions create matrix wrapper which can cache 
## inverse result and return if it was calculated before.
## 

## create "matrix" which can cache inverse result

makeCacheMatrix <- function(x = matrix()) {
  # inverse result. NULL at init
  inv <- NULL
  
  # set new source matrix. Clear cached inverse value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return real matrix value
  get <- function() x
  
  # set calculated inverse matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # return cached inverse matrix
  getinverse <- function() inv
  
  # create "interface" for this matrix wrapper
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## try to find cached inverse value and return it 
## or calculate and cache if not found

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## try to find in cache
  inv <- x$getinverse()
  if (!is.null(inv)){
    # Found. Return cached value
    message ("getting cached data")
    return (inv)
  }
  
  ## if cached  result not found calculate, cache and return it
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
