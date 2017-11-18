## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this functions
## 1. set the value of the Matrix x=matrix()
## 2. get the value of the Matrix
## 3. set the value of the inverse inv <<- solve
## 4. get the value of the inverse results
makeCacheMatrix <- function(x = matrix()) {
          inv <- NULL
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          get <- function() x
          setInverse <- function(solve) inv <<- solve
          getInverse <- function() inv
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## Write a short comment describing this function
## 1. First check if the inverse calculation has already been made.
## 2. If calculated, then return the results from cache directly
## 3. If not, calculate inverse function and then return the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            inv <- x$getInverse()
            if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setInverse(inv)
            inv
}
