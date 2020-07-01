## The functions presented here cache the inverse of a given matrix.

## The following function creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
     
     inversa <- NULL
     set <- function(y){
          x <<- y
          inversa <<- NULL
     }
     get <- function() {x}
     setinv <- function(solve) {inversa <<- solve}
     getinv <- function() {inversa}
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The next function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
      
     inversa <- x$getinv()
     if(!is.null(inversa)){
          message("geting cache data")
          return(inversa)
     }
     data <- x$get()
     inversa <- solve(data,...)
     x$setinv(inversa)
     inversa  ## Return a matrix that is the inverse of 'x'
}
