cachematrix
===========

Programming Assignment 2 (peer assessment)
 x <- matrix(1:4, 2, 2)
 y <- makeCacheMatrix(x)
 
 makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
        set <- function(y) {
                 x <- y
                 inv <- NULL
         }
         get <- function() x
         setinv <- function(nInv) inv <- nInv
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)}

 cacheSolve <- function(x, ...) {
         inv <- x$getinv()
         if(!is.null(inv)) {
                message("getting cached data")
                return(inv)         }
         data <- x$get()
         inv <- solve(data, ...)
         x$setinv(inv)
         inv }
         
