## Put comments here that give an overall description of what your
## Functions calculate the inverse of a matrix. makeCacheMatrix create the variables 
##that will store the values. And cacheSolve does all the operations on the matrix to
##return the result

## Write a short comment describing this function
## setting variables
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function
## Solving inverse matrix
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

