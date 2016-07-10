## Overall these functions are designed to decrease the amount of time
## it takes to compute something. They cache the results so we won't
## have to compute long computations later on. 

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function returns the inverse of the matrix. 
## This function assumes that the matrix is always invertible. 
## If I run a computation that I already ran a message saying "getting
## cached data" will appear. 
cacheSolve <- function(x, ...) {
inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
inv
}
