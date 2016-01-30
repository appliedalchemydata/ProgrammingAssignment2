## Matrix inversion can be expensive. These functions perform inverstion
## and caches the resulting matrix

## This function creates a special "matrix" object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	
	list(set=set,get=get, 
		setinverse=setinverse, 
		getinverse=getinverse)
}


## function computes and caches the inverse of the special matrix

cacheSolve <- function(x, ...) {
      
	inverse <- x$geinverse()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	
	data <- x$get()
	inverse <- solve(data)
	x$setinverse(inverse)
	inverse
	
}
