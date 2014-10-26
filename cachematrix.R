## Put comments here that give an overall description of what your
## functions do

## Create a list containing functions to set\get a matrix value
#  set\get inverse value

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv 
	list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
	
}


## Calculate inverse of a given matrix. It first checks if inverse is 
#	calculated already, in which case returns cached value, otherwise 
#	it calculates caches and returns inverse.

cacheSolve <- function(x, ...) {
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
