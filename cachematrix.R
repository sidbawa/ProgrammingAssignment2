## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

## makeCacheMatrix creates a special "matrix" which is really a list containing functions to set and get a matrix, set and get value of inverse

makeCacheMatrix <- function(a = matrix()) {
	inv <- NULL
	set <- function(b) {
		a <<- b
		inv <<- NULL
	}
	get <- function() a 
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse matrix from cache when available

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- a$getinverse()
		if(!all(is.na(inv))){
			message("getting cached data")
			return(inv)
		}
		
		data <- a$get()
		inv <- solve(data,...)
		a$setinverse(inv)
		inv
}
