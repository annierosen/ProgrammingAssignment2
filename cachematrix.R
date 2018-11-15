
## In conjunction, makeCacheMatrix and cacheSolve accepts a matrix (presumuably an invertible one) as input 
## and checks whether it's inverse has already been computed. If it has, the inverse matrix is returned. 
## If it hasn't, the inverse is computed, saved, and returned. 

## makeCacheMatrix accepts a matrix as input, and returns a list of functions which 
## save the input matrix and its computed inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL 
	}
	get <- function() x 
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve accepts a matrix as input and checks to see if the inverse has already been computed and saved within 
## the list returned by makeCacheMatrix. If it hasn't, then the inverse is computed and saved in the list. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
