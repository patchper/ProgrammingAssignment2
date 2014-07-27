## This function tries to cache the inverse of a matrix in case 
## the matrix is large and takes a long time to caculate even if 
## it has not been changed since lat time

## makeCacheMatrix creates a list containing a function to
##    set the value of the matrix
##    get the value of the matrix
##    set the inverse of the matrix
##    get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
		}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set =set, get = get,
		 setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the list created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the mean from the cache. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)){
		message("getting cached inverse")
		return(m)
	}
	mdata <- x$get()
	m <- solve(mdata, ...)
	x$setinverse(m)
	m
    ## Return a matrix that is the inverse of 'x'
}
