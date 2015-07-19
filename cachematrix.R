##
##	Project 2
##
##			R Programming
##

##
#
# function makeCacheMatrix 
#
#	This function creates a special "matrix" object 
#	that can cache its inverse.
#
#	It is a slight modification of makeVector function 
#	illustrated in the instruction.
#
#	No error check is performed for this project.
#
##
makeCacheMatrix <- function ( x = matrix ( ) ) {
	m <- NULL
	set <- function ( y ) {
		x <<- y
		m <<- NULL
	}
	get <- function ( ) x
	setinv <- function ( inv ) m <<- inv
	getinv <- function ( ) m
	list (	set = set, get = get,
			setinv = setinv,
             	getinv = getinv
		)
}

##
#
# function cacheSolve
#
#	This function computes the inverse of the special "matrix" 
#	returned by makeCacheMatrix. If the inverse has already 
#	been calculated (and the matrix has not changed), 
#	then the cachesolve should retrieve the inverse from the cache.
#
#	It is a slight modification of cachemean illustrated in the instruction.
#
##
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinv ( )
	if ( ! is.null ( m ) ) {
		message("getting cached data")
		return(m)
	}
	data <- x$get ( )
	m <- solve ( data , ... )
	x$setinv ( m )
	m
}
