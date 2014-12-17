## the makeCacheMatrix function will return a list containing four functions
##  1. set: will set the value of a matrix and cache it This will also 
##     clear the previously chched inverse matrix
##  2. get: returned the cached matrix.
##  3. setinverse: this will set the matrix inverse and cache it
##  4. getinverse: this will get the cached matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    ## clear the previously inverse
	inverse <- NULL
	set <- function (y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(newinverse) inverse <<- newinverse
	getinverse <- function() inverse
	##The list contains four function the get and set the matrix and its inverse
	list(set = set, get = get, setmean = setmean, getmean = getmean)

}


## The parameter of this function is the list object returned from
## makeCacheMatrix function. It first check if there is a previously
## cached matrix inverse. If the cached value exists, then just return it.
## Otherwise, it gets the matrix data and compute its inverse and cache the 
## inverse into the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getinverse()
		##If the inverse is cached, just return it.
		if(!is.null(inverse)) {
			message("getting cached data")
			return(inverse)
		}
		data <- x.get()
		##Calculate the matrix inverse and cache it.
		inverse <- solve(x)
		x$setinverse(inverse)
		inverse
}
