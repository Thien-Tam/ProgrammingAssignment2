## Put comments here that give an overall description of what your
## functions do

# 1. makeCacheMatrix:
#    This function creates a special "matrix" object that can cache its inverse
# 2. cacheSolve:
#    This function computes the inverse of the special "matrix" returned by
#    makeCacheMatrix above. If the inverse has already been calculated
#    (and the matrix has not changed), then the cachesolve should retrieve
#    the inverse from the cache

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set_inv the value of the inverse of the matrix
# 4. get_inv the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL # initialization
    
	# set the value of the matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	get <- function() x # get the value of the matrix
	set_inv <- function(inverse) inv <<- inverse # set the value of the inverse
	get_inv <- function() inv # get the value of the inverse
	
	# return a list of all above info
	list(set = set, get = get,
		 set_inv = set_inv,
		 get_inv = get_inv)

}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse 
# has already been calculated. If so, it gets the inverse from the cache via 
# the get_inv function and skips the computation. Otherwise, it calculates 
# the inverse of the matrix and sets the value of the inverse in the cache 
# via the set_inv function

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'

	# check if the inverse is already cached
	inv <- x$get_inv()
	if(!is.null(inv)) {
		# if so, we get the inverse from the cache directly
		message("getting cached inverse")
		return(inv)
	}

	data <- x$get() # else, we get the matrix
	inv <- solve(data, ...) # in order to calculate its inverse
	x$set_inv(inv) # then, cache the inverse of the matrix
	inv

}
