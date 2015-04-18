## This function is to cache potentially time-consuming computations, for this
## case is the inversion of a matrix. We cache the inverse of a matrix so that 
## when we need it again, it can be looked up in the cache rather than recomputed.

## This function is to create a special "matrix", which is really a list containing
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	i <- NULL
	## function 1
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	## function 2
	get <- function() x
	## function 3
	setinverse <- function(inverse) i <<- inverse
	## function 4
	getinverse <- function() i
	## create the list
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function calculates the inverse of the special "matrix"
## created with the above function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

	## if already calculated, just get the inverse from
	## the cache
	i <- x$getinverse()
	if(!is.null(i)) {
                message("getting cached data")
                return(i)
      }
	
	## otherwise, calculate the inverse by "solve" function
	data <- x$get()
      i <- solve(data, ...)
	
	## set the value of the inverse in the cache
      x$setinverse(i)
      i
	
}

