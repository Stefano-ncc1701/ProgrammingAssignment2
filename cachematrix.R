## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matr <- NULL
        set <- function(y) {
                x <<- y
                matr <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) matr <<- solve
        getsolve <- function() matr
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


##	This function computes the inverse of the special
##	"matrix" returned by `makeCacheMatrix` above. If the inverse has
##	already been calculated (and the matrix has not changed), then
##	`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matr <- x$getsolve()
        if(!is.null(matr)) {
                message("getting cached data")
                return(matr)
        }
        data <- x$get()
        matr <- solve(data, ...)
        x$setsolve(matr)
        matr
}
		
