## The first function CacheMatrix cache's the inverse of matrix when called first time through cachesolve functions;

makeCacheMatrix <- function(x = matrix()) {

	    m <- NULL #### Initializing object
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setmat <- function(solve) m <<- solve  #### getting the inverse matrix 
            getmat <- function() m
            list(set = set, get = get,
                 setmat = setmat,
                 getmat = getmat)
}


## The cacheSolve function first tries to get the inverse of matrix by getmat function from makeCacheMatrix; 
## if it is not present then calculates it using solve function and then further set it up in makeCacheMatrix using setmat function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 	    m <- x$getmat()
            if(!is.null(m)) {
                    message("getting cached data") #### Getting the inverse from cache
                    return(m)
            }
            mat <- x$get()
            m <- solve(mat, ...) #### If not present in cache then solving it
            x$setmat(m) ####Then setting it in cachematrix
            m
}
