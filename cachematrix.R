## Put comments here that give an overall description of what your
## functions do
##The two functions below can cache a inverse matrix of 'x',and next time if we 
##need the inverse matrix of 'x' we can call it instead of computing it again.
## Write a short comment describing this function
##The function makeCacheMatrix can cache a matrix 'x'.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
##The function cacheSolve can calculate the inverse matrix of 'x',and it can tell
##us whether we have done it before.
##If we have done it ,we can get the value of the inverse matrix of 'x' instead of
##computing it again.
cacheSolve <- function(x, ...) { 
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
