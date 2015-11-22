## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y = matrix()){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(y) i <<- y
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
##The below function has takes the functions returned by the makeCacheMatrix as the arguments

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        i
}
