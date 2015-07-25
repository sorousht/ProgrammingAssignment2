# Creates a special type of matrix that caches its inversed value

# Creates CacheMatrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # get and set for matrix
    get <- function() x
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    # get and set for mean value
    getinverse <- function() m
    setinverse <- function(inverse) i <<- inverse

    list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


# Inverses a matrix 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)){
        message('getting cached data')
        return(i)
    }

    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)

    i
}
