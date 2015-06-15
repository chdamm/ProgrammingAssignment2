## The makeCacheMatrix function creates an object that can hold a matrix along with its (cached) inverted matrix.
##
## The cacheSolve function inverts such an object. It caches the result, so subsequent calls to cacheSolve
## passing the same object will instead return the cached result.



## Creates an object which can hold a matrix along with its inverted matrix.
## Use set/get to set/get the matrix.
## Use setInverse/getInverse to set/get the matrix' inverse.
makeCacheMatrix <- function(x = matrix()) {
    xInverse <- NULL
    
    set <- function(y) {
        x <<- y
        xInverse <- NULL
    }
    get <- function() {
        x
    }
    
    setInverse <- function(inverse) {
        xInverse <<- inverse
    }
    getInverse <- function() {
        xInverse
    }
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function inverts a cachedMatrix object.
## If the cachedMatrix already contains a cached version of the inverted matrix, that gets returned.
## Otherwise, cacheSolve performs the inversion and then caches the result in the cachedMatrix object.
cacheSolve <- function(x, ...) {
    xInverse <- x$getInverse()
    if (!is.null(xInverse)) {
        message("getting cached data")
        return(xInverse)
    }
    
    # Calculate and cache
    data <- x$get()
    xInverse <- solve(data)
    x$setInverse(xInverse)
    xInverse
}



# Sample code
# 1. Create a sample matrix
x=rbind(c(1, -1/4), c(-1/4, 1))  
cacheMatrix <- makeCacheMatrix(x)
# 2. Now invert the cacheMatrix twice. The first time it should perform the inversion.
#    The second time, it should indicate that it's getting the cached data instead.
print(cacheSolve(cacheMatrix))
print(cacheSolve(cacheMatrix))



