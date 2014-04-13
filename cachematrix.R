## This piece of code can be used to cache result of solve function to inverse
## square matrix. Solve funtion is called only once for each matrix. 

## Function (object) containing cached inversed matrix 

makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL # variable to store cached value
    set <- function(y) 
    {
        x <<- y
        cached <<- NULL # empty the cache, because matrix is new
    }
    get <- function() 
    {
        x
    }
    setInverted <- function(inverted)
    {
        cached <<- inverted #sets computed value to cache
    }
    getInverted <- function()
    {
        cached #returns cached value
    }
    list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## This function returns cached result of solve function if it's exists. 
## if not it computes it and set result to cache and then returns

cacheSolve <- function(x, ...) {
    inverted <- x$getInverted()
    if(!is.null(inverted)) # checks if is cached
    {
        message("from cache")
        return(inverted)
    }
    
    data <- x$get()
    computed <- solve(data, ...) #if not cached lets compute inversed matrix
    x$setInverted(computed) #set to cache
    computed
}
