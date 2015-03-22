## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function set and get the value of the matrix, 
# set/get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inversa <- NULL  #limpiamos
    set <- function(y) {
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    setinversa <- function(invertimos) inversa <<- invertimos 
    
    getinversa <- function() inversa  
    
    list(set = set, get = get,
         setinversa = setinversa,
         getinversa = getinversa)
    
    
    
}


## Write a short comment describing this function
# the function checks if the inverse of a matrix is in the cache, if it's not , 
# it computes it and set it in the cache for future computations. 
# If it is already in the cache, it returns the inverse and do not compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversa <- x$getinversa()
    # si  esta cacheada la recuperamos y paramos la funcion
    if(!is.null(inversa)) {
#         cat("get the inverse from cache")
        return(inversa)
    }
    data    <- x$get()      # retrieve matrix
    inversa <- solve(data)  # calculate the inverse of the matrix
    x$setinversa(inversa)   # cacheamos for next run 
    
    inversa
    
    
}


