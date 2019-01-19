## Put comments here that give an overall description of what your
## functions do
## The two functions here return the inverse of a matrix. If its inverse is stored in the cache, 
## the inverse is retrieved directly from cache, othewise it is calculated and returned.

## Write a short comment describing this function
## The function makeCacheMatrix creates a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                 
    set <- function(y) {      
        x <<- y               
        i <<- NULL            
    }
    get <- function() x       
    setinv <- function(inv) i <<- inv      
    getinv <- function() i                   
    list(set = set, get = get,                 
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function
## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    i <- x$getinv()           
    if(!is.null(i)) {          
        message("getting cached data")          
        return(i)                               
    }
    data <- x$get()            
    i <- solve(data, ...)       
    x$setinv(i)               
    i        
    ## Return a matrix that is the inverse of 'x'
    
}
