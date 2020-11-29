## This function creates a special "matrix" object that can 
## cache its inverse.

## The function makeCacheMatrix creates a special "matrix", which is really a 
## list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the Inverse of the matrix
## 4. get the value of the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    Ima <- NULL
    set <- function(y) {
        x <<- y
        Ima <<- NULL
    }
    get <- function() x
    
    if(nrow(x)==ncol(x) && det(x)!= 0) {
        setsolve <- function(solve) Ima <<- solve
        
    }else { message("The matrix is not invertible")
        return(x)
    }
    
    getsolve<- function() Ima
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
    
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    Ima <- x$getsolve()
    if(!is.null(Ima)) {
        message("getting cached data")
        return(Ima)
    }
    get <- function() x
    data <- x$get()
    Ima <- solve(data, ...)
    print(Ima)
}
