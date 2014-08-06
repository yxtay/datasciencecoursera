## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## makeCacheMatrix() is a special matrix containing a list of function associated to it: 
## 1. set() sets the value of the matrix
## 2. get() gets the value of the matrix
## 3. setInv() set the inverse of the matrix
## 4. getInv() get the inverse of the matrix
## note the use of "<<-" for assignment in the global environment, which is the trick to caching the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # assigns the value of the matrix to x and reset the inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # retrieve the value of the matrix
    get <- function() x
    
    # assigns the value of the inverse and cache it in inv variable
    setInv <- function(inverse) inv <<- inverse
    
    # retrieve the value of the inverse
    getInv <- function() inv
    
    # returns the function as a list
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
##
## cacheSolve() calculates the inverse of the special matrix from makeCacheMatrix()
## it first checks if the inverse had been calculated previously and return the inverse from the cache if so.
## this saves time as calculating inverse can be computationally intensive for large matrix.
## if the inverse was not cached, it calculates the inverse and store it in the cache before returning the outcome.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check if inverse has been calculated previously and return it if so
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("using cached data")
        return(inv)
    }
    
    # else, retrieve the value of the special matrix and calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # cache the inverse calculated before returning the outcome
    x$setInv(inv)
    inv
}