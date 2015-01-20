## Below are two functions that are used to create a special object that stores a numeric 
## vector and caches its inverse matrix.

## makeCacheMatrix creates a special "vector", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    

}


## CacheSolve calculates the inverse matrix of the special "matrix" created with the above function.
## 1 Checks to see if the inverse matrix has already been calculated. 
## 2 If so, it gets the inverse matrix from the cache and skips the computation. 
## 3 If no, it calculates the inverse matrix of the data and sets the value 
## of the inverse matrix in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

