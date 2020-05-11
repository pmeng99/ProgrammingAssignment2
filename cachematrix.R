## The calculation of a matrix's inversion is often time consuming. 
## By caching the inverse of a matrix, subsequent call to get the inverse 
## will directly return from the cache and without the re-computation.

## makeCacheMatrix take a matrix as parameter. It "holds" 2 values: 
## 	the original matrix and 
##	its inverse (null when not yet calculated)
## makeCacheMatrix returns a vector of 4 functions:
## 	get(): get the matrix
## 	set(): set the matrix (to later get the inverse of)
## 	getinverse(): get the inverse matrix
##	setinverse(): set the inverse cache
## What makeCacheMatrix does not do is to call the solve function to calculate the inverse

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(mm) {
        m <<- mm
        i <<- NULL		## need to reset the cached inverse when the matrix is changed
    }
    get <- function() m		## retrun the matrix
    setinverse <- function(inverse) i <<- inverse	## set the cached inverse
    getinverse <- function() i	## return the cached inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)	## return a vector of 4 functions

}


## cacheSolve is a function that takes the return from makeCacheMatrix (a vector of 4 functions)
## and returns the inverse of the original matrix. If this is the first time this function is called
## on a matrix, it will call the R solve() function to calcualte the inverse and put it in the cache. 
## Subsequect call for the same matrix, it will return the inverse from the cache without calling
## R solve() function and therefore save time.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
