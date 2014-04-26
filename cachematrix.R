## Below source code defines two function two work with matrix inverse calculation.
## Function 'makeCacheMatrix' takes a square matrix and 
## returns a list of functions that implement caching for the given matrix.
##
## Function 'cacheSolve' is similar to 'solve' except it returns a cached solution if one already exist.
## If there is no cached value exist it delegates to 'solve' function to compute the result and maintains it in cache.
## Input for 'cacheSolve' must be a matrix with caching support( makeCacheMatrix returns a matrix with caching support ). 
##
##  Example usuage 
##      cachableMatrix <- makeCacheMatrix(rnorm(16),4,4)
##      # execution 1
##      result1 <- cacheSolve(cachableMatrix)    # computes and caches the result first time
##      ...
##      resultn <- cacheSolve(cachableMatrix)    # return already cached result.
## 



## 'makeCacheMatrix' funciton takes an imput matrix and returns a 
##  list of functions that support caching inverse matrix.
##  

makeCacheMatrix <- function(x = matrix()) {
    
    inverseCached <- NULL       # initialize cache with NULL.
    
    # set function to set the matrix data.
    set <- function(dataMatrix){
        x<<-dataMatrix              
        inverseCached <<- NULL
    }
    
    # returns the matrix data.
    get <- function(){
        x
    }
    
    # store the inverse in cache ( global environment)
    setInverse <- function(inverseMatrix){
        inverseCached <<- inverseMatrix
    }
    
    # return the value stored in cache( global environment)
    getInverse<-function(){
        inverseCached
    }
    
    #return a list that contains funcitons to read and store matrix(get ,set)and inverse matrix (getInverse,setInverse)
    list(set =set, get=get,setInverse = setInverse,getInverse=getInverse)
}




## Function that takes a square matrix and return inverse.
## this function calculates inverse (solve) for a given input list that is created using 'makeCacheMatrix' 
## and caches the result; any subsequent function call  returns the same cached inverse value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverseResult <- x$getInverse()  # get the inverse value from cache
    
    if(!is.null(inverseResult)){  # if there is a cached value return it.
        message("Cache hit success : returning cached data")
        return (inverseResult)
    }
    
    dataMatrix <- x$get()                   # extract the input matrix for which inverse needs to be calculated.
    inverseResult <- solve(dataMatrix, ...) # calculate inverse
    x$setInverse(inverseResult)             # store it in cache for future calls.
    inverseResult                           # return the result.
}
