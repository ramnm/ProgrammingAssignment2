###################################################################################
## Programming Assignment 2 - R Programming                                      ##
## Author       - Ram Nadakuduru                                                 ##
## Date         - 2014-08-23                                                     ##
## Version History                                                               ##
## 0.0   2014-08-23     Program Created                                          ##
###################################################################################
##
## The makeCacheMatrix function will create the list that returns functions 
## to manipulate a particular matrix and its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
        inverse <- matrix()
        set <- function (y) {
                x <<- y
                inverse <<- matrix()
        }
        
        get <- function() x
##        
        setinverse <- function(i) {
                inverse <<- i
        }
##
        getinverse <- function() inverse
##
        list(set = set,get = get, setinverse = setinverse, getinverse = getinverse)
}


## 
## The CacheSolve function only accepts lists that are built by the makeCacheMatrix
## function. It checks if the inverse was already calculated and if so gets it from 
## the cache, if not it calculates it and then stores it in the cache for that matrix.
##
cacheSolve <- function(x, ...) {
        y <- x$getinverse()
        if(!is.na(y[1,1])){
                print("getting cached inverse")
                i <- x$getinverse()
                return(i)
        } 
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}