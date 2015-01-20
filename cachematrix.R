## Johns Hopkins University / Coursera
## R Programming
## Programming Assignment 2
## January 2015
##
## This module implements a CacheMatrix object, which stores a matrix and caches its inverse upon first computation.  If
## the matrix is updated, the cached inverse is cleared to maintain integrity.  This object should be used when a matrix
## and its inverse will be needed multiple times, in order to avoid unnessary recalculation of the inverse at the small
## cost of doubling memory required to store.
##

## makeCacheMatrix (x = matrix())
##   Receives a matrix to be stored, and returns a list of methods available for use:
##   $set(x = matrix()) - assign a new matrix to be stored
##   $get() - retrieve the matrix
##
##   To retrieve the inverse, use cacheSolve (see below)
##
## Two additional methods are provided but intended for internal use (by cacheSolve()).  Use outside of cacheSolve ()
## may have unintended consequences
##
##   $setInverse(y = matrix ()) - assign a new matrix to be stored as the inverse
##   $getInverse() - retrieve the matrix inverse as stored (may be NULL)
##

makeCacheMatrix <- function(x = matrix()) {

        xInverse <- NULL
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) xInverse <<- inverse
        getInverse <- function() xInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve (x = makeCacheMatrix())
##   Returns the inverse of matrix x in a computationally efficient manner, recalculating only if needed
##   Any included attributes are passed on to the R native matrix inversion function
##   NOTE: cacheSolve assumes the passed matrix is always invertible and does not check for invertibility


cacheSolve <- function(x, ...) {

        xInverse <- x$getInverse()
        if(!is.null(xInverse)) {
##		     Message to confirm cache usage 
##               message("getting cached data")
                return(xInverse)
        }
        data <- x$get()
        xInverse <- solve(data, ...)
        x$setInverse(xInverse)
        xInverse

}
