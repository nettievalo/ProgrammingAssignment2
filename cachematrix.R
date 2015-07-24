## R Programming --- Online course 
######################################################################
## This file contains two functions:
##  - makeCacheMatrix: creates a matrix object which can store
##                     its inverse.
##  - cacheSolve: compute/recovers the inverse matrix of a
##                makeCacheMatrix object.
## @author Mariana Rodrigues <nettievalo-coursera@yahoo.com.br>
######################################################################


## makeCacheMatrix creates a matrix object with three attributes:
##   - m: the original matrix. Default value = NA
##   - inv: the inverse matrix of m
makeCacheMatrix <- function(m = matrix()) {
        ## Attribute m --- Original matrix.
        ## Attribute inv --- inverse matrix of m.
        inv <- NULL
        ## Function setMatrix(y) --- saves in m the matrix given 
        ## in y. inv value is set to NULL so that it is known that
        ## original matrix m was modified.
        setMatrix <- function(y) {
                m <<- y
                inv <<- NULL
        }
        ## Function getMatrix() --- returns original matrix m.
        getMatrix <- function() m
        ## Function setInverse(mInv) --- sets the inverse of m
        ## to be mInv.
        setInverse <- function(mInv) inv <<- mInv
        ## Function getInverse() --- returns inverse matrix of m.
        getInverse <- function() inv
        ## List of functions.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve takes a cacheMatrix object and either computes its
## inverse, if not yet computed, or recovers it from object otherwise.
cacheSolve <- function(cacheMatrix, ...) {
        ## Get inverse from object
        inv <- cacheMatrix$getInverse()
        if(!is.null(inv)){
                ## There is already a computed inverted matrix.
                message("getting cached data")
        }else{
                ## No inverse exists ---- compute and save it.
                inv <- solve(cacheMatrix$getMatrix())
                cacheMatrix$setInverse(inv)
        }
        ## Return inverse matrix
        inv
}
