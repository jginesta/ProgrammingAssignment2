## Caching the Inverse of a Matrix
## This function calculates the inverse of the matrix and stores it in cache to be retrieved later.
## This function assumes that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

#  1.sets the value of the matrix
#  2.gets the value of the matrix
#  3.sets the value of the inverse matrix
#  4.gets the value of the interse matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix = matrix()) inverse <<- inverseMatrix
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting inverse matrix from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

## Example for running:
#  matrix<-makeCacheMatrix(matrix(c(4,2,7,6), 2, 2))

#  >matrix$get()
#  [,1] [,2]
#  [1,]    4    7
#  [2,]    2    6

#  >matrix$getinverse()
#  NULL

#  >cacheSolve(matrix)
#  [,1] [,2]
#  [1,]  0.6 -0.7
#  [2,] -0.2  0.4

#  Second time it solves the matrix
# getting inverse matrix
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

#  matrix$getinverse()
#  [,1] [,2]
#  [1,]  0.6 -0.7
#  [2,] -0.2  0.4