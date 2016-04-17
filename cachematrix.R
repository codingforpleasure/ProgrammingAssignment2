## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

# Below in function "makeCacheMatrix" I'm registering 4 functions 
# set - sets the matrix and cleans the inv, since we should recalculate it.
# get - retrieve the matrix
# setInverse - setting the inverse matrix in the cache
# getInverse - returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheInvert <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# For benchmarking we can use the tests which I write blow:

# source("cachematrix.R")
# original_matrix <- matrix(runif(16,0,200), 4, 4)
# 
# # without caching it took:
# start.time <- Sys.time()
# for (i in 1:10000)
#   result <- solve(original_matrix)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(paste("Without caching it took:",time.taken,"seconds."))
# 
# my_matrix <- makeCacheMatrix(original_matrix)
# # with caching it took:
# start.time <- Sys.time()
# for (i in 1:10000)
#   result <- cacheInvert(my_matrix)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# print(paste("With caching it took:",time.taken,"seconds."))