## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# Test Output
# > myMat<-makeCacheMatrix(matrix(c(4,3,3,2),2,2))
# > myMat$get()
# [,1] [,2]
# [1,]    4    3
# [2,]    3    2
# > myMat$getInverse()
# NULL
# > cacheSolve(myMat)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > myMat$getInverse()
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(myMat)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > myMat$getInverse()
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > 
