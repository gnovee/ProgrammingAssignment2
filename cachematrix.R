# R Programming - Week 3 Assignment: Caching the Inverse of a Matrix

# Functions ----
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


# Main ----
## 1. Create a matrix, save as `val` variable
val <- rbind(c(1, -1/4), c(-1/4, 1))
## 2. Create cache matrix from `val`
val.cached <- makeCacheMatrix(val)
## 3. Get inversed matrix
cacheSolve(val.cached)
## 4. Get inversed matrix again (cached)
cacheSolve(val.cached)


