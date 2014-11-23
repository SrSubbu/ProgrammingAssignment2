## 2 Functions to make a special matrix and cache the matrix inverse

## Function to make special matrix - list of get and set methods are returned

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the value of m
  m <- NULL
  ##function to set the value of m and x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##function to get the value of x
  get <- function() x
  ##function to get the value of matrix inverse
  setinverse <- function(inverse) m <<- inverse
  ##function to set the value of matrix inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## function to get inverse of matrix given by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##get the inverse value of matrix
  m <- x$getinverse()
  ##check whether its value is null
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##get the matrix for which inverse has to be obtained
  data <- x$get()
  ##Get the matrix inverse
  m <- solve(data)
  ##set the matrix inverse for caching
  x$setinverse(m)
  m
}
