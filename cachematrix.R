
## A pair of functions that take a matrix, create an object that can cache the
## inverse of that matrix, which is then passed to a function that either
## computes the inverse of that matrix or retrieves it from the cache.


## Takes a matrix, returns a list of named functions that retrieve (get) the
## value of the matrix and its inverse and assign (set) them in the parent
## environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(get = get, set = set,
       getinv = getinv,
       setinv = setinv)
}


## Takes a cacheMean object, retrieves the inverted matrix from it and returns
## it. If the inverse has not yet been cached (set), instead retrieves the
## original matrix and computes its inverse, returning the inverse as well as
## cacheing it


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) 
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


