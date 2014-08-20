## These to functions allow to use functions objects which incapsulate a matrix.  
## The function object caches the invers of the matrix to save computational time.
##
## To access the cached invers use the function cacheMatrix with a makeCacheMatrix object
## as an argument


## This function incapsulate a matrix and caches its inverse matrix.
##
## x        : non singluar matrix
## return   : a list of all possible functions

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinvers <- function(invers) m <<- invers
      getinvers <- function() m
      list(set = set, get = get,
           setinvers = setinvers,
           getinvers = getinvers)
}


## Use this function to access the inverse matrix of a makeCacheMatrix function
## object.  
##
## x        : makeCacheMatrix function object   
## return   : the invers matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinvers()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setinvers(m)
      m
}

## This alternative method also creates a function object which encapsulates it invers
## matrix.  However, here the cached matrix can be accessed directly from the 
## function object.  The inverse matrix is evaluated and cached on demand
## (lazy initialization).
##
## x        : non singluar matrix
## return   : a list of all possible functions

makeCacheMatrix2 <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      getinvers <- function(){
            if (is.null(m)){
                  message("create invers matrix")
                  m<<-solve(x)
            } 
            m
      } 
      list(set = set, get = get, getinvers = getinvers)
}

