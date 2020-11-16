## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its solve. 

makeCacheMatrix <- function(x=matrix()) { 
      matr <- NULL
      set <- function (y) { 
            x <<- y 
            matr <<- NULL
      }
      get < function() x
      setsolve <- function(solve) matr <<- solve
      getsolve <- function() matr
      list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## cacheSolve: This function computes the solve of the special "matrix" returned by 
## makeCacheMatrix above. If the solve has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the solve from the cache. 

cacheSolve <- function(x, ...) { 
      matr <- x$getsolve()
      if(!is.null(matr)) {
            message("getting cached data")
            return(matr)
      }
      data <- x$get()
      matr <- solve(data, ...)
      x$setsolve(matr)
      matr
}
