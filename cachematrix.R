## These functions perform Caching the squared matrix that is irreversible
## through the use of the below functions.


## Function 'makeCacheMatrix' create a matrix value that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x ## return the value of the matrix.
    setReverse <- function(r) minv <<- r
    getReverse <- function() minv # return the reverse of the matrix data.
    list(set = set, get = get,
         setReverse = setReverse,
         getReverse = getReverse)
        
}


##cacheSolve computes the inverse of the returned matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      m = x$getReverse()
      if (!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...) # solve the inverse of x
       x$setReverse(m)
       m
}



##random simple square matrix
##this is the input of the uniform distribution and runs two cache and the reverse functions.
checkme <- matrix(runif(100, 2, 20), 2, 2) 
cCached <- makeCacheMatrix(checkme)
cReverse <- cacheSolve(cCached)
cReverse