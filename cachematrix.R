##This program caches previously calculated matrix inverses in order to 
##retrieve them instead of recalculating them.

##This function,creates a special "vector", which is really a list containing 
##a function to:
      ## 1. set the value of the matrix
      ## 2. get the value of the matrix
      ## 3. set the value of the inverse
      ## 4. get the value of the inverse
##These functions are ultimately used to cache the solution to a inverse matrix 
##calculation which can then be retrieved later.
makeMatrix <- function(x = matrix()) {
      ##Inverse is defualt set to null until it is calculated and set
      i <- NULL
      ##Define a function that caches the matrix as a reference which
      ## will correspond to its inverse calculation once it has been cached
      set <- function(y) {
            x <<- y
            i <<- NULL
      } 
      ##Define a function that returns the matrix that was previously set 
      get <- function() x
      ##Define a function that caches the calculated inverse 
      setinverse <- function(solve) i <<- inverse
      ##Define a function that retrieved the previously cached inverse function
      ##returns null if not previously calculated
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}
##This function calculates the inverse of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the
##value of the inverse in the cache via the setmean function.
cacheinverse <- function(x, ...) {
      ##The getinverse function is called from the special vector
      i <- x$getinverse()
      ##The value of the inverse is tested to see if it has been previosly 
      ##calculated: if it has it returns the stored inverse value
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      ##If the inverse has not been previously calculated it proceeds to calculate it
      data <- x$get()
      i <- solve(data, ...)
      ##The setinverse function is called to cache the calculated inverse
      x$setinverse(i)
      i
}
