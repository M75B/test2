## Cache inverse matrix to save computing time in case of repeat
## use as cacheSolve(makeCacheMatrix(m)) or step by step
## all similar to vector example but for matrix

makeCacheMatrix <- function(x = matrix()) {                   
                 m <- NULL                                    ## initialize
                 set <- function(y) {                         ## define the set function
                               x <<- y                        ## to parent environment
                               m <<- NULL
                           }
                 get <- function() x                          ## define the get fucntion
                 setsolve <- function(solve) m <<- solve
                 getsolve <- function() m                     ## gets the value of m where called
                 list(set = set, get = get, setsolve = setsolve,getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
            m <- x$getsolve()                                 ## operating only with prepare matrix
            if(!is.null(m)) {                                 ## in case of exists gets the value from cache
                      message("getting cached data")
                      return(m)
                  }
            data <- x$get()
            m <- solve(data, ...)                             ## computes inverse of matrix
            x$setsolve(m)
            m
        }