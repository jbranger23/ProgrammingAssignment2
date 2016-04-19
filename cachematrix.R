## Put comments here that give an overall description of what your
## functions do

## This function returns a list of function to set and get the matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
        invM <- matrix(data=NA,nrow = nrow(x),ncol = nrow(x))
        set <- function(y) {
                x <<- y
                invM <<- matrix(data=NA,nrow = nrow(x),ncol = nrow(x))
        }
        get <- function() x
        setInv <- function(z) invM <<- z
        getInv <- function() invM
        list(set = set, get = get,setInv = setInv,getInv = getInv)
}


## WThis function calculates the inverse of a matrix or returns its
## previus calculated value if exited

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invM <- x$getInv()
        if (!all(is.na(invM))) {
                message("getting cached data")
                return(invM)
        }
        data <- x$get()
        invM <- solve(data,...)
        x$setInv(invM)
        invM        
}
