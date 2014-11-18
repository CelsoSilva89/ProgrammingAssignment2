# Programming Assignment 2
# The matrix inverse will be stored in inv_X

makeCacheMatrix <- function(X = matrix()) {
        
        inv_X <- NULL             # set the inverse to NULL when function is initialized
        set <- function(y) {      # function to set X matrix directly
                X <<- y           # set X on the global environment
                inv_X <<- NULL    # set inv_X to NULL when set function is called
        }
        
        get <- function() X
        setinv <- function(inv) inv_X <<- inv   # same structure as the given example
        getinv <- function() inv_X
        
        list(set = set, get = get,              # list with the four functions
             setinv = setinv,
             getinv = getinv)
}

# Same structure as the one on the given example

cacheSolve <- function(x, ...) {
        
        inv_X <- x$getinv()
        if(!is.null(inv_X)) {         # logical test to determine if inv_X needs to be recalculated             
                message("getting cached data")      # message stating that previously cached value is used
                return(inv_X)         # returns inv_X in case it has already been calculated
        }
        
        data <- x$get()               # get matrix from x (inputed to cacheSolve) and stores locally at data
        I<-diag(nrow(data))           # creates a indentity matrix of same size as data
        
        inv_X <- solve(data,I)        # solve the system for data and the identity matrix
        x$setinv(inv_X)               # caches the calculated inverse matrix
        inv_X                         # outputs the inverse matrix
}
