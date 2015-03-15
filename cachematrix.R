## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## This is a funtion to create a list including 4 functions
        ## (get(), set(), setinverse(), getinverse())
        
        get <- function() x
        
        # the value of 'matrix_inverse' in the function---set_ivs(matrix_inverse)---- would be passed to m . 
        set_ivs <- function(matrix_inverse) m <<- matrix_inverse    ##The matrix_inverse is a formal argument  
        
        #return the the matrix m
        get_ivs <- function() m
        
        ##print the list,can be cited with "$"
        
        list(set = set, get = get,
             setinverse = set_ivs,
             getinverse = get_ivs)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        
        ## to jugded if the matrix  inverse data have been contained
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ##reset. so there is no message.
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
        
        
}