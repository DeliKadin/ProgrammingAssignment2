## cachematrix.R
        
## The functions in this file will allow you to cache the inverse of a matrix
## and use the cached inverse if it already exists instead of creating it over again.
## Note that the internal functions (set, get, etc.) are not meant to be called directly.

####    usage:
##  m <- matrix(c(2,4,3,1,5,7,3,6,7),3,3)  ## Create a square invertible matrix
##  a <- makeCacheMatrix(m)     ## instantiate the special matrix object
##  cacheSolve(a)               ## Get the inverse of the matrix


## This will make a matrix object that will cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
        ## 'x' is assumed to be an invertible square matrix
        
        ## Initialize the inverse matrix variable, setting it to NULL
        m <- NULL
        
        ## Function to store the matrix in cache
        ## Anytime the set function is run, the inverse matrix m is reset to null.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        
        ## Function to return the value of the matrix stored in cache
        get <- function() x
        

        ## Store the inverse of the matrix in cache
        setinverse <- function(solve) m <<- solve
        
       
        ## function to return the inverse stored in cache
        getinverse <- function() m
        
        
        ## return list of the defined functions
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x', either from the inverse stored
## in cache, if any, or from the newly calculated inverse.
cacheSolve <- function(x = matrix(), ...) {
        ## 'x' is assumed to be a square, invertible matrix
        
        ## Check to see if the inverse for this matrix already exists in cache
        m <- x$getinverse()
        
        ## if the inverse exists already for this matrix, use the cached value
        if (!is.null(m)) {
                message("Getting cached data")
                return(m)
        } 
        
        ## if the inverse doesn't exist, get the value of the matrix and put it in 
        ## a variable called "data"
        data <- x$get()
        
        ## Get the Inverse of the "data" matrix and store in variable "m"
        m <- solve(data, ...)
        
        ## Cache the resulting inverse
        x$setinverse(m)
        
        ## Return the inverse of the passed in matrix
        m
}
