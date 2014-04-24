## This R script has two functions:
## "makeCacheMatrix": it will create a special matrix 
##                    with functions to access/update values of the matrix
## "cacheSolve": it will produce and store inverse of the matrix produced 
##               by "makeCacheMatrix" function


## This function creates a special "matrix" object that can cache its inverse
##
## Input : a matrix 'x'
## Output: a 'list' of 4 functions with below purposes:
##         - set the value of the matrix
##         - get the value of the matrix
##         - set the value of the inverse matrix
##         - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## variable to hold inverse of input matrix 'x'
        inverseMatrix <- NULL   # initialize the value with NULL
        
        ## function to set the value of the matrix
        set <- function(y) {            
                
                x <<- y                 # update the matrix with a new value 
                
                inverseMatrix <<- NULL  # So, reset the value with NULL
                
        }
        
        ## function get value of the matrix
        get <- function() x
        
        ## function to set value of the inverse matrix 
        setinverse  <- function(inverse) inverseMatrix <<- inverse  
        
        ## function get value of the inverse matrix
        getinverse <- function() inverseMatrix
        
        ## Prepare & Retrun a list of functions created within this function
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes and caches the inverse of 
## the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## it will retrun value from the cache
##
## Input : a list 'x' representing a "special matrix" 
##         returned by function "makeCacheMatrix"
## output: inverse of the input "special matrix" 

cacheSolve <- function(x, ...) {
        
        inverseMatrix <- x$getinverse()         # get current cached value
        
        if(!is.null(inverseMatrix)) {           # Check whether already cached 
                
                message("getting cached data")  # Print info message
                
        } else {                                # Calculate inverse 
                
                directMatrix <- x$get()         # get value of "special matrix"
                
                inverseMatrix <- solve(directMatrix, ...) # calculate inverse
                
                x$setinverse(inverseMatrix)     # set(cache) value of inverse
                
        }
        
        inverseMatrix	                        # retrun inverse
}

