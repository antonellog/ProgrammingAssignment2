##
## The function 'makeCacheMatrix' creates a data structure for speeding up 
## the computation of a matrix inverse. 
##
## The function 'cacheSolve' returns the matrix inverse and caches the 
## result for subsequent calls to the function
##

##
## makeCacheMatrix(x) takes as input a matrix 'x' and returns a list of 
## functions for setting/getting the matrix entries ('set'/'get' functions) and 
## for setting/getting the matrix inverse ('setinv'/'getinv' functions).
##
## NB. The matrix 'x' is assumed to be always invertible.
##
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        # Update the matrix according to 'y'
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        # Return the matrix
        get <- function() {
                x
        }
        
        # Set the matrix inverse according to 'xi'
        setinv <- function(xi){
                inv <<- xi
        }
        
        # Return the matrix inverse
        getinv <- function(){
                inv
        }
        
        # Return the newly built object
        list( set=set, get=get, setinv=setinv, getinv=getinv)

}



##
## cacheSolve(x, ...) takes as input a matrix represented as object 'x' built 
## with the 'makeCacheMatrix' function and returns its inverse. The result is 
## cached by updating the object 'x' for speeding up subsequent calls 
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        

        if ( is.null(x$getinv()) ){
                
                # Inverse must be computed and cache updated
                x$setinv(solve(x$get()))
                
                message("Computing the inverse and updating the cache")
                
        }
        
        # Returns the inverse
        x$getinv()
                
}
