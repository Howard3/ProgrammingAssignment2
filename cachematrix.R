## Receives a matrix and scopes it so that a cache can be maintained of the
##      inverse of the cache matrix. Used in conjunction with cacheSolve
##      to minimize wasted CPU cycles in using the matrix inverse.
##
## This function operates by scoping the matrix to the class and exposing
##      a list of methods which will operate on the scoped variables.
##      
##  These methods allow the matrix to be defined via $set, which will 
##      reset the cache, received via $get, have the matrix inverse read
##      via $getInverse and set via $setInverse.
makeCacheMatrix <- function(matrix_normal = matrix()) {
    matrix_inverse <- NULL
    
    # Define set/get functions for the normal matrix.
    # Set will reset the matrix_inverse value
    #   while get merely returns matrix_normal
    set <- function(x) {
        matrix_normal <<- x
        # invalidate the cache
        matrix_inverse <- NULL
    }
    get <- function() matrix_normal
    
    # Define set/get functions for the matrix inverse.
    setInverse <- function(i) matrix_inverse <<- i
    getInverse <- function() matrix_inverse
    
    # Expose the internally scoped methods externally so that we can 
    #   make use of them.
    list(
        set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Returns the inverse of a matrix. Attempts to utilize cache to minimize
##      CPU cycles.
##
## Receives the list that was generated via the makeCacheMatrix call to return
##      the inverse of the matrix. Currently this assumes that the matrix being 
##      provided to the function is squared. Performs some simple sanity checks
##      on the incoming matrix_normal variable to ensure it's what we expect.
##  
##  This function operates by checking the value of matrix_normal$getInverse()
##      if this value is null, it'll be calculated via r's solve() function
##      and then saved inside of the scoped cache matrix via $setInverse()

cacheSolve <- function(matrix_normal, ...) {
    # Some quick checks to verify the object we're receiving is okay.
    invalid_value <- class(matrix_normal) != 'list'
    invalid_value <- invalid_value || class(matrix_normal$getInverse) != 'function'
    invalid_value <- invalid_value || class(matrix_normal$setInverse) != 'function'
    
    if(invalid_value) {
        stop('Invalid object received. Must be a result of the makeCacheMatrix call.')
    }
    matrix_inverse = matrix_normal$getInverse()
    
    # Check if we received a cached matrix_inverse value
    if(is.null(matrix_inverse)) {
        # No cached result exists. Write one now.
        message('Writing matrix inverse.')
        # Assign the matrix_inverse value to the calculated result.
        matrix_inverse = solve(matrix_normal$get(), ...)
        # Cache the result
        matrix_normal$setInverse(matrix_inverse)
    }
    
    # Return the matrix inverse
    matrix_inverse
}