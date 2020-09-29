# The overall concept of these functions is to save computation power
# by saving calculations.
# R scoping rules allow to cache previous calculations and therefore it
# allows to save computation power.

# These two functions "makeCacheMatrix" and "cacheSolve" 
# are using R scoping rules to check if a specific inverse matrix 
# already been calculated.

##########################################################

# The first function "makeCacheMatrix" allows to create a special "matrix"
# which is really a list containing the following:

# 1.set the matrix
# 2.get the matrix
# 3.set the inverse matrix
# 4.get the inverse matrix
makeCacheMatrix <- function(x = numeric()) {
    # Initializing our inverse_mat to null 
    inverse_mat <- NULL
    # Setting matrix into our "special matrix"
    set <- function(our_mat) {
        x <<- our_mat
        inverse_mat <<- NULL
    }
    # Getting our matrix
    get <- function() x
    # Setting the inverse matrix to our_mat
    setinv <- function(inv_to_our_mat) inverse_mat <<- inv_to_our_mat
    # Getting our inverse matrix
    getinv <- function() inverse_mat
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##########################################################

# The second function calculates the inverse matrix
# if it not been calculated before.

cacheSolve <- function(x, ...) {
    # Getting our inverse matrix from our special "matrix"
    inv_mat <- x$getinv()
    # Checking if this specific inverse matrix already been calculated before
    if(!is.null(inv_mat)) {
        message("getting cached data")
        # Returning the cached inverse matrix 
        return(inv_mat)
    }
    # Solving and returning the inverse matrix
    data <- x$get()
    inv_mat <- solve(data, ...)
    x$setinv(inv_mat)
    # Returning our inverse matrix
    return(inv_mat)
}