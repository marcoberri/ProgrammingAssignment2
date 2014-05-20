
# Example:
#  x <- matrix(rnorm(10), nrow = 8)        
# cx <- makeCacheMatrix(x)  // Create our special matrix
# cx$get()                // Return the matrix
# cacheSolve(cx)                            // Return the inverse
# cacheSolve(cx)                            // return the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set value of the matrix
# 2. Get value of the matrix
# 3. SetinvMat value of the inverse
# 4. GetinvMat value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    #  cached inverse matrix
    invMat <- NULL

    # Setter
    set <- function(y) {
        x <<- y
        invMat <<- NULL
    }
    
    # Getter
    get <- function() x

    # Setter 
    setinvMat <- function(inverse) invMat <<- inverse
    # Getter 
    getinvMat <- function() invMat

    list(set = set, get = get, setinvMat = setinvMat, getinvMat = getinvMat)
}


# cacheSolve: Compute the inverse of the matrix
# calculated before and returns the cached
cacheSolve <- function(x, ...) {
    inv <- x$getinvMat()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache
    x$setinvMat(inv)

    # Return
    inv
}
