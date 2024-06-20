makeCacheMatrix <- function(m = matrix()) {
    # Initialize the matrix and inverse cache
    mat <- m
    inv <- NULL
    
    # Function to set the matrix
    set <- function(matrix) {
        mat <<- matrix
        inv <<- NULL  # Reset the cache when matrix changes
    }
    
    # Function to get the matrix
    get <- function() {
        mat
    }
    
    # Function to compute the inverse and cache it
    cacheInverse <- function() {
        if (!is.null(inv)) {
            message("getting cached inverse")
            return(inv)
        }
        inv <- solve(mat)
        cacheSetInverse(inv)
        inv
    }
    
    # Function to set the cached inverse
    cacheSetInverse <- function(inverse) {
        inv <<- inverse
    }
    
    # Return a list of functions
    list(set = set,
         get = get,
         cacheInverse = cacheInverse,
         cacheSetInverse = cacheSetInverse)
}

cacheSolve <- function(x, ...) {
    # Retrieve cached inverse if available
    inv <- x$cacheInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise, compute the inverse and cache it
    data <- x$get()
    inv <- solve(data, ...)
    x$cacheSetInverse(inv)
    inv
}

# Create a new matrix object
m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))

# Compute the inverse (this will cache it)
cacheSolve(m)

# Retrieve the cached inverse (no recomputation)
cacheSolve(m)