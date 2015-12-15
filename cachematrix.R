# Matrix inversion is usually a costly computation so it's a good idea to
# cache the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing functions which
# (a) set the value of the matrix
# (b) get the value of the matrix
# (c) set the value of inverse of the matrix
# (d) get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolveMatrix <- function(solve) m <<- solve
        getsolveMatrix <- function() m
        list(set = set, get = get,
             setsolveMatrix = setsolveMatrix,
             getsolveMatrix = getsolveMatrix)
}

# The following function returns the inverse of the matrix. Firstly, it checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse. It sets the value in the cache via
# the setsolveMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolveMatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolveMatrix(m)
        m
}

