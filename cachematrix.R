## This function takes a matrix x and returns a list whose first four elements 
## are the functions set, get, setinv and getinv which cache the value of x, 
## retrieve the value of x, cache the inverse of x and retrieve the inverse of 
## x, respectively. The last element is a backup that allows me to check whether 
## x has been modified since the inverse was calculated.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        backup <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) {
                inv <<- inverse
                backup <<- x
        }
        getinv <- function() inv
        getback <- function() backup
        
        list(set = set, get = get, setinv = setinv, getinv = getinv, 
             getback = getback)
        
}


## This function takes x, the special list "matrix" object created by 
## makeCacheMatrix. It checks whether the inverse has already been calculated, 
## and checks that the matrix is unchanged. If both are true, it returns the 
## cached inverse. If not, it calculates and caches the inverse.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        
        if(!is.null(inv) && identical(x$get(), x$getback())) {
                message("Matrix unchanged, getting cached inverse")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        
        inv
        
}
