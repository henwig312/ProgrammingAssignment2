 ## This script takes a matrix, inverse it and store it in memory

## This function takes a matrix argument and create a matrix object with 
## four functions to modify the data
makeCacheMatrix <- function(x = matrix()) {
    message("Building a matrix object containing functions")
    m <- NULL

    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    ## Returning a list of functions used to retieve and set the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function inverts a matrix if it's not stored
## in memory or if it has changed. If it's already stored
## or unchanged it simply reads it back from memory
cacheSolve <- function(x, ...) {
    ## Get the new and inverted matrices from memory
    m <- x$getinverse()
    myMatrix <- x$get()
    
    if(exists("previousMatrix")){
        ## If the matrix is unchanged and stored, return from memory
        if(identical(myMatrix,previousMatrix) & !is.null(m)) {
            message("Returning the inverse matrix from memory")
            return(m)
        }
    ## If changed or not stored, take the matrix from the matrix object and
    ## inverse it and store back to memory. Also store the unchanged matrix.
    }else{
        message("Calculating the inverse matrix and store it to memory")
        previousMatrix <<- myMatrix
        m <- solve(myMatrix, ...)
        x$setinverse(m)
    m
    }
}