## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Function to create a list of functions
# to get, set,getInverse and setInverse
# of a matrix
makeCacheMatrix <- function(x = matrix()) {
    # inverse value
    inverse <- NULL
    
    # set function. Set a new value
    # for the original matrix
    set <- function(nuevaMatriz,...){
        x <<- nuevaMatriz
        inverse <<- NULL
    }
    
    # get function. Returns the
    # value of the original matrix
    get <- function(){
        x
    }
    
    # set inverse function. Set a
    # new value for the inverse matrix
    setInverse <- function(inv)
    {
        inverse <<- inv
    }
    
    # get inverse function. Returns
    # the value of the inverse matrix
    getInverse <- function(){
        inverse
    }
    
    # Returns all the functions
    list(set = set, get = get, setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
# Function to calculate the inverse of a 
# matrix. If the inverse has already been calculated,
# it gets the inverse from the cache. Otherwise,
# it calculates the inverse of the matrix
# and sets the value in the cache
cacheSolve <- function(x, ...) {
    # get the inverse of the matrix
    inverse <- x$getInverse()
    
    # if the matrix exists,
    # it returns the value
    if (!is.null(inverse))
    {
        message("getting cached data")
        return (inverse)
    }
 
    # otherwise, 
    # we get the value of the matrix
    mat <- x$get()
    
    # calculate the inverse
    inverse <- solve(mat)
    
    # set the inverse value
    x$setInverse(inverse)
    
    # and returns the inverse matrix
    inverse
}
