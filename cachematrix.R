## This function creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x=matrix()){
        ## Initializing the inverse property
        inv <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                matrix <<- y
                inv <<- NULL
        }
        
        ## Method to get the matrix
        get <- function(){
                ## returning matrix
                matrix
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                ## storing inverse 
                inv <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## returns the inverse
                inv
        }
        
        ## Returns the list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the matrix 
## If the inverse has already been calculated (and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## getting a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## returns if the inverse has already been calculated
        if(!is.null(inv)) {
                message("Inverse is already calculated, retrieving cached data")
                return(inv)
        }
        
        ## getting the matrix from our object
        data <- x$get()
        
        ## calculating the inverse by using matrix multiplication
         m <- solve(data) %*% data
        
        ## storing the inverse to the object to future usage
        x$setInverse(m)
        
        ## returning a matrix that is the inverse of 'x'
        m 
}
