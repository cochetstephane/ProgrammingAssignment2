makeCacheMatrix <- function(x = matrix() {
        ## It returns a list which containsfunctions to
        ##              - set the matrix
        ##              - get the matrix
        ##              - set the inverse
        ##              - get the inverse
        ## The list is used as an input to cacheSolve() 
        
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv = x$getinv()
        
        # Test if the inverse has been calculated, if so return the cache
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # If not, calculate the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # puts the value of the inverse in the cache 
        x$setinv(inv)
        
        return(inv)
}
