## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special 
#"matrix" object that can cache its inverse.
makeCacheMatrix<- function(m=matrix()) {
        m<-NULL    # sets the value of m to NULL (provides a default if cacheSolve has not yet been used)
        mat<-NULL  # sets the value of mat to NULL (provides a default if cacheSolve has not yet been used)        
        setmat <- function(mat) {
                m <<- mat
        }
        getmat <- function() m
        setinv <- function(solve) m <<- solve   
        getinv <- function() m
        ## creates a list to house the four functions
        list(setmat = setmat, getmat = getmat       
             setinv = setinv, getinv = getinv)
}

#cacheSolve: This function computes the inverse of the 
#special "matrix" returned by makeCacheMatrix above. If 
#the inverse has already been calculated (and the matrix 
#has not changed), then the cachesolve should retrieve 
#the inverse from the cache.
cacheSolve<- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- m$getinv()
        if(!is.null(mat)) {   # check to see if cacheSolve has been run before
                message("getting cached data")
                return(mat)
        }
        ## otherwise
        mat <- m$getinv()       #run the getmatrix function to get the value of the input matrix
        m <- solve(mat, ...)    # compute the value of the inverse of the input matrix
        m$setinv(m)             # run the setinverse function on the inverse to cache the inverse
        m                       # return inverse
}
