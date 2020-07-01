#
#   These two functions work together to provide an ability to create a squared matrix inverse
#   and caches that inverse so it can be reused without having to recompute it. 
#	
#	The makeCacheMatrix function creates a special "matrix" object.
#   Creating an object in this fashion is roughly equivalent of "instantiating" an object in 
#   object oriented programming where classes, and instances thereof, of objects are used. 
#
#
#   The "matrix" object is comprised of four object oriented type members; get, set, setinverse and
#   and getinverse. These members are actually R functions. They are provided for 
#   use by this function by it returning an R list object with the four members each named for
#   what they do. 
#
#   The cacheSolve function uses the special matrix object and checks to see 1. if the inverse
#   of a square matrix has been calculated and 2. if the matrix has changed.
#   If the matrix inverse has been calculated and the matrix has not changed 
#   then it retrieves the matrix's inverse from cache. If the inverse has not been calculated,
#   or the matrix has changed, then this function computes the inverse of the matrix. It is assumed
#   the matrix is invertible. 
#  
#   (A square matrix is comprised of the same number of      
#   rows and columns. Non-square matrices (m x n matrices
#   for which m does not equal n) do not have an inverse.



## This function returns a R list object that works with 
## the square matrix provided to it. It sets the matrix object to NULL.
## The "<<-" operator puts the value into cache.    

makeCacheMatrix <- function(x = matrix()){

    mat <- NULL
    set <- function(y){
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinverse <-function(inverse) mat <<- inverse
    getinverse <- function() mat
    list(set = set,
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)

}


##   
##  This function either retrieves the matrix inverse from cache or creates the inverse if it 
##	is not cached or the matrix has changed. In either case it returns the matrix inverse.  
##

cacheSolve <- function(x, ...) {

		
	    
    mat <- x$getinverse()
    data <- x$get()
    
    if (!is.null(mat) & identical(data,mymat)){
        message("Retrieving cached matrix inverse.")
        return(mat)
    }
    
    message("Matrix has changed or inverse was not in cache. Calculating the inverse. Not retrieving the inverse from cache.")
    
    mat <- solve(data, ...)
    x$setinverse(mat)
## Return a matrix that is the inverse of 'x'
    mat
	
		
}
