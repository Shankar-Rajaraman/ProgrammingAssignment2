#Overall description 
#The function  makeCacheMatrix() has four functions within it for matrix operations such that, 
# 1. getmatrix()-> reads a matrix
# 2. setmatrix()-> sets a matrix
# 3. getinverse()-> reads the inverse matrix cached from setinverse()
# 4. setinverse()-> caches or stores the matrix inverse when computed from another funtion cacheSolve().
# The set functions  can either be manually assigned or called from other function as in cacheSolve. 
 
#cacheSolve() - This function returns the inverse of a matrix already cached or if the inverse is unavailable to begin with,it then computes the inverse and 
# calls the setinverse() function to cache the inverse value.  

## The makeCacheMatrix reads a matrix and caches its inverse using the four functions described above. It outputs the functions as a list of functions.


makeCacheMatrix <- function(X = matrix()) {
        INV <- NULL
        
        setmatrix <- function(Y = matrix()) {
                message("Set called")
                X <<- Y
                INV<<- NULL
                return(X)
        }
        
        getmatrix <- function() { 
                X 
        }
        
        setinverse <- function(I= matrix()){
                INV <<- I
        }
        
        getinverse <- function() {
                INV
        }
        
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


##The cacheSolve() function below reads a cached inverse matrix or if the cached value is unavailable, computes the inverse and caches it. 
##Its argument is a makeCacheMatrix() function.  When it reads a cached matrix it checks whether the matrix is invertible.
 
cacheSolve <- function(X, ...) {
        INV <- X$getinverse()
        
        if(!is.null(INV) && !is.na(INV) && !det(INV)==0) {
                message("getting cached data")
                return(INV)
        }
        
        M <- X$getmatrix()
        INV <- solve(M)
        X$setinverse(INV)
        INV
        
}