## The following 2 functions enable the user to create a matrix 
## and compute its inverse matrix. 
## Once the inverse matrix is calculated, it is stored in cache, and can 
## be accessed directly without recalculating it.

## The function makeCacheMatrix creates a special "matrix" which is actually a list with functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv<-NULL
        set<-function(y){
                x<<-y
                Inv<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) Inv<<-inverse
        getinverse<-function() Inv
                
        list(set=set,
             get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## The function cacheSolve returns the inverse of the special "matrix".
## If the inverse matrix has already been calculated, the function used the cached value
## If the inverse has not been calculated yet, it calculates (and also store it in cache)

cacheSolve <- function(x, ...) {
        Inv<-x$getinverse()
        
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setinverse(Inv)
        Inv
        ## Return a matrix that is the inverse of 'x'
}
