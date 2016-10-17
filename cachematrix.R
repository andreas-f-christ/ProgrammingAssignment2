## These two functions return the inverse of a matrix. This inverse is only 
## calculated if it has not been calculated before. Otherwise the stored inverse
## is returned. The special thing about this form of storage is that, once a 
## makeCacheMatrix object has been created, the variables (the matrix and its
## inverse) are not directly accessible. They exist only in the environment
## of the function definition. To get to these values, special get and set 
## functions have to be called. These get and set function get called by the 
## actual cacheSolve function.


## when called, makeCacheMatrix creates an object of of 4  visible functions
## and two variables which exist non-visibly to the outside world in the
## environment of the function definition. However, due to the 4 functions 
## one has access to the 2 stored variables. On the other hand the list of
## functions as return value prevent the deletion of the variables. So to 
## speak the functions leave the door open.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){
                x
        }
        setinv <- function(I){
                inv <<- I
        }
        getinv <- function(){
                inv
        }
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## The function cacheSolve gets the inverse of a Matrix stored in makeCacheMatrix.
## If the stored inverse Matrix has a non null value that matrix is returned.
## If there is a null value in the stored matrix, cacheSolve computes the 
## inverse of the matrix stored in makeCacheMatrix via solve() and stores that
## computed value as the inverse in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
