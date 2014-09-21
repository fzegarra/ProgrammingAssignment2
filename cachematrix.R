##Caching the inverse of a matrix.
## First store the function "makeCacheMatrix()" in a variable
## Then run variable$set(matrix(data, nrow, ncol)). Notice that the matrix must be an square and invertible or  
## nonsingular matrix because non-square matrices do not have inverses and not all square matrices have inverses.


makeCacheMatrix<-function(x = matrix()) {
    m <- NULL
    ## represents "m" as a NULL object then returned by a function whose value is undefined
    set <- function(y){
    x<<-y
    ## assigning a value to an object x in a different environment different from the current
    m<<-NULL
    }
get <- function() x
## x as a free argument not defined 
setinverse <- function(solve) m <<- solve
## the variable "m" of the parent environment will recieve the value of the inverse matrix  
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
## gets a list whose elements are functions
}

cacheSolve <- function(x=matrix(), ...) {
    m <- x$getinverse()
    ## subsetting the function in the "m" variable 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if "m" was stored and we can find "m" then return "getting cache data" and the value of "m"  
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## if not then calculate the inverse of the matrix and return the value. 
}
