makeCacheMatrix <- function(x=matrix()){
        mat <- NULL
        set <- function(y=matrix()) {
            x <<- y
            mat <<- NULL
        }
        get <- function() x
        setinv <- function(solve) mat <<- solve
        getinv <- function() mat
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        mat <- x$getinv()
        if(!is.null(mat)) {
            message("getting cached matrix")
            return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinv(mat)
        mat
}