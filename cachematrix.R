makeCacheMatrix <-function(x = matrix()){
        ## return a list of functions:
        ## set the matrix (x)
        ## get the matrix (x)
        ## set the inverse (setsolve = m)
        ## get the inverse (getsolve = m)
        ##      start everything saying we don´have m yet:
        m <- NULL 
        set <- function (x){
                x <<- y
                m <<- NULL
                }
        get <- function () x
        setsolve <- function (solve) m <<- solve ## stablishes this goes to another environment
        getsolve <- function () m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
        ## this list will be used by cachesolve (not the matrix)
}


cachesolve <- function(x = matrix(), ...) {
        ## first, name things:
        m <- x$getsolve ()
        v <- matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                ## m if it has already been calculated
        } ##otherwise, calculate it
        new <-x$get()
        v <- solve(new, ...)
        v
}
