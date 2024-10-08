
## Set the input 'x' as a Matrix
## Set the solve value 's' as NULL
## Change every 'mean' into 'solve'

makeCacheMatrix <- function(x = matrix(c(1:100, 9), 3, 3) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve)
                s <<- solve
        getsolve <- function() s
        list(set = set,
             get = get,
             getsolve = getsolve,
             setsolve = setsolve)
}

## Change 'mean' to 'solve', and 'm' to 's'

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)){
                message('Getting Inverse Matrix')
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
