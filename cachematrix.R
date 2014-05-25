## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {  # create function makeCacheMatrix with a default matrix
        m <- NULL   # put NULL at the inverse matrix
        set <- function(y) {    # create a function with the original matrix as argument
                x <<- y         # put the original matrix in the specific environment
                m <<-  NULL     # put NULL at the inverse matrix in the specific environment. The function return m   
        }

        get <- function() x  # Create a function with any argument. This function return the original matrix from the specific environment 	 
        setsolve <- function(solve) m <<- solve   # create the function solve in the specific environment with result the inverse matrix into m
        getsolve <- function() m   # Create a function with any argument. This function return the inverse matrix from the specific environment

        list(set = set, get = get,     # return the four functions define above to be used in the global environment   
             setsolve = setsolve,
             getsolve = getsolve)  
}



cachesolve <- function(x, ...) {         # create function cachesolve with the original matrix as first argument
        m <- x$getsolve()        # search the inverse matrix of x in specific environment
        if(!is.null(m)) {        # if the inverse matrix is found 
                message("getting cached data")        # send a message to the console
                return(m)                             # Go to the end of the function and return the inverse matrix m  
        }

        # if the inverse matrix isn't found
        data <- x$get()        # put the matrix argument of the cachesolve in data   
        m <- solve(data, ...)  # compute the inverse matrix with the function solve
        x$setsolve(m)          # save the result in the specific environment
        m                      # the inverse matrix return by the function
}