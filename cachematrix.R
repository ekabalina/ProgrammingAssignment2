#First Function:
#makeCacheMatrix  function creates a special "matrix" object that can cache its inverse  
#which is a list containing a function to"
# * set the value of the matrix
# * get the value of the matrix
# * set the value of the inverse
# * get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_solution <- function(solution) m <<- solution
        get_solution <- function() m
        list(set = set, get = get,
             set_solution = set_solution,
             get_solution = get_solution)


}

#Second Function
#cacheSolve  function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#It first checks to see if the inverse matrix has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and 
#sets the inverse matrix in the cache via the set_solution function.

#It's assumed that the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$get_solution()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_solution(m)
        m

}
