## 
# This function puts the supplied matrix into the envelope which later will be used
# to calculate the inversed matrix and save it in this envelope.
##
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    
    ## this function returns x which is available from the scope of the parent functions
    ## due to lexical scoping
    get_matrix <- function() x
    
    ## this function is called to set the inversed matrix    
    set_solved <- function(mean) m <<- mean
    
    ## this function return the inversed matrix or NULL if the one is not available
    get_solved <- function() m
    
    ## returning the envelope as a list which contains 
    ## the functions defined above and the original matrix
    list(get_matrix = get_matrix,
         set_solved = set_solved,
         get_solved = get_solved)
    
}

##
#  This function calculates the inverse matrix for a given
#  source matrix and caches the result for the future use.
#
#  It accepts an envelope of the list type which contains
#  the source matrix and the utility functions which are used
#  to get and save the inversed matrix.
#
#  Before the calculation it checks whether there is the 
#  inverse matrix for the given source matrix in the 
#  envelope.
#
#  If the inverse matrix is not available it calculates the 
#  one and puts it into the envelope and returns the result.
##
cacheSolve <- function(x) {
    m <- x$get_solved()
    
    if( !is.null(m) ) 
    {
        message("getting cached data")
        return(m)
    }
    
    message("no cached inverse matrix, calculating the one")
    
    data <- x$get_matrix()
    
    inverse_matrix <- solve(data)
    x$set_solved(inverse_matrix)
    inverse_matrix
}


## Below is the test which demonstrates the work of cacheSolve()
## and compares its result with the result of solve().
nrows <- 2000
ncols <- 2000
x <- stats::rnorm(nrows*ncols)
dim(x) <- c(nrows, ncols)
y <- makeCacheMatrix(x)
solved_with_caching_solve <- cacheSolve(y)
solved_with_solve <- solve(x)
identical(solved_with_caching_solve, solved_with_solve)





