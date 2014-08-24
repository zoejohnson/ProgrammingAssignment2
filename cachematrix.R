
## makeCacheMatrix is a function that creates three methods (original_matrix, set_inverse, 
## and get_inverse) and caches the inverse of matrix 'x'. These methods are used by cacheSolve. 
## If the inverse of matrix 'x' exists, cacheSolve will return the cached version of that inverse, 
## otherwise it will calculate the inverse and return it.


## Creates object (list) to store the inverse matrix of a matrix calculated in cacheSolve
## Includes functions accessed by cacheSolve

makeCacheMatrix <- function(x = matrix()) {     # input x is a matrix
        inverse_matrix <- NULL  # inverse_matrix is reset to NULL whenever makeCacheMatrix
                                # is called. Will store the inverse matrix of x
        
        # next fcns are used by cacheSolve() to get x or inverse_matrix, and for
        # setting the inverse matrix.  (Don't run when makeCacheMatrix is called)
        
        original_matrix <- function() {x}       # returns the original matrix
        
        set_inverse <- function()               # called by cacheSolve during the first access
                {inverse_matrix <<- solve(x)}   # will store the inverse of x using superassignment
                                        
        get_inverse <- function() {inverse_matrix}      # returns the cached value to cacheSolve
                                                        # after first access
        list(original_matrix = original_matrix, 
             get_inverse = get_inverse, 
             set_inverse = set_inverse)
}


## Returns the inverse matrix of x. First checks if the inverse is cached; 
##   if not, calculates and returns the inverse.

cacheSolve <- function(x, ...) {
        matrix <- makeCacheMatrix(x)
        if (is.null(matrix$get_inverse()){      # If there is no inverse, calculate it using set_inverse
                matrix$set_inverse()
        }
        return matrix$get_inverse()             # Return the inverse.
}
