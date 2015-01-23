## ---------------------------------------------------------------------
## |   The following functions help us define a "special matrix"       |
## |   and find its inverse. If the inverse is already in cache,       |
## |   a cache result is returned. Else it is calculated by solve().   |
## ---------------------------------------------------------------------

## --------------------------makeCacheMatrix()--------------------------

## This function returns a list of four function. The first function
## is used to assign value to the matrix. The second function returns the
## the value of the matrix. The third assigns the value of the matrix's
## inverse. While the last just returns the matrix's inverse.
## We treat the returned list as a "special matrix" but in fact its
## just a list. e.g. If we create an object "a" and assigns the returned
## list, we have four things to use. a$set(square matrix), a$get(),
## a$setinverse(matrix), a$getinverse().

makeCacheMatrix <- function(x = matrix())
{
        ## Returns a list of four functions
        inverse <- NULL                     ## inverse has to be calculated
        set <- function(y)
        {
                ## Set the value for the matrix "x"
                x <<- y
                inverse <<- NULL            ## ensures the recomputation of inverse
        }
        get <- function()
        {
                ## Returns the matrix "x"
                return(x)
        }
        setinverse <- function(inv)
        {
                ## Set the Value for inverse of matrix "x"
                inverse <<- inv
        }
        getinverse <- function()
        {
                ##  Returns the inverse matrix for "x"
                return(inverse)
        }
        list(   set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse) ## This list is returned
}

## ----------------------END of makeCacheMatrix-------------------------

## ----------------------------cacheSolve()-----------------------------

## This function returns the inverse of a given matrix. It is assumed that
## the matrix is always an invertible matrix. First, the function gets
## the value of the inverse by calling the special objects's getinverse().
## If the returned value is not null, this means that the inverse is already
## in cache and therefore returns the cached value. Otherwise this function
## gets the matrix by calling get() of the special matrix and find its inverse
## by solve(matrix). Then it assigns the inverse matrix to the special matrix's
## "inverse" field by calling the setinverse(inverse of the matrix) and also
## returns the inverse matrix.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse))
        {
                ## The inverse is already calculated
                message("getting cached data")
                return(inverse)
        }
        ## These will execute only if the inverse in not already calculated
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        return(inverse)
}
## ---------------------End of cacheSolve()-----------------------------