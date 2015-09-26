## This is a pair of functions that will invert a matrix if it has not been 
## inverted already. In either case it will return the inverted matrix this 
## could same signifigant time depending on the size of the matrix.


## Creates a list of functions that allow access to a matrix and its inverse
## if it exists.  Though hypothecically this provides access to any original data
## and a cache for a modification to it.  Its use in this case will be for the
## inverse of a matrix.
##
## Note: This does not calulate anything merely provide an interface for 
##       the cacheSolve function to use.
##  
##
## Inputs: The matrix to invert. By default, this is an empty matrix
## Outputs: A list of four functions.
##          set(x): Takes in a matrix to be inverted. This also clears out the
##                  inverse matrix if it exists.
##
##          get(): Gets the original matrix if it exists, NULL otherwise
##
##          setinverse(x): Takes in the inverse matrix after it has been 
##                         calculated
##
##          getinverse(): Get the inverse matrix if it exists, NULL otherwise
##
##
##TODO add some error checking that prevents strange behavoir.
##  - Check to make sure the type of the original and the cached are the same
##  - Add a warning to the set function if the inverse is currently set
##  - Check the if a new matrix is being set if it is equal to the current
##    matrix then don't clear out the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    ## x in the internal storage of the original matrix
    
    ## m is the internal storage of the inverse.
    m <- NULL
    
    #sets the original matrix and clears the inverse
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #Get the original matrix
    get <- function() x
    
    #Takes the inverse matrix and stores it in m.
    setinverse <- function(inverse) m <<- inverse 
    
    #Gets the inverse matrix
    getinverse  <- function() m
    
    #Creates the list of functions 
    list(set = set, get = get,
         setinverse  = setinverse ,
         getinverse  = getinverse )
    
}

##Solves for the inverse of the matrix, unless it exists already then
## it will return the cached copy.
##
## This uses the solve function internally.
##
## Inputs: an object created by makeCacheMatrix
##         any additional arguements are passed to solve() 
## Outputs: The inverse of the matrix stored in the object.
##
## TODO: 
##   - Add some error checking if the martix isn't invertable
##   - check that the object in the function list is indeed a matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    
    #Check if the inverse exists
    if(!is.null(m)) {
        message("getting cached data")
        #If so return the cached version
        return(m)
    }
    
    #grab the matrix to invert
    data <- x$get()
    #invert the matrix
    m <- solve(data, ...)
    
    #store the inverted matrix
    x$setinverse(m)
    
    #return the inverted matrix
    m
}
