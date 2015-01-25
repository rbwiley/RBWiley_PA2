## cachematrix.R
## rbwiley
## 2015/01/23  Created; rbw;  ver  1.0
## yyyy/mm/dd Revised: ...
##
## R. functions:
##  - makeCacheMatrix
##    creates (set) matrix object
##    retrieves (get) matrix object
##  - cacheSolve
##    calculates (set) matrix inverse
##    retrieves (get) matrix inverse
##

## makeCacheMatrix(x,...) function
##  pass (x) parameter & (,...) other parameters
##

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL               # empty placeholder for inverse matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
}
  get <- function() x
  set_IM <- function(solve) m <<- solve
  get_IM <- function() m
  list(set = set, get = get,         #create list with the vars & calcs
     set_IM = set_IM,
     get_IM = get_IM)


## cacheSolve function
##  pass (x) parameter & (,...) other parameters
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse (IM) of 'x'
  m <- x$get_IM()
  if(!is.null(m)) {
    message("getting cached inverse matrix")   # let the user know IM comes from the cache
    return(m)           # if calc'd inverse matrix *is*, ( not null), return it 
  
    data <- x$get()
    m <- solve(data, ...)    # calc'd inverse matrix wasn't, so calc it
    x$set_IM(m)              # calc & cache new inverse matrix
    m                        # calc'd inverse matrix is last statement so is returned
    
}
