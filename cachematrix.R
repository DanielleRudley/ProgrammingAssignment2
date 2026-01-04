## Put comments here that give an overall description of what your
## functions do

# These function are used to cache the inverse of a matrix. makeCacheMatrix()
# creates a parent environment which stores a matrix and its inverse, as well as  
# a set of functions that can be used to access and modify them. cacheSolve()
# either retrieves the inverse if it has been calculated, or calculates it if necessary.


### Write a short comment describing this function
# 1. makeCacheMatrix() is a parent function that sets x as a matrix within this function's  
#    environment and also stores a NULL variable called inverse.
# 2. set() function within the makeCacheMatrix environment updates the x matrix in the 
#    makeCacheMatrix environment to the newx values and resets the NULL inverse variable.
# 3. get() function returns the current value of x within the makeCacheMatrix environment.
# 4. setinverse() function stores a newly computed inverse, overwriting the original NULL
#    inverse variable in the makeCacheMatrix environment with values from the newinverse
# 5. getinverse() function returns the current inverse. Returns NULL if no inverse has been computed
# 6. Returns a list of functions that will reference the makeCacheMatrix environment 

# 1.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
# 2.
  set <- function(newx) {
    x <<- newx
    inverse <<- NULL
  }
# 3. 
  get <- function() x
# 4. 
  setinverse <- function(newinverse) inverse <<- newinverse
# 5. 
  getinverse <- function() inverse
# 6.   
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# 1. Create function cacheSolve() to run on the matrices established from the
#    makeCacheMatrix() function ('x' and 'inverse')
# 2. Return a matrix that is the inverse of x or NULL if no inverse computed
# 3. If the inverse is already computed then the function ends here
# 4. If the inverse is NULL the current x matrix is retrieved and the solve() function
#    is used to calculate the inverse
# 5. Calls the setinverse() function from the makeCacheMatrix environment to save the 
#    newly calculated inverse so it retrieved when getinverse() is called
# 6. Returns the inverse matrix

# 1. 
cacheSolve <- function(x) {
# 2.        
  inverse <- x$getinverse()
# 3.   
  if (!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
# 4. 
  data <- x$get()
  inverse <- solve(data)
# 5.  
   x$setinverse(inverse)
# 6.   
   inverse
}



mat <- matrix(c(1, 3, 2, 4), nrow = 2)
cachedMat <- makeCacheMatrix(mat)
cacheSolve(cachedMat)
cacheSolve(cachedMat)
