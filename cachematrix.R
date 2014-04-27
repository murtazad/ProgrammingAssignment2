## As the assignment states, this file contains 2 functions to calculate and cache the Inverse 
## of a matrix. Of course, the assumption is the the matrix is always invertible.
##If a non-inveritble matrix is passed (non-square or singular) and if an attempt to calculate the matrix is made, then
## an error will be thrown.




## This function creates 4 functions
## get() = display the current matrix 
## set() = set a new matrix
## setInverse() = Stores the inverse of the Matrix in cache. 
##                DO NOT CALL THIS FUNCTION MANUALLY. 
##                It is meant to be called only from CacheInverse.
## getInverse() = diaplay the inverse of the Matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set =set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes a matrix and then attempts to get the inverse from Cache. 
## If inverse does not exist, then it computes it and then saves it to cache 
## by calling the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(invisible(m))
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}



##Sample OUTPUT from above functions
####STEP 1 :- Initialize a variable a by calling the makeCacheMatrix function 
##> a <- makeCacheMatrix(matrix(15:18,2))
####STEP 2 :- Use the get function to see the value of the matrix
##> a$get()
##[,1] [,2]
##[1,]   15   17
##[2,]   16   18
####STEP 3 :- Use the set function to set the value of a to a different matrix 
##> a$set(matrix(25:28,2))
####STEP 4 :- Use the get function to see the value of the matrix
##> a$get()
##[,1] [,2]
##[1,]   25   27
##[2,]   26   28
####STEP 5 :- Use the getInverse function to see the inverse of the matrix. Since it is not
####          calculated yet, it will return NULL
##> a$getInverse()
##NULL
#### STEP 6 :- Calculate the inverse of matrix and store in cache.
##> cacheSolve(a)
##[,1]  [,2]
##[1,]  -14  13.5
##[2,]   13 -12.5
#### STEP 7 :- Try calculating it again and we get a message saying that it got the value from cache
##> cacheSolve(a)
##getting cached data
#### STEP 8 :- Use the getInverse function to see the inverse of the matrix. 
##> a$getInverse()
##[,1]  [,2]
##[1,]  -14  13.5
##[2,]   13 -12.5
#### STEP 9 :- Multiply the 2 matrices and we should get the Unity Matrix confirming that the
####           inverse was calculated correctly.
##> a$get() %*% a$getInverse()
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1
##