## The pair of functions "makeCacheMatrix" and "cacheSolve" can be used to
## efficiently cache and retrieve a matrix's inverse. The usual, un-cached,
## computation of a matrix's inverse is potentially expensive if such computation
## is repeatedly performed in the case of a large matrix.



## The following "makeCacheMatrix" function returns a list of functions that:
## 1. set the value of the matrix "Mat"
## 2. get the value of the matrix "Mat"
## 3. set the value of the inverse of "Mat"
## 4. get the value of the inverse of "Mat"
makeCacheMatrix <- function(Mat = matrix()) {
      
      MatInv <- NULL
      
      setMat <- function(M) {
            Mat <<- M
            MatInv <<- NULL
      }
      
      getMat <- function() Mat
      
      setMatInv <- function(calculatedInv) {
            MatInv <<- calculatedInv
      }
      
      getMatInv <- function() MatInv
      
      list(setMat = setMat, getMat = getMat,
           setMatInv = setMatInv, getMatInv = getMatInv)
}



## The following "cacheSolve" function calculates the inverse of the matrix "Mat".
## It first checks to see if the inverse has already been calculated. If so, it
## gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix "Mat" and sets the value of the inverse
## in the cache via the "setMatInv" function.
cacheSolve <- function(Mat, ...) {
      ## Return a matrix that is the inverse of "Mat"      
      
      MatInv <- Mat$getMatInv()
      if (!is.null(MatInv)) {
            message("getting cached inverse")
            return(MatInv)
      } ## return cached inverse matrix if it already exists
      
      data <- Mat$getMat() ## if cached inverse does not exist, calculate fresh
      MatInv <- solve(data, ...)
      Mat$setMatInv(MatInv) ## save calculated inverse in the cache
      MatInv
}