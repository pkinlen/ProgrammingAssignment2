# This file contains some R functions that can be used to cache the
# inverse of a square matrix.

# Author: Philip Kinlen

# The following function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  m_inv       <- NULL
  m_mat       <- x   # users should NOT set m_mat directly,
  # but rather use the setMat(.) function
  ##############################################################
  # We first define 3 functions:
  
  getMat      <- function() m_mat
  ##############################################################    
  # We have a 'lazy' implementation here.
  # The m_inv will only be evaluated when it is needed
  # And if we've calculated it already, 
  # then it isn't re-calculated.
  # Also note that the setMat(mat) function will set reset m_invM2 to null
  getInv    <- function() {
    if ( is.null(m_inv)) {
      m_inv <<- solve( m_mat )
      print("Just calculated the inverse of the square matrix")
    }
    
    return (m_inv)
  }
  ##############################################################
  setMat      <- function( mat ) {
    if ( !identical(mat, m_mat)){
      m_mat   <<- mat  
      m_inv <<- NULL   # when the matrix has been reset
    }                    # the previous value of m_inv is no longer valid
  }
  ##############################################################
  
  # return a list containing the two get and one set functions
  list( getMat = getMat, getInv = getInv, setMat = setMat)    
}



#####################################################################
# computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  x$getInv()   # Note we will only do the work of evaluating the 
}              # inverse of the square if it is necessary.

###################################################################
# Now we have a function to test the functions above.
tester <- function() {
  
  # we now construct a 2x2 and a 3x3 matrix
  print("We have 2 different matrices, so we should calculate the inverse twice and only twice.")
  m2    <- matrix(c(1, 0.5, 0.4, 1), 2, 2)
  m3    <- matrix(c(1, 0.5, 0.2, 0.1, 1, 0.4, 0.2, 0.3, 1), 3, 3)
  
  cached2 <- makeCacheMatrix(m2)
  cached3 <- makeCacheMatrix()
  cached3$setMat(m3)
  
  m2Inv <- cacheSolve(cached2)
  m3Inv <- cacheSolve(cached3)
  
  cached2$setMat(m2)
  ################################################################
  print( "Now we'll print out m2 followed by m2Inv and then the identity")
  print( cached2$getMat())
  print( cacheSolve(cached2))
  print( m2 %*% m2Inv)
  
  print( "And now m3 followed by m3Inv and then the identity")
  print( cached3$getMat())
  print( cacheSolve(cached2))
  print( m3 %*% m3Inv) 
}

