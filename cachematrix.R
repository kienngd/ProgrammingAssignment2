## Make special matrix. It like normal matrix
## but when you solve it, it try to get Inverse value in the cached first
## if the cached is not null, it no need to recalculate.

makeCacheMatrix <- function(x = matrix()) 
{
	## In init, set the cached to NULL
  cachedMatrix <- NULL
  
  ## Set value for the matrix
  setMatrix <- function (y)
  {
    x <<- y
    ## Everytime, when the value of matrix is set, set the cached to NULL too. 
    cachedMatrix <<- NULL
  }
  
  ## Return value the matrix
  getMatrix <- function()
  {
	return(x)
  }
  
  ## Set value for the cached matrix
  setCached <- function(objMatrix)
  {
	cachedMatrix <<- objMatrix
  }
  
  ## Get value of the cached matrix
  getCached <- function()
  {
	return(cachedMatrix)
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setCached = setCached, getCached = getCached)
}


## function cacheSolve try to get value of Inverse matrix in the cached first
## if the cached is not null, it return the cached's value
## otherwise it will try to solve the matrix and return value

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        
        ## Check if "the cached matrix" is null of not, if it not null, no need to recalculate it.
        tmpMatrix <- x$getCached()
        if(!is.null(tmpMatrix))
        {
			message("Get data from cached")
			return(tmpMatrix)
        }
        
        ## If the cached matrix is null, solve the matrix and set it to the cachec matrix
        if(isInverseable(tmpMatrix) )
        {
			tmpMatrix <- x$getMatrix()
			tmpMatrix <- solve(tmpMatrix)
			x$setCached(tmpMatrix)
			## Return the inverse matrix
			return(tmpMatrix)
        }
        else
        {
			message("The matrix is not invertible")
			return(NULL)
        }
}

isInverseable <- function(x)
{
	## For this assignment, assume that the matrix supplied is always invertible.
	## so this function will return TRUE
	return(TRUE)
}
