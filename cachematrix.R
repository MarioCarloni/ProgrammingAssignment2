# Cache matrix x 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL # Initialize object to hold cached matrix
  set<-function(y){ # Function to cache or "set"
    x<<-y # Cache
    m<<-NULL
  }
  get<-function() x # Function 'get' returns matrix x
  setmatrix<-function(solve) m <<- solve # Sets matrix
  getmatrix<-function() m # Retuns 'm'
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

# Return inverse matrix
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() # Pull matrix x
  if(!is.null(m)){ # If 'm' contains something
    message("getting cached data")
    return(m)
  }
  matrix<-x$get() # Save matrix x in object 'matrix'
  m<-solve(matrix, ...) # Equation solver
  x$setmatrix(m) # Cache matrix
  m
}
