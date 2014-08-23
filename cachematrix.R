## makeCacheMatrix() function takes matrix as input and cachesolve() takes the 
## output object of makeCacheMatrix() function as its argument and returns
## the inverse of the matrix

## Takes matrix in its argument 

makeCacheMatrix <- function(x = matrix()) {
          m<-NULL
  #  m will store our 'mean' and it's reset to NULL every 
  #    time makeVector is called
  set<-function(y) ## Takes an input vector
  {                 
    x<<-y          ## Saves the vector
    m<<-NULL       ## Resets the mean to NULL
  }
  get<-function()
  {
    x
  }
  setinv<-function(mean) ##this is called by cachesolve() during  
  {                       ##the first cachesolve()
    m<<-mean
  }
  getinv<-function() ## this will return the cached value to cachesolve()                        
  {                   ## on subsequent access
    m
  }
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Return a matrix that is the inverse of the input matrix

cacheSolve <- function(x, ...) { ##input of this function is object created by makeCacheMatrix
  
  m <- x$getinv()               # accesses the object 'x' and gets the value of the inverse
  if(!is.null(m)) {              # if mean was already cached (not NULL) ...
    
    message("getting cached data")  # ... send this message to the console
    return(m)                       # ... and return the inverse ... "return" ends 
                                     #   the function cacheSolve(), note
  }
  data <- x$get()        # we reach this code only if x$getinv() returned NULL
  m <- solve(data, ...)   # if m was NULL then we have to calculate the inverse
  x$setinv(m)           # store the calculated mean value in x (see setinv() in makeCachematrix)
  m                      # return the inverse to the code that called this function
}
