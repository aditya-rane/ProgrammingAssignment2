makeCacheMatrix<-function(x=matrix())
{
   i<<-NULL
   
   setmatrix<-function(v)
  {
    x<-v
    i<<-NULL
  }
  
   getmatrix<-function()
  {
    x
  }

   setinverse<-function(inverse)
  {
    i<<-inverse
  }
  
   getinverse<-function()
  {
    i
  }
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)

}  

cacheSolve<-function(x)
{
  if(is.null(x$getinverse()))
  {
    m<-x$getmatrix()
    i<-solve(m)
    x$setinverse(i)
    i
  }
  else
  {
    print("Retrieving inverse from Cache")
    return(x$getinverse())
  }
}
