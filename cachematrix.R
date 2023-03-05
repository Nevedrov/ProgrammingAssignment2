#Implements inverse matrix memoization 

# Creates cache wrapper for inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix_cache = NULL
    set_matrix<-function(new_matrix){
        x<<-new_matrix
        inverse_matrix_cache<<-NULL
    }
    get_matrix<-function(){
        x
    }
    set_inverse<-function(new_inverse_matrix){
        inverse_matrix_cache<<-new_inverse_matrix
    }
    get_inverse<-function(){
        inverse_matrix_cache
    }
    list(set=set_matrix, get=get_matrix, set_cache=set_inverse, get_cache=get_inverse)
}


# Gets inverse matrix from cache wrapper

cacheSolve <- function(x, ...) {
    inv_matrix<-x$get_cache()
    if(!is.null(inv_matrix)){
        print("cached")
    }
    else{
        print("not cached")
        inv_matrix<-solve(x$get(), ...)
        x$set_cache(inv_matrix)
    }
    inv_matrix 
}
