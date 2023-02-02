

uc <- function(source, ..., allow_partial_subst = T, warn_on_partial_subst = T, envir = parent.env(environment())){
  dots = substitute(alist(...))
  target_names = names(dots)

  if(typeof(source) != "list"){
    stop(paste("Error in uc(...), the source to be unpacked is not a list but", typeof(source)))
  }
  if(is.null(target_names)){

    uc_positional_(source, ...,
                allow_partial_subst=allow_partial_subst, warn_on_partial_subst=warn_on_partial_subst, envir = envir)
  }else{
    if(any(target_names=='_') | any(target_names=='__')){
      stop("Error in uc(...), you cannot use `_` placeholders while using named arguments. Use either positional unpacking or name-based unpacking, not mix them.")
    }
    if(any(target_names[2:length(target_names)]=="")){
      stop("Error in uc(...), either every target variable should be named or non should be. Use either positional unpacking or name-based unpacking, not mix them.")
    }

    uc_named_(source, ...,
              allow_partial_subst=allow_partial_subst, warn_on_partial_subst=warn_on_partial_subst, envir = envir)
  }
}

uc_positional_ <- function(source, ..., allow_partial_subst = F, warn_on_partial_subst = T, envir = envir){
  #browser()
  dots = substitute(alist(...))
  target_names = names(dots)
  if(source %>% length() < dots %>% length()-1){
    stop("Error in uc(...), the number of elements in source list is smaller than the number of target variables.")
  }
  if(source %>% length() != dots %>% length()-1){
    if(dots[[length(dots)]]!="__"){
      if(allow_partial_subst){
        warning("Warning in uc(...), the number of elements in source list does not match the number of target variables (no `__` given).")
      }else{
        stop("Error in uc(...), the number of elements in source list does not match the number of target variables (no `__` given).")
      }
    }
  }
  #TODO: what about target = (a, __, b)?


  for(ind in 2:length(dots)){
    if(substr(dots[[ind]], 1,1)=='_'){
      if(substr(dots[[ind]], 2,2)=='_'){
        break
      }
      else{
        next
      }
    }
    assign(dots[[ind]] %>% as.character(), source[[ind-1]], envir=envir)
  }
}
uc_named_ <- function(source, ..., allow_partial_subst = F, warn_on_partial_subst = T, envir = envir){
  #TODO: implement the named variant
}

list(a=1, z=0) %>% uc(b = 3, c = 4)
list(a=3, z=4) %>% uc(`_a`, d)
environment()
list(a=1, z=0) %>% uc(b, c)
list(a=1, z=0) %>% uc(b, NULL, .warn = T)
