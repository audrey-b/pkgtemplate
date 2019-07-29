check_flag <- function(x, x_name = deparse(x)) {
  if(isTRUE(x) || isFALSE(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be TRUE or FALSE")
}

check_flag_na <- function(x, x_name = deparse(x)) {
  if(isTRUE(x) || isFALSE(x)) 
    return(TRUE)
  if((identical(length(x), 1L) && is.na(x) && is.logical(x)))
    return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be TRUE, FALSE or NA")
}

check_string <- function(x, x_name = deparse(x)) {
  if((identical(length(x), 1L) && !is.na(x) && is.character(x)))
    return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be a character vector of length 1")
}

check_unused <- function (...) {
  if (length(list(...))) 
    err("... must be unused")
  TRUE
}

check_vector <- function(x) {
  if(is.vector(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be a vector")
}

check_scalar <- function(x) {
  if(is.vector(x) && identical(length(x), 1L)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be a vector of length 1")
}

check_atomic <- function(x) {
  if(is.atomic(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be an atomic (vector, matrix or array) object")
}

check_atomic_vector <- function(x) {
  if(is.atomic(x) && is.vector(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be an atomic vector")
}

check_logical <- function(x) {
  if(is.integer(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be integer")
}

check_integer <- function(x) {
  if(is.integer(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be integer")
}

check_double <- function(x) {
  if(is.double(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be double")
}

check_numeric <- function(x) {
  if(is.numeric(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be numeric")
}

check_function <- function(x) {
  if(is.function(x)) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be a function")
}

check_named <- function (x) {
  if(!is.null(names(x))) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be named")
}

check_no_na <- function(x) {
  if(!anyNA(x)) return(TRUE)
  err(x, "must not include any missing values")
}

check_unique <- function(x) {
  if(!anyDuplicated(x)) return(TRUE)
  err(x, "must be unique")
}

check_in <- function(x, y) {
  if(all(x %in% y)) return(TRUE)
  err()
}

check_length <- function (x, length = c(1L, chk_max_int())) {
  check_vector(x)
  check_atomic_vector(length)
  check_numeric(length)
  if(!length(length)) err("length must have at least one value")
  check_no_na(length)
  length <- as.integer(length)
  length <- sort(length)
  
  if(all(length == length[1])) length <- length[1]
  
  if(identical(length(length), 1L)) {
    if(identical(length(x), length)) return(TRUE)
    x <- deparse(substitute(x))
    err(x, " must have a length of ", length)
  }
  
  if(identical(length(length), 2L)) {
    if(length(x) >= length[1] && length(x) <= length[2]) 
      return(TRUE)
    x <- deparse(substitute(x))
    err(x, " must have a length between ", length[1], " and ", length[2])
  }
  length <- unique(length)
  if(length(x) %in% length) return(TRUE)
  x <- deparse(substitute(x))
  err(x, " must be of length ", cc(length, " or "))
}
