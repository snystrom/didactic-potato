get_file_type <- function(tracks){
  # can be:
  # rtracklayer::BigWigFile
  # Rsamtools::BamFile
  # character vector:
  #  - .bam$
  #  - .bw$
}


#check that seqlevelsStyle of x and y match
seqlevels_match <- function(x,y) {
  
	if(seqlevelsStyle(x) == seqlevelsStyle(y)){
		return(TRUE)
	} else{
		return(FALSE)		
	}
}

#check that the seqnames in x are valid - relying on built in error for invalid seqlevelsStyle
seqlevels_valid <- function(x) {
  out <- lapply(seqlevels(x), function(y) {
    check <- try(seqlevelsStyle(y), silent = TRUE)
    if(class(check) == "try-error"){
      return(FALSE)
    } else{
      return(TRUE)
    }
    return(check)
  })
  
  suppressWarnings(if(!all(out)){
    return(FALSE)
  }else{
    return(TRUE)
  })
  
}


#check if seqlevels of x are a subset of seqlevels in y
seqlevels_all_within <- function(x, y) {
  if(all(seqlevels(x) %in% seqlevels(y))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#return any levels in x that are missing in y 
seqlevels_missing <- function(x,y) {
  missing_levels <- seqlevels(x)[!seqlevels(x) %in% seqlevels(y)]  
  return(missing_levels)
}

#return any levels of x that are shared in y
seqlevels_shared <- function(x,y) {
  if(any(seqlevels(x) %in% seqlevels(y))) {
    shared_levels <- seqlevels(x)[seqlevels(x) %in% seqlevels(y)]  
    return(shared_levels)
  } else {
    return(FALSE)
  }
}

## TO DO -- ADD match of style check
## this almost works right -- currently generates multiple error messages for each missing level instead of a single message with list of levels
#full style check function to validate seqlevels
check_regions_bigwig_seqlevels <- function(x,y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
 
  ##tried this inside if() and out to fix multiple error messages but no dice 
  #get lists of missing and shared levels
  missing_levels <- seqlevels_missing(x,y)
  shared_levels <- seqlevels_shared(x,y)
  
  if(!seqlevels_all_within(x,y)) {
    if(!seqlevels_valid(x)){
      stop(paste0(x_name," uses seqnames that do not have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    } 
    if(!seqlevels_valid(y)){
      stop(paste0(y_name," uses seqnames that do not have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    } 
    ##TO DO -- fix multiple error messages bug and add shared levels information?
    if(!all(seqlevels(x) %in% seqlevels(y))) {
      stop(paste0("The seqname(s) ",missing_levels," in [",x_name,"] are missing in [", y_name,"]"))
    }
  }
}





