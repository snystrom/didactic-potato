get_file_type <- function(tracks){
  # can be:
  # rtracklayer::BigWigFile
  # Rsamtools::BamFile
  # character vector:
  #  - .bam$
  #  - .bw$
}


#check that seqlevelsStyle of x and y match
#' @noRd
seqlevels_match <- function(x,y) {
  
	if(seqlevelsStyle(x) == seqlevelsStyle(y)){
		return(TRUE)
	} else{
		return(FALSE)		
	}
}


##this is crude and could be improved
#check that all the seqlevelsStyles found in X are also found in y
#' @noRd
seqlevels_style <- function(x,y) {
  x_styles <- lapply(seqlevels(x), function(a) seqlevelsStyle(a))
  y_styles <- lapply(seqlevels(y), function(a) seqlevelsStyle(a))
  if(!all(x_styles %in% y_styles)){
    return(FALSE)
  }else {
    return(TRUE)
  }
}

#check that the seqnames in x are valid - relying on built in error for invalid seqlevelsStyle
#' @noRd
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
#' @noRd
seqlevels_all_within <- function(x, y) {
  if(all(seqlevels(x) %in% seqlevels(y))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#return any levels in x that are missing in y 
#' @noRd
seqlevels_missing <- function(x,y) {
  missing_levels <- seqlevels(x)[!seqlevels(x) %in% seqlevels(y)]  
  return(missing_levels)
}

#return any levels in x that are shared in y 
#' @noRd
seqlevels_shared <- function(x,y) {
  if(any(seqlevels(x) %in% seqlevels(y))) {
    shared_levels <- seqlevels(x)[seqlevels(x) %in% seqlevels(y)]  
    return(shared_levels)
  } else {
    return(FALSE)
  }
}

## TO DO -- Fully test and improve style check?

#' Checks the seqlevels of regions and bw
#' 
#' Implemented to validate regions and bw seqnames/seqlevels prior to generating matrices
#' Will stop function if seqlevels in regions and bw:
#'     1. use different styles 
#'     2. have invalid styles -- see genomeStyles() for supported species/styles 
#'     3. if regions has seqlevels that are not present in bw
#'     
#' @param x an object with a Seqinfo class eg. GRanges object
#' @param y an object with a Seqinfo class eg. Granges object
#'
#' @noRd
#' @examples
check_regions_bigwig_seqlevels <- function(x,y) {
  x_name <- deparse(substitute(x))
  y_name <- deparse(substitute(y))
  missing_levels <- seqlevels_missing(x,y)
  shared_levels <- seqlevels_shared(x,y)
  
  if(!seqlevels_all_within(x,y)) {
    if(!seqlevels_valid(x)){
      stop(paste0(x_name," uses seqnames that do not have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    }
    if(!seqlevels_style(x,y)) {
      stop("Mismatched seqlevelStyles detected bewteen [",x_name,"] and [", y_name,"].\n Verify inputs have matching seqlevelsStyles.")
    }
    if(!seqlevels_valid(y)){
      stop(paste0(y_name," uses seqnames that do not have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    } 
    if(!all(seqlevels(x) %in% seqlevels(y))) {
      stop(paste0("The seqname(s) ",missing_levels," in [",x_name,"] are missing in [", y_name,"]"))
    }
  }
}





