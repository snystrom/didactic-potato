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

##sort of reinventing the wheel -- seqlevelsStyle(x) will error out with non-standard x
#check that seqlevelsStyle of x is a valide type
##This does not work as intended -- if all styles valid returns styles of a seqnames
seqlevels_valid <- function(x) {
	out <- tryCatch(
	  {
	    lapply(seqlevels(x), function(x) {
	    seqlevelsStyle(x)
	    })
	  },
	  error=function(cond) {
	    return(FALSE) 
	  }
  )
	return(out)
}


#check if seqlevels of x are a subset of seqlevels in y
seqlevels_all_within <- function(x, y) {
	if(seqlevels(x) %in% seqlevels(y)) {
		return(TRUE)
	} else {
		return(FALSE)
	}
}

#return any levels in y that are missing in x
seqlevels_missing <- function(x,y) {
    missing_levels <- seqlevels(y)[!seqlevels(y) %in% seqlevels(x)]  
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

##clunky first draft
#full style check function to validate seqlevels
check_regions_bigwig_seqlevels <- function(x,y) {
  if(!seqlevels_all_within(x,y)) {
    if(!seqlevels_valid(x)){
      stop(paste0(x," uses seqnames that do no have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    } 
    if(!seqlevels_valid(y)){
      stop(paste0(y," uses seqnames that do no have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles."))
    } 
    if(!all(seqlevels(y) %in% seqlevels(x))) {
      missing_levels <- seqlevels_missing(x,y)
      shared_levels <- seqlevels_shared(x,y)
      stop(paste0(missing_levels," seqlevels from ",y," are missing in ",x,"\n", shared_levels," are shared in both ",x," and ",y,"."))
    }
  }
}





