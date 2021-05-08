get_file_type <- function(tracks){
  # can be:
  # rtracklayer::BigWigFile
  # Rsamtools::BamFile
  # character vector:
  #  - .bam$
  #  - .bw$
}

check_style <- function(regions, bw){
  out <- tryCatch(
    {
      message("Checking seqnameStyles...")
      seqlevelsStyle(regions)
      seqlevelsStyle(bw) 
    },
    error = function(cond) {
      stop("One or both range objects use seqnames that do no have a compatible entry for the species supported by Seqname.\nSee genomeStyles() for supported species/styles.")
    }
  )
}

