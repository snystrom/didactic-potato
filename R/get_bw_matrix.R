#' Get signal matrix for regions using bigwig input
#'
#' @param bw an [rtracklayer::BigWigFile], [rtracklayer::BigWigFileList] or path to bigwig file
#' @param regions a GRanges object (all regions must be equal width)
#' @param type one of: c("mean", "min", "max", "coverage", "sd")
#'
#' @return a matrix of bigwig signal.
#' @export
#'
#' @examples
get_bw_matrix <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd"),
                          by = NULL){
  UseMethod("get_bw_matrix")
}

#' @noRd
#' @export
get_bw_matrix.character <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd"),
                          by = NULL){
  get_bw_matrix.BigWigFile(rtracklayer::BigWigFile(bw),
                           regions, type)

}

#' @noRd
#' @export
get_bw_matrix.BigWigFileList <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd"),
                          by = NULL){
  if(!is.null(by)){
     #split regions by grouping metaCol
    regions.by <- regions %>% split(., mcols(.)[,by])
     #outs a list of 3d arrays of dimension (regions, position, track)
    lapply(names(regions.by), function(x) {
      #generate a 3d array for all bw in the bw_list for this specific region
      #setting by = NULL here because already breaking out by grouping before sending to get_bw_matrix(), too clunky?
     
      #trying to pull out name data from bw list... 
      sapply(bw, function(y) {
        get_bw_matrix(y@resource, regions.by[[x]], type, by = NULL) 
      }, simplify = "array", USE.NAMES = TRUE)
    })   
  }else{
    sapply(bw, get_bw_matrix, regions, type, by, simplify = "array")
  }
  
  # TODO: consider 3d matrix output:
  # simplify2array()
  # TODO: consider setting dimnames:
  # (this is pseudocode, really you'd have to iterate region & position along entries)
  # dimnames(mm) <- list("region", "position", "track")
}

#' @noRd
#' @export
#' @import GenomicRanges
#' @importFrom GenomeInfoDb seqlevels seqlevels<-
#' @importFrom rtracklayer summary
get_bw_matrix.BigWigFile <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd"),
                          by = NULL){
  
  type <- match.arg(type, choices = c("mean", "min", "max", "coverage", "sd"))
                           
  # TODO: expect input is a BigWigFile,
  # need to coerce path to this before passing
  #bw <- rtracklayer::BigWigFile(path)

  size <- unique(width(regions))
  if (length(size) != 1) {
    stop("All regions must be equal width", call. = FALSE)
  }

  ##TO DO -- fully test with different invalid conditions
  check_regions_bigwig_seqlevels(regions,bw) 
  

## FOR REVIEW TO DROP ##
  # not 100% sure how the seqlevels thing works, need to do some reality
  # checking that no errors introduced here: WRITE TESTS!! (but for what?)
#  regions_seqs <- seqnames(seqinfo(regions))
#  bw_seqs <- seqnames(seqinfo(bw))

#  if (!all(regions_seqs %in% bw_seqs)) {
#    bad_seqnames <- regions_seqs[!(regions_seqs %in% bw_seqs)]
#    stop(paste0("Some input regions do not match chromosomes found in the bigwig file.\nThe following seqnames are not found: ",
#                bad_seqnames), call. = FALSE)
#  }

  #match seqlevels between inputs 
  seqlevels(regions) <- seqlevels(bw)
  seqinfo(regions) <- seqinfo(bw)

  # suppress warnings because of out of bounds?
  # Nah, probably just let it bubble up
  if(!is.null(by)){
    regions.by <- regions %>% split(., mcols(.)[,by])
    matrix <- lapply(regions.by, function(x){
      rtracklayer::summary(bw, which = x, as = "matrix",
                           type = type, size = size)
    })
  }else{
    matrix <- rtracklayer::summary(bw, which = regions, as = "matrix",
                                  type = type, size = size)   
  }
 

  matrix
}



