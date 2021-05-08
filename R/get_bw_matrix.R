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
                          type = c("mean", "min", "max", "coverage", "sd")){
  UseMethod("get_bw_matrix")
}

#' @noRd
#' @export
get_bw_matrix.character <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd")){
  get_bw_matrix.BigWigFile(rtracklayer::BigWigFile(bw),
                           regions, type)

}

#' @noRd
#' @export
get_bw_matrix.BigWigFileList <- function(bw, regions,
                          type = c("mean", "min", "max", "coverage", "sd")){
  lapply(bw, get_bw_matrix, regions, type)
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
                          type = c("mean", "min", "max", "coverage", "sd")){
  type <- match.arg(type, choices = c("mean", "min", "max", "coverage", "sd"))

  # TODO: expect input is a BigWigFile,
  # need to coerce path to this before passing
  #bw <- rtracklayer::BigWigFile(path)

  size <- unique(width(regions))
  if (length(size) != 1) {
    stop("All regions must be equal width", call. = FALSE)
  }

  # not 100% sure how the seqlevels thing works, need to do some reality
  # checking that no errors introduced here: WRITE TESTS!! (but for what?)

  regions_seqs <- seqnames(seqinfo(regions))
  bw_seqs <- seqnames(seqinfo(bw))

  if (!all(regions_seqs %in% bw_seqs)) {
    bad_seqnames <- regions_seqs[!(regions_seqs %in% bw_seqs)]
    stop(paste0("Some input regions do not match chromosomes found in the bigwig file.\nThe following seqnames are not found: ",
                bad_seqnames), call. = FALSE)
  }

  seqlevels(regions) <- seqlevels(bw)
  seqinfo(regions) <- seqinfo(bw)

  # suppress warnings because of out of bounds?
  # Nah, probably just let it bubble up
  matrix <- rtracklayer::summary(bw, which = regions, as = "matrix",
                                 type = type, size = size)

  matrix
}



