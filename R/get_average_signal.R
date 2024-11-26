#' Get a dataframe of average signal from bw within regions
#'
#' @param bw bigwig file
#' @param regions regions
#' @param by by
#'
#' @return
#' @export
#'
#' @examples
get_average_signal <- function(bw, regions, by = NULL) {
  size <- unique(width(regions))

  if (!is.null(by)) {
    matrix <- get_bw_matrix(bw, regions, by = by)

    df_list <- lapply(names(matrix), function(name) {
      data.frame(matrix[name], check.names = FALSE) %>%
        setNames(1:size) %>%
        reshape2::melt(variable.name = "position") %>%
        dplyr::mutate(
          id = name,
          position = as.double(position)
        ) %>%
        dplyr::group_by(position, id) %>%
        dplyr::summarise(mean = mean(value, na.rm = TRUE))
    })

    signal.df <- dplyr::bind_rows(df_list)

    return(signal.df)
  } else {
    matrix <- get_bw_matrix(bw, regions)

    signal.df <- matrix %>%
      data.frame() %>%
      setNames(1:size) %>%
      reshape2::melt(variable.name = "position") %>%
      dplyr::mutate(position = as.double(position)) %>%
      dplyr::group_by(position) %>%
      dplyr::summarise(value.mean = mean(value, na.rm = TRUE))

    return(signal.df)
  }
}
