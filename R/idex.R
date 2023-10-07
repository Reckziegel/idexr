#' Idex-CDI Database
#'
#' JGP Corporate Debt Securities CDI Index.
#'
#' @param index The index to be called. One of: \code{geral}, \code{core} or \code{low_rated}.
#'
#' @return A \code{tibble} with \code{12} columns.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   idex_cdi("geral")
#' }
idex_cdi <- function(index = c("geral", "core", "low_rated")) {

  assertthat::assert_that(assertthat::is.string(index))
  rlang::arg_match(index, c("geral", "core", "low_rated"))

  if (index == "geral") {
    url <- "https://jgp-credito-s3.s3.sa-east-1.amazonaws.com/idex/idex_cdi_geral_datafile.xlsx"
  } else if (index == "core") {
    url <- "https://jgp-credito-s3.s3.sa-east-1.amazonaws.com/idex/idex_cdi_core_datafile.xlsx"
  } else {
    "https://jgp-credito-s3.s3.sa-east-1.amazonaws.com/idex/idex_cdi_low_rated_datafile.xlsx"
  }

  destfile <- "idex_cdi_datafile.xlsx"

  curl::curl_download(url, destfile)
  idex <- readxl::read_excel(destfile)

  names(idex) <- names(idex) |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(pattern = " ", replacement = "_") |>
    stringr::str_remove_all(pattern = "_\\(\\%\\)") |>
    accentless()

  idex |>
    dplyr::mutate(data = lubridate::as_date(data)) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), forcats::as_factor))

}

#' Idex-Ifra Database
#'
#' JGP Corporate Debt Securities Infrastructure Index.
#'
#' @param index The index to be called. One of: \code{geral}, \code{core}.
#'
#' @return A \code{tibble} with \code{12} columns.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   idex_ifra("core")
#' }
idex_ifra <- function(index = c("geral", "core")) {

  assertthat::assert_that(assertthat::is.string(index))
  rlang::arg_match(index, c("geral", "core"))

  if (index == "geral") {
    url <- "https://jgp-credito-s3.s3.sa-east-1.amazonaws.com/idex/idex_infra_geral_datafile.xlsx"
  } else {
    "https://jgp-credito-s3.s3.sa-east-1.amazonaws.com/idex/idex_infra_core_datafile.xlsx"
  }

  destfile <- "idex_ifra_datafile.xlsx"

  curl::curl_download(url, destfile)
  idex <- readxl::read_excel(destfile)

  names(idex) <- names(idex) |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(pattern = " ", replacement = "_") |>
    stringr::str_remove_all(pattern = "_\\(\\%\\)") |>
    stringr::str_remove_all(pattern = "\\(") |>
    stringr::str_replace_all(pattern = "/", replacement = "_") |>
    stringr::str_remove_all(pattern = "\\)") |>
    accentless()

  idex |>
    dplyr::mutate(data = lubridate::as_date(data)) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), forcats::as_factor))

}

