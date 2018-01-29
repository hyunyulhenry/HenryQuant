#' Download & Load Packages
#'
#' ipak function will Download unpacked packages, then load packages.
#'
#' @param pkg List of packages to install.
#' @return Download & Install packages
#' @importFrom utils install.packages installed.packages
#' @examples
#' \dontrun{
#'   pkg = c("PerformanceAnalytics", "quantmod")
#'   ipak(pkg)
#'   }
#' @export
ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% utils::installed.packages()[, "Package"])]
  if (length(new.pkg))
    utils::install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
