#' Scan cellular neighbourhoods.
#'
#' @param m Distance matrix. Can be obtained from function findNearCells.
#' @param mode Character. Either proximityFocused or smoothFadeout.
#' By default is proximityFocused.
#' @param tau The hyperparameter tau, by default is median(m**2)/5
#' @param t_init An initial tau. In the smoothFadeout mode, user can provide
#' an initial tau for optimization.
#'
#' @return A probability matrix.
#' @export
#'
#' @examples
#'
#' m <- matrix(abs(rnorm(1000 * 100)), 1000, 100)
#'
#' pm <- scanHoods(m)
#'
scanHoods <- function(m, mode = c("proximityFocused", "smoothFadeout"),
                      tau = NA, t_init = NA) {
  if (!is(m, "matrix")){
    stop("The input m must be a matrix.")
  }
  
  if (length(mode) == 2) {
    mode <- "proximityFocused"
  }

  if (!(all(mode %in% c("proximityFocused", "smoothFadeout")))) {
    stop("mode must be either proximityFocused or smoothFadeout.")
  }

  if (mode == "proximityFocused") {
    if (is(tau, "logical")) {
      tau <- median(m**2) / 5
    }
  } else if (mode == "smoothFadeout") {
    if (is(t_init, "logical")) {
      t_init <- median(m**2)
    }

    optim_result <- stats::optim(par = t_init, fn = f_nll,
                                 m = m, method = "BFGS")

    tau <- optim_result$par

    msg <- paste0("Optimized tau is: ", tau)

    message(msg)
  }

  msg <- paste0("Tau is set to: ", tau)
  message(msg)
  probm <- soft_max_intl(m, tau)

  return(probm)
}


soft_max_intl <- function(m, t) {
  m <- m**2
  m <- -m / t
  exp_m <- exp(m)
  probm <- sweep(exp_m, 1, rowSums(exp_m), "/")
  return(probm)
}

f_nll <- function(m, t) {
  probm <- soft_max_intl(m, t)
  nll <- -sum(log(probm + 1e-8))
  return(nll)
}
