factor.score.reliability <- function(Lambda, Phi, Estimators=c("Regression", "Bartlett", "McDonald")) {
  # Helper functions for frequently used matrix operations
  Mdiag <- function(x) return(diag(diag(x)))
  inv <- function(x) return(solve(x))
  # If a 'loadings' class is provided for lambda, we can easily convert it
  if (is(Lambda, "loadings"))
    Lambda <- Lambda[,]
  # Perform several validity checks of the provided arguments
  if (any(missing(Lambda), missing(Phi), is.null(Lambda), is.null(Phi)))
    stop("Missing argument(s).")
  if (any(nrow(Phi) == 0, nrow(Lambda) == 0, ncol(Phi) == 0, ncol(Lambda) == 0))
    stop("Some diemension(s) of Phi or Lambda seem to be empty.")
  if (nrow(Phi) != ncol(Phi))
    stop("Phi has to be a q x q matrix.")
  if (ncol(Lambda) != nrow(Phi))
    stop("Phi and Lambda have a different count of factors.")
  if (any(round(min(Phi)) < 0, round(max(Phi)) > 1))
    stop("Phi contains invalid values (outside [0; 1]).")
  Estimators.Allowed <- c("Regression", "Bartlett", "McDonald")
  if (is.null(Estimators)) {
    message("No 'Estimators' defined, use 'Regression' as default.")
    Estimators <- c("Regression")
  }
  Estimators <- match.arg(Estimators, Estimators.Allowed, several.ok = TRUE)
  # Regenerate covariance matrix from factor loadings matrix
  Sigma <- (Lambda %*% Phi %*% t(Lambda))
  Sigma <- Sigma - Mdiag(Sigma) + diag(nrow(Lambda))
  # Calculate uniqueness/error of items
  Psi <- Mdiag(Sigma - Lambda %*% Phi %*% t(Lambda))^0.5
  if (round(min(diag(Psi))) < 0)
    stop("The diagonal of Psi contains negative values.")
  ret <- list()
  if ("Regression" %in% Estimators) {
    # Reliability of Thurstone's Regression Factor Score Estimators
    # cf. Equation 6 in manuscript
    Rtt.Regression <-
      inv( Mdiag( Phi %*% t(Lambda) %*% inv(Sigma) %*% Lambda %*% Phi ) )^0.5 %*%
      Mdiag( Phi %*% t(Lambda) %*% inv(Sigma) %*% Lambda %*% Phi %*% t(Lambda) %*% inv(Sigma) %*%
               Lambda %*% Phi) %*%
      inv( Mdiag( Phi %*% t(Lambda) %*% inv(Sigma) %*% Lambda %*% Phi ) )^0.5
    ret$Regression <- diag(Rtt.Regression)
  }
  if ("Bartlett" %in% Estimators) {
    # Reliability of Bartlett's Factor Score Estimators
    # cf. Equation 11 in manuscript
    Rtt.Bartlett <- inv( Mdiag( inv(t(Lambda) %*% inv(Psi)^2 %*% Lambda) + Phi ) )
    ret$Bartlett <- diag(Rtt.Bartlett)
  }
  if ("McDonald" %in% Estimators) {
    # Reliability of McDonald's correlation preserving factor score estimators
    # cf. Equation 12 in manuscript
    Decomp <- svd(Phi)
    N <- Decomp$u %*% abs(diag(Decomp$d))^0.5
    sub.term <-
      t(N) %*% t(Lambda) %*% inv(Psi)^2 %*% Sigma %*% inv(Psi)^2 %*% Lambda %*% N
    Decomp <- svd(sub.term)
    sub.term <- Decomp$u %*% (diag(Decomp$d)^0.5) %*% t(Decomp$u)
    Rtt.McDonald <-
      Mdiag( inv(sub.term) %*% t(N) %*% t(Lambda) %*% inv(Psi)^2 %*% Lambda %*% Phi %*% t(Lambda) %*%
               inv(Psi)^2 %*% Lambda %*% N %*% inv(sub.term))
    ret$McDonald <- diag(Rtt.McDonald)
  }
  # Return reliabilities as list, so it can be accessed via e.g. factor.score.reliability(L, P)$Regression
  return(ret)
}