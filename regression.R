det <- function(matrix) {
    if (nrow(matrix) != ncol(matrix)) stop("Matrix must be square")
    if (nrow(matrix) == 1) return(matrix[1,1])
    d <- 0
    multiplier <- 1
    for (i in 1:ncol(matrix)) {
        minor <- matrix[-1, -i, drop = FALSE]
        d <- d + multiplier * matrix[1, i] * det(minor)
        multiplier <- -multiplier
    }
    return(d)
}

coefs <- function(x, y, m) {
    coefs_ <- matrix(nrow = m+1, ncol = m+1)
    # Fill X'X matrix
    for (i in 1:(m+1)) {
        for (j in 1:(m+1)) {
            exponent <- (i-1) + (j-1)
            coefs_[i, j] <- sum(x^exponent)
        }
    }
    # Fill RHS vector (X'y)
    for (i in 1:(m+1)) {
        exponent <- i - 1
        coefs_[i, m+1] <- sum(y * x^exponent)
    }
    return(coefs_)
}

reg <- function(x, y, m) {
    a <- numeric(m+1)
    cfs <- coefs(x, y, m)
    XTX <- cfs[, 1:(m+1)]
    XTy <- cfs[, m+1]
    det_XTX <- det(XTX)
    if (det_XTX == 0) stop("System is singular; no unique solution")
    
    for (i in 1:(m+1)) {
        XTX_i <- XTX
        XTX_i[, i] <- XTy
        a[i] <- det(XTX_i) / det_XTX
    }
    return(a)
}
