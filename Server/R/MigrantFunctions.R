# The function log_postW() gives the posterior distribution of w,
# i.e. the time gap between seroconversion and diagnosis
# We dropped the additive constants, i.e. the posterior distribution is unnormalized
LogPostW <- function(
  w,
  AIDSdateKnown = FALSE
) {
  # Design matrices of the i-th patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  consc <- x[, 'consc']
  consr <- x[, 'consr']

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formulae used for constructing the appropriate design matrices

  # Get the design matrix for CD4
  fxcd4 <- formula(
    yvar ~
    I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )
  x.cd4 <- model.matrix(fxcd4, data = data.frame(yvar = y, x)[consc == 1, ])
  dim.cd4 <- dim(x.cd4)

  # Get the design matrix for VL
  fxvr <- formula(
    yvar ~
    I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      I(log(dtime + w + 0.013)) * gender +
      I(log(dtime + w + 0.013)) * region_group +
      I(log(dtime + w + 0.013)) * mode +
      I(log(dtime + w + 0.013)) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )

  x.vr <- model.matrix(fxvr, data = data.frame(yvar = y, x)[consr == 1, ])
  dim.vr <- dim(x.vr)

  # Combine into one design matrix
  x <- rbind(
    cbind(matrix(0, nr = dim.vr[1], nc = dim.cd4[2]), x.vr),
    cbind(x.cd4, matrix(0, nr = dim.cd4[1], nc = dim.vr[2]))
  )

  # Formula for the design matrices of the random effects
  fz <- formula(
    yvar ~
      -1 + consc + I((dtime + w) * consc) + consr + I((dtime + w) * consr) +
      I(log(dtime + w + 0.013) * consr)
  )
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal distribution
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(VarCovRE, z) + diag(consr * sigma2[2] + consc * sigma2[1])

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  # Return -p since the "optim" function works for minimization problems
  return(-p)
}

LogPostW2 <- function(
  w,
  x,
  y,
  z,
  xAIDS,
  maxDTime,
  betaAIDS,
  bFE,
  sigma2,
  varCovRE
) {
  consc <- x[, Consc]
  consr <- x[, Consr]

  # Design matrix of the time-to-AIDS model
  xAIDS[3] <- xAIDS[3] - w
  lambda <- exp(xAIDS %*% betaAIDS)

  # Formulae used for constructing the appropriate design matrices

  # Get the design matrix for CD4
  fxCD4 <- formula(
    YVar ~
    I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegion +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(AgeDiag - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )
  xCD4 <- model.matrix(fxCD4, data = cbind(y, x)[Consc == 1])
  dimCD4 <- dim(xCD4)

  # Get the design matrix for VL
  fxVR <- formula(
    YVar ~
    I(DTime + w) * Gender +
      I(DTime + w) * GroupedRegion +
      I(DTime + w) * Mode +
      I(DTime + w) * lspline::lspline(I(AgeDiag - w), knots = c(25, 35, 45)) +
      I(log(DTime + w + 0.013)) * Gender +
      I(log(DTime + w + 0.013)) * GroupedRegion +
      I(log(DTime + w + 0.013)) * Mode +
      I(log(DTime + w + 0.013)) * lspline::lspline(I(AgeDiag - w), knots = c(25, 35, 45)) +
      lspline::lspline(I(Calendar - w), knots = c(16, 22))
  )
  xVR <- model.matrix(fxVR, data = cbind(y, x)[Consr == 1])
  dimVR <- dim(xVR)

  # Combine into one design matrix
  x <- rbind(
    cbind(matrix(0, nr = dimVR[1], nc = dimCD4[2]), xVR),
    cbind(xCD4, matrix(0, nr = dimCD4[1], nc = dimVR[2]))
  )

  # Formula for the design matrices of the random effects
  fz <- formula(
    YVar ~
      -1 + Consc + I((DTime + w) * Consc) + Consr + I((DTime + w) * Consr) +
      I(log(DTime + w + 0.013) * Consr)
  )
  z <- model.matrix(fz, data = cbind(y, z))

  # Mean and variance of the normal distribution
  mu <- c(x %*% bFE)
  var <- z %*% tcrossprod(varCovRE, z) + diag(consr * sigma2[2] + consc * sigma2[1])

  p <- mvnfast::dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  # Return -p since the "optim" function works for minimization problems
  return(-p)
}

LogPostWCD4 <- function(
  w
) {
  # Design matrices of i-patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formula for design matrix of fixed effects
  fxcd4 <- formula(
    yvar ~
      I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )
  x <- model.matrix(fxcd4, data = data.frame(yvar = y, x))

  # Formula for design matrix of random effects
  fz <- formula(yvar ~ -1 + consc + I(dtime + w))
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE.CD4)
  var <- z %*% tcrossprod(VarCovRE.CD4, z) + sigma2.CD4 * diag(length(x[, 1]))

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  return(-p)
}

LogPostWCD42 <- function(
  w,
  x,
  y,
  z,
  xAIDS,
  maxDTime,
  betaAIDS,
  bFE,
  sigma2,
  varCovRE
) {

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formula for design matrix of fixed effects
  fxcd4 <- formula(
    yvar ~
      I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )
  x <- model.matrix(fxcd4, data = data.frame(yvar = y, x))

  # Formula for design matrix of random effects
  fz <- formula(yvar ~ -1 + consc + I(dtime + w))
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE.CD4)
  var <- z %*% tcrossprod(VarCovRE.CD4, z) + sigma2.CD4 * diag(length(x[, 1]))

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  return(-p)
}

LogPostWvl <- function(
  w
) {
  # Design matrices of i-patient
  x <- x[[i]]
  z <- z[[i]]
  y <- y[[i]]

  # Design matrix of the time-to-AIDS model
  xaids <- xaids[i, ]
  xaids[3] <- xaids[3] - w
  maxdtime <- maxdtime[i]
  lambda <- exp(xaids %*% beta.aids)

  # Formula for design matrices
  fxvr <- formula(
    yvar ~
      I(dtime + w) * gender +
      I(dtime + w) * region_group +
      I(dtime + w) * mode +
      I(dtime + w) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      I(log(dtime + w + 0.013)) * gender +
      I(log(dtime + w + 0.013)) * region_group +
      I(log(dtime + w + 0.013)) * mode +
      I(log(dtime + w + 0.013)) * lspline(I(ageDiag - w), knots = c(25, 35, 45)) +
      lspline(I(calendar - w), knots = c(16, 22))
  )

  x <- model.matrix(fxvr, data = data.frame(yvar = y, x))

  # Formula for design matrix of random effects
  fz <- formula(yvar ~ -1 + consr + I((dtime + w) * consr) + I(log(dtime + w + 0.013) * consr))
  z <- model.matrix(fz, data = data.frame(yvar = y, z))

  # Mean and variance of the normal kernel
  mu <- c(x %*% bFE.VL)
  var <- z %*% tcrossprod(VarCovRE.VL, z) + sigma2.VL * diag(length(x[, 1]))

  p <- dmvn(y, mu = mu, sigma = var, log = TRUE) - lambda * (w + maxdtime)^kappa

  return(-p)
}

# Posterior up to a proportionality constant
PostW <- function(w, ...) {
  return(exp(-LogPostW(w, ...)))
}

PostW2 <- function(w, ...) {
  return(exp(-LogPostW2(w, ...)))
}

PostWcd4 <- function(w) {
  return(exp(-LogPostWcd4(w)))
}

PostWCD42 <- function(w) {
  return(exp(-LogPostWCD42(w)))
}

PostWvl <- function(w) {
  return(exp(-LogPostWvl(w)))
}

MeanPostW <- function(w, ...) {
  return(w * PostW(w, ...))
}

MeanPostWcd4 <- function(w, ...) {
  return(w * PostWcd4(w, ...))
}

MeanPostWvl <- function(w, ...) {
  return(w * PostWvl(w, ...))
}

# Vectorize the functions as the "integrate" function works with vectorized functions
VlogPostW <- Vectorize(LogPostW)
VlogPostWcd4 <- Vectorize(LogPostWcd4)
VlogPostWvl <- Vectorize(LogPostWvl)
VpostW <- Vectorize(PostW)
VpostW2 <- Vectorize(PostW2, vectorize.args = c('w'))
VpostWcd4 <- Vectorize(PostWcd4)
VpostWCD42 <- Vectorize(PostWCD42, vectorize.args = c('w'))
VpostWvl <- Vectorize(PostWvl)
VmeanPostW <- Vectorize(MeanPostW)
VmeanPostWcd4 <- Vectorize(MeanPostWcd4)
VmeanPostWvl <- Vectorize(MeanPostWvl)

# AIDS only cases
LogPostWaids <- function(w) {
  x <- xAIDS[i, ]
  x[3] <- x[3] - w
  dTime <- dTime[i]

  lambda <- exp(x %*% betaAIDS)

  val <- log(kappa) + log(lambda) + (kappa - 1) * log(w + dTime) - lambda * (w + dTime)^kappa
  return(-val)
}

LogPostWaids2 <- function(w, x, dTime, betaAIDS, kappa) {
  x[3] <- x[3] - w
  lambda <- exp(x %*% betaAIDS)

  val <- log(kappa) + log(lambda) + (kappa - 1) * log(w + dTime) - lambda * (w + dTime)^kappa
  return(-val)
}


PostWaids <- function(w) {
  return(exp(-LogPostWaids(w)))
}

PostWaids2 <- function(w, ...) {
  return(exp(-LogPostWaids2(w, ...)))
}


MeanPostWaids <- function(w) {
  return(w * PostWaids(w))
}

VlogPostWaids <- Vectorize(LogPostWaids)
VlogPostWaids2 <- Vectorize(LogPostWaids2, vectorize.args = c('w'))
VpostWaids <- Vectorize(PostWaids)
VpostWaids2 <- Vectorize(PostWaids2, vectorize.args = c('w'))
VmeanPostWaids <- Vectorize(MeanPostWaids)
