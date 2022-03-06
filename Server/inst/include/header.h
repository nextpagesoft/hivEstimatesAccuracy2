#ifndef _hivPlatform_header_
#define _hivPlatform_header_

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(RcppNumerical)]]

#define BOOST_DISABLE_ASSERTS true

#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <RcppEigen.h>
#include <RcppNumerical.h>

#include "BetaPDF.hpp"
#include "PostW.hpp"
#include "Lspline.hpp"
#include "GetLogMVNPdf.hpp"

#ifdef _OPENMP
#include <omp.h>
#endif

#endif // _hivPlatform_header_
