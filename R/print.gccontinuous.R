print.gccontinuous<- function (x, digits=4, ...)
{
  if (x$model %in% c("lasso","ridge","elasticnet")) {
    cat("model: ",x$model, sep = "")
    if (is.null(x$m)) {
      cat(", tuning parameters: ", sep = "")
      if (x$model == "elasticnet") {
        cat("lambda= ",round(x$tuning.parameters$lambda,digits=digits), " alpha= ",round(x$tuning.parameters$alpha,digits=digits),sep=" ")}
      if (x$model == "lasso") {
        cat("lambda= ",round(x$tuning.parameters$lambda,digits=digits),sep=" ")}
      if (x$model == "ridge") {
        cat("lambda= ",round(x$tuning.parameters$lambda,digits=digits),sep=" ")}
    }
    cat("\nfamily: gaussian(link = 'identity')")
    cat("\nCall:", "\n", sep = "")
    dput(x$formula)
  }
  if (x$model %in% c("all","aic","bic")) {
    cat(x$model," model \nfamily: gaussian(link = 'identity')\nCall:", "\n", sep = "")
    if (is.null(x$m)) {dput(x$tuning.parameters)} else {dput(x$tuning.parameters[[1]])}
  }
  cat("\n")
  
  cat("Estimates: \n")
  res <- matrix(c(mean(x$adjusted.results$m0, na.rm=TRUE),
                  mean(x$adjusted.results$m1, na.rm=TRUE),
                  mean(x$adjusted.results$delta, na.rm=TRUE),
                  mean(x$adjusted.results$ratio, na.rm=TRUE)),
                nrow = 1)
  colnames(res) <- c("m0", "m1", "m1-m0", "m1/m0")
  rownames(res) <- ""
  
  printCoefmat(res, digits = digits, ..., P.values = FALSE, has.Pvalue = FALSE, na.print = "")
  
  
  cat("\n")
  cat(paste0("n= ",x$n))
  
  cat("\n")
  if (!is.null(x$nimput)) {
    if (x$nimput == 1) { cat(x$nimput, " observation imputed", sep=""); cat("\n") }
    if (x$nimput > 1) { cat(x$nimput, " observations imputed", sep=""); cat("\n") }
  }
  if(x$missing==1) { cat(x$missing, " observation deleted due to missingness", sep=""); cat("\n") }
  if(x$missing >1) { cat(x$missing, " observations deleted due to missingness", sep=""); cat("\n") }
  if(!is.null(x$missing_origin)){if(x$missing_origin == 1){cat(x$missing_origin, " observation deleted due to missingness in original dataset", sep=""); cat("\n")}}
  if(!is.null(x$missing_origin)){if(x$missing_origin > 1){cat(x$missing_origin, " observations deleted due to missingness in original dataset", sep=""); cat("\n")}}
  
}
