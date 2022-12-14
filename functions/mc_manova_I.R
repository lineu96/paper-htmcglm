#' @name mc_manova_I
#'
#' @author Lineu Alberto Cavazani de Freitas,
#' \email{lineuacf@@gmail.com}
#'
#' @export
#'
#' @title MANOVA type I table for mcglm objects via Wald test.
#'
#' @description Performs Wald tests to generate multivariate type-I
#' analysis-of-variance tables for model objects produced by mcglm.
#'
#' @param object An object of \code{mcglm} class.
#' 
#' @param verbose a logical if TRUE print some information about the 
#' tests performed. Default verbose = TRUE.
#'
#' @return Type I MANOVA table for mcglm objects.
#'
#' @seealso \code{mc_manova_II}, \code{mc_manova_III} and
#' \code{mc_manova_disp}.
#'
#' @examples
#'
#' library(mcglm)
#' library(Matrix)
#' library(htmcglm)
#'
#' data("soya", package = "mcglm")
#'
#' form.grain <- grain ~ water * pot
#' form.seed <- seeds ~ water * pot
#'
#' soya$viablepeasP <- soya$viablepeas / soya$totalpeas
#' form.peas <- viablepeasP ~ water * pot
#'
#' Z0 <- mc_id(soya)
#' Z1 <- mc_mixed(~0 + factor(block), data = soya)
#'
#' fit_joint <- mcglm(linear_pred = c(form.grain,
#'                                    form.seed,
#'                                    form.peas),
#'                    matrix_pred = list(c(Z0, Z1),
#'                                       c(Z0, Z1),
#'                                       c(Z0, Z1)),
#'                    link = c("identity",
#'                             "log",
#'                             "logit"),
#'                    variance = c("constant",
#'                                 "tweedie",
#'                                 "binomialP"),
#'                    Ntrial = list(NULL,
#'                                  NULL,
#'                                  soya$totalpeas),
#'                    power_fixed = c(TRUE,TRUE,TRUE),
#'                    data = soya)
#'
#' mc_manova_I(fit_joint)
#'

mc_manova_I <- function(object, verbose = TRUE){

  #----------------------------------------------------------------

  # Vetor beta chapeu
  beta <- coef(object, type = "beta")[,c(1, 4)]

  #----------------------------------------------------------------

  # N??mero de betas
  n_beta <- sum(as.vector(table(beta$Response)))

  #----------------------------------------------------------------

  # N??mero de respostas
  n_resp <- length(as.vector(table(beta$Response)))

  #----------------------------------------------------------------

  # ERROS E AVISOS

  preds <- c()

  for (i in 1:n_resp) {
    preds[i] <- sub(".*~", "",gsub(" ", "",
                                   as.character(object$linear_pred)[i]))
  }

  if(length(unique(preds)) != 1) stop("For MANOVA functions, the predictors must be the same for all outcomes.")

  if(n_resp == 1) warning("You are applying a MANOVA function to a univariate problem.")

  #----------------------------------------------------------------

  # vcov desconsiderando parametros de dispersao e potencia
  vcov_betas <- vcov(object)[1:n_beta, 1:n_beta]

  #----------------------------------------------------------------

  # ??ndice que associa beta a vari??vel
  p_var <- attr(object$list_X[[1]], "assign")

  #----------------------------------------------------------------

  # Matriz F para todos os par??metros (Hypothesis matrix)
  F_all <- diag(length(p_var))

  #----------------------------------------------------------------

  # Matriz F por vari??vel (Hypothesis matrix)
  expand <- by(data = F_all,
               INDICES = p_var,
               FUN = as.matrix)

  beta_names <- object$beta_names[[1]]

  testes <- data.frame(beta_names,
                       interacao = stringr::str_detect(beta_names, ':'))

  for (i in 1:(length(expand))) {
    testes[,i+2] <- colSums(expand[[i]])
  }

  aux <- matrix(nrow = nrow(testes),
                ncol = ncol(testes)-2)


  for (i in 3:(ncol(testes)-1)) {
    aux[,i-2] <- rowSums(testes[,i:ncol(testes)])
  }

  aux[,ncol(aux)] <- testes[,ncol(testes)]

  p_varII <- aux

  #cbind(beta_names,p_varII)

  F_par <- list()

  for (i in 1:ncol(p_varII)) {
    F_par[[i]] <- by(data = F_all,
                     INDICES = p_varII[,i],
                     FUN = as.matrix)$`1`
  }

  #----------------------------------------------------------------

  # Matriz G
  G <- diag(n_resp)

  #----------------------------------------------------------------

  # Matriz L

  L_par <- list()

  for (i in 1:length(F_par)) {
    L_par[[i]] <- kronecker(G, F_par[[i]])
  }


  #----------------------------------------------------------------

  ## Tabela

  W <- vector() # Vetor para a estat??stica de teste
  gl <- vector() # Vetor para graus de liberdade
  p_val <- vector() # Vetor para p-valor

  ### Estat??stica de teste:
  #### t(L*beta) x (L*vcov*t(L))^-1 x (L*beta) ~ Qui-quadrado(numero de
  #### parametros testados)

  for (i in 1:length(L_par)) {

    W[i] <- as.numeric((t(L_par[[i]]%*%beta$Estimates)) %*%
                         (solve(L_par[[i]]%*%
                                  vcov_betas%*%
                                  t(L_par[[i]]))) %*%
                         (L_par[[i]]%*%
                            beta$Estimates))
    gl[i] <- nrow(L_par[[i]])
    p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)
  }

  tabela <- data.frame(Covariate = c("Intercept",
                                     attr(terms(object$linear_pred[[1]]),
                                          "term.labels")),
                       Df = gl,
                       Chi = round(W, 4),
                       'Pr(>Chi)' = round(p_val, 4),
                       check.names = F)

  #----------------------------------------------------------------
  
  if (verbose == TRUE) {
    cat("MANOVA type I using Wald statistic for fixed effects\n\n")
    cat("Call: ")
    cat(paste0('~ ', preds[[1]]))
    cat("\n")
    print(tabela)
    
    return(invisible(tabela))
  } else {
    return(tabela)
  }

}
