#' @name mc_anova_I
#'
#' @author Lineu Alberto Cavazani de Freitas,
#' \email{lineuacf@@gmail.com}
#'
#' @export
#'
#' @title ANOVA type I table for mcglm objects via Wald test.
#'
#' @description Performs Wald tests to generate type-I analysis-of-
#' variance tables per response for model objects produced by mcglm.
#'
#' @param object An object of \code{mcglm} class.
#' 
#' @param verbose a logical if TRUE print some information about the 
#' tests performed. Default verbose = TRUE.
#'
#' @return Type I ANOVA table for mcglm objects.
#'
#' @seealso \code{mc_anova_II}, \code{mc_anova_III} and
#' \code{mc_anova_disp}.
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
#' mc_anova_I(fit_joint)
#'

mc_anova_I <- function(object, verbose = TRUE){

  #----------------------------------------------------------------

  # Vetor beta chapeu e indice de resposta
  beta <- coef(object, type = "beta")[,c(1, 4)]

  #----------------------------------------------------------------

  # N??mero de betas por resposta
  n_beta <- as.vector(table(beta$Response))

  #----------------------------------------------------------------

  # N??mero de respostas
  n_resp <- length(n_beta)

  #----------------------------------------------------------------

  # Lista vcov por resposta desconsiderando parametros de dispersao e
  # potencia

  vcov_betas <- list()

  if (n_resp == 1) {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
  } else {
    vcov_betas[[1]] <- vcov(object)[1:n_beta[1], 1:n_beta[1]]
    for (i in 2:n_resp) {
      vcov_betas[[i]] <-
        vcov(object)[(cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i]),
                     (cumsum(n_beta)[i-1]+1):(cumsum(n_beta)[i])]

    }
  }

  #----------------------------------------------------------------

  # ??ndice que associa beta a vari??vel por resposta

  p_var <- list()

  for (i in 1:n_resp) {
    p_var[[i]] <- attr(object$list_X[[i]], "assign")
  }

  #----------------------------------------------------------------

  # Matriz L para todos os par??metros (Hypothesis matrix), por resposta
  L_all <- list()

  for (i in 1:n_resp) {
    L_all[[i]] <- diag(length(p_var[[i]]))
  }

  #----------------------------------------------------------------

  # ??ndice que associa beta a vari??vel por resposta  para
  # teste sequencial

  expand <- list()

  for (i in 1:length(L_all)) {
    expand[[i]] <- by(data = L_all[[i]],
                      INDICES = p_var[[i]],
                      FUN = as.matrix)
  }


  beta_names <- list()

  for (i in 1:length(L_all)) {
    beta_names[[i]] <- object$beta_names[[i]]
  }


  testes <- list()

  for (i in 1:length(L_all)) {
    testes[[i]] <- data.frame(beta_names = beta_names[[i]],
                              interacao =
                                stringr::str_detect(beta_names[[i]],
                                                    ':'))
  }


  for (i in 1:length(L_all)) {
    for (j in 1:(length(expand[[i]]))) {
      testes[[i]][,j+2] <- colSums(expand[[i]][[j]])
    }
  }

  p_varII <- list()

  for (i in 1:n_resp) {
    p_varII[[i]] <- matrix(nrow = nrow(testes[[i]]),
                           ncol = ncol(testes[[i]])-2)
  }


  for (j in 1:n_resp) {
    for (i in 3:(ncol(testes[[j]])-1)) {
      p_varII[[j]][,i-2] <- rowSums(testes[[j]][,i:ncol(testes[[j]])])
    }

    p_varII[[j]][,ncol(p_varII[[j]])] <- testes[[j]][,ncol(testes[[j]])]
  }

  #----------------------------------------------------------------

  # Matriz L por vari??vel (Hypothesis matrix), por resposta

  L_par <- list()
  length(L_par) <- n_resp


  for (j in 1:length(p_varII)) {
    for (i in 1:ncol(p_varII[[j]])) {
      L_par[[j]][[i]] <- by(data = L_all[[j]],
                            INDICES = p_varII[[j]][,i],
                            FUN = as.matrix)$`1`
    }
  }

  #----------------------------------------------------------------

  ## Tabela

  tabela <- list()

  for (j in 1:n_resp) {

    W <- vector() # Vetor para a estat??stica de teste
    gl <- vector() # Vetor para graus de liberdade
    p_val <- vector() # Vetor para p-valor

    for (i in 1:length(L_par[[j]])) {
      W[i] <- as.numeric(
        (t(L_par[[j]][[i]] %*% subset(beta,
                                      beta$Response == j)$Estimates))%*%
          (solve(L_par[[j]][[i]]%*%
                   vcov_betas[[j]]%*%
                   t(L_par[[j]][[i]]))) %*%
          (L_par[[j]][[i]] %*%
             subset(beta, beta$Response == j)$Estimates))
      gl[i] <- nrow(L_par[[j]][[i]])
      p_val[i] <- pchisq(W[i], df = gl[i], lower.tail = FALSE)

    }
    tabela[[j]] <-
      data.frame(Covariate = c("Intercept",
                               attr(terms(object$linear_pred[[j]]),
                                    "term.labels")),
                 Df = gl,
                 Chi = round(W, 4),
                 'Pr(>Chi)' = round(p_val, 4),
                 check.names = F)
  }

  #----------------------------------------------------------------
  
  if (verbose == TRUE) {
    cat("ANOVA type I using Wald statistic for fixed effects\n\n")
    for (i in 1:n_resp) {
      cat("Call: ")
      print(object$linear_pred[[i]])
      cat("\n")
      print(tabela[[i]])
      cat("\n")
    }
    
    return(invisible(tabela))
    
  } else {
    return(tabela)
  }
  
}
