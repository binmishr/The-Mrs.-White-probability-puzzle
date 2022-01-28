# The-Mrs.-White-probability-puzzle

The details of the codeset and plots are included in the attached Microsoft Word Document (.docx) file in this repository. 
You need to view the file in "Read Mode" to see the contents properly after downloading the same.

Clue_Functions.R
=================
    #' Discrete uniform distribution
    #' @param x 
    #' @param min
    #' @param max 
    dunifdisc <- function(x, min=0, max=1){
      ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
    }

    #' Random sample from discrete uniform distribution
    #' @param n Number of draws
    #' @param k Number of possible (equiprobable) outcomes
    runifdisc <-function(n,k){
      sample(1:k,n,replace=T)
    } 


    #' Plot prior for the number of games
    plot_clue_prior <- function(x, dprior) {
      plot_prior <- ggplot(tibble(x=x, prior=dprior), aes(x=x, y=prior)) +
        geom_line(colour="#007BA7") +
        geom_area(fill="#7DF9FF") +
        # scale_x_log10(breaks=c(1,10,25,50,100,200), limits=c(1,200)) +
        scale_x_continuous(breaks=c(3,10,50,100,150,200), limits=c(1,200)) +
        xlab("Number of games") +
        ylab("Prior probability")
      print(plot_prior)
    }

    #' Plot posterior for the number of games
    plot_clue_posterior <- function(x, dposterior) {
      plot_prior <- ggplot(tibble(x=x, prior=dposterior), aes(x=x, y=dposterior)) +
        geom_line(colour="#FF0000") +
        geom_area(fill="#DD0000") +
        scale_x_log10(breaks=c(1,10,25,50,100,200), limits=c(1,200)) +
        xlab("Number of games") +
        ylab("Posterior probability")
      print(plot_prior)
    }


    #' Plot prior for the number of games
    plot_clue_prior2000 <- function(x, dprior) {
      plot_prior <- ggplot(tibble(x=x, prior=dprior), aes(x=x, y=prior)) +
        geom_line(colour="#007BA7") +
        geom_area(fill="#7DF9FF") +
        scale_x_log10(breaks=c(1,10,100,500,1000,2000), limits=c(1,2000)) +
        xlab("Number of games") +
        ylab("Prior probability")
      print(plot_prior)
    }

    #' Plot posterior for the number of games
    plot_clue_posterior2000 <- function(x, dposterior) {
      plot_prior <- ggplot(tibble(x=x, prior=dposterior), aes(x=x, y=dposterior)) +
        geom_line(colour="#FF0000") +
        geom_area(fill="#DD0000") +
        scale_x_log10(breaks=c(1,10,100,500,1000,2000), limits=c(1,2000)) +
        xlab("Number of games") +
        ylab("Posterior probability")
      print(plot_prior)
    }
    
Clue_Main.R
==============
    library(tidyverse)

    ## Parameters
    k_mrs_white <- 2 # Number of times Mrs. White was the murderer
    prob <- 1/6 # Probability of Mrs. White being the murderer for one game
    x <- 1:200 # Reduction of the problem to a finite number of games

    ## Likelihood
    dlikelihood <- dbinom(k_mrs_white, x, prob)

    ## Priors

    # Uniformative prior
    # dprior1 <- dunifdisc(x,10,2000)
    dprior1 <- dunifdisc(x,3,100)
    plot_clue_prior(x, dprior1)


    ## First interval

    # dposterior2 <- dlikelihood * dprior2
    dposterior1 <- dlikelihood * dprior1
    dposterior1 <- dposterior1 / sum(dposterior1)
    plot_clue_posterior(x, dposterior1)

    which.max(dposterior1)

    threshold_val <- 0.975
    which(cumsum(dposterior1) > (threshold_val))[1]

    cumsum(dposterior1)[max_prior]

    dposterior2 <- dposterior2 / sum(dposterior2)
    plot_clue_posterior(x, dposterior2)

    which.max(dposterior2)

    threshold_val <- 0.95
    which(cumsum(dposterior2) > (threshold_val))[1]
    cumsum(dposterior2)[max_prior]

    ## Other Data generation mechanism

    # Simulation
    set.seed(9)
    N_sim_games <- 40
    sim_murderer <- runifdisc(N_sim_games, 6)

    plot_murderer <- ggplot(tibble(x=1:N_sim_games, y=sim_murderer), aes(y, stat(count))) +
      geom_histogram(aes(y =..count..),
                     bins=6, fill="white",colour="black") +
      ylab("Frequency - murderer") +
      xlab("Character #") +
      scale_x_continuous(breaks = 1:6)
    print(plot_murderer)



    gumbelclue_2 <- readRDS("clue/dcluegumbel_2.rds")
    gumbelclue_2 <- gumbelclue_2[x]

    dposterior_gen <- gumbelclue_2 * dprior1
    dposterior_gen <- dposterior_gen / sum(dposterior_gen)

    plot_clue_posterior(x, dposterior_gen)



    dposterior_gen <- gumbelclue_2 * dprior2
    # dposterior_gen <- gumbelclue_2 * dprior1
    dposterior_gen <- dposterior_gen / sum(dposterior_gen)

    plot_clue_posterior(x, dposterior_gen)

    which.max(dposterior_gen)

    threshold_val <- 0.975
    which(cumsum(dposterior_gen) > (threshold_val))[1]

    cumsum(dposterior_gen)[max_prior]

    ######## Other prior

    # Informative prior
    library(e1071)
    # prior_probs <- (1:1000)**4
    # prior_probs <- c(prior_probs, rev(prior_probs))

    max_prior <- 30
    prior_probs <- (1:max_prior)**4
    prior_probs <- c(prior_probs, rev(prior_probs), rep(0, 200-2*max_prior))

    dprior2 <- ddiscrete(x, prior_probs)
    plot_clue_prior(x, dprior2)

    dposterior2 <- gumbelclue_2 * dprior2
    dposterior2 <- dposterior2 / sum(dposterior2)

    plot_clue_posterior(x, dposterior2)

    which.max(dposterior2)

    threshold_val <- 0.975
    which(cumsum(dposterior2) > (threshold_val))[1]
    
gumbel_binomial.R
====================

    library(sampling)
    library(tidyverse)

    # n <- 24
    N <- 1e4

    # Warning: this code is very slow and inefficient

    min_sample_clue <- function(n) {
      sims_clue <- data.frame(nrow=n, ncol=6)

      test <- map(1:n, function(x) tibble(srswor(1, 6)) )
      test <- t(bind_cols(test))

      return(min(colSums(test)))
    }

    gumbel_clue <- function(n,N) {
      return(round(mean(map_dbl(rep(n,N), min_sample_clue)),0))
    }

    # param x vector values for which probability of min
    # should be returned
    dcluegumbel_onen <- function(x,n,N) {
      min_distr <- map_dbl(rep(n,N), min_sample_clue)
      min_distr <- table(min_distr) / N

      return_vec <- rep(0,length(x))

      for(k in 1:length(x)) {
        temp_val_distr <- min_distr[as.character(x[k])]
        if(!is.na(temp_val_distr)) {
          return_vec[k] <- temp_val_distr
        } else {
          return_vec[k] <- 0
        }

      }

      return(return_vec)
    }

    # param x numeric value for which probability of min
    # should be returned
    dcluegumbel_onen_onex <- function(x,n,N) {
      min_distr <- map_dbl(rep(n,N), min_sample_clue)
      min_distr <- table(min_distr) / N

      return_val <- min_distr[as.character(x)]
      return_val <- return_val[[1]]

      if(is.na(return_val)) {
        return(0)
      } else {
        return(return_val[[1]])
      }

      return(0)
    }


    # Outputs a matrix of values of likelihood for values
    # of n (in rows). in columns: prob of min having value k
    dcluegumbel_gentable <- function(kVec, nVec, N) {
      # TODO

      return_table <- matrix(0, nrow=length(nVec), ncol=length(kVec))

      for(k in 1:length(nVec)) {
        print(k)
        return_table[k, ] <- dcluegumbel_onen(kVec, nVec[k], N)
      }

      return_table <- as.tibble(return_table)
      names(return_table) <- as.character(kVec)

      return(return_table)
    }

    # testVec <- dcluegumbel(1:10, 60, 1000)
    testVec
    # 
    # testMat <- dcluegumbel_gentable(0:10, 0:2000, 1000)
    # testMat

    k_min <- 2

    testMat <- dcluegumbel_gentable(k_min, 0:2000, 1000)
    testMat

    testVec <- rep(0,80)

    for(k in 3:80) {
      print(k)
      val <- dcluegumbel_onen_onex(k_min, k, 1000)
      testVec[k] <- val
      print(val)
    }

    gumbelclue_2 <- c(testVec, rep(0, 1920))
