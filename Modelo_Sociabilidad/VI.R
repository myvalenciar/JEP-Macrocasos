# Bayesian Sociality Models: A Scalable and Flexible Alternative for Network Analysis
#
# Section 4.1

# Function to initialize variational parameters --------------------------------

initialize_variational_parameters <- function(n, a_sigma, b_sigma, a_tau, b_tau) {
     # Initial values
     tau2   <- jitter(rep(1, n))
     sigma2 <- jitter(1)
     delta  <- jitter(rep(0, n))
     mu     <- jitter(0)
     z      <- matrix(0, nrow = n, ncol = n)

     # Return
     list(
          mu_z         = z,
          mu_mu        = mu,
          sigma_mu2    = sigma2,
          mu_delta     = delta,
          sigma_delta2 = tau2,
          alpha_sigma  = a_sigma,
          beta_sigma   = b_sigma,
          alpha_tau    = a_tau,
          beta_tau     = b_tau
     )
}

# Function to compute the ELBO -------------------------------------------------

compute_elbo <- function(params, y, a_sigma, b_sigma, a_tau, b_tau) {
     # Sizes
     n <- nrow(y)
     
     # Variational parameters
     mu_z         <- params$mu_z
     mu_mu        <- params$mu_mu
     sigma_mu2    <- params$sigma_mu2
     mu_delta     <- params$mu_delta
     sigma_delta2 <- params$sigma_delta2
     alpha_sigma  <- params$alpha_sigma
     beta_sigma   <- params$beta_sigma
     alpha_tau    <- params$alpha_tau 
     beta_tau     <- params$beta_tau
     
     # Compute P
     P <- 0
     for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
               P <- P - 0.5*(sigma_mu2 + sigma_delta2[i] + sigma_delta2[j] + (mu_z[i, j] - mu_mu - mu_delta[i] - mu_delta[j])^2 + log(2*pi))
          }
     }
     P <- P - 0.5*((mu_mu^2 + sigma_mu2)*(alpha_sigma/beta_sigma) + digamma(alpha_sigma) - log(beta_sigma) + log(2*pi))
     P <- P - 0.5*sum((mu_delta^2 + sigma_delta2)*(alpha_tau/beta_tau) + digamma(alpha_tau) - log(beta_tau) + log(2*pi))
     P <- P + a_sigma*log(b_sigma) - lgamma(a_sigma) - (a_sigma + 1)*(digamma(alpha_sigma) - log(beta_sigma)) - b_sigma*(alpha_sigma/beta_sigma)
     P <- P + a_tau*log(b_tau) - lgamma(a_tau) - (a_tau + 1)*(digamma(alpha_tau) - log(beta_tau)) - b_tau*(alpha_tau/beta_tau)
     
     # Compute Q
     Q <- -0.5*log(2*pi) 
     Q <- Q - 0.5*(log(2*pi*sigma_mu2) + 1) - 0.5*n*(log(2*pi) + 1) - 0.5*sum(log(sigma_delta2))
     Q <- Q + alpha_sigma*log(beta_sigma) - lgamma(alpha_sigma) - (alpha_sigma + 1)*(digamma(alpha_sigma) - log(beta_sigma)) - alpha_sigma
     Q <- Q + alpha_tau*log(beta_tau) - lgamma(alpha_tau) - (alpha_tau + 1)*(digamma(alpha_tau) - log(beta_tau)) - alpha_tau
     for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
               x <- -mu_z[i, j]
               if (y[i, j] == 1) {
                    d <- pnorm(x, lower.tail = F)
                    if (d == 0) {
                         Q <- Q - 0.5 
                    } else {
                         a <- dnorm(x)
                         Q <- Q - 0.5*(1 + (x*a/d) - (a/d)^2)
                    }
                    Q <- Q - pnorm(x, lower.tail = F, log.p = T) 
               } else {
                    d <- pnorm(x, lower.tail = T)
                    if (d == 0) {
                         Q <- Q - 0.5 
                    } else {
                         a <- dnorm(x)
                         Q <- Q - 0.5*(1 + (x*a/d) - (a/d)^2)
                    }
                    Q <- Q - pnorm(x, lower.tail = T, log.p = T)
               }
          }
     }
     
     # Return ELBO
     return(P - Q)
}

# VI algorithm -----------------------------------------------------------------

vi_sociality <- function(y, a_sigma, b_sigma, a_tau, b_tau, global_bound, epsilon, max_iter) {
     # Sizes
     n <- nrow(y)

     # Initialize variational parameters
     params <- initialize_variational_parameters(n, a_sigma, b_sigma, a_tau, b_tau)
     
     # Variational parameters
     mu_z         <- params$mu_z
     mu_mu        <- params$mu_mu
     sigma_mu2    <- params$sigma_mu2
     mu_delta     <- params$mu_delta
     sigma_delta2 <- params$sigma_delta2
     alpha_sigma  <- params$alpha_sigma
     beta_sigma   <- params$beta_sigma
     alpha_tau    <- params$alpha_tau 
     beta_tau     <- params$beta_tau
     
     # VI algorithm
     elbo_old <- -Inf
     
     for (iter in 1:max_iter) {
          # Update variational parameters for z_{i,j}
          for (i in 1:(n - 1)) {
               for (j in (i + 1):n) {
                    m_zij <- mu_mu + mu_delta[i] + mu_delta[j]
                    if (y[i, j] == 1) {
                         E_zij <- truncnorm::etruncnorm(a = 0, b = Inf,  mean = m_zij, sd = 1)
                    } else {
                         E_zij <- truncnorm::etruncnorm(a = -Inf, b = 0, mean = m_zij, sd = 1)
                    }
                    # Apply bounding
                    mu_z[i, j] <- min(global_bound, max(-global_bound, E_zij))
                    mu_z[j, i] <- mu_z[i, j]
               }
          }

          # Update variational parameters for mu
          sigma_mu2 <- 1/(alpha_sigma/beta_sigma + n*(n-1)/2)
          sum_z_mu_delta <- 0
          for (i in 1:(n - 1)) {
               for (j in (i + 1):n) {
                    sum_z_mu_delta <- sum_z_mu_delta + (mu_z[i, j] - mu_delta[i] - mu_delta[j])
               }
          }
          mu_mu <- sigma_mu2 * sum_z_mu_delta

          # Update variational parameters for delta_i
          for (i in 1:n) {
               sigma_delta2[i] <- 1/(alpha_tau/beta_tau + n - 1)
               sum_z_mu_delta_i <- 0
               for (j in 1:n) {
                    if (i != j) {
                         sum_z_mu_delta_i <- sum_z_mu_delta_i + (mu_z[i, j] - mu_mu - mu_delta[j])
                    }
               }
               mu_delta[i] <- sigma_delta2[i] * sum_z_mu_delta_i
          }
          mu_delta <- mu_delta - mean(mu_delta)
          
          # Update variational parameters for sigma^2
          alpha_sigma <- a_sigma + 0.5
          beta_sigma  <- b_sigma + 0.5*(mu_mu^2 + sigma_mu2)
          
          # Update variational parameters for tau^2
          alpha_tau <- a_tau + 0.5*n
          beta_tau  <- b_tau + 0.5*sum(mu_delta^2 + sigma_delta2)
          
          # Compute ELBO
          elbo_new <- compute_elbo(params, y, a_sigma, b_sigma, a_tau, b_tau)
          
          # Store variational parameters
          params$mu_z         <- mu_z
          params$mu_mu        <- mu_mu
          params$sigma_mu2    <- sigma_mu2
          params$mu_delta     <- mu_delta
          params$sigma_delta2 <- sigma_delta2
          params$alpha_sigma  <- alpha_sigma
          params$beta_sigma   <- beta_sigma
          params$alpha_tau    <- alpha_tau 
          params$beta_tau     <- beta_tau
          params$elbo         <- elbo_new

          # Check for convergence
          if (abs(elbo_new - elbo_old) < epsilon) {
               cat("Converged at iteration ", iter, "\n",
                   "Current ELBO = ", elbo_new, "\n",
                   "Delta ELBO = ", abs(elbo_new - elbo_old), "\n", sep = "")
               break
          }
          
          elbo_old <- elbo_new
     }
     
     return(params)
}
# End --------------------------------------------------------------------------