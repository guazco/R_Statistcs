


# IC average with population std 

ic_media_n <- function(alpha, avg, sd, n) {
  
  z_a2 <- -qnorm(alpha/2)
  
  ic <- c(avg - z_a2*sd/sqrt(n), avg + z_a2*sd/sqrt(n))
  
  return(ic)
  
}

# IC average with sample std

ic_media_t <- function(alpha, avg, sd, n) {
  
  degree <- n - 1
  
  t_a2 <- qt(1-alpha/2, degree)
  
  ic <- c(avg - t_a2*sd/sqrt(n), avg + t_a2*sd/sqrt(n))
  
  return(ic)
}

# IC Variance

ic_var <- function(alpha, var, sd, n){
  
  alpha <- 0.01
  
  degree <- n - 1
  
  qsqr_sup <- qchisq(alpha,degree)
  
  qsqr_inf <- qchisq(1-alpha,degree)
  
  ic <- c(degree*sd*sd/(qsqr_inf),degree*sd*sd/(qsqr_sup))
  
  return(ic)
}

# IC Standard Deviation

ic_desv <- function(alpha, var, sd, n){
  
  degree <- n - 1
  
  qsqr_sup <- qchisq(alpha,degree)
  
  qsqr_inf <- qchisq(1-alpha,degree)
  
  ic <- c(sqrt(degree*sd*sd/(qsqr_inf)),sqrt(degree*sd*sd/(qsqr_sup)))
  
  return(ic)
  
}


# IC Proportion

ic_prop <- function(alpha, p, sd, n){
  
  z_a2 <- -qnorm(alpha/2)
  
  ic <- c(p - z_a2*sqrt(p*(1-p)/n), p + z_a2*sqrt(p*(1-p)/n))
  
  return(ic)
}


# IC Averages sums or differences with population std

ic_dif_med_p <- function(avg1,avg2,len1,len2,sd1,sd2, alpha){

  dif <- avg1 - avg2

  z_a2 <- -qnorm(alpha/2)

  exp1 <- z_a2*sqrt((sd1**2)/len1 + (sd2**2)/len2)

  ic1 <- c(dif - exp1, dif + exp1)
  
  return(ic1)

}

# IC Averages sums or differences with sample std

ic_dif_avg_a <- function(avg1,avg2,len1,len2,sd1,sd2, alpha){

  dif <- avg1 - avg2

  degree <- len1 + len2 - 2 

  t_a2 <- qt(1-alpha/2, degree)

  exp2 <- t_a2*sqrt((sd1**2)/len1 + (sd2**2)/len2)

  ic2 <- c(dif - exp2, dif + exp2)

  return(ic2)

}

# IC Proportions sums and differences

ic_dif_prop <- function(p1,p2,len1,len2,sd1,sd2,alpha){

  dif <- p1 - p2

  z_a2 <- -qnorm(alpha/2)

  exp <- z_a2*sqrt(p1*(1-p1)/len1 + p2*(1-p2)/len2)

  ic <- c(dif - exp, dif + exp)

  return(ic)

}

# IC Vaariances differences

ic_dif_var <- function(var1, var2, len1, len2, sd1, sd2, alpha){

  gl1 <- len1 - 1

  gl2 <- len2 - 1

  dif <- var1/var2

  f_s1 <- qf(1 - alpha/2, df1=gl1, df2=gl2)

  f_s2 <- qf(alpha/2, df1=gl1, df2=gl2) 

  ic <- c(dif/f_s1, dif/f_s2)

  return(ic)

}






















