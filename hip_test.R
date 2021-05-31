# Erro Tipo 1 - Rejeitar H0 quando H0 é verdadeira
# Erro Tipo 2 - Aceitar H0 quando H0 é falso
# TRUE - Aceito h1
# FALSE - Rejeito h1

# -1 -> <
#  0 -> !=
#  1 -> >


# Hypothesis test for average

## Known population std

hp_media_n <- function(h1,media,media_a,sd,n, alpha){
  z0 <- (media_a - media)/sqrt(sd/n)
  if(h1==-1){
    z_a2 <- -qnorm(alpha)
    if(z0 < - z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    z_a2 <- -qnorm(alpha/2)
    if(z0 < - z_a2 | z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    z_a2 <- -qnorm(alpha)
    if(z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(FALSE)
}


## Unknown population std

hp_media_n <- function(h1,media,media_a,sd,n,alpha){
  t0 <- (media_a - media)/sqrt(sd/n)
  grau <- n - 1
  if(h1==-1){
    t_a2 <- qt(1-alpha, grau)
    if(t0 < - t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    t_a2 <- qt(1-alpha/2, grau)
    if(t0 < - t_a2 | z0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    t_a2 <- qt(1-alpha, grau)
    if(t0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(FALSE)
}


# Hypothesis test for variation

hp_prop <- function(h1,p,p_a,n, alpha){
  z0 <- (p_a - p)/sqrt((p*(1-p))/n)
  if(h1==-1){
    z_a2 <- -qnorm(alpha)
    if(z0 < - z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    z_a2 <- -qnorm(alpha/2)
    if(z0 < - z_a2 | z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    z_a2 <- -qnorm(alpha)
    if(z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}

# Hypothesis test for variation

hp_var <- function(h1,var, var_a,n,alpha){
  grau <- n - 1
  qsqr_sup <- qchisq(alpha,grau)
  qsqr_inf <- qchisq(1-alpha,grau)
  x0 <- var_a*var_a*grau/(var*var)
  if(h1==-1){
    if(x0 < qsqr_inf){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    if(x0 < qsqr_inf | x0 > qsqr_sup){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    if(x0 > qsqr_sup){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
}

# Averages comparison with population std

comp_avs_wd <- function(h1, alpha, a1, a2, n1, n2, sd){
  z0 <- (a1 - a2)/(sd*sqrt(1/n1 + 1/n2))
  if(h1==-1){
    z_a2 <- -qnorm(alpha)
    if(z0 < - z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    z_a2 <- -qnorm(alpha/2)
    if(z0 < - z_a2 | z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    z_a2 <- -qnorm(alpha)
    if(z0 > z_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(FALSE)
}


# Averages comparison without population std and
# samples stds are not equal

comp_avs_nd_ne <- function(h1, alpha, a1, a2, n1, n2, s1, s2){
  sp <- sqrt(((n1 - 1)*s1*s1 + (n2 - 1)*s2*s2)/(n1 + n2 - 2))
  t0 <- (a1 - a2)/(sp*sqrt(1/n1 + 1/n2))
  print(t0)
  grau <- n1 + n2 - 2
  print(grau)
  if(h1==-1){
    t_a2 <- qt(1-alpha, grau)
    if(t0 < - t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    t_a2 <- qt(1-alpha/2, grau)
    print(t_a2)
    if(t0 < - t_a2 | t0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    t_a2 <- qt(1-alpha, grau)
    if(t0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(FALSE)
}

# Averages comparison without population std and
# samples stds are equal

comp_avs_nd_e <- function(h1, alpha, a1, a2, n1, n2, s1, s2){
  t0 <- (a1 - a2)/(sqrt(s1*s1/n1 + s2*s2/n2))
  print(t0)
  grau <- n1 + n2 - 2
  
  if(h1==-1){
    t_a2 <- qt(1-alpha, grau)
    print(t_a2)
    if(t0 < - t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==0){
    t_a2 <- qt(1-alpha/2, grau)
    print(t_a2)
    if(t0 < - t_a2 | z0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  if(h1==1){
    t_a2 <- qt(1-alpha, grau)
    print(t_a2)
    if(t0 > t_a2){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  return(FALSE)
}


