generateFunctionForPerson <- function(
  name
) {
  
  #k <- getPossibleGroups(name)
  k <- c(1, 2, 3, 4, 5)
  
  k_fix <- 5
  x_fix <- k_fix/2
  R <- (2*(runif(1)>=0.5)-1)*floor(runif(1,1,6))+(x_fix-floor(x_fix))
  
  response <- as.vector(replicate(k_fix,sample(k,length(k),replace = FALSE)))[1:k_fix]
  reg1 <- seq(from = R-x_fix, to = R+x_fix-1)
  reg2 <- reg1**2
  reg3 <- reg1**3
  reg4 <- reg1**4
  df = data.frame(
    response, reg1, reg2, reg3, reg4
  )
  
  coef <- round(unname(lm(response ~ reg1 + reg2 + reg3 + reg4, data = df)$coefficients), 4)
  
  return(list(
    k = k,
    b0 = coef[1],
    b1 = coef[2],
    b2 = coef[3],
    b3 = coef[4],
    b4 = coef[5],
    R = R,
    x = x_fix
  ))
  
}


evaluateFunctionForPerson <- function(
  gffp,
  N
) {
  
  k = gffp$k
  
  getSinPart <- function(N, x, R) {
    return(
      floor(R+x*sin(9*N))
    )
  }
  
  category = round(
    gffp$b0+
      gffp$b1*(getSinPart(N, gffp$x, gffp$R))+
      gffp$b2*(getSinPart(N, gffp$x, gffp$R)**2)+
      gffp$b3*(getSinPart(N, gffp$x, gffp$R)**3)+
      gffp$b4*(getSinPart(N, gffp$x, gffp$R)**4)
  )
  
  # check if category in k, else raise error
  
  return(category)
}

generatePlot <- function(formula = TRUE, gffp = NULL, SKUPINA = 0, ) {
  return(
    if (formula) {
      generateFormulaPlot(gffp)
    } else {
      generateSkupinaPlot(SKUPINA)
    }
  )
}

generateSkupinaPlot <- function(SKUPINA) {
  return(
    text(
      x=0.5, y=0.5,
      cex = 8, family="mono", col = "Blue", font = 2,
      labels = paste0("TYM CISLO: \n", SKUPINA)
  ))
}

generateFormulaPlot <- function(gffp) {
  
  library(gtools)
 
  
  convToString <- function(
    X, 
    digits_bef = 5,
    digits_after = 4,
    plus_sign = TRUE
  ) {
    
    fx <- floor(X)
    rx <- round(X-floor(X),digits_after)
    rx2 <- 10**(nchar(as.character(rx))-2)*rx
    
    output = ""
    output = paste0(output, paste(replicate(digits_bef-nchar(as.character(abs(fx))), " "), collapse = ""))
    output = paste0(output, 
      if (X>=0) {
        if (plus_sign) {
          "+"
        } else {
          " "
        }
      } else {
        "-"
      }
    )
      
    output = paste0(output, abs(fx))
    if (rx>0) {
      output = paste0(output, ".", rx2)
    } else {
      output = paste0(output, "  ")
    }
    output = paste0(output, paste(replicate(digits_after-nchar(as.character(rx2)), " "), collapse = ""))
    
  }
  
  x <-  convToString(gffp$x, 1,1, plus_sign = FALSE)
  R <-  convToString(gffp$R, 1,1, plus_sign = FALSE)
  b0 <- gffp$b0
  b1 <- convToString(gffp$b1)
  b2 <- convToString(gffp$b2)
  b3 <- convToString(gffp$b3)
  b4 <- convToString(gffp$b4)

  return(
    text(
      x=0.5, y=0.5,
      cex = 4, family="mono",
      labels = paste0(
        "TYM = ", 
        if(gffp$b0!=0){paste0(b0)} else {""},
        if((abs(gffp$b1)+abs(gffp$b2)+abs(gffp$b3)+abs(gffp$b4))>0) {paste0(" +")} else {""},
        "\n",
        if(gffp$b1!=0){paste0(b1, "[", R, " +", x, chr(183), "sin(9N)]   \n")} else {""},
        if(gffp$b2!=0){paste0(b2, "[", R, " +", x, chr(183), "sin(9N)]^2 \n")} else {""},
        if(gffp$b3!=0){paste0(b3, "[", R, " +", x, chr(183), "sin(9N)]^3 \n")} else {""},
        if(gffp$b4!=0){paste0(b4, "[", R, " +", x, chr(183), "sin(9N)]^4 \n")} else {""},
        "\n [X] je dolni cela cast cisla"
    ))
  )

}

