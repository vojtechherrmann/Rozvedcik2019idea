




k_fix <- 5
x_fix <- k_fix/2

# overeno
R <- (2*(runif(1)>=0.5)-1)*floor(runif(1,1,6))+(x_fix-floor(x_fix))

# overeno
user_N <- 1909
getSinPart(user_N, x_fix, R)


# k_fix is odd, R is Z.5 and the numbers are centered at Z - 
# in our case Z-2, Z-1, Z, Z+1, Z+2

# we create regression dataframe

# real nr of categories
k <- c(1,2,4)
response <- as.vector(replicate(k_fix,sample(1:k,k,replace = FALSE)))[1:5]
reg1 <- seq(from = R-2.5, to = R+1.5)
reg2 <- reg1**2
reg3 <- reg1**3
reg4 <- reg1**4
df = data.frame(
  response, reg1, reg2, reg3, reg4
)

coef <- round(unname(lm(response ~ reg1 + reg2 + reg3 + reg4, data = df)$coefficients), 4)

createFinalFormula <- function(x_fix, R, coef) {
  
  function(N) {
    coef[1]+
    coef[2]*1+
    #coef[3]*(getSinPart(N, x_fix, R)**2)+
    coef[4]*(2**3)+
    coef[5]*(3**4)
  }
}


a <- createFinalFormula(x_fix, R, coef)


print(df$response)

vysledek = numeric(10000)
for(i in 1:10000) {
  vysledek[i] = round(a(floor(runif(1,1,1000000))))
}
table(vysledek)











g <- function( index ){
  function( x ) x + index
}
index <- 3
f <- g( index )
f(4)
index<-20
f(4)
