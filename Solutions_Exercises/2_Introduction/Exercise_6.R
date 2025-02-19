n_sim <- 1000

cor_vector <- numeric(n_sim)
for(i in 1:n_sim){
 # Generate X from a symmetric t-distribution (df=3 for example)
 n <- 100
 X <- rt(n, df=3)  
 Y <- X^2  
 cor_vector[i] <- cor(X, Y)
}

mean(cor_vector)
hist(cor_vector, breaks=50, 
     main="Histogram of correlation between X and Y", 
     xlab="Correlation", col="lightblue", border="black")
