differentialEvolution =function(f, l_obs, dim, F, crossp, pop, ub_iter){
  x_best = pop[1,]
  result <- list(x_opt = c(), f_opt = c(), x_hist = c(), f_hist = c())
  for (j in 1:ub_iter) {
    
    for (current in 1:l_obs) {
      
      #losuje trzy liczby
      n_a <- ceiling(l_obs*rand())
      repeat {
        n_b <-  ceiling(l_obs*rand())
        if (n_b != n_a) {break}
      }
      repeat {
        n_c <-  ceiling(l_obs*rand())
        if (n_c != n_a && n_c != n_b) {break}
      }
      
      a <- pop[n_a,]
      b <- pop[n_b,]
      c <- pop[n_c,]
      
      ######MUTATION
      v <- a + F*(b-c)
      
      
      ######CROSSOVER
      crosspoints <- rand(dim, 1) <crossp
      test <- pop[current,]
      for (i in 1:dim) {
        if (crosspoints[i]) {
          test[i] <- v[i]
        }
      }
      
      ######SELECTION
      #zastapienie wektora jezeli jest lepszy (wartosc f celu nizsza)
      if (f(pop[current,])>f(test)) {
        pop[current,] <- test
      }
      
      if (f(pop[current,])<f(x_best)) {
        x_best <- pop[current,]
      }
    } #koniec iteracji po osobnikach
    result$x_opt  <- x_best
    result$f_opt  <- f(x_best)
    result$x_hist <- rbind(result$x_hist, x_best)
    result$f_hist <- rbind(result$f_hist, f(x_best))
    
  } ##koniec iteracji 
  
  return(result)      
}