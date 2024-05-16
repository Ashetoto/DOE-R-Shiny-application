rutgers.optimal.cat.blocks.random = function(n.fact=2,
                                             num_namelist=NULL,
                                             num_values=NULL,
                                             n.cat=0,
                                             cat_name=NULL,
                                             in.frml="",
                                             size=4,
                                             blocks=2,
                                             n.runs = 8,
                                             r.starts=4,
                                             iter=1000,
                                             random.blocks=FALSE,
                                             plot=TRUE,
                                             updateProgress = NULL) {
  
  
  start_time <- Sys.time() 
  
  default_value = function(input, val) {
    if(length(input) == 0){
      res=val
    } else{
      res=input
    }
    return (res)}
  
  # function to generate D-optimal designs for two-level factor systems
  my.factors = default_value(num_values, lapply(1:n.fact, function(x) c(-1,1)))
  # create a list of two-level factors, (-1, 1) for each factor specified by n.fact
  names(my.factors) <- default_value(num_namelist, lapply(1:n.fact, function(i) sprintf("X%i", i)))                                   

  
  # Augment to handle multiple blocks
  n = ifelse(blocks > 0, size * blocks, n.runs)   
  random = ifelse(random.blocks & blocks>1,TRUE,FALSE)
  
  
  # add categorical factors                                                      # if number of categorical factors given > 0, create and appends these factors to the list of factors
  if (n.cat > 0) {
    my.cat.factor = sprintf("L%02d",1:n.cat)                                     # create a list of factors
    my.factors = append(my.factors,list(my.cat.factor) )
    names(my.factors)[n.fact+1] <- default_value(cat_name, sprintf("X%i", n.fact+1)) 
  }
  
  
  # add block factor
  if (blocks>0) {
    my.block.factor = sprintf("Block%02d",1:blocks) # create a list of factors
    my.factors = append(my.factors,list(my.block.factor) )
    names(my.factors)[length(my.factors)] <- "Blocks"
  }
  
  #
  my.grid = expand.grid(my.factors) # create a grid of all unique combinations
  
  ## create the base model matrix--------------
  if (in.frml==""){
    if (blocks == 0){
      this.frml <- sprintf("~(%s)^%s", paste(names(my.factors)[1:length(my.factors)], collapse = "+"), length(my.factors))
    } else {
      this.frml <- sprintf("~(%s)^%s", paste(names(my.factors)[1:(length(my.factors)-1)], collapse = "+"), length(my.factors)-1)
      if (blocks>1 & !random) {
        this.frml = sprintf("%s+Blocks",this.frml)
      }
    }
  } else {
    this.frml <- in.frml
  }
  
  # I think method lacks a condition for scenarios where there is no blocking, resulting in the removal of an unnecessary factor.
  # this.frml = ifelse(in.frml=="",
  #                    sprintf("~(%s)^%s", paste(names(my.factors)[1:(length(my.factors)-1)], collapse = "+"), length(my.factors)),
  #                    in.frml)
  # if (blocks>1 & !random) {
  #   this.frml = sprintf("%s+Blocks",this.frml)
  # }

  options(contrasts = c("contr.XuWu", "XuWu.poly"))
  #
  Xbase = model.matrix(as.formula(this.frml), my.grid)
  #
  Vinv=diag(1)
  if (random) {
    xdat = cbind(data.frame(y=rnorm(n=NROW(my.grid))),my.grid)
    m.random = lme4::lmer(sprintf("y%s+(1|Blocks)",this.frml),data=xdat)
    Xbase = lme4::getME(m.random,"X") # this is probably not necessary
    V = diag(blocks*size)
    for (k2 in 1:blocks) {
      this.rc = (1:size)+(k2-1)*size
      V[this.rc,this.rc] = V[this.rc,this.rc] + 1
    }
    Vinv = solve(V)
  }
  # 
  
  # generate D-optimal Design
  detFinal = NA
  all.runs = 1:NROW(Xbase)
  final.runs = NA
  
  for (k in 1:r.starts) { # 20 random starts
    temp.runs = sample(all.runs, n, replace=TRUE)
    #
    if (blocks>1) {
      temp.runs = numeric(0)
      for (bi in 1:blocks) {
        bi1 = (NROW(Xbase)/blocks)*(bi-1) + 1
        bi2 = bi1 + (NROW(Xbase)/blocks)-1
        temp.runs = c(temp.runs,sample(all.runs[bi1:bi2],size,replace=T))
      }
    }
    #
    if (is.na(detFinal)) {
      final.runs <- temp.runs
      Xfinal = Xbase[final.runs,]
      if (!random) {
        
        detFinal = det(t(Xfinal) %*% Xfinal)
      } else {
        detFinal = det(t(Xfinal)%*% Vinv %*% Xfinal)
        
      }
    }
    
    for (i in 1:iter) { # 2000 iterations per start
      for (j in 1:n) {
        temp.runs[j] <- sample(all.runs, 1)
        #
        if (blocks>1) {
          bi1 = (NROW(Xbase)/blocks)*(ceiling(j/size)-1) + 1
          bi2 = bi1 + (NROW(Xbase)/blocks)-1
          temp.runs[j]  = sample(all.runs[bi1:bi2],1)
        }
        Xchallenger = Xbase[temp.runs,]
        if (!random) {
          detChallenger = det(t(Xchallenger) %*% Xchallenger)
          
        } else {
          detChallenger = det(t(Xchallenger)%*% Vinv %*% Xchallenger)
          
        }
        
        if (detChallenger > detFinal) {
          final.runs <- temp.runs
          Xfinal = Xbase[final.runs,]
          detFinal = det(t(Xfinal) %*% Xfinal)
          #
          if (random) {
            detFinal = det(t(Xfinal)%*% Vinv %*% Xfinal)
            
          }
          #
          
        }
      }
    }
    
    # Preset progress bar
    # If we were passed a progress update function, call it
    if (is.function(updateProgress)) {
      text <- paste0("already finished: ",round(k/r.starts*100,2), "%")
      updateProgress(detail = text)
    }
  }
  # calculate power
  options(contrasts=c("contr.sum", "contr.poly"))
  Xpower = model.matrix(as.formula(this.frml), my.grid)[final.runs,] # == Xfinal
  #
  if (random) {
    
    xdat = cbind(data.frame(y=rnorm(n=NROW(my.grid))),my.grid)
    m.random = lme4::lmer(sprintf("y%s+(1|Blocks)",this.frml),data=xdat)
    Xbase = lme4::getME(m.random,"X")
    Xpower = Xbase[final.runs,]
  }
  
  Beta = matrix(rep(c(-1,1), NCOL(Xpower))[1:NCOL(Xpower)], nrow=NCOL(Xpower), ncol=1)
  xpxinv = solve(t(Xpower) %*% Xpower)
  df = NROW(Xpower) - NCOL(Xpower)
  
  if (random) {
    xpxinv = solve(t(Xpower)%*% Vinv %*% Xpower)
    df = NROW(Xpower) - NCOL(Xpower)
    df = numeric(NCOL(xpxinv)) + df
    df[1] = blocks-1
  }
  
  
  Fcrit = qf(.95, df1=1, df2=df)
  coef.power = tibble(coef=colnames(Xpower), Beta=Beta, 
                      power=pf(Fcrit, df1=1, df2=df, ncp=1/diag(xpxinv), lower.tail = FALSE))
  
  
  ## FDS plot (Fraction of Design Space)----------------------------------
  
  # take a random sample of a large number of points (say 1,000 points) inside the experimental region
  continuouslength <- 101 # larger -> smoother
  factorrange = list()
  
  # numeric factors
  for (col in 1:n.fact) {
    factorrange[[colnames(my.grid)[col] ]] = seq(-1, 1, length.out = continuouslength)
  }
  
  # categorical factor
  if (n.cat > 0) {
    factorrange[[names(my.factors[n.fact+1])]] <- my.cat.factor
  }
  
  # fixed blocks
  if (blocks>1 & !random) {
    factorrange$Blocks <- my.block.factor
  }
  
  n_samples <- 1000
  
  fullgrid = expand.grid(factorrange)
  if (ncol(fullgrid) > 1) {
    samples = fullgrid[sample(1:nrow(fullgrid), n_samples, replace = TRUE), ]
  } else {
    samples = data.frame(fullgrid[sample(1:nrow(fullgrid), n_samples, replace = TRUE), ])
    colnames(samples) = colnames(fullgrid)
  }
  
  options(contrasts=c("contr.sum", "contr.poly"))
  samplemm = model.matrix(as.formula(this.frml), samples)
  
  # For each point, compute the relative variance of the predicted value
  v = list() # small v
  for (i in 1:nrow(samplemm)) {
    xi = samplemm[i, ]
    v[[i]] = t(xi) %*% xpxinv %*% xi 
  }
  vars = do.call(rbind, v) # variance of prediction
  
  # Then sort these values from smallest to largest
  varsordered = vars[order(vars)]
  
  midval = varsordered[n_samples/2] # Average variance of prediction
  maxyaxis = max(varsordered) + max(varsordered) / 20
  
  if(plot) {
    df <- data.frame(x = 1:length(varsordered) / length(varsordered), y = varsordered)
    fds <- ggplot(df, aes(x = x, y = y)) +
      geom_line(color = 4, linewidth = 1) +
      labs(x = "Fraction of Design Space", y = "Prediction Variance", title = "Fraction of Design Space Plot") +
      xlim(0, 1) + ylim(0, maxyaxis) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = 2, linewidth = 1) +
      geom_hline(yintercept = midval, linetype = "dashed", color = 2, linewidth = 1) +
      # geom_text(aes(x = 0.2, y = midval, label = "Avg Var Pred:"), color = 2, vjust = -3) +
      # geom_text(aes(x = 0.02, y = midval, label = round(midval,4)), color = 2, vjust = -1) +
      geom_text(aes(x = 0.15, y = midval, label = paste0("Avg Var Pred: ", round(midval,4))), color = 2, vjust = -1, size = 5) +
      theme_minimal() +
      theme(axis.title = element_text(size = 16), 
            plot.title = element_text(size = 20))
  } else {
  }
  
  
  ## Efficiency
  Deff = detFinal^(1/NCOL(Xfinal))/n
  Aeff = NCOL(Xfinal)/sum(diag(n*xpxinv)) # sum(diag()) == tr() in packages "psych"
  Geff = sqrt(NCOL(Xfinal)/n)/sqrt(max(vars))
  eff <- data.frame(name = c("D efficiency","A efficiency","G efficiency","Average variance of prediction"),
                    value = c(Deff, Aeff, Geff, midval))
  
  # Run List
  final_runs <- my.grid[final.runs,]
  run_list <- cbind(data.frame(Runs = 1:NROW(final_runs)),
                    final_runs)
  
  end_time <- Sys.time() 
  time <- difftime(end_time, start_time, units = "secs")[[1]]
  time <- round(time, 3)
  
  res = list(eff=eff, power=coef.power, matrix=Xfinal, finalRuns=run_list,
             n=n,blocks=blocks,size=size, fds=fds, time=time, inputFct=my.factors, frml=this.frml)

  return(res)
}


# # example 0 - No Blocks- work (cat>2)
 # rutgers.optimal.cat.blocks.random(n.fact=2,n.cat=3,
 #                                   in.frml="", blocks=0, n.runs = 20,
 #                                   r.starts=10,iter=1000,random.blocks=F)
# # example 1 - Random Blocks- work
 # rutgers.optimal.cat.blocks.random(n.fact=2,n.cat=2,
 #                                   in.frml="",size=6,blocks=3,
 #                                   r.starts=10,iter=1000,random.blocks=T)

# # example 2 - fixed blocks - work (blocks > 2)
 # rutgers.optimal.cat.blocks.random(n.fact=2,n.cat=2,
 #                                   in.frml="",size=6,blocks=3,
 #                                   r.starts=10,iter=1000,random.blocks=F)
