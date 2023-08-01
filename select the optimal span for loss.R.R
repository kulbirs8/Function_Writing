
  
  ### Prepare your function
  
  Write your own function to use LOO-CV and GCV to select the optimal span for loss. If you do not know where to start, you can follow the structure below to prepare your functions

```{r}
lo.lev <- function(x1, sp){
  # x1: feature vector of length n
  # sp: a value for "span"
  
  n = length(x1);
  dgnl <- diag(n)
  lev = rep(0, n)
  
  ##############################################
  # YOUR CODE: Compute the diagonal entries of the 
  #            smoother matrix S and 
  #            store it in a vector "lev"
  # Tip: check how we compute the smoother matrix
  #      for smoothing spline models
  ##############################################
  
  for(i in 1:n){
    y <- dgnl[,i]
    loe.fit <- loess(y~x1, span = sp,control = loess.control(surface = "direct"))
    y_hat <- fitted.values(loe.fit)
    lev[i] <- y_hat[i]
  }
  return(lev)
}

onestep_CV <- function(x1, y1, sp){
  
  ##############################################
  #  YOUR CODE: 
  #  1) Fit a loess model y1 ~ x1 with span = sp, and extract 
  #     the corresponding residual vector
  #  2) Call lo.lev to obtain the diagonal entries of S
  #  3) Compute LOO-CV and GCV using formula from lecture notes
  #    [lec_W5_NonlinearRegression.pdf] page 33. 
  ##############################################
  
  len <- length(x1)
  loe.fit <- loess(y1 ~ x1, span = sp,control = loess.control(surface = "direct"))
  res <- residuals(loe.fit)
  
  s_diag <- lo.lev(x1,sp)
  tr_hat <- sum(s_diag)
  sse <- sum(res^2)
  
  cv <- sum((res/(1 - s_diag))^2)/len
  gcv <- sse/(len * (1 - (tr_hat/len))^2)
  
  return(list(cv = cv, gcv = gcv))
}

myCV <- function(x1, y1, span){
  # x1: feature vector of length n
  # y1: response vector of length n
  # span: a sequence of values for "span"
  
  m = length(span)
  cv = rep(0, m)
  gcv = rep(0, m)
  
  for(i in 1:m){
    tmp = onestep_CV(x1, y1, span[i])
    cv[i] = tmp$cv
    gcv[i] = tmp$gcv
  }
  return(list(cv = cv, gcv = gcv))
}
```

### Test your function

Test your function with data "coding3_data.csv"

```{r}
mydata = read.csv(file = "Coding3_Data.csv")
dim(mydata)
```

```{r}
plot(mydata$x, mydata$y, xlab="", ylab="")
```

Create a grid of values for span: 15 values that are equally spaced between 0.20 and 0.90. Call your function myCV to compute the corresponding LOO-CV and GCV.

```{r}
span1 = seq(from = 0.2, by = 0.05, length = 15 )
cv.out = myCV(mydata$x, mydata$y, span1)
cbind(CV=cv.out$cv, GCV=cv.out$gcv)
```

### Print out your results

Print your results on LOO-CV and GCV. Both achieve their minimal at 0.5.

```{r}
myout = data.frame(CV = cv.out$cv, 
                   GCV = cv.out$gcv, 
                   span = span1)
myout
```


```{r}
myout$span[myout$GCV == min(myout$GCV)]
```

```{r}
myout$span[myout$CV == min(myout$CV)]
```

### Plot the fitted curve

Plot the data (red circles), the true curve (gray) and the fitted curve (blue dashed line) using the optimal span.

```{r}
spangcv.min = 0.5
plot(mydata$x, mydata$y, xlab="", ylab="", col="gray");
fx = 1:50/50;
fy = sin(12*(fx+0.2))/(fx+0.2)
lines(fx, fy, col=8, lwd=2);

f = loess(y ~ x, mydata, span = spangcv.min)
lines(fx, predict(f, data.frame(x = fx), surface = "direct"), 
      lty=2, lwd=2, col="blue")
```


