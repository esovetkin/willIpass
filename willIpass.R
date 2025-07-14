arrangements <- function(N, K)
{
  res <- list()

  generate <- function(current, remaining, k) {
    if (k == 1) {
      res[[length(res) + 1]] <<- c(current, remaining)
      return()
    }
    for (i in 0:remaining) {
      generate(c(current, i), remaining - i, k - 1)
    }
  }

  generate(c(), N, K)
  do.call(rbind, res)
}


chi_generator <- function(maxscore = 15, scores = c(0,2,3,4,5), N = 30)
{
    K <- length(scores)-1
    res <- matrix(c(0, N, rep(0, K)), nrow=1)

    for (i in 1:N) {
        x <- arrangements(i,K)
        x <- cbind(N-rowSums(x),x)
        x <- cbind(colSums(scores * t(x)), x)
        idx <- x[,1] <= maxscore

        if (0 == sum(idx))
            break

        res <- rbind(res, x[idx,])
    }

    res[order(res[,1]),,drop=FALSE]
}


alpha <- function(p0, data, chi_table)
{
    Nquestions <- sum(chi_table[1,-1])

    x <- t(sapply(names(data), function(l) {
        chi <- chi_table[chi_table[,1] == as.numeric(l), 2:ncol(chi_table), drop=FALSE]

        if (1 == nrow(chi)) {
            omega <- 1
        } else {
            omega <- apply(chi, 1, dmultinom, size=Nquestions, prob=p0)
            omega <- omega / sum(omega)
        }

        colSums(omega * chi)
    }))
    colSums(x * as.numeric(data))
}


Q_step <- function(p, alpha_values, epsilon = 1e-5) {
    idx <- alpha_values > epsilon
    sum(alpha_values[idx] * log(p[idx]))
}


EM_steps <- function(data, chi_table, maxiter = 100, mindelta = 1e-1)
{
    p0 <- rep(1,ncol(chi_table)-1) / (ncol(chi_table)-1)

    alpha_values <- alpha(p0, data, chi_table)

    prev <- Q_step(p0, alpha_values)

    for (i in 1:maxiter) {
        p0 <- alpha_values / sum(alpha_values)
        curr <- Q_step(p0, alpha_values)

        if (abs(prev-curr) < mindelta)
            break

        prev <- curr
        alpha_values <- alpha(p0, data, chi_table)
    }

    p0
}


do_once <- function(i, counts, chi, maxpass, possible_scores, Nquestions)
{
    if (1 == length(counts))
        data <- table(rep(as.numeric(names(counts)), sum(counts)))
    else
        data <- table(sample(as.numeric(names(counts)), size=sum(counts), replace=TRUE, prob=counts))
    p <- EM_steps(data, chi)

    idx <- chi[,1] <= 10
    x <- aggregate(apply(chi[idx, -1], 1, dmultinom, size=Nquestions, prob=p),
                   by=list(points = chi[idx ,1]),
                   FUN=sum)
    res <- x[,2]
    names(res) <- x[,1]
    res
}


points_cdf <- function(counts, possible_scores = c(0,2,3,4,5), Nquestions = 30, maxpass = 10, nsim=1e3, alpha = 0.05)
{
    require(parallel, quietly=TRUE)
    chi <- chi_generator(max(as.numeric(names(counts)), maxpass), possible_scores, Nquestions)

    res <- mclapply(1:nsim, FUN=do_once,
                    counts = counts,
                    chi = chi,
                    maxpass = maxpass,
                    possible_scores = possible_scores,
                    Nquestions = Nquestions,
                    mc.cores = detectCores())

    # compute CI of a CDF
    res <- simplify2array(res)
    res <- res[order(as.numeric(rownames(res))),,drop=FALSE]
    res <- apply(res,2,cumsum)
    res <- t(apply(res,1,quantile, probs=c(alpha/2,0.5,1-alpha/2)))
    res <- as.data.frame(res)
    colnames(res) <- c("lower","median","upper")
    res$x <- as.numeric(rownames(res))
    res
}

###

require(ggplot2)
require(scales)

expand_around_one <- trans_new(
  name = "expand1",
  transform = function(x) sign(x - 1) * sqrt(abs(x - 1)) + 1,
  inverse   = function(y) sign(y - 1) * (abs(y - 1))^2 + 1,
  domain    = c(0, Inf),
  breaks = function(x) {
    t <- function(x) sign(x - 1) * sqrt(abs(x - 1)) + 1
    inv <- function(y) sign(y - 1) * (abs(y - 1))^2 + 1
    tx <- range(t(x), na.rm = TRUE)
    tb <- pretty(tx, n = 20)
    inv(tb)
  }
)


png("cdf.png", width=2000, height=1700, res=300)
d <- points_cdf(table(c(3,0,7,3,2,0)))
print(round(d["10",],2))
print(round(points_cdf(table(c(7,5,8,9,5,6)))["10",],2))
ggplot(d, aes(ymin=lower,ymax=upper,y=median,x=x)) +
    geom_line() +
    geom_errorbar(lty=2) +
    scale_y_continuous(trans=expand_around_one) +
    scale_x_continuous(breaks=unique(d$x)) +
    xlab("points") + ylab("cdf")
dev.off()


## prüfung simulation points
scores <- c(3,0,7,3,9,0,6,0,5,0,2,3,0,0,3,0,0,3,0,0,2,9,6,0,0,0,0,3,0,0,8,0,0,2,0,5,3)

pdf("willIpass.pdf", width=10, height=8)
d <- points_cdf(table(scores), nsim=1e4)
ggplot(d, aes(ymin=lower,ymax=upper,y=median,x=x)) +
    geom_line() +
    geom_errorbar(lty=2) +
    scale_y_continuous(trans=expand_around_one) +
    scale_x_continuous(breaks=unique(d$x)) +
    xlab("points") + ylab("cdf")

K <- length(scores) %/% 3
res <- rbind(
    cbind(points_cdf(table(scores[1:K])), "attempt"=paste("first", K)),
    cbind(points_cdf(table(scores[(K+1):(2*K)])), "attempt"=paste("second", K)),
    cbind(points_cdf(table(scores[(length(scores)-K+1):length(scores)])), "attempt"=paste("last", K))
)
res[res$attempt == paste("second", K),"x"] <- res[res$attempt == paste("second", K),"x"] + 0.2
res[res$attempt == paste("last", K),"x"] <- res[res$attempt == paste("last", K),"x"] + 0.4
ggplot(res, aes(ymin=lower,ymax=upper,y=median, x=x, color=attempt)) +
    geom_line() +
    geom_errorbar(lty=2) +
    scale_y_continuous(trans=expand_around_one) +
    scale_x_continuous(breaks=unique(res[res$attempt == paste("first", K),"x"])) +
    xlab("points") + ylab("cdf")
dev.off()
