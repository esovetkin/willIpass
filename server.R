require(ggplot2)
require(memoise)
require(scales)
require(shiny)


expand_around_one <- trans_new(
  name = "expand1",
  transform = function(x) sign(x - 1) * sqrt(abs(x - 1)) + 1,
  inverse   = function(y) sign(y - 1) * (abs(y - 1))^2 + 1,
  domain    = c(0, Inf),
  breaks = function(x) {
    t <- function(x) sign(x - 1) * sqrt(abs(x - 1)) + 1
    inv <- function(y) sign(y - 1) * (abs(y - 1))^2 + 1
    tx <- range(t(x), na.rm = TRUE)
    tb <- pretty(tx, n = 11)
    inv(tb)
  }
)


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
chi_generator <- memoise(chi_generator)


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

    idx <- chi[,1] <= maxpass
    x <- aggregate(apply(chi[idx, -1], 1, dmultinom, size=Nquestions, prob=p),
                   by=list(points = chi[idx ,1]),
                   FUN=sum)
    # probability of two fives means failure
    x[maxpass,"x"] <- x[maxpass,"x"] - dmultinom(c(Nquestions-2,0,0,0,2), prob=p)
    res <- x[,2]
    names(res) <- x[,1]
    res
}


points_cdf <- function(counts, possible_scores = c(0,2,3,4,5), Nquestions = 30, maxpass = 10, nsim=1e3, alpha = 0.05)
{
    require(parallel, quietly=TRUE)
    chi <- chi_generator(max(as.numeric(names(counts)), maxpass, 20), possible_scores, Nquestions)

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



server <- function(input, output, session) {

    observeEvent(input$numbers_from_cookie, {
        updateTextInput(session, "numbers", value = input$numbers_from_cookie)
    }, ignoreInit = TRUE)

    observeEvent(input$numbers, {
        session$sendCustomMessage("saveCookie", input$numbers %||% "")
    }, ignoreInit = FALSE)

    parse_ints <- function(s) {
        if (!nzchar(s)) {
            return(list(vals = integer(0), bad = character(0)))
        }
        toks <- unlist(strsplit(s, "[,; ]+"))
        toks <- toks[nzchar(toks)]
        vals <- as.integer(toks)
        is_bad <- is.na(vals) | vals < 0 | vals == 1 | vals > 40
        list(vals = vals[!is_bad], bad = toks[is_bad])
    }

    vec <- eventReactive(input$compute, {
        x <- parse_ints(input$numbers)
        x$probs <- points_cdf(table(x$vals), nsim=200, alpha=0.1)
        x
    }, ignoreInit = FALSE)

    output$parse_notes <- renderUI({
        pr <- vec()
        if (length(pr$bad) > 0) {
            HTML(paste0(
                "<span style='color:#b33;'>Nicht ganzzahlige Token wurde ignoriert: ",
                paste(htmltools::htmlEscape(unique(pr$bad)), collapse = ", "),
                "</span>"
            ))
        } else {
            HTML("<span style='color:#3a3;'>Alle Token werden als Ganzzahlen prozessiert.</span>")
        }
    })

    output$summary <- renderText({
        pr <- vec()
        x  <- pr$vals
        d <- pr$probs
        if (length(x) == 0) {
            return("Noch keine Ganzzahlen. Geben Sie Werte ein und klicken Sie auf 'Berechnen'")
        }
        
        paste0("Wahrscheinlichkeit, den Test nicht zu bestehen (%, 90%-Konfidenzintervall): [",
               paste(round(100*(1-d["10",c(3,1)]),2), collapse=", "),"]")
    })

    output$freq_plot <- renderPlot({
        pr <- vec()
        x  <- pr$vals
        validate(need(length(x) > 0, "Noch nichts zum Plotten. Geben Sie Ganzzahlen ein und klicken Sie auf 'Berechnen'"))

        d <- pr$probs
        ggplot(d, aes(ymin=lower,ymax=upper,y=median,x=x)) +
            geom_line() +
            geom_errorbar(lty=2) +
            scale_y_continuous(trans=expand_around_one) +
            scale_x_continuous(breaks=unique(d$x)) +
            xlab("Anzahl der Fehlerpunkte") + ylab("Wahrscheinlichkeit zu bestehen")
    })
}
