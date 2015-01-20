baseline.fit.function <- function(data, epsilon) {
    glm.fit <- glm(
        LaterOptionChosen ~ 1,
        data=data,
        family=binomial(link="logit")
    )
    return(glm.fit)
}

baseline.test.function <- function(data, theta, epsilon, err) {
    p <- inv.logit(predict(theta, newdata=data))
    return(err(p, data$LaterOptionChosen))
}

ITCH.fit.function <- function(data, epsilon) {
    glm.fit <- glm(
        LaterOptionChosen ~ G + R + D + T,
        data=data,
        family=binomial(link="logit")
    )
    return(glm.fit)
}

ITCH.test.function <- function(data, theta, epsilon, err) {
    p <- inv.logit(predict(theta, newdata=data))
    return(err(p, data$LaterOptionChosen))
}

DRIFT.fit.function <- function(data, epsilon) {
    glm.fit <- glm(
        LaterOptionChosen ~ DriftD + DriftR + DriftI + DriftT,
        data=data,
        family=binomial(link="logit")
    )
    return(glm.fit)
}

DRIFT.test.function <- function(data, theta, epsilon, err) {
    p <- inv.logit(predict(theta, newdata=data))
    return(err(p, data$LaterOptionChosen))
}

cnv <- function(x, g) {log(1 + g * x) / g}

tradeoff.log.likelihood.function <- function(data, theta, epsilon) {
    a <- theta[1]
    k <- theta[2]
    g.x <- theta[3]
    g.t <- theta[4]

    z <- with(
        data,
        (cnv(X2, g.x) - cnv(X1, g.x)) - k * (cnv(T2, g.t) - cnv(T1, g.t))
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

tradeoff.fit.function <- function(data, epsilon) {
    # a in [0.0001, 10.00]
    # k in [0.0000001, 10000000.00]
    # g.x in [0.0000001, 10000000.00]
    # g.t in [0.0000001, 10000000.00]
    f <- function (theta) {
        tradeoff.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.001, 1.0, 1.0, 1.0),
        f,
        method="L-BFGS-B",
        lower=c(0.0001, 0.0000001, 0.0000001, 0.0000001),
        upper=c(10.0, 10000000.00, 10000000.00, 10000000.00),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

tradeoff.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    k <- theta[2]
    g.x <- theta[3]
    g.t <- theta[4]

    z <- with(
        data,
        (cnv(X2, g.x) - cnv(X1, g.x)) - k * (cnv(T2, g.t) - cnv(T1, g.t))
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    return(err(p, data$LaterOptionChosen))
}

exponential.log.likelihood.function <- function(data, theta, epsilon) {
    a <- theta[1]
    delta <- theta[2]

    z <- with(data, X2 * delta^T2 - X1 * delta^T1)
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

exponential.fit.function <- function(data, epsilon) {
    # a in [0.0001, 500.00]
    # delta in [0.01, 0.99]
    f <- function (theta) {
        exponential.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(1.0, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.0000001, 0.01),
        upper=c(500.00, 0.99),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

exponential.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    delta <- theta[2]

    z <- with(data, X2 * delta^T2 - X1 * delta^T1)
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    return(err(p, data$LaterOptionChosen))
}

hyperbolic.log.likelihood.function <- function(data, theta, epsilon) {
    a <- theta[1]
    k <- theta[2]

    z <- with(data, X2 * (1.0 / (1.0 + k * T2)) - X1 * (1.0 / (1.0 + k * T1)))
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

hyperbolic.fit.function <- function(data, epsilon) {
    # a in [0.0001, 500.00]
    # k in [0.0001, 100.00]
    f <- function (theta) {
        hyperbolic.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.001, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.0000001, 0.0001),
        upper=c(500.00, 100.00),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

hyperbolic.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    k <- theta[2]

    z <- with(data, X2 * (1.0 / (1.0 + k * T2)) - X1 * (1.0 / (1.0 + k * T1)))
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    return(err(p, data$LaterOptionChosen))
}

quasihyperbolic.log.likelihood.function <- function(data, theta, epsilon) {
    a <- theta[1]
    beta <- theta[2]
    delta <- theta[3]

    z <- with(
        data,
        X2 * beta^(T2 > 0) * delta^T2 - X1 * beta^(T1 > 0) * delta^T1
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

quasihyperbolic.fit.function <- function(data, epsilon) {
    # a in [0.0001, 500.00]
    # beta in [0.01, 0.99]
    # delta in [0.01, 0.99]
    f <- function (theta) {
        quasihyperbolic.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(1.0, 0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.0000001, 0.01, 0.01),
        upper=c(500.00, 0.99, 0.99),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

quasihyperbolic.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    beta <- theta[2]
    delta <- theta[3]

    z <- with(
        data,
        X2 * beta^(T2 > 0) * delta^T2 - X1 * beta^(T1 > 0) * delta^T1
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    return(err(p, data$LaterOptionChosen))
}

generalized.hyperbolic.log.likelihood.function <- function(
    data,
    theta,
    epsilon
) {
    a <- theta[1]
    beta <- theta[2]
    alpha <- theta[3]

    z <- with(
        data,
        X2 * (1.0 + alpha * T2)^(-beta / alpha) -
        X1 * (1.0 + alpha * T1)^(-beta / alpha)
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

generalized.hyperbolic.fit.function <- function(data, epsilon)
{
    # a in [0.0001, 500.00]
    # beta in [0.0001, 10.00]
    # alpha in [0.0001, 10.00]
    f <- function (theta) {
        generalized.hyperbolic.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.001, 0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.0000001, 0.0001, 0.0001),
        upper=c(500.00, 100.00, 100.00),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

generalized.hyperbolic.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    beta <- theta[2]
    alpha <- theta[3]

    z <- with(
        data,
        X2 * (1.0 + alpha * T2)^(-beta / alpha) -
        X1 * (1.0 + alpha * T1)^(-beta / alpha)
    )
    p <- inv.logit(a * z)
    p <- epsilon * 0.5 + (1 - epsilon) * p

    return(err(p, data$LaterOptionChosen))
}

homothetic.exponential.log.likelihood.function <- function(
    data,
    theta,
    epsilon
) {
    a <- theta[1]
    delta <- theta[2]

    z <- with(data, log((X2 * delta^T2) / (X1 * delta^T1)))
    p <- inv.logit(a * z)

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

homothetic.exponential.fit.function <- function(data, epsilon) {
    # a in [0.01, 10.00]
    # delta in [0.01, 0.99]

    f <- function (theta) {
        homothetic.exponential.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.01, 0.01),
        upper=c(10.00, 0.99),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

homothetic.exponential.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    delta <- theta[2]

    z <- with(data, log((X2 * delta^T2) / (X1 * delta^T1)))
    p <- inv.logit(a * z)

    return(err(p, data$LaterOptionChosen))
}

homothetic.hyperbolic.log.likelihood.function <- function(
    data,
    theta,
    epsilon
) {
    a <- theta[1]
    k <- theta[2]

    z <- with(
        data,
        log((X2 * (1.0 / (1.0 + k * T2))) / (X1 * (1.0 / (1.0 + k * T1))))
    )
    p <- inv.logit(a * z)

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

homothetic.hyperbolic.fit.function <- function(data, epsilon) {
    # a in [0.01, 10.00]
    # k in [0.01, 10.00]

    f <- function (theta) {
        homothetic.hyperbolic.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.01, 0.01),
        upper=c(10.00, 10.00),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

homothetic.hyperbolic.test.function <- function(data, theta, epsilon, err) {
    a <- theta[1]
    k <- theta[2]

    z <- with(
        data,
        log((X2 * (1.0 / (1.0 + k * T2))) / (X1 * (1.0 / (1.0 + k * T1))))
    )
    p <- inv.logit(a * z)

    return(err(p, data$LaterOptionChosen))
}

homothetic.quasihyperbolic.log.likelihood.function <- function(
    data,
    theta,
    epsilon
) {
    a <- theta[1]
    beta <- theta[2]
    delta <- theta[3]

    z <- with(
        data,
        log((X2 * beta^(T2 > 0) * delta^T2) / (X1 * beta^(T1 > 0) * delta^T1))
    )
    p <- inv.logit(a * z)

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

homothetic.quasihyperbolic.fit.function <- function(data, epsilon) {
    # a in [0.01, 10.00]
    # beta in [0.01, 0.99]
    # delta in [0.01, 0.99]

    f <- function (theta) {
        homothetic.quasihyperbolic.log.likelihood.function(data, theta, epsilon)
    }

    results <- optim(
        c(0.5, 0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.01, 0.01, 0.01),
        upper=c(10.00, 0.99, 0.99),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

homothetic.quasihyperbolic.test.function <- function(
    data,
    theta,
    epsilon,
    err
) {
    a <- theta[1]
    beta <- theta[2]
    delta <- theta[3]

    z <- with(
        data,
        log((X2 * beta^(T2 > 0) * delta^T2) / (X1 * beta^(T1 > 0) * delta^T1))
    )
    p <- inv.logit(a * z)

    return(err(p, data$LaterOptionChosen))
}

homothetic.generalized.hyperbolic.log.likelihood.function <- function(
    data,
    theta,
    epsilon
) {
    a <- theta[1]
    alpha <- theta[2]
    beta <- theta[3]

    z <- with(
        data,
        log(
            (X2 * (1.0 / (1.0 + alpha * T2)^(beta / alpha))) /
            (X1 * (1.0 / (1.0 + alpha * T1)^(beta / alpha)))
        )
    )
    p <- inv.logit(a * z)

    l <- ifelse(data$LaterOptionChosen == 1, p, 1 - p)
    ll <- sum(log(l))

    return(ll)
}

homothetic.generalized.hyperbolic.fit.function <- function(data, epsilon) {
    # a in [0.01, 10.00]
    # alpha in [0.01, 10.00]
    # beta in [0.01, 10.00]

    f <- function (theta) {
        homothetic.generalized.hyperbolic.log.likelihood.function(
            data,
            theta,
            epsilon
        )
    }

    results <- optim(
        c(0.5, 0.5, 0.5),
        f,
        method="L-BFGS-B",
        lower=c(0.01, 0.01, 0.01),
        upper=c(10.00, 10.00, 10.00),
        control=list(fnscale=-1)
    )

    if (results$convergence != 0) {
        warning("Fitting function failed to converge")
    }

    return(results$par)
}

homothetic.generalized.hyperbolic.test.function <- function(
    data,
    theta,
    epsilon,
    err
) {
    a <- theta[1]
    alpha <- theta[2]
    beta <- theta[3]

    z <- with(
        data,
        log(
            (X2 * (1.0 / (1.0 + alpha * T2)^(beta / alpha))) /
            (X1 * (1.0 / (1.0 + alpha * T1)^(beta / alpha)))
        )
    )
    p <- inv.logit(a * z)

    return(err(p, data$LaterOptionChosen))
}
