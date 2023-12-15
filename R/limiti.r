limbinom<-function (x, n, conf.lim=0.95)
{
    alfa <- 1 - conf.lim
    p <- x/n
    liminf <- x/(x + (n - x + 1) * qf(alfa/2, 2 * (n - x + 1),
        2 * x, lower.tail = F))
    limsup <- ((x + 1) * qf(alfa/2, 2 * (x + 1), 2 * (n - x),
        lower.tail = F))/(n - x + (x + 1) * qf(alfa/2, 2 * (x +
        1), 2 * (n - x), lower.tail = F))
    limsup[x == n] <- 1
    liminf[x == 0] <- 0
    limsup[x == n - 1] <- (1 - alfa/2)^(1/n)# correzione diBlyth
    limsup[x == 0] <- 1 - (alfa/2)^(1/n) # correzione diBlyth
    cbind(x, n, p, liminf, limsup)
}
