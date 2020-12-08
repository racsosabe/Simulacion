n = 300
u = runif(n, 0, 1)
g_u = exp(u)

nn = 1 : n
uu = cumsum(g_u) / nn
plot(nn, uu, type = "l")
abline(h = 0.5)
