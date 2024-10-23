btm <- function(eta, eta0, psi1, psi2) {
  if (eta >= eta0) {
    if (psi1 == 0) return(eta0 + log(eta - eta0 + 1))
    return(eta0 + ((eta - eta0 + 1)^psi1 - 1) / psi1)
  } else {
    if (psi2 == 0) return(eta0 - log(-eta + eta0 + 1))
    return(eta0 - ((-eta + eta0 + 1)^psi2 - 1) / psi2)
  }
}


eta0 <- 0
eta_values <- seq(-5, 5, length.out = 100)
psi_values <- seq(-3, 3, by = 1)

colors <- rainbow(length(psi_values))

plot(eta_values, sapply(eta_values, btm, eta0 = eta0, psi1 = -3, psi2 = 1),
     type = "n", ylim = c(-5, 5), xlab = expression(eta), ylab = expression(h(eta)),
     main = expression("Both Tail Modification: Varying " * psi[1] * " and " * psi[2]),
     cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.1, lwd = 1, bty = "l")

for (i in 1:length(psi_values)) {
  psi1 <- psi_values[i]
  lines(eta_values, sapply(eta_values, btm, eta0 = eta0, psi1 = psi1, psi2 = 1), 
        col = colors[i], lty = 1, lwd = 1)
}

for (i in 1:length(psi_values)) {
  psi2 <- psi_values[i]
  lines(eta_values, sapply(eta_values, btm, eta0 = eta0, psi1 = 1, psi2 = psi2), 
        col = colors[i], lty = 2, lwd = 1)
}

legend("topleft", legend = c(expression("Varying " * psi[1]), expression("Varying " * psi[2])),
       col = "black", lty = c(1, 2), lwd = 1, box.lty = 0)

legend("bottomright", legend = paste("psi =", psi_values), col = colors, lty = 1, lwd = 1, box.lty = 0)
