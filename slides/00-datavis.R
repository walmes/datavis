#---- anscombe_table ---------------------------------------------------

library(xtable)

tb <- anscombe[, c(1, 5, 2, 6, 3, 7, 4, 8)]
names(tb) <- gsub("^([xy])([1-4])$", "$\\1_\\2$", names(tb))

xtb <- xtable(tb,
              caption = "Os 4 pares de variáveis do quarteto de Ancombe.",
              digits = c(0, 0, 2, 0, 2, 0, 2, 0, 2))
print.xtable(xtb,
             caption.placement = "top",
             include.rownames = FALSE,
             sanitize.colnames.function = identity)

#---- anscombe_regression ----------------------------------------------

fits <- with(anscombe,
             expr = {
                 list(
                     "1" = lm(y1 ~  x1),
                     "2" = lm(y2 ~  x2),
                     "3" = lm(y3 ~  x3),
                     "4" = lm(y4 ~  x4))
             })

stat <- lapply(fits,
               FUN = function(m0) {
                   b <- coef(m0)
                   s <- summary(m0)
                   r2 <- s$r.squared
                   pval <- anova(m0)[1, 5]
                   data.frame("$\\hat{\\beta}_{0}$" = b[1],
                              "$\\hat{\\beta}_{1}$" = b[2],
                              "$R^2$" = r2,
                              "Valor $p$" = pval,
                              check.names = FALSE,
                              stringsAsFactors = FALSE)
               })
stat <- do.call(rbind, stat)

xtb <- xtable(stat,
              caption = "Resumo do ajuste da regressão linear simples com cada par de variáveis do quarteto de Anscombe.",
              digits = c(0, 2, 2, 2, 4))
print.xtable(xtb,
             caption.placement = "top",
             # include.rownames = FALSE,
             sanitize.colnames.function = identity)

#---- anscombe_plot ----------------------------------------------------

library(tidyverse)

cap <- "O quarteto de Anscombe em um diagrama de dispersão."

anscombe_m <- data.frame()

for (i in 1:4) {
    anscombe_m <- rbind(anscombe_m,
                        data.frame(set = i,
                                   x = anscombe[, i],
                                   y = anscombe[, i + 4]))
}

ggplot(anscombe_m, aes(x, y)) +
    geom_point(size = 3,
               fill = "orange",
               shape = 21) +
    geom_smooth(col = "gray30",
                method = lm,
                fullrange = TRUE) +
    facet_wrap(~set, ncol = 2) +
    xlab("Valores de x") +
    ylab("Valores de y") +
    theme_light()

#-----------------------------------------------------------------------
