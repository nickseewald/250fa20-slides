penguins <- read.csv("https://raw.githubusercontent.com/STATS250SBI/palmerpenguins/master/inst/extdata/penguins_NArm.csv",
                     stringsAsFactors = TRUE)
set.seed(8743)
dev.off()
for (n in seq(10, 200, by = 10)) {
  pHats <- replicate(1000, {
    s <- penguins[sample(1:333, size = n), ]
    proportions(table(s$species))["Gentoo"]
  })
  png(filename = file.path("histgif", paste0("hist", n, ".png")),
      pointsize = 16)
  hist(pHats,
       main = paste("n = ", n),
       xlab = "p-hat values",
       col = "darkturquoise",
       breaks = seq(0, 1, .025),
       xlim = c(0, 1),
       ylim = c(0, 500),
       cex.lab = 1.5,
       cex.main = 3,
       cex.axis = 1.5)
  abline(v = proportions(table(penguins$species))["Gentoo"],
         lwd = 2, lty = "dashed", col = "darkblue")
  dev.off()
}
