library(openxlsx)
library(e1071)
library(ggplot2)

# 通过目标函数计算差值
calc_value <- function(x, y, ex, cv, factor) {
   cs <- factor * cv
   a <- 4 / (cs * cs)
   b <- 2 / (ex * cs * cv)
   a0 <- ex * (1 - 2 * cv / cs)
   sum <- 0;
   for (i in 1:length(x)) {
      xp <- qgamma(1 - pnorm(x[i], mean = 0, sd = 1), a, b) + a0
      sum <- sum + (y[i] - xp) ^ 2
   }
   return(sum)
}

# 绘制海森概率格纸和散点
draw_hazen <- function() {
   xz <- c(0.01, 0.5, 1, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 99.9, 99.99)
   x_axis_values <- double(length(xz))
   for (i in 1:length(xz)) {
      x_axis_values[i] <- qnorm(xz[i] * 0.01, mean = 0, sd = 1)
      # x轴实际坐标值
   }
   pvalues <- double(n)
   for (i in 1:n) {
      pvalues[i] <- qnorm(i / (n + 1), mean = 0, sd = 1)
      # 概率
   }
   plot(x = pvalues, y = data, xlab = "概率/%", xaxt = "n",
        ylab = bquote(paste("流量/10" ^ "8", "m" ^ "3")),
        title("水文频率适线"),
        xlim = c(-4, 4), ylim = c(0, 1500))
   # 描点
   axis(1, x_axis_values, labels = xz)
   # 设置坐标轴
   for (i in x_axis_values) {
      abline(v = i)
      # 绘制坐标线
   }
   return(pvalues)
}

data <- read.xlsx(
  "Data.xlsx",
  sheet = 1,
  startRow = 0,
  colNames = TRUE,
  rowNames = FALSE,
  detectDates = FALSE,
  skipEmptyRows = TRUE,
  skipEmptyCols = TRUE,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  sep.names = ".",
  namedRegion = NULL,
  na.strings = "NA",
  fillMergedCells = FALSE
)[[2]]
n <- length(data)
ex <- sum(data) / n
cv <- sd(data) / ex
data <- sort(data, decreasing = TRUE)
png(file = "P3-plot.png",
    width     = 800,
    height    = 800,
    units     = "px",
    res       = NA,
    pointsize = 20)
pvalues <- draw_hazen()
m <- 3000
csfactor <- seq(1, 4, length.out = m)
csfactorvalue <- double(m)
for (i in 1:m){
   csfactorvalue[i] <- calc_value(pvalues, data, ex, cv, csfactor[i])
}
fact <- csfactor[order(csfactorvalue)][1]
fact
# cs <- n * skewness(data) / (n - 3)
cs <- fact * cv
a <- 4 / (cs * cs)
b <- 2 / (ex * cs * cv)
a0 <- ex * (1 - 2 * cv / cs)
m <- 1000;
yy <- seq(10, 1200, length.out = m)
xx <- seq(0.01, 99.99, length.out = m)
for (i in 1:m) {
   yy[i] <- qgamma(1 - xx[i] * 0.01, a, b) + a0
   xx[i] <- qnorm(xx[i] * 0.01, mean = 0, sd = 1)
}
lines(xx, yy, col = "blue")
legend("topright", legend = c(bquote(paste(
  "e"["x"] == "", .(ex),
  " c"["v"] == "", .(cv),
  " c"["s"] == "", .(fact), "" * "c"["v"]))),
  col = c("blue"), lty = 1, lwd = 2)
dev.off()
warnings()