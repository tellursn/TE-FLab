library(ggplot2)

initLen         = 100
trainLen        = 7000
testLen         = 3000
inSize          = 1
outSize         = 1
resSize         = 1500
a               = 0.2
spectral_radius = 0.99
reg             = 1e-04

data = as.matrix(read.table('kitaibaraki_temp1.txt'))
original_mean = mean(data)
original_sd = sd(data)
data = scale(data)

Win = matrix(runif(resSize * (1 + inSize), -0.5, 0.5), resSize)
W   = matrix(runif(resSize * resSize, -0.5, 0.5), resSize)

cat('Computing spectral radius...')
rhoW = abs(eigen(W, only.values = TRUE)$values[1])
W = W * spectral_radius / rhoW
print('done.')

X  = matrix(0, 1 + inSize + resSize, trainLen - initLen)
Yt = data[(initLen + 2):(trainLen + 1), 1, drop = FALSE]
x = rep(0, resSize)

for (t in 1:trainLen) {
  u = data[t, 1]
  x = (1 - a) * x + a * tanh(Win %*% rbind(1, u) + W %*% x)
  if (t > initLen) {
    X[, t - initLen] = rbind(1, u, x)
  }
}

X_T  = t(X)
Wout = t(solve(X %*% X_T + reg * diag(1 + inSize + resSize)) %*% X %*% Yt)

Y = matrix(0, outSize, testLen)
u = data[trainLen + 1, 1]

for (t in 1:testLen) {
  x = (1 - a) * x + a * tanh(Win %*% rbind(1, u) + W %*% x)
  y = Wout %*% rbind(1, u, x)
  Y[, t] = y
  u = y
}

predicted_values_scaled_back = Y * original_sd + original_mean


target_full_scaled_back = data[(trainLen + 2):(trainLen + 1 + testLen)] * original_sd + original_mean


errorLen       = 500
target_data    = target_full_scaled_back[1:errorLen]
predicted_data = predicted_values_scaled_back[1, 1:errorLen]
mse            = sum((target_data - predicted_data)^2) / errorLen
print(paste('MSE (first', errorLen, 'steps) =', mse))

plotLen = 2000
plot_df <- data.frame(
  time = 1:plotLen,
  target = target_full_scaled_back[1:plotLen],
  predicted = predicted_values_scaled_back[1, 1:plotLen]
)

print(
  ggplot(plot_df, aes(x = time)) +
    geom_line(aes(y = target, color = "Target Signal"), linewidth = 1) +
    geom_line(aes(y = predicted, color = "Predicted Signal"), linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c(
      "Target Signal" = "green",
      "Predicted Signal" = "blue"
    )) +
    labs(
      title = paste('Reservoir Computing Prediction (First', plotLen, 'Steps)'),
      x = 'Time Step',
      y = 'Value',
      color = 'Legend'
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
)
