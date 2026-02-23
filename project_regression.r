# Projektabgabe: Diamonds
# Datensatz: 
# - [ggplot2::diamonds](https://ggplot2.tidyverse.org/reference/diamonds.html)
# Teammitglieder:
# - Julia Tuka
# - Clemens Hafenscher
# - Michael Maximilian Werfring
library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)
library(tidyverse)
library(GGally)
library(infer)
library(ggcorrplot)
library(stringr)
library(Metrics)
library(lsr)
library(MASS)

# Train, Validation & Test Split
df <- ggplot2::diamonds
set.seed(123) # sorgt dafür, dass Zufall immer der selbe ist, macht script reproduzierbar.

n <- nrow(df)
idx <- sample(n) # daten durchmischen, falls diese nach einer Variale sortiert sind

train_indices <- idx[1:floor(0.6 * n)] # erste 60% des indizes
val_indices <- idx[(floor(0.6 * n) + 1):floor(0.8 * n)]
test_indices <- idx[(floor(0.8 * n) + 1):n]

train_set <- df[train_indices, ]
validation_set <- df[val_indices, ]
test_set <- df[test_indices, ]
# Deskriptive Statistik
## NA-Einträge
# im diamonds dataset sind keine NA-Einträge vorhanden...
colSums(is.na(train_set)) 

# ...also setzen wir 10 Werte selbstständig auf NA
train_set$carat[sample(1:nrow(train_set), 10)] <- NA 
train_set$cut[sample(1:nrow(train_set), 10)] <- NA

# für das spätere lineare Modell, wenn NA vorhanden, wird die Zeile nicht übernommen
train_set <- train_set |> filter(!is.na(price))

# NAs behandeln, da wir nur carat und cut auf NA gesetzt haben, müssen nur 
# diese behandelt werden median aus der carat spalte berechnen ohne die na 
# Werte zu beachten
median_carat <- median(train_set$carat, na.rm = TRUE) 
# diesen median überall dort einsetzten wo wert = NA 
train_set$carat[is.na(train_set$carat)] <- median_carat 

mode_cut <- names(sort(table(train_set$cut), decreasing = TRUE))[1]
# table -> zählt häufigkeiten der vorkommenden Kategorien
# Sort -> sortiert das Array absteigend
# names [1] -> gibt den namen der ersten kategorie zurück
# modus von cut wird für die NA Werte eingesetzt
train_set$cut[is.na(train_set$cut)] <- mode_cut 
## Ausreißer im Datensatz
ggplot(train_set, aes(x = price, y = "")) +
  geom_boxplot() +
  labs(title = "Boxplot: Preis", x = "Preis (USD)", y = NULL)
# viele Ausreiße durch hohe Preise vorhanden, werden beibehalten, 
# da diese Preise in der realen Marktbeobachtung relevant sind, Diamanten
# sind ja Luxusartikel
ggplot(train_set, aes(x = carat, y = "")) +
  geom_boxplot() +
  labs(title = "Boxplot: Carat", x = "Carat", y = NULL)
# Einige sehr große Diamanten vorhanden, meisten sind unter 1 carat, 
# Ausreißer representieren die seleteneren/wertvolleren Diamanten

train_xyz_long <- train_set[, c("x", "y", "z")] |>
  pivot_longer(cols = c(x, y, z), names_to = "dimension", values_to = "mm")

ggplot(train_xyz_long, aes(x = mm, y = dimension)) +
  geom_boxplot() +
  labs(title = "Boxplots: Abmessungen x/y/z", x = "mm", y = "Dimension")
# Einzelne sehr große Werte, einige 0 Werte, diese sind nicht realistisch -> 
# sollten eventuell behandelt werden könnten Eingabe- oder Messfehler sein
boxplot(train_set$depth, 
    main = "Boxplot: Tiefe (%)", 
    ylab = "Depth", horizontal = TRUE)
# moderate ausreißer mit plausiblen Werten
# Ausreißer behandeln: Die einzig wirklich auffälligen, 
# die nicht plausibel erscheienn sind x, y, z Werte. Siehe =>
train_set |> filter(x == 0 | y == 0 | z == 0)

# Deher werden Beobachtungen mit 'unmöglichen' Abmessungen entfernt, 
# da diese wahrscheinlich Messfehler oder Eingabefehler sind
train_set <- train_set |> filter(x > 0, y > 0, z > 0)
## Visualisierungen
desc_table <- train_set[, c("price", "carat")] |>
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Wert"
  ) |>
  group_by(Variable) |>
  summarise(
    N = n(),
    Min = min(Wert),
    Q1 = quantile(Wert, 0.25),
    Median = median(Wert),
    Mean = mean(Wert),
    Q3 = quantile(Wert, 0.75),
    Max = max(Wert),
    .groups = "drop"
  )

desc_table
ggplot(train_set, aes(x = price)) +
  geom_histogram(bins = 40, color = "black") +
  coord_cartesian(xlim = c(0, 20000)) +
  labs(title = "Histogramm des Diamantenpreises", x = "Preis", y = "Anzahl")

ggplot(train_set, aes(x = price)) +
  geom_density(aes(y = after_stat(density * 100))) +
  coord_cartesian(xlim = c(0, 20000)) +
  labs(title = "Dichteplot des Diamantenpreises", x = "Preis", y = "Dichte (%)")

ggplot(train_set, aes(x = cut)) +
  geom_bar(color = "black") +
  labs(title = "Häufigkeit der Schliffqualität (cut)", x = "Cut", y = "Anzahl")

ggplot(train_set, aes(x = carat, y = price)) +
  geom_point(alpha = 0.2) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 20000)) +
  labs(title = "Preis in Abhängigkeit vom Karatgewicht", 
  x = "Carat", y = "Preis (USD)")

# Induktive Statistik
df <- ggplot2::diamonds
df |> head()

## Inference
#For inference the variable **depth** was chosen. It describes the height of a diamond from the **table** (flat top surface) to the **cutlet** (tip at the bottom) 
# and can be measured in millimeters. In this Dataset it is relative to the width of the diamond, so it must be interpreted as a percentage. 
# See [here](https://www.diamonds.pro/education/diamond-depth-and-table/) for further information.
confidence_level <- 0.95
q_lower <- (1 - confidence_level) / 2
q_upper <- q_lower + confidence_level

df |>
  pull(depth) |>
  quantile(c(q_lower, q_upper)) |>
  print() # For better formatting

## Test for Normal Distribution
# The histogram and the qq-plot show pretty clearly that a normal distribution is very unlikely to be present. First we will create functions to plot our data against normal distribution.
options(repr.plot.width = 10, repr.plot.height = 4) # make plot wider

plot_depth_agains_norm <- function(df) {
    # scale standard-normal distribution with our data
    m <- mean(df$depth)
    s <- sd(df$depth)

    norm_density <- function(x) dnorm(x, 
        mean = m,
        sd = s
    )

    df |> 
    ggplot(aes(x = depth)) +
    geom_histogram(aes(y = after_stat(density)), bins = 70, 
                   fill = "#cccccc", color = "white", alpha = 0.85) + 
    geom_density(linewidth = 1.2, color = "#0073C2FF") +
    geom_function(fun = norm_density, 
                  linewidth = 1.2, color = "#006b3d", linetype = "dashed") +
    theme_minimal(base_size = 14) +
    labs(
        title = "Diamond Depth Distribution vs. Normal Curve",
        subtitle = paste0( # concats strings with no separator
            "Blue line = Sample Distribution
            Dashed Green = Normal Distribution 
            (µ=", round(m, 1), ", σ=", round(s, 1), ")"),
        x = "depth (%)",
        y = "density",
    ) +
    theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold")
    )
}

qq_plot_depth <- function(df) {
    df |> 
    ggplot(aes(sample = depth)) + 
    stat_qq(color = "#0073C2FF") + 
    stat_qq_line(color="#006b3d", linewidth=1.5 , alpha=0.7) + 
    labs(title = "QQ-Plot depth", 
        x = "Quantiles Normaldistribution", 
        y = "Quantiles depth")
}
plot_depth_agains_norm(df)
qq_plot_depth(df)
# Lets try if removing the smallest and largest 1% of data would possibly improve the fit.
df_filtered <- df |>
    filter(depth > quantile(depth, 0.01, na.rm = TRUE),
           depth < quantile(depth, 0.99, na.rm = TRUE))

df_filtered |>
    ggplot(aes(x = depth)) +
    geom_histogram(bins = 70, fill = "#0073C2FF", color = "white") +
    theme_minimal() +
    labs(title = "Depth Distribution (Outliers Removed)",
         subtitle = "Excluding the top 1% and bottom 1% from sample")

# We see in the histogram, that both functions came closer to each other but the qq-plot still shows some discrepancies. As expected mu has not changed but sigma did.
# plot_depth_agains_norm(df_filtered)
# qq_plot_depth(df_filtered)

# We can now test our **df** againts normal distribution to validate the assumptions from our plots.
# Lets assume that: \
# **H0:** "depth" follows normal distribution \
# **H1:** "depth" does not follow normal distribution
ks.test(
    x = df$depth,
    y = "pnorm", # Test against normal distribution
    mean = mean(df$depth),
    sd = sd(df$depth),
    alternative = "two.sided" # Standard value
)

# We can see from the Kolmogorov-Smirnov test, that the p-value is very small so we need to **reject H0**. Now lets test **df_filtered** for normal distribution to see if 
# sacrificing 2% of our data was worth it. Again we state that:
# **H0:** "depth" follows normal distribution
# **H1:** "depth" does not follow normal distribution

ks.test(
    x = df_filtered$depth, # now we use filtered data
    y = "pnorm", # Test against normal distribution
    mean = mean(df_filtered$depth),
    sd = sd(df_filtered$depth),
    alternative = "two.sided" # Standard value
)
# Again p-value is really small so we have to **reject H0**. In conclusion it can be said, that there is very strong evidence, that our data (filtered and unfiltered) does not follow normal distribution.

## Directed T-Test
# Based on our extensive knowledge about diamonds we came to the conclusion that the depth of a diamond has some kind of influence on its cut. 
# This means that a diamond with a "Fair" cut should be more shallow than a diamond with an "Ideal" cut according to [this](https://www.diamondguidance.com/education/diamond-grading/4-cs/cut/fair/) article.

# We state our Hypothesis like this:
# **H0**: Mean depth of "Fair" cut diamonds is less than (or equal) to the mean depth of "Ideal" cut diamonds
# **H1**: Mean depth of "Fair" cut diamonds is greater than the mean depth of "Ideal" cut diamonds
df_fair_ideal <- df |> filter(cut == "Fair" | cut == "Ideal")

df_fair_ideal |> t_test(
    formula = depth ~ cut, # compare depth, groups defined by cut
    alternative = "greater", # also see H1
    order = c("Fair", "Ideal"),
    conf_level = 0.95,
)
# Since the p-value is very small (basically zero) we can **reject H0** and have strong evidence based on our data that the mean depth of "Fair" cut diamonds 
# is grater than the mean depth of "Ideal" cut diamonds. Based on the conf_level of 95% and the lower_ci being at about 2.18% we can also state that we have 95% 
# confidence that "Fair" cut diamonds are 2.18% deeper than "Ideal" cut diamonds.

# Lineare Regression
## Correlation
# Below we create a correlation Matrix for all the numeric values from our train set. We see that there is strong positive correlation between the price, 
# carat and the xyz values. While there is very little correlation between price and depth and table. Also the variables carat and x, y and z are positively 
# correlated, which makes perfect sense due to the fact that carat is the weight of the diamond and x, y and z are length, width and depth of the diamond (in mm). 
# [Official Documentation](https://ggplot2.tidyverse.org/reference/diamonds.html)
# Create correlation Matrix
train_corel <- train_set |>
  dplyr::select(where(is.numeric)) |>
  cor() |>
  round(digits = 2)

# Display correlation plot, easier to us than just correlation Matrix
ggcorrplot(train_corel, lab = TRUE)
## Creating Linear Models
# Based on our insights from the previous sections we create 4 linear models with the following variables to predict a diamonds price: \
# - **model1**: carat, x, y, z (naive approach, just use all variables that have high correlate with price)
# - **model2**: carat, depth, table, x, y, z (use all numeric variables)
# - **model3**: carat, cut, color, clarity, depth, table, x, y, z (all variables available)
# - **model4**: carat, cut, color, clarity (based on [this article](https://4cs.gia.edu/en-us/4cs-of-diamond-quality/) these influence the quality and maybe also the price)
model1 <- lm(price ~ carat + x + y + z, data = train_set)
coef(model1) |>
  print() # pretty formatting

model2 <- lm(price ~ carat + depth + table + x + y + z, data = train_set)
coef(model2) |>
  print()

model3 <- lm(price ~ carat + cut + color + clarity + depth + table + x + y + z, data = train_set)
coef(model3) |>
  print()

model4 <- lm(price ~ carat + cut + color + clarity, data = train_set)
coef(model4) |>
  print()
  
# Ranking der wichtigsten Effekte (nach absolutem Betrag) -> 
# keine Standardisierung
rank_betas <- function(model, top_n = NULL, remove_intercept = TRUE) {
  df <- coef(model) |>
    tibble::enframe(name = "term", value = "beta") |>
    mutate(abs_beta = abs(beta))

  # Bedingung außerhalb der Pipe anwenden
  if (remove_intercept) {
    df <- df |> filter(term != "(Intercept)")
  }

  df <- df |> arrange(desc(abs_beta))

  if (!is.null(top_n)) {
    df <- head(df, top_n)
  }

  return(df)
}

## Model Ranking
cat("\n====== MODEL 1: price ~ carat + x + y + z ======\n")
rank_m1 <- rank_betas(model1)
print(rank_m1, n = nrow(rank_m1))

cat("\n====== MODEL 2: price ~ carat + depth + table + x + y + z ======\n")
rank_m2 <- rank_betas(model2)
print(rank_m2, n = nrow(rank_m2))

cat("\n====== MODEL 3: price ~ carat + cut + color + clarity + 
depth + table + x + y + z ======\n")
rank_m3 <- rank_betas(model3)
print(rank_m3, n = 20)  # viele Terme durch Faktoren

cat("\n====== MODEL 4: price ~ carat + cut + color + clarity ======\n")
rank_m4 <- rank_betas(model4)
print(rank_m4, n = 20)

model_stats <- function(model) {
  s <- summary(model)

  list(
    t_tests = s$coefficients,      # T-Tests der Regressionskoeffizienten
    f_test = s$fstatistic,         # F-Test des Gesamtmodells
    r2 = s$r.squared,              # R²
    adj_r2 = s$adj.r.squared,      # Adjusted R²
    aic = AIC(model)               # AIC
  )
}

stats_m1 <- model_stats(model1)
stats_m2 <- model_stats(model2)
stats_m3 <- model_stats(model3)
stats_m4 <- model_stats(model4)

## Modelle Übersicht
cat("===== MODEL 1: price ~ carat + x + y + z =====\n")
print(stats_m1$t_tests)
cat("\nF-Test:\n")
print(stats_m1$f_test)
cat("\nR²:", stats_m1$r2, "| adj. R²:", stats_m1$adj_r2, "\n")
cat("AIC:", stats_m1$aic, "\n\n")

cat("===== MODEL 2: price ~ carat + depth + table + x + y + z =====\n")
print(stats_m2$t_tests)
cat("\nF-Test:\n")
print(stats_m2$f_test)
cat("\nR²:", stats_m2$r2, "| adj. R²:", stats_m2$adj_r2, "\n")
cat("AIC:", stats_m2$aic, "\n\n")

cat("===== MODEL 3: price ~ carat + cut + color + clarity + depth + 
table + x + y + z =====\n")
print(stats_m3$t_tests)
cat("\nF-Test:\n")
print(stats_m3$f_test)
cat("\nR²:", stats_m3$r2, "| adj. R²:", stats_m3$adj_r2, "\n")
cat("AIC:", stats_m3$aic, "\n\n")

cat("===== MODEL 4: price ~ carat + cut + color + clarity =====\n")
print(stats_m4$t_tests)
cat("\nF-Test:\n")
print(stats_m4$f_test)
cat("\nR²:", stats_m4$r2, "| adj. R²:", stats_m4$adj_r2, "\n")
cat("AIC:", stats_m4$aic, "\n\n")

## RMSE
rmse_model <- function(model, train_data, val_data) {
  pred_train <- predict(model, train_data)
  pred_val   <- predict(model, val_data)

  rmse_train <- rmse(train_data$price, pred_train)
  rmse_val   <- rmse(val_data$price, pred_val)

  return(list(train_rmse = rmse_train, val_rmse = rmse_val))
}

rmse_m1 <- rmse_model(model1, train_set, validation_set)
rmse_m2 <- rmse_model(model2, train_set, validation_set)
rmse_m3 <- rmse_model(model3, train_set, validation_set)
rmse_m4 <- rmse_model(model4, train_set, validation_set)

cat("===== MODEL 1 =====\n")
print(rmse_m1)

cat("\n===== MODEL 2 =====\n")
print(rmse_m2)

cat("\n===== MODEL 3 =====\n")
print(rmse_m3)

cat("\n===== MODEL 4 =====\n")
print(rmse_m4)
# Model 3 is has the best RMSE Score, followed closely by Model 4, for the next plots we will use the best Model (3).
res <- residuals(model3)
fitted_vals <- fitted(model3)

# Residuen vs. Fitted Plot (Homoskedastizität)
plot(fitted_vals, res,
     main = "Residuals vs Fitted",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(h = 0, col = "red", lwd = 2)
qqnorm(res, main = "QQ-Plot der Residuen", pch = 20, col = rgb(0, 0, 0, 0.3))
qqline(res, col = "red", lwd = 2)
hist(res, breaks = 50,
     main = "Histogramm der Residuen",
     xlab = "Residuen",
     col = "lightblue", border = "white")
std_res <- studres(model3)

plot(std_res,
     main = "Standardisierte Residuen",
     ylab = "Standardisierte Residuen",
     xlab = "Index",
     pch = 20, col = rgb(0, 0, 0, 0.3))
abline(h = c(-3, 3), col = "red", lwd = 2)
## Prognose am Test-Set
# ---- Prognose für das Test-Set ----
pred_test <- predict(model3, test_set)

# ---- RMSE berechnen ----
rmse_test <- rmse(test_set$price, pred_test)

cat("RMSE Test-Set (Modell 3):", rmse_test, "\n")

rmse_test_all <- function(model, name) {
  pred <- predict(model, test_set)
  val <- rmse(test_set$price, pred)
  cat(name, ": ", val, "\n")
}

rmse_test_all(model1, "Model 1")
rmse_test_all(model2, "Model 2")
rmse_test_all(model3, "Model 3")
rmse_test_all(model4, "Model 4")
# Hier zeigt sich, dass Modell 4 im Gegensatz zu vorher auf den Testdaten besser abschneidet als Modell 3. Das könnte daran liegen, dass Modell 3 mehr Variablen beinhaltet und somit overfittet.