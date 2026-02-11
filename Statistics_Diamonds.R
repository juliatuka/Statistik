library(ggplot2)
library(dplyr)
library(tidyr)
library(gt)
library(tidyverse)
library(GGally)
library(infer)
library(ggcorrplot)

# \describe{
#   \item{price}{price in US dollars ($326--$18,823)}
#   \item{carat}{weight of the diamond (0.2--5.01)}
#   \item{cut}{quality of the cut (Fair, Good, Very Good, Premium, Ideal)}
#   \item{color}{diamond colour, from D (best) to J (worst)}
#   \item{clarity}{a measurement of how clear the diamond is (I1 (worst), SI2,
#     SI1, VS2, VS1, VVS2, VVS1, IF (best))}
#   \item{x}{length in mm (0--10.74)}
#   \item{y}{width in mm (0--58.9)}
#   \item{z}{depth in mm (0--31.8)}
#   \item{depth}{total depth percentage = z / mean(x, y) = 2 * z / (x + y) (43--79)}
#   \item{table}{width of top of diamond relative to widest point (43--95)}

df <- ggplot2::diamonds
set.seed(123) # sorgt dafür, dass der Zufall immer der selbe ist, macht das script reproduzierbar.

n <- nrow(df)
idx <- sample(n) # daten durchmischen, falls diese nach einer Variale sortiert sind

train_indices <- idx[1:floor(0.6 * n)] # erste 60% des indizes
val_indices <- idx[(floor(0.6 * n) + 1):floor(0.8 * n)]
test_indices <- idx[(floor(0.8 * n) + 1):n]

train_set <- df[train_indices, ]
validation_set <- df[val_indices, ]
test_set <- df[test_indices, ]

# Deskriptive Statistik

colSums(is.na(train_set)) # im diamonds dataset sind keine NA-Einträge vorhanden...

train_set$carat[sample(1:nrow(train_set), 10)] <- NA # ...also setzen wir 10 Werte selbstständig auf NA
train_set$cut[sample(1:nrow(train_set), 10)] <- NA

train_set <- train_set %>% filter(!is.na(price)) # für das spätere lineare Modell, wenn NA vorhanden, wird die Zeile nicht übernommen

# NAs behandeln, da wir nur carat und cut auf NA gesetzt haben, müssen nur diese behandelt werden
median_carat <- median(train_set$carat, na.rm = TRUE) # median aus der carat spalte berechnen ohne die na werte zu beachten
train_set$carat[is.na(train_set$carat)] <- median_carat # diesen median überall dort einsetzten wo wert = NA

mode_cut <- names(sort(table(train_set$cut), decreasing = TRUE))[1]
# table -> zählt häufigkeiten der vorkommenden Kategorien
# Sort -> sortiert das Array absteigend
# names [1] -> gibt den namen der ersten kategorie zurück
train_set$cut[is.na(train_set$cut)] <- mode_cut # modus von cut wird für die NA Werte eingesetzt


# Ausreißer Analyse:

ggplot(train_set, aes(x = price, y = "")) +
  geom_boxplot() +
  labs(title = "Boxplot: Preis", x = "Preis (USD)", y = NULL)
# viele Ausreiße durch hohe Preise vorhanden, werden beibehalten, da diese Preise in der realen Marktbeobachtung relevant sind, Diamanten sind ja Luxusartikel

ggplot(train_set, aes(x = carat, y = "")) +
  geom_boxplot() +
  labs(title = "Boxplot: Carat", x = "Carat", y = NULL)
# Einige sehr große Diamanten vorhanden, meisten sind unter 1, Ausreißer berepresentieren die seleteneren/wertvolleren Diamanten

# par(mfrow = c(1,3))
# The next 3 plots created will be plotted next to each other
# boxplot(train_set$x, main = "x (Länge)", ylab = "mm")
# boxplot(train_set$y, main = "y (Breite)", ylab = "mm")
# boxplot(train_set$z, main = "z (Tiefe)", ylab = "mm")
# Put plotting arrangement back to its original state
# par(mfrow = c(1,1))

train_xyz_long <- train_set %>%
  select(x, y, z) %>%
  pivot_longer(cols = c(x, y, z), names_to = "dimension", values_to = "mm")

ggplot(train_xyz_long, aes(x = mm, y = dimension)) +
  geom_boxplot() +
  labs(title = "Boxplots: Abmessungen x/y/z", x = "mm", y = "Dimension")
# Einzelne sehr große Werte, einige 0 Werte, diese sind nicht realistisch -> sollten eventuell behandelt werden könnten Eingabe- oder Messfehler sein

boxplot(train_set$depth, main = "Boxplot: Tiefe (%)", ylab = "Depth", horizontal = TRUE)
# moderate ausreißer mit plausiblen Werten

# Ausreißer behandeln: Die einzig wirklich auffälligen, die nicht plausibel erscheienn sind x, y, z Werte
# siehe =>
train_set %>% filter(x == 0 | y == 0 | z == 0)

# Deher werden Beobachtungen mit 'unmöglichen' Abmessungen entfernt, da diese wahrscheinlich Messfehler oder Eingabefehler sind
train_set <- train_set %>% filter(x > 0, y > 0, z > 0)


# Grafiken und Tabellen:

# Tabelle:
desc_table <- train_set %>%
  select(price, carat) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "Wert"
  ) %>%
  group_by(Variable) %>%
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

desc_table %>%
  gt() %>%
  tab_header(
    title = "Metrischen Variablen (Carat und Preis)"
  )

# Histogramm:
ggplot(train_set, aes(x = price)) +
  geom_histogram(bins = 40, color = "black") +
  coord_cartesian(xlim = c(0, 20000)) +
  labs(title = "Histogramm des Diamantenpreises", x = "Preis", y = "Anzahl")

# Dichteplot:
ggplot(train_set, aes(x = price)) +
  geom_density(aes(y = after_stat(density * 100))) +
  coord_cartesian(xlim = c(0, 20000)) +
  labs(title = "Dichteplot des Diamantenpreises", x = "Preis", y = "Dichte (%)")
# Die dichte wurde hier auf Prozent pro einheit skaliert, Fläche unter der Kurve = 100


# Balkendiagramm:
ggplot(train_set, aes(x = cut)) +
  geom_bar(color = "black") +
  labs(title = "Häufigkeit der Schliffqualität (cut)", x = "Cut", y = "Anzahl")

# Streudiagramm:
ggplot(train_set, aes(x = carat, y = price)) +
  geom_point(alpha = 0.2) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0, 20000)) +
  labs(title = "Preis in Abhängigkeit vom Karatgewicht", x = "Carat", y = "Preis (USD)")

## Correlation
# Below we create a correlation Matrix for all the numeric values from our train set.
# We see that there is strong postive correlation between the price, carat and the xyz values.
# While there is very little correlation between price and depth and table.
# Also the variables carat and x, y and z are postively correlated, which makes perfect sense
# due to the fact that carat is the weight of the diamond and x, y and z are lenght, width
# and depth of the diamond (in mm). [Official Documentation](https://ggplot2.tidyverse.org/reference/diamonds.html)

# Create correlation Matrix
train_corel <- train_set |>
  select(where(is.numeric)) |>
  cor() |>
  round(digits = 2)

# Display correlation plot, easier to us than just correlation Matrix
ggcorrplot(train_corel, lab = TRUE)

## Creating Linear Models
# Based on our insights from the previous section we create 4 linear models with the following variables to predict a diamonds price: \
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
