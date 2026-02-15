library("tidyverse")
library("infer") # for tests
library("lsr") # just for cohensD

confidence_level <- 0.95
q_lower <- (1 - confidence_level) / 2
q_upper <- q_lower + confidence_level

df |>
    pull(depth) |>
    quantile(c(q_lower, q_upper)) |>
    print() # For better formatting

options(repr.plot.width = 10, repr.plot.height = 4) # make plot wider

plot_depth_agains_norm <- function(df) {
    # scale standard-normal distribution with our data
    m <- mean(df$depth)
    s <- sd(df$depth)

    norm_density <- function(x) {
        dnorm(x,
            mean = m,
            sd = s
        )
    }

    df |>
        ggplot(aes(x = depth)) +
        geom_histogram(aes(y = after_stat(density)),
            bins = 70,
            fill = "#cccccc", color = "white", alpha = 0.85
        ) +
        geom_density(linewidth = 1.2, color = "#0073C2FF") +
        geom_function(
            fun = dnorm, args = list(mean = m, sd = s),
            linewidth = 1.2, color = "#006b3d", linetype = "dashed"
        ) +
        theme_minimal(base_size = 14) +
        labs(
            title = "Diamond Depth Distribution vs. Normal Curve",
            subtitle = paste0( # concats strings with no separator
                "Blue line = Sample Distribution\nDashed Green = Normal Distribution (µ=", round(m, 1), ", σ=", round(s, 1), ")"
            ),
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
        stat_qq_line(color = "#006b3d", linewidth = 1.5, alpha = 0.7) +
        labs(
            title = "QQ-Plot depth",
            x = "Quantiles Normaldistribution",
            y = "Quantiles depth"
        )
}


plot_depth_agains_norm(df)
qq_plot_depth(df)

# Remove top and bottom 1%
df_filtered <- df |>
    filter(
        depth > quantile(depth, 0.01, na.rm = TRUE),
        depth < quantile(depth, 0.99, na.rm = TRUE)
    )

df_filtered |>
    ggplot(aes(x = depth)) +
    geom_histogram(bins = 70, fill = "#0073C2FF", color = "white") +
    theme_minimal() +
    labs(
        title = "Depth Distribution (Outliers Removed)",
        subtitle = "Excluding the top 1% and bottom 1% from sample"
    )

plot_depth_agains_norm(df_filtered)
qq_plot_depth(df_filtered)

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

# We can see from the Kolmogorov-Smirnov test, that the p-value is very small so we need to **reject H0**.
# Now lets test **df_filtered** for normal distribution to see if sacrificing 2% of our data was worth it. Again we state that: \
# **H0:** "depth" follows normal distribution \
# **H1:** "depth" does not follow normal distribution

ks.test(
    x = df_filtered$depth, # now we use filtered data
    y = "pnorm", # Test against normal distribution
    mean = mean(df$depth),
    sd = sd(df$depth),
    alternative = "two.sided" # Standard value
)
# Again p-value is really small so we have to **reject H0**. In conclusion it can be said, that there is very strong evidence, that our data (filtered and unfiltered) does not follow normal distribution.

## Directed T-Test
# Based on our extensive knowledge about diamonds we came to the conclusion that the depth of a diamond
# has some kind of influence on its cut. This means that a diamond with a "Fair" cut should be more shallow
# than a diamond with an "Ideal" cut according to [this](https://www.diamondguidance.com/education/diamond-grading/4-cs/cut/fair/) article.

# We state our Hypothesis like this: \
# **H0**: Mean depth of "Fair" cut diamonds is less than (or equal) to the mean depth of "Ideal" cut diamonds \
# **H1**: Mean depth of "Fair" cut diamonds is greater than the mean depth of "Ideal" cut diamonds

df_fair_ideal <- df |> filter(cut == "Fair" | cut == "Ideal")
df_fair_ideal |> t_test(
    formula = depth ~ cut, # compare depth, groups defined by cut
    alternative = "greater", # also see H1
    order = c("Fair", "Ideal"),
    conf_level = 0.95,
)
