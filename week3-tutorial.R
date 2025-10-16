## Solutions to Tutorial

# ==== 1. Scatter with legend and reference line ====

#--- 1. Scatter with legend and reference line ---
plot(iris$Sepal.Length, iris$Petal.Length,
     col = as.numeric(iris$Species),
     pch = 16,
     xlab = "Sepal Length (cm)",
     ylab = "Petal Length (cm)",
     main = "Scatter with Dashed Reference Line")

abline(a = 0, b = 1, lty = 2)  # dashed 1:1 line

legend("topleft", legend = levels(iris$Species),
       col = 1:3, pch = 16, title = "Species")






# ==== 2. Grouped line plot ====
x <- 1:5
setosa_means     <- c(1.4, 1.45, 1.5, 1.55, 1.6)
versicolor_means <- c(4.0, 4.1, 4.2, 4.3, 4.4)
virginica_means  <- c(5.5, 5.6, 5.7, 5.8, 5.9)

plot(x, setosa_means, type = "l", lwd = 2, col = 1,
     ylim = c(1, 6), xlab = "Index", ylab = "Mean Petal Length",
     main = "Grouped Line Plot by Species")
lines(x, versicolor_means, col = 2, lwd = 2)
lines(x, virginica_means,  col = 3, lwd = 2)
legend("topleft", legend = c("setosa", "versicolor", "virginica"),
       col = 1:3, lwd = 2, title = "Species")




# ==== 3. Labelled plot with text marks ====
plot(iris$Sepal.Length, iris$Petal.Length,
     type = "n", xlab = "Sepal Length", ylab = "Petal Length",
     main = "Labelled Plot (Initials Instead of Points)")

text(iris$Sepal.Length, iris$Petal.Length,
     labels = substr(iris$Species, 1, 1),
     col = as.numeric(iris$Species))

legend("topleft", legend = levels(iris$Species),
       col = 1:3, pch = 15, title = "Species")







# ==== Simulate a tedious task that OOD solves better ====

# ==== Base R: side-by-side panels of random subsets ====

set.seed(123)
n_subsets <- 3
subset_size <- 50

# Common axis limits
xlim <- range(iris$Sepal.Length)
ylim <- range(iris$Petal.Length)

# Set up layout
par(mfrow = c(1, n_subsets), mar = c(4, 4, 3, 1))

for (i in 1:n_subsets) {
  sub <- iris[sample(nrow(iris), subset_size), ]
  
  plot(sub$Sepal.Length, sub$Petal.Length,
       col = as.numeric(sub$Species),
       pch = 16,
       xlim = xlim, ylim = ylim,
       xlab = "Sepal Length", ylab = "Petal Length",
       main = paste("Subset", i))
}

# Reset layout and add a single legend (in a blank plot)
par(mfrow = c(1,1))
plot.new()
legend("center", legend = levels(iris$Species),
       col = 1:3, pch = 16, title = "Species")

# ==== ggplot: side-by-side panels of random subsets ====
library(ggplot2)

set.seed(123)
n_subsets <- 3
subset_size <- 50

# Create a combined dataframe of random subsets
subset_list <- lapply(1:n_subsets, function(i) {
  sub <- iris[sample(nrow(iris), subset_size), ]
  sub$Subset <- paste("Subset", i)
  sub
})
iris_subs <- do.call(rbind, subset_list)

# Faceted plot
ggplot(iris_subs, aes(Sepal.Length, Petal.Length, color = Species)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("darkgreen", "steelblue", "darkorange")) +
  coord_cartesian(
    xlim = range(iris$Sepal.Length),
    ylim = range(iris$Petal.Length)
  ) +
  facet_wrap(~ Subset) +
  labs(
    x = "Sepal Length (cm)",
    y = "Petal Length (cm)",
    color = "Species",
    title = "Random Subsets of the Iris Dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


# ===================== Commentary =======================================
# COMPARISON: Base R vs ggplot2 for multi-panel scatterplots

# Base R:
# - Uses par(mfrow) to manually define layout grid.
# - Must explicitly fix axis limits (xlim, ylim) for comparability.
# - Each subset plotted separately in a loop.
# - Legend must be added manually in a new blank plot.
# - Style and layout control require multiple manual calls.
# - Procedural: you draw each element yourself (imperative graphics).

# ggplot2:
# - Uses facet_wrap() to automatically arrange multiple panels.
# - Axes, scales, and legends are shared automatically.
# - Each subset can be tagged in the data and plotted in one go.
# - Legend generation and styling are automatic (through aes()).
# - Cleaner syntax for layers and consistent theming.
# - Declarative: you describe mappings, ggplot2 handles the drawing.

# Summary:
# Base R gives explicit low-level control but more manual effort.
# ggplot2 provides higher-level, consistent, and scalable plotting logic.
# Both can produce identical results; choice depends on workflow style.




# Question 2

#--- Base R scatter + summary overlay ---
# ==== Base R: scatter + summary overlay ====
# Base scatter
plot(iris$Sepal.Length, iris$Petal.Length,
     col = as.numeric(iris$Species),
     pch = 16,
     xlab = "Sepal Length (approx log₂ scale)",
     ylab = "Petal Length (cm)",
     main = "Sepal vs Petal Length with Mean Smoother")

# Add per-species smoothed (lowess) lines
for(i in 1:3) {
  sub <- iris[iris$Species == levels(iris$Species)[i], ]
  lines(lowess(sub$Sepal.Length, sub$Petal.Length),
        col = i, lwd = 2)
}

# Custom legend
legend("topleft", legend = levels(iris$Species),
       col = 1:3, pch = 16, lwd = 2,
       title = "Iris Species")

# Custom axis (simulate a log₂-like transformation)
axis(1, at = c(2, 4, 8), labels = c("log₂=1", "log₂=2", "log₂=3"))


# ==== ggplot: scatter + summary overlay ====

library(ggplot2)

#--- ggplot2 scatter + smoother + custom axis ---

ggplot(iris, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1.2) +
  scale_color_manual(values = c("darkgreen", "steelblue", "darkorange"),
                     name = "Iris Species") +
  scale_x_continuous(
    name = "Sepal Length (approx log scale)",
    breaks = c(2, 4, 8),
    labels = c("log=1", "log=2", "log=3")
  ) +
  ylab("Petal Length (cm)") +
  ggtitle("Sepal vs Petal Length with Mean Smoother") +
  theme_minimal(base_size = 13)

# ======================= Commentary =====================================
# COMPARISON: Base R vs ggplot2 for scatter + summary overlay

# Base R:
# - Procedural approach: each layer (points, smoother, legend, axis) is added step by step.
# - Uses lowess() for manual smoothing per species.
# - Axis customization handled through explicit axis() calls.
# - Legend and colour mapping require manual setup.
# - Offers fine-grained control over every element, but code becomes verbose.
# - Style consistency and layout must be managed manually.

# ggplot2:
# - Declarative layering: geom_point() and geom_smooth() specify marks directly.
# - Smoothing is automatic via geom_smooth(method="loess") with species-based colour groups.
# - Axis labels and tick marks are easily controlled with scale_* functions.
# - Colour mapping and legend are automatically generated and thematically consistent.
# - Uses unified theming (theme_minimal) for aesthetic consistency.
# - Produces cleaner, more reproducible graphics with less code.

# Summary:
# Base R requires explicit control of layers and aesthetics,
# while ggplot2 automatically handles layering, scaling, and guides.
# Both can produce identical outputs; ggplot2 is more concise and consistent,
# whereas Base R gives more manual flexibility for low-level customization.
