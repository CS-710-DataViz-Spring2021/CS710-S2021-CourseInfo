library(scatterplot3d)

x <- c(0.5, 0.75, 1)
y <- c(0.75, 1, 0.5)
z <- c(3, 4, 5)

# No depth cues
pdf(file = "nocues.pdf")
scatterplot3d(x, y, z, xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 6), pch = 19,
              box = FALSE, grid = FALSE)
dev.off()

# Perspective Geometry
pdf(file = "occlusion.pdf")
scatterplot3d(x, y, z, xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 6), pch = 19,
              box = TRUE, grid = FALSE)
dev.off()

# Grid
pdf(file = "grid.pdf")
scatterplot3d(x, y, z, xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 6), pch = 19,
              box = TRUE, grid = TRUE)
dev.off()

# Dropped lines
pdf(file = "lines.pdf")
scatterplot3d(x, y, z, xlim = c(0, 2), ylim = c(0, 2), zlim = c(0, 6), pch = 19,
              box = TRUE, grid = TRUE, type="h")
dev.off()