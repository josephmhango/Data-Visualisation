# --------------------------------------------
# Gestalt Principles — Simulated Visualisations
# --------------------------------------------
# Outputs: ./gestalt_outputs/*.png (and one .gif)
# Dependencies: ggplot2, dplyr, tidyr, patchwork, gganimate (for common fate)
# --------------------------------------------
set.seed(42)

# ---- Packages ----
pkgs <- c("ggplot2","dplyr","tidyr","patchwork","gganimate")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
lapply(pkgs, library, character.only = TRUE)

# ---- Output dir & helpers ----
out_dir <- "gestalt_outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

save_plot <- function(p, name, w=6, h=6, dpi=200) {
  ggsave(file.path(out_dir, paste0(name, ".png")), p, width=w, height=h, dpi=dpi)
}

base_void <- theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

# =========================================================
# 1) PROXIMITY — items closer together are perceived as a group
# =========================================================
n_per <- 70
clusterA <- tibble(x = rnorm(n_per, -1.2, 0.25), y = rnorm(n_per,  0.1, 0.25), grp="A")
clusterB <- tibble(x = rnorm(n_per,  1.2, 0.25), y = rnorm(n_per, -0.1, 0.25), grp="B")
midPts   <- tibble(x = rnorm(35, 0, 0.9), y = rnorm(35, 0, 0.9), grp="mid")

dat_prox <- bind_rows(clusterA, clusterB, midPts)

p_prox <- ggplot(dat_prox, aes(x, y)) +
  geom_point(size=2.6, alpha=0.9) +
  coord_equal(xlim=c(-2.5,2.5), ylim=c(-2.0,2.0)) +
  labs(title="Gestalt: Proximity",
       subtitle="Closer dots appear grouped (left & right clusters vs middle)") +
  base_void
# =========================================================
# 2) SIMILARITY — similar items (shape/colour) group together
# =========================================================
grid <- expand.grid(ix=1:10, iy=1:8) %>% as_tibble()
dat_sim <- grid %>%
  mutate(x = ix, y = iy,
         colgrp = ifelse(ix %% 2 == 0, "A", "B"),
         shapegrp = ifelse(iy %% 2 == 0, 21, 24))

p_sim <- ggplot(dat_sim, aes(x, y)) +
  geom_point(aes(fill = colgrp, shape = factor(shapegrp)), size=4, colour="black") +
  scale_shape_manual(values = unique(dat_sim$shapegrp)) +
  scale_fill_manual(values=c("A"="#4C9BE8","B"="#E86C5B")) +
  coord_equal() +
  labs(title="Gestalt: Similarity",
       subtitle="Colour & shape create perceived columns/rows") +
  base_void
p_sim
save_plot(p_sim, "02_similarity")

# =========================================================
# 3) GOOD CONTINUATION — we follow smooth paths/curves
# =========================================================
t <- seq(-2*pi, 2*pi, length.out = 300)
curve1 <- tibble(x=t, y = sin(t), id="curve 1")
curve2 <- tibble(x=t, y = cos(t), id="curve 2")
dat_cont <- bind_rows(curve1, curve2)

p_cont <- ggplot(dat_cont, aes(x, y, colour=id)) +
  geom_path(size=2) +
  scale_colour_manual(values=c("#3E8E7E","#B85C38")) +
  coord_equal(xlim=c(-5,5), ylim=c(-1.6,1.6)) +
  labs(title="Gestalt: Good Continuation",
       subtitle="Intersecting smooth curves are perceived as continuous paths") +
  base_void + theme(legend.position = "none")
p_cont
save_plot(p_cont, "03_continuation")

# =========================================================
# 4) CLOSURE — incomplete shapes are perceived as whole
# (Kanizsa-style triangle using 'Pac-Man' disks)
# =========================================================
# helper to draw a 'pacman' sector as polygon
pacman <- function(cx, cy, r=1, angle=60, start=30, n=60) {
  ang <- seq((start+angle/2), (start-angle/2), length.out=n) * pi/180
  tibble(x = c(cx, cx + r*cos(ang)),
         y = c(cy, cy + r*sin(ang)))
}
# positions of 3 pacmen
pac1 <- pacman(-1, -0.7, r=1.4, angle=70, start=90)
pac2 <- pacman( 1, -0.7, r=1.4, angle=70, start=90)
pac3 <- pacman( 0,  1.1, r=1.4, angle=70, start=270)

triangle_hint <- tibble(
  x=c(-1.55, 1.55, 0), y=c(-0.2, -0.2, 1.7)
)

p_closure <- ggplot() +
  geom_polygon(data=pac1, aes(x,y), fill="black") +
  geom_polygon(data=pac2, aes(x,y), fill="black") +
  geom_polygon(data=pac3, aes(x,y), fill="black") +
  geom_polygon(data=triangle_hint, aes(x,y),
               fill=NA, colour="grey70", linewidth=0.6, linetype="dashed") +
  coord_equal(xlim=c(-2.5,2.5), ylim=c(-2.2,2.2)) +
  labs(title="Gestalt: Closure",
       subtitle="Three 'Pac-Man' disks imply a bright, upright triangle") +
  base_void
p_closure
save_plot(p_closure, "04_closure")

# =========================================================
# 5) COMMON REGION — items inside same boundary group together
# =========================================================
pts <- tibble(x = runif(120, 0, 10), y = runif(120, 0, 6))
rects <- tibble(xmin=c(1,5.4), xmax=c(4.6,9),
                ymin=c(1,1.2), ymax=c(4.8,4.6), grp=c("L","R"))

p_region <- ggplot() +
  geom_point(data=pts, aes(x,y), size=2) +
  geom_rect(data=rects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            fill=NA, colour="#2D2D2D", linewidth=1.2) +
  coord_equal(xlim=c(0,10), ylim=c(0,6)) +
  labs(title="Gestalt: Common Region",
       subtitle="Points within drawn regions are perceived as groups") +
  base_void
save_plot(p_region, "05_common_region")

# =========================================================
# 6) CONNECTEDNESS — connected elements are grouped
# =========================================================
pairs <- tibble(
  x1 = rep(seq(1,9,by=2), each=1),
  y1 = runif(5, 1.2, 4.8),
  x2 = x1 + 0.8,
  y2 = y1 + rnorm(5, 0, 0.2)
)
points_conn <- pairs %>%
  pivot_longer(cols = everything(), names_to = c(".value","end"),
               names_pattern="(x|y)([12])") %>%
  mutate(id = rep(1:nrow(pairs), each=2))

p_conn <- ggplot() +
  geom_segment(data=pairs, aes(x=x1,y=y1,xend=x2,yend=y2),
               linewidth=1.2, lineend="round") +
  geom_point(data=points_conn, aes(x,y), size=3.5) +
  coord_equal(xlim=c(0.5,10), ylim=c(0.5,6)) +
  labs(title="Gestalt: Connectedness",
       subtitle="Paired dots linked by lines read as units") +
  base_void
save_plot(p_conn, "06_connectedness")

# =========================================================
# 7) FIGURE–GROUND — foreground emerges against background
# (Rubin vase style silhouette)
# =========================================================
# Build symmetric profile curve and mirror it
yy  <- seq(-3, 3, length.out = 300)
face <- tibble(
  x = 0.6 + 0.25*sin(yy*1.2) + 0.1*cos(yy*2.6) + 0.02*yy,
  y = yy
)
vase_left  <- face %>% mutate(x = -x)
vase_right <- face

p_fg <- ggplot() +
  geom_ribbon(data=bind_rows(vase_left %>% mutate(side="L"),
                             vase_right %>% mutate(side="R")),
              aes(ymin=-3, ymax=y, x=x, fill=side), alpha=1) +
  scale_fill_manual(values=c("L"="black","R"="black")) +
  geom_polygon(data=tibble(x=c(-0.05,0.05,0.05,-0.05), y=c(-3,-3,3,3)),
               aes(x,y), fill="white") +  # central vase
  coord_equal(xlim=c(-1.2,1.2), ylim=c(-3,3)) +
  labs(title="Gestalt: Figure–Ground",
       subtitle="Ambiguous Rubin vase: faces (black) vs vase (white)") +
  base_void + theme(legend.position="none")
save_plot(p_fg, "07_figure_ground")

# =========================================================
# 8) SYMMETRY — symmetric arrangements are perceived as grouped
# =========================================================
sym_pts <- tibble(
  x = c(-3,-2,-1,1,2,3, -2,2, -1,1, 0,0),
  y = c( 0, 1, 0,0,1,0,  -1,-1,  2,2, -2,2),
  grp = c(rep("pairs",10), rep("axis",2))
)

p_sym <- ggplot(sym_pts, aes(x,y)) +
  geom_point(data=subset(sym_pts, grp=="pairs"), size=4) +
  geom_segment(aes(x=-0.05,y=-2.5,xend=-0.05,yend=2.5), linewidth=1) +
  geom_segment(aes(x= 0.05,y=-2.5,xend= 0.05,yend=2.5), linewidth=1) +
  coord_equal(xlim=c(-3.5,3.5), ylim=c(-3,3)) +
  labs(title="Gestalt: Symmetry",
       subtitle="Symmetric pairs around a central axis appear grouped") +
  base_void
save_plot(p_sym, "08_symmetry")

# =========================================================
# 9) PRÄGNANZ (Good Form) — simplest, most stable shape emerges
# (Square implied among noise)
# =========================================================
noise <- tibble(x = runif(350,-3.5,3.5), y = runif(350,-3.5,3.5))
square <- expand.grid(x=seq(-2,2,length.out=40),
                      y=c(-2,2)) %>% as_tibble() %>%
  bind_rows(expand.grid(y=seq(-2,2,length.out=40),
                        x=c(-2,2)) %>% as_tibble()) %>%
  mutate(kind="square")
square_pts <- bind_rows(noise %>% mutate(kind="noise"), square)

p_prag <- ggplot(square_pts, aes(x,y)) +
  geom_point(data = subset(square_pts, kind=="noise"), alpha=0.6, size=1.8) +
  geom_point(data = subset(square_pts, kind=="square"), size=2.6) +
  coord_equal(xlim=c(-3.8,3.8), ylim=c(-3.8,3.8)) +
  labs(title="Gestalt: Prägnanz (Good Form)",
       subtitle="A clean square pops out from random dots") +
  base_void
save_plot(p_prag, "09_pragnanz")

# =========================================================
# 10) COMMON FATE (ANIMATION) — elements moving together group
# =========================================================
# Two groups of dots: one moves coherently, the other jitters randomly
library(gganimate) # ensure loaded for transition
nA <- 30; nB <- 40; T <- 40
A0 <- tibble(id = paste0("A",1:nA), x0 = runif(nA, -4,-1), y0 = runif(nA, -2,2))
B0 <- tibble(id = paste0("B",1:nB), x0 = runif(nB,  1, 4), y0 = runif(nB, -2,2))

# coherent motion for A (same dx, dy each frame)
dxA <- 0.09; dyA <- 0.02
A <- expand_grid(A0, t=0:T) %>%
  mutate(x = x0 + dxA*t, y = y0 + dyA*t, grp="coherent")

# random jitter for B
B <- expand_grid(B0, t=0:T) %>%
  mutate(x = x0 + cumsum(rnorm(n(), 0, 0.03)),
         y = y0 + cumsum(rnorm(n(), 0, 0.03)),
         grp="jitter")

dat_fate <- bind_rows(A %>% select(id, x, y, t, grp),
                      B %>% select(id, x, y, t, grp))

p_fate <- ggplot(dat_fate, aes(x, y, group=id)) +
  geom_point(aes(shape=grp), size=2.8) +
  scale_shape_manual(values=c("coherent"=16,"jitter"=1)) +
  coord_equal(xlim=c(-5,5), ylim=c(-3,3)) +
  labs(title="Gestalt: Common Fate",
       subtitle="Dots that move together are perceived as a group",
       caption="Filled = coherent group, Open = random jitter") +
  base_void +
  transition_time(t) +
  ease_aes("linear")

anim_file <- file.path(out_dir, "10_common_fate.gif")
animate(p_fate, nframes = 80, fps = 20, width = 600, height = 400, renderer = gifski_renderer(anim_file))
# (The GIF is saved to gestalt_outputs/10_common_fate.gif)

# -------------------------------
# Montage (optional): quick 3x3 grid of the first nine
# -------------------------------
library(patchwork)
montage <- p_prox + p_sim + p_cont +
  p_closure + p_region + p_conn +
  p_fg + p_sym + p_prag +
  plot_layout(ncol=3)
save_plot(montage, "00_montage", w=12, h=12, dpi=180)

message("Done! Check the 'gestalt_outputs' folder for PNGs and the common-fate GIF.")


# =========================================================
# 11) FOCAL POINT — one element stands out
# =========================================================
# Uniform grid of grey circles with one red standout
grid_fp <- expand.grid(x=1:10, y=1:10) %>% as_tibble()
grid_fp <- grid_fp %>%
  mutate(col = ifelse(x==5 & y==6, "focal", "bg"),
         size = ifelse(col=="focal", 6, 3))

p_focal <- ggplot(grid_fp, aes(x, y)) +
  geom_point(aes(size=size, fill=col), shape=21, colour="black") +
  scale_fill_manual(values=c("bg"="grey70", "focal"="red")) +
  scale_size_identity() +
  coord_equal() +
  labs(title="Gestalt: Focal Point",
       subtitle="One element that differs strongly in colour/size captures attention") +
  base_void + theme(legend.position="none")

save_plot(p_focal, "11_focal_point")



############### tufte ##################################
# =========================================================
# 3.1 GRAPHICAL INTEGRITY — proportional representation
# =========================================================
library(ggplot2)
library(dplyr)
library(patchwork)

dat_vals <- tibble(category=c("A","B","C"), value=c(10,30,50))

# Correct bar chart
p_int_correct <- ggplot(dat_vals, aes(category, value)) +
  geom_col(fill="#4C9BE8") +
  labs(title="Accurate Bars (proportional)") +
  theme_minimal(base_size=12)

# Misleading: exaggerate bar heights by scaling
dat_wrong <- dat_vals %>% mutate(value = value * c(10,3,1)) # mis-scaled
p_int_wrong <- ggplot(dat_wrong, aes(category, value)) +
  geom_col(fill="#E86C5B") +
  labs(title="Misleading Bars (lie factor > 1)") +
  theme_minimal(base_size=12)

p_integrity <- p_int_correct + p_int_wrong + plot_layout(ncol=2)
save_plot(p_integrity, "31_graphical_integrity", w=8, h=4)

# =========================================================
# 3.2 DATA-INK RATIO — chartjunk vs minimalism
# =========================================================
dat_line <- tibble(x=1:12, y=cumsum(rnorm(12,0.5,1)))

# Minimalist
p_clean <- ggplot(dat_line, aes(x,y)) +
  geom_line(size=1, colour="#4C9BE8") +
  theme_minimal(base_size=12) +
  labs(title="High Data-Ink Ratio")

# Chartjunk: grid, shading, colours, extra labels
p_junk <- ggplot(dat_line, aes(x,y)) +
  geom_area(fill="orange", alpha=0.4) +
  geom_line(size=2, colour="darkred") +
  geom_point(size=3, colour="black") +
  annotate("text", x=6, y=max(dat_line$y)+1,
           label="Overdecorated!", colour="red", fontface="bold") +
  theme_dark(base_size=12) +
  labs(title="Low Data-Ink Ratio (Chartjunk)")

p_dataink <- p_clean + p_junk + plot_layout(ncol=2)
save_plot(p_dataink, "32_data_ink_ratio", w=8, h=4)

# =========================================================
# 3.3 SMALL MULTIPLES — consistent panels
# =========================================================
set.seed(123)
dat_multi <- tibble(
  year=rep(2000:2009, 3),
  value=cumsum(rnorm(30,0.5,1)),
  country=rep(c("Country A","Country B","Country C"), each=10)
)

p_multiples <- ggplot(dat_multi, aes(year,value)) +
  geom_line(colour="#4C9BE8") +
  facet_wrap(~country, ncol=3) +
  theme_minimal(base_size=12) +
  labs(title="Small Multiples: Time Series by Country")

save_plot(p_multiples, "33_small_multiples", w=8, h=3.5)

# =========================================================
# 3.4 AVOID CHARTJUNK — clarity vs 3D clutter
# =========================================================
dat_pie <- tibble(cat=c("A","B","C","D"), val=c(10,20,30,40))

# Clean flat pie
p_pie_clean <- ggplot(dat_pie, aes(x="", y=val, fill=cat)) +
  geom_col(width=1, colour="white") +
  coord_polar(theta="y") +
  labs(title="Clean Flat Pie") +
  theme_void() + theme(legend.position="bottom")

# "3D effect" pie (faked with stacked bars + coord_polar)
p_pie_junk <- ggplot(dat_pie, aes(x="", y=val, fill=cat)) +
  geom_col(width=1, colour="black", size=0.3, alpha=0.9) +
  coord_polar(theta="y") +
  annotate("text", x=1, y=50, label="3D Pie Junk!", colour="red", fontface="bold") +
  labs(title="Decorated (chartjunk)") +
  theme_void() + theme(legend.position="bottom")

p_chartjunk <- p_pie_clean + p_pie_junk + plot_layout(ncol=2)
save_plot(p_chartjunk, "34_avoid_chartjunk", w=8, h=4)

# =========================================================
# Montage of all 4
# =========================================================
montage_tufte <- p_integrity / p_dataink / p_multiples / p_chartjunk
save_plot(montage_tufte, "30_tufte_principles", w=8, h=10, dpi=180)


dat_vals <- tibble(category=c("A","B","C"), value=c(22,30,50))

p_truth <- ggplot(dat_vals, aes(category, value)) +
  geom_col(fill="#4C9BE8") +
  labs(title="Accurate: Axis starts at 0") +
  theme_minimal()

p_lie <- ggplot(dat_vals, aes(category, value)) +
  geom_col(fill="#E86C5B") +
  coord_cartesian(ylim=c(20,50)) + # truncates y-axis
  labs(title="Misleading: Axis truncated") +
  theme_minimal()
pboth <- p_truth + p_lie + plot_layout(ncol=2)
pboth


library(ggplot2)
library(dplyr)

set.seed(123)
dat_line <- tibble(x=1:12, y=cumsum(rnorm(12,0.5,1)))

# Clean minimalist chart
p_clean <- ggplot(dat_line, aes(x,y)) +
  geom_line(colour="#4C9BE8", size=1.2) +
  labs(title="Clean Minimalist Chart") +
  theme_minimal(base_size=12)

# Chartjunk: cluttered with unnecessary "ink"
p_junk <- ggplot(dat_line, aes(x,y)) +
  geom_area(fill="orange", alpha=0.3) +
  geom_line(colour="darkred", size=2) +
  geom_point(size=5, shape=21, fill="yellow", colour="black") +
  annotate("text", x=6, y=max(dat_line$y)+1,
           label="Look at this!", colour="red", fontface="bold", size=5) +
  theme_classic(base_size=12) +
  theme(panel.background = element_rect(fill="lightblue"),
        plot.background = element_rect(fill="lightgrey"),
        panel.grid.major = element_line(colour="black", linetype="dotted"),
        axis.text = element_text(size=14, colour="purple", face="bold"),
        axis.title = element_text(size=16, face="italic")) +
  labs(title="Chartjunk: Too Much Decoration")

# Save side by side
library(patchwork)
pj <- p_clean + p_junk + plot_layout(ncol=2)
ggsave("chartjunk_example.png", pj, width=10, height=4, dpi=200)
