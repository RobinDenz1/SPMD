
library(data.table)
library(ggplot2)
library(ggbrace)
library(scales)

## time trend plot
plotdata <- data.frame(
  time = rep(1:1000, 4),
  p = c(fbasehaz_Y1(1:1000), p_Y=fbasehaz_Y2(1:1000),
        fbasehaz_A1(1:1000), p_A=fbasehaz_A2(1:1000)),
  kind = rep(c("h_Y(t)", "h_A(t)"), each=2000),
  scenario = c(rep("Scenario 1", 1000), rep("Scenario 2", 1000),
               rep("Scenario 1", 1000), rep("Scenario 2", 1000))
)

ggplot(plotdata, aes(x=time, y=p, color=kind, linetype=scenario)) +
  geom_line(linewidth=0.85) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="t", y="Baseline Hazard", color=NULL, linetype=NULL) +
  scale_y_continuous(labels=label_number()) +
  scale_color_manual(
    values = c("h_Y(t)" = "#C46A50", "h_A(t)" = "#35658A"),
    breaks = c("h_Y(t)", "h_A(t)"),
    labels = c(expression(h[Y0](t)), expression(h[A0](t)))
  ) +
  scale_linetype_manual(values=c("dashed", "solid"),
                        labels=c("Scenario 1", "Scenario 2"))
ggsave("./simulation/plots/time_trends.pdf", width=6, height=4)

## time trend plot appendix
plotdata <- data.frame(
  time = rep(1:1000, 4),
  p = c(fbasehaz_Y3.1(1:1000), fbasehaz_Y3.2(1:1000)),
  half = factor(rep(c(0, 1), each=1000))
)

ggplot(plotdata, aes(x=time, y=p, color=half)) +
  geom_line(linewidth=0.85) +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="t", y="Baseline Hazard", color=NULL, linetype=NULL) +
  scale_y_continuous(labels=label_number()) +
  scale_color_manual(
    values = c("0" = "#C46A50", "1" = "#35658A"),
    breaks = c("0", "1"),
    labels = c("First Half", "Second Half")
  )
ggsave("./simulation/plots/time_trends_scenario3.pdf", width=6, height=4)

## main symmetric pair illustration plot
plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual a", "Individual b")),
                       A=factor(c("Unexposed","Exposed","Unexposed","Exposed")),
                       time=c(50,50,120,120),
                       time_end=c(50,50,120,120) + 40)

## best pair
ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=50, xmax=90, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  # comparison label
  annotate("text", x=70, y=Inf, label=expression(paste("Period after ", t[1])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  annotate("text", x=140, y=Inf, label=expression(paste("Period after ", t[2])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  # j label
  annotate("text", x=70, y="Individual a", label=expression(lambda[a1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=70, y="Individual b", label=expression(lambda[b1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=140, y="Individual a", label=expression(lambda[a2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=140, y="Individual b", label=expression(lambda[b2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  # points and lines
  geom_hline(yintercept=c(1,2), linetype="dashed") +
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  geom_point(aes(x=time, shape=A, color=A), size=5) +
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 170, 20), limits=c(35, 170)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_example.pdf", width=6, height=4)


## partially valid pair
plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual a", "Individual b")),
                       A=factor(c("Unexposed","Exposed","Unexposed","Exposed")),
                       time=c(100,100,140,140),
                       time_end=c(120,120,160,160))

plotdata2 <- data.table(id=factor(c(2,1), levels=c(2, 1),
                                  labels=c("Individual a", "Individual b")),
                        time=c(120,120),
                        time_end=c(140,140))

plotdata3 <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                  labels=c("Individual a", "Individual b")),
                        A=factor(c("Unexposed","Exposed","Unexposed","Exposed")),
                        time=c(100,100,140,120),
                        time_end=c(120,120,160,160))

plotdata4 <- data.table(x=c(100, 120), y=c(1.38, 1.38))
plotdata5 <- data.table(x=c(100, 120), y=c(2.38, 2.38))
plotdata6 <- data.table(x=c(140, 160), y=c(1.38, 1.38))
plotdata7 <- data.table(x=c(140, 160), y=c(2.38, 2.38))

ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=100, xmax=140, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=140, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.3) +
  # comparison label
  annotate("text", x=120, y=Inf, label=expression(paste("Period after ", t[1])),
           vjust=1.5, family="serif", size=5) +
  annotate("text", x=140, y=Inf, label=expression(paste("Period after ", t[2])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  # j label
  annotate("text", x=110, y="Individual a", label=expression(lambda[a1]),
           vjust=2.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=110, y="Individual b", label=expression(lambda[b1]),
           vjust=2.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual a", label=expression(lambda[a2]),
           vjust=2.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual b", label=expression(lambda[b2]),
           vjust=2.5, family="serif", fontface="italic", size=4) +
  # dashed lines to the left and right
  annotate("segment", x=90, xend=120, y=1, yend=1, linetype="dashed") +
  annotate("segment", x=140, xend=Inf, y=1, yend=1, linetype="dashed") +
  annotate("segment", x=90, xend=120, y=2, yend=2, linetype="dashed") +
  annotate("segment", x=140, xend=Inf, y=2, yend=2, linetype="dashed") +
  # curly brackets
  stat_brace(data=plotdata4, aes(x=x, y=y), rotate=180, inherit.aes=FALSE,
             width=0.13, size=0.2) +
  stat_brace(data=plotdata5, aes(x=x, y=y), rotate=180, inherit.aes=FALSE,
             width=0.13, size=0.2) +
  stat_brace(data=plotdata6, aes(x=x, y=y), rotate=180, inherit.aes=FALSE,
             width=0.13, size=0.2) +
  stat_brace(data=plotdata7, aes(x=x, y=y), rotate=180, inherit.aes=FALSE,
             width=0.13, size=0.2) +
  # main thick lines
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  geom_segment(data=plotdata2, aes(y=id, yend=id, x=time, xend=time_end),
               linewidth=1, linetype="dotted") +
  # points
  geom_point(data=plotdata3, aes(x=time, shape=A, color=A), size=5) +
  # aesthetics
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 170, 10), limits=c(85, 165)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_overlap.pdf", width=7, height=4.5)

ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=100, xmax=140, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=140, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.3) +
  # comparison label
  annotate("text", x=120, y=Inf, label=expression(paste("Period after ", t[1])),
           vjust=1.5, family="serif", size=5) +
  annotate("text", x=140, y=Inf, label=expression(paste("Period after ", t[2])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  # j label
  annotate("text", x=110, y="Individual a", label=expression(lambda[a1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=110, y="Individual b", label=expression(lambda[b1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual a", label=expression(lambda[a2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual b", label=expression(lambda[b2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  # main thick lines
  geom_hline(yintercept=c(1, 2), linetype="dashed") +
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  # points
  geom_point(data=plotdata3, aes(x=time, shape=A, color=A), size=5) +
  # aesthetics
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 170, 10), limits=c(85, 165)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_overlap.pdf", width=7, height=4.5)


plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual a", "Individual b")),
                       A=factor(c("Unexposed","Exposed","Unexposed","Exposed")),
                       time=c(50,50,120,120),
                       time_end=c(90,90,140,140))

## valid pair with censoring
ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=50, xmax=90, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=140, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.3) +
  # comparison label
  annotate("text", x=70, y=Inf, label=expression(paste("Period after ", t[1])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  annotate("text", x=140, y=Inf, label=expression(paste("Period after ", t[2])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  # j label
  annotate("text", x=70, y="Individual a", label=expression(lambda[a1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=70, y="Individual b", label=expression(lambda[b1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=130, y="Individual a", label=expression(lambda[a2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=130, y="Individual b", label=expression(lambda[b2]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  # points and lines
  geom_hline(yintercept=c(1,2), linetype="dashed") +
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  geom_point(aes(x=time, shape=A, color=A), size=5) +
  geom_point(data=NULL, aes(x=140, y=1), color="black", size=5, shape=4) +
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 170, 20), limits=c(35, 170)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_censor_valid.pdf", width=7, height=4.5)

## invalid pair with censoring
plotdata <- data.table(id=factor(c(2,1,2), levels=c(2, 1),
                                 labels=c("Individual a", "Individual b")),
                       A=factor(c("Unexposed","Exposed","Exposed")),
                       time=c(50,50,120),
                       time_end=c(80,80,120))

ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=50, xmax=90, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=120, xmax=120+40, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.3) +
  annotate("rect", xmin=80, xmax=90, ymin=-Inf, ymax=Inf,
           fill="red", alpha=0.3) +
  # comparison label
  annotate("text", x=70, y=Inf, label=expression(paste("Period after ", t[1])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  annotate("text", x=140, y=Inf, label=expression(paste("Period after ", t[2])),
           vjust=1.5, family="serif", fontface="italic", size=5) +
  # j label
  annotate("text", x=65, y="Individual a", label=expression(lambda[a1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=65, y="Individual b", label=expression(lambda[b1]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  # points and lines
  geom_hline(yintercept=c(1,2), linetype="dashed") +
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  geom_point(aes(x=time, shape=A, color=A), size=5) +
  geom_point(data=NULL, aes(x=80, y=2), color="black", size=5, shape=4) +
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 170, 20), limits=c(35, 170)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_censor_invalid.pdf", width=7, height=4.5)


plotdata <- data.table(id=factor(c(2,1,1,2,1,2), levels=c(2, 1),
                                 labels=c("Individual a", "Individual b")),
                       A=factor(c("Unexposed","Exposed","Unexposed","Exposed",
                                  "Exposed", "Unexposed")),
                       time=c(70,70,130,130,10,10),
                       time_end=c(70,70,130,130,10,10) + 40)

## pair with multiple exposures in a
ggplot(plotdata, aes(y=id)) +
  # rectangle background
  annotate("rect", xmin=70, xmax=110, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=130, xmax=170, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  annotate("rect", xmin=10, xmax=50, ymin=-Inf, ymax=Inf,
           fill="grey80", alpha=0.3) +
  # j label
  annotate("text", x=90, y="Individual a", label=expression(lambda[a50]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=90, y="Individual b", label=expression(lambda[b50]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual a", label=expression(lambda[a130]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=150, y="Individual b", label=expression(lambda[b130]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=30, y="Individual a", label=expression(lambda[a10]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  annotate("text", x=30, y="Individual b", label=expression(lambda[b10]),
           vjust=1.5, family="serif", fontface="italic", size=4) +
  # points and lines
  geom_hline(yintercept=c(1,2), linetype="dashed") +
  geom_segment(aes(y=id, yend=id, x=time, xend=time_end), linewidth=1) +
  geom_point(aes(x=time, shape=A, color=A), size=5) +
  theme_minimal() +
  theme(axis.text.y=element_text(family="serif", face="italic", size=14),
        legend.position="bottom") +
  scale_x_continuous(breaks=seq(0, 180, 20), limits=c(0, 180)) +
  labs(x="Time", y="", shape="", color="") +
  # point colors
  scale_color_manual(
    values = c("Exposed" = "#C46A50", "Unexposed" = "#35658A"),
    breaks = c("Exposed", "Unexposed")
  )
ggsave("./simulation/plots/pair_multiple.pdf", width=7, height=4.5)

## main results for the illustrative example

# data.frame of the main results
dat <- data.frame(
  method = c("CCO", "CTC", "SCCS", "SCCS Spline (5)",
             "SCCS Spline (15)", "SPM"),
  estimate = c(1.303, 0.973, 0.749, 0.822, 0.811, 0.815),
  se = c(0.080, 0.112, 0.054, 0.055, 0.057, 0.080),
  lower = c(1.113, 0.781, 0.674, 0.725, 0.725, 0.697),
  upper = c(1.525, 1.212, 0.832, 0.914, 0.907, 0.953)
)

dat$y <- 6:1
dat$group <- ifelse(dat$method %in% c("CCO", "CTC"), "CC",
                    ifelse(dat$method == "SPM", "SPM", "SCCS"))

dat$estimate_label <- sprintf("%.3f", dat$estimate)
dat$se_label <- sprintf("%.3f", dat$se)
dat$ci_label <- sprintf("[%.3f, %.3f]", dat$lower, dat$upper)

# forest plot coordinate transformation (logarithmic scale)
forest_left <- 54
forest_right <- 99
log_limits <- log(c(0.65, 1.60))

to_x <- function(x) {
  forest_left + (log(x) - log_limits[1]) /
    diff(log_limits) * (forest_right - forest_left)
}

dat$x <- to_x(dat$estimate)
dat$x_lower <- to_x(dat$lower)
dat$x_upper <- to_x(dat$upper)

# alternating row backgrounds
dat$shade <- "#FFFFFF"#ifelse(dat$y %% 2 == 0, "#F5F7FA", "#FFFFFF")

# column headers
headers <- data.frame(
  x = c(1.5, 35, 49, 76),
  label = c("Method", "SE", "95% CI", "Forest plot"),
  hjust = c(0, 1, 1, 0.5)
)

# axis ticks
axis_values <- c(0.70, 0.80, 1.00, 1.25, 1.50)

# colours
method_colours <- c(
  "CC" = "#64748B",
  "SCCS" = "#25827B",
  "SPM" = "#C46B46"
)
dat$point_fill <- unname(method_colours[dat$group])

p <- ggplot() +
  # row backgrounds
  geom_rect(data = dat, aes(xmin=0, xmax=100, ymin=y - 0.48,
                            ymax=y + 0.48, fill=shade), inherit.aes=FALSE) +
  # reference line and grid lines
  geom_segment(aes(x=to_x(1), xend=to_x(1), y=0.52, yend=6.48),
               colour="#475569", linewidth=0.75, linetype="dashed") +
  geom_segment(data=data.frame(x=to_x(axis_values[axis_values != 1])),
               aes(x=x, xend=x, y=0.52, yend=6.48),
               colour="#E5EAF0", linewidth=0.4) +
  # confidence intervals
  geom_segment(data=dat, aes(x=x_lower, xend=x_upper, y=y, yend=y,
                             colour=group),
               linewidth=1.15, lineend="round") +
  # point estimates
  geom_point(data=dat, aes(x=x, y=y, fill=point_fill), shape=21, colour="white",
             size=3.8, stroke=0.9) +
  # method names
  geom_text(data=dat, aes(x=1.5, y=y, label=method, fontface="plain"),
            hjust=0, size=3.5, colour="#263445") +
  # numerical estimates
  geom_text(data=dat, aes(x=26, y=y, label=estimate_label), hjust=1, size=3.3,
            colour="#263445") +
  geom_text(data=dat, aes(x=35, y=y, label=se_label), hjust=1, size=3.3,
            colour="#263445") +
  geom_text(data=dat, aes(x=49, y=y, label=ci_label), hjust=1, size=3.3,
            colour="#263445") +
  # column headers
  geom_text(data=headers, aes(x=x, y=7.05, label=label, hjust=hjust),
            fontface="bold", size=4.0, colour="#526174") +
  geom_text(aes(x=26, y=7.05, label="exp(hat(theta))"), parse=TRUE, hjust=1,
            fontface="bold", size=3.0, colour="#526174") +
  # separators between method groups
  geom_hline(yintercept=c(4.5, 1.5), colour="#DCE2E9", linewidth=0.55) +
  scale_fill_identity() +
  scale_colour_manual(values=method_colours) +
  scale_x_continuous(
    limits = c(0, 100),
    breaks = to_x(axis_values),
    labels = c("0.70", "0.80", "1.00", "1.25", "1.50"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks=NULL, expand=c(0, 0)) +
  coord_cartesian(xlim=c(0, 100), ylim=c(0.25, 7.4), clip="off") +
  labs(x = expression(exp(hat(theta))), y=NULL) +
  theme_minimal(base_size=11) +
  theme(
    axis.text.x = element_text(size=9.5, colour="#475569",
                               margin=margin(t = 7)),
    axis.title.x = element_text(size=10.5, colour="#475569", hjust=0.77,
                                margin=margin(t = 12)),
    axis.ticks.x = element_line(colour="#94A3B8", linewidth=0.4),
    axis.ticks.length.x = unit(2, "mm"),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill="white", colour=NA),
    panel.background = element_rect(fill="white", colour=NA),
    plot.margin = margin(12, 16, 8, 10),
    legend.position = "none"
  )
p

ggsave("forest_plot.pdf", p, width=8.5, height=6, dpi=300, bg="white")
