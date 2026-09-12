
library(data.table)
library(ggplot2)
library(ggbrace)

## time trend plot
plotdata <- data.frame(
  time = rep(1:1000, 4),
  p = c(rep(0.000008508646, 1000), p_Y=fbasehaz_Y2(1:1000),
        rep(0.0002503218, 1000), p_A=fbasehaz_A2(1:1000)),
  kind = rep(c("h_Y(t)", "h_A(t)"), each=2000),
  scenario = c(rep("Scenario 1", 1000), rep("Scenario 2", 1000),
               rep("Scenario 1", 1000), rep("Scenario 2", 1000))
)

ggplot(plotdata, aes(x=time, y=p, color=kind, linetype=scenario)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="bottom") +
  labs(x="t", y="Baseline Hazard", color=NULL, linetype=NULL) +
  scale_y_continuous(labels=label_number()) +
  scale_color_discrete(labels=parse(text=c("h[Y0](t)", "h[A0](t)"))) +
  scale_linetype_manual(values=c("dashed", "solid"),
                        labels=c("Scenario 1", "Scenario 2"))
ggsave("./simulation/plots/time_trends.pdf", width=6, height=4)

plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual b", "Individual a")),
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
  labs(x="Time", y="", shape="", color="")
ggsave("./simulation/plots/pair_example.pdf", width=6, height=4)


## partially valid pair
plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual b", "Individual a")),
                       A=factor(c("Unexposed","Exposed","Unexposed","Exposed")),
                       time=c(100,100,140,140),
                       time_end=c(120,120,160,160))

plotdata2 <- data.table(id=factor(c(2,1), levels=c(2, 1),
                                  labels=c("Individual b", "Individual a")),
                        time=c(120,120),
                        time_end=c(140,140))

plotdata3 <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                  labels=c("Individual b", "Individual a")),
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
  labs(x="Time", y="", shape="", color="")
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
  labs(x="Time", y="", shape="", color="")
ggsave("./simulation/plots/pair_overlap.pdf", width=7, height=4.5)


plotdata <- data.table(id=factor(c(2,1,1,2), levels=c(2, 1),
                                 labels=c("Individual b", "Individual a")),
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
  labs(x="Time", y="", shape="", color="")
ggsave("./simulation/plots/pair_censor_valid.pdf", width=7, height=4.5)

## invalid pair with censoring
plotdata <- data.table(id=factor(c(2,1,2), levels=c(2, 1),
                                 labels=c("Individual b", "Individual a")),
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
  labs(x="Time", y="", shape="", color="")
ggsave("./simulation/plots/pair_censor_invalid.pdf", width=7, height=4.5)


plotdata <- data.table(id=factor(c(2,1,1,2,1,2), levels=c(2, 1),
                                 labels=c("Individual b", "Individual a")),
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
  labs(x="Time", y="", shape="", color="")
ggsave("./simulation/plots/pair_multiple.pdf", width=7, height=4.5)
