library(maps)
library(ggplot2)
library(gganimate)

setwd("/Users/soskuthy/Documents/Research/current/2018/nz_vowels/icphs/talk_graphs")


cairo_pdf("nz.pdf",width=10,height=10)
par(bg="#EAF0EF", mar=c(0,0,0,0))
map('nz', fill=T, col="#24485D", border="#24485D")
dev.off()

xs <- seq(-10,10,0.01)
f1.1 <- 1 / (1 + exp(-xs))
f1.2 <- 0.2 + (1 / (1 + exp(-xs))) * 0.6
f1.3 <- c()
ts <- c(rep(0, 4),
        seq(0,4,length.out=26),
        rep(4, 4),
        seq(4,0,length.out=26))
for (t in ts) {
  f1.3 <- c(f1.3, 1 / (1 + exp(-(xs-t))))
}

f1.4 <- c()
ts <- c(rep(1, 4),
        seq(1,4,length.out=26),
        rep(4, 4),
        seq(4,1,length.out=26))
for (t in ts) {
  f1.4 <- c(f1.4, 1 / (1 + exp(-(xs/t))))
}

vertical <- data.frame(x=rep(xs,2), y=c(f1.1,f1.2), state=rep(c(0,1), each=length(xs)))
horizontal.1 <- data.frame(x=rep(xs,60), y=f1.3, state=rep(1:60, each=length(xs)))
horizontal.2 <- data.frame(x=rep(xs,60), y=f1.4, state=rep(1:60, each=length(xs)))

p <- ggplot(vertical, aes(x=x, y=y)) +
  geom_line(lwd=3, col="#24485D") +
  transition_states(state, transition_length=c(0.2,0.2), state_length=0.04, wrap=T) +
  theme_void() +
  theme(panel.background=element_rect(fill="#EAF0EF"),
        plot.background=element_rect(fill="#EAF0EF"))
animate(p, nframes=60, fps=24, renderer=av_renderer(file="horizontal_ex.mov"))


p <- ggplot(horizontal.1, aes(x=x, y=y)) +
  geom_line(lwd=3, col="#24485D") +
  transition_time(state) +
  theme_void() +
  theme(panel.background=element_rect(fill="#EAF0EF"),
        plot.background=element_rect(fill="#EAF0EF"))
animate(p, nframes=60, fps=24, renderer=av_renderer(file="horizontal_1_ex.mov"))

p <- ggplot(horizontal.2, aes(x=x, y=y)) +
  geom_line(lwd=3, col="#24485D") +
  transition_time(state) +
  theme_void() +
  theme(panel.background=element_rect(fill="#EAF0EF"),
        plot.background=element_rect(fill="#EAF0EF"))
animate(p, nframes=60, fps=24, renderer=av_renderer(file="horizontal_2_ex.mov"))

price_vd_sub <- filter(price_vd, speaker %in% sample(unique(speaker), 16, replace=F))
price_vd_sub$spe <- paste0("spe", as.numeric(as.factor(price_vd_sub$speaker)))

ggplot(price_vd_sub, aes(x=measurement_no, y=f1, group=id)) +
  facet_wrap(~spe) +
  geom_line(alpha=0.2, col="#24485D") +
  geom_line(aes(y=f2), alpha=0.2, col="#24485D") +
  theme_void() + 
  theme(panel.background=element_rect(fill="#EAF0EF"),
        plot.background=element_rect(fill="#EAF0EF"),
        strip.text=element_blank())
ggsave("rubbish_examples.png", width=6, height=6)
