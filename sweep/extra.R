# Extra figure: share of mutualism / parasitism along soil P S (fine bins), with sampled U_0 vs the same sets with U_0 = 0
suppressMessages({ library(data.table); library(ggplot2) })
D <- fread("csv/samples_tab1.csv"); br <- c(0.5, 1, 1.5, 2, 3, 4, 5, 6, 8, 10, 13, 16, 20, 25, 30, 40, 50, 60)
D[, bin := cut(S, br, include.lowest = TRUE, right = FALSE)]; D[, mid := (br[-length(br)] + diff(br) / 2)[as.integer(bin)]]
P <- rbind(D[, .(pct = 100 * mean(class == "mutualism"), what = "mutualism", model = "U_0 sampled (0–0.8)"), by = mid],
           D[, .(pct = 100 * mean(class_U0zero == "mutualism"), what = "mutualism", model = "same sets, U_0 = 0"), by = mid],
           D[, .(pct = 100 * mean(class == "parasitism"), what = "parasitism", model = "U_0 sampled (0–0.8)"), by = mid],
           D[, .(pct = 100 * mean(class_U0zero == "parasitism"), what = "parasitism", model = "same sets, U_0 = 0"), by = mid])
fwrite(P[order(what, model, mid)], "csv/tab1_share_along_S.csv")
g <- ggplot(P, aes(mid, pct, colour = what, linetype = model)) + geom_line(linewidth = 1) + geom_point(size = 1.4) +
  scale_colour_manual(values = c(mutualism = "#1a9850", parasitism = "#d7301f")) + scale_x_log10() + facet_wrap(~ what, scales = "free_y") +
  labs(x = "soil P, S (log scale)", y = "% of parameter sets", colour = NULL, linetype = NULL,
       title = "tab1: outcome share along soil P (all other parameters sampled)") + theme_minimal(base_size = 11) + theme(legend.position = "bottom")
ggsave("figs/tab1_share_along_S_U0.png", g, width = 8, height = 4, dpi = 130)
print(dcast(P, mid ~ what + model, value.var = "pct"), digits = 3)
