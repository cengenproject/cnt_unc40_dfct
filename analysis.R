library(tidyverse)

dat <- readxl::read_excel("count_defects.xlsx",
                          col_types = c("text","text","text",
                                        "numeric", "numeric", "numeric", "numeric")) |>
  mutate(day = as.factor(day),
         genotype = factor(genotype,
                           levels = c("zdIs5", "wp221;zdIs5", "e271;zdIs5")),
         worm = as.factor(worm))


dat |>
  summarize(prop_PLM_loop = mean(PLMloop>0),
            prop_any_other = mean(other > 0),
            prop_avm = mean(AVM > 0),
            .by = genotype)

dat |>
  summarize(prop_PLM_loop = mean(PLMloop>0),
            prop_any_other = mean(other > 0),
            prop_avm = mean(AVM > 0),
            .by = c(genotype, day)) |>
  ggplot() +
  theme_classic() +
  geom_col(aes(x = genotype, fill = day,
               y = prop_avm),
           position = "dodge")


dat |>
  summarize(n_defects = sum(AVM > 0),
            n_worms = n(),
            perc_defects = 100 * n_defects / n_worms,
            .by = genotype) |>
  mutate(100*map2_dfr(n_defects, n_worms,
                  ~ {
                    binom.test(x = .x, n = .y)$conf.int |> setNames(c("lower", "upper"))}
                  )) |>
  ggplot(aes(x = genotype, y = perc_defects)) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 40)) +
  scale_fill_manual(values = c("grey90", "grey60", "grey35")) +
  scale_x_discrete(labels = c("Wild type",
                              bquote(italic("unc-40("*Delta*"14.5)")),
                              bquote(italic("unc-40(e271)")))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  ylab("% animals with AVM guidance defect") +
  xlab(bquote(P[AVM]*"::GFP")) +
  geom_col(aes(fill = genotype),
           color = 'black') +
  geom_text(aes(label = n_worms, y = upper),
            nudge_y = 1.5) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                width = .1)

# ggsave("avm_defects.pdf",
#        width = 8, height = 10, units = "cm")


#~ Test ----


mat <- dat |>
  summarize(n_defects = sum(AVM > 0),
            n_worms = n(),
            perc_defects = 100 * n_defects / n_worms,
            .by = genotype) |>
  mutate(genotype = genotype,
         n_success = n_defects,
         n_failure = n_worms - n_defects,
         .keep = "none") |>
  column_to_rownames("genotype") |>
  as.matrix()

mat  

pvals <- c(prop.test(mat[-3,])$p.value,
           prop.test(mat[-1,])$p.value,
           prop.test(mat[-2,])$p.value) |>
  setNames(c("WT-e271", "delta-e271", "WT-delta"))

p.adjust(pvals)




