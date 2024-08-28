
# First plot: Gas Production Volume
#p1 <- 
ggplot(top_basins, aes(x = reorder(Basin, Gas_Production_MCF), y = Gas_Production_MCF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Basin", y = "Gas Production Volume (MCF)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  theme_NAG_publication() +
  coord_flip()

# Second plot: Carbon Density
#p2 <- 
top_basins %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_field_mid_em) %>%
  pivot_longer(
    cols = c("Upstream", "Midstream"),
    names_to = "component",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = reorder(Basin, Gas_Production_MCF),y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  labs(x = "Basin", y = "Carbon Density (CI)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_NAG_publication() +
  coord_flip()

# Combine plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)






## f3 CI, gas production volume ----


df_v %>%
  arrange(desc(Gas_Production_MCF)) %>%
  slice_head(n = 15) %>%
  rename(Upstream = weighted_upstream_CI, Midstream = weighted_field_mid_em) %>%
  pivot_longer(
    cols = c("Upstream", "Midstream"),
    names_to = "component",
    values_to = "value"
  ) %>% 
  filter(!is.na(value)) %>%
  ggplot(aes(x = reorder(Basin, Gas_Production_MCF), y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  geom_bar(aes(x = reorder(Basin, Gas_Production_MCF), y = -Gas_Production_MCF), stat = "identity", fill = "#F47560") +
  theme_minimal() +
  xlab("AAPG Basin") +
  ylab("Gas Production [MCF]   Carbon Intensity [gCO2e/MJ]") +
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  annotate("text", x = Inf, y = 0, label = "CI", vjust = -1, hjust = 1, color = "forestgreen") +
  scale_y_continuous(labels = abs, sec.axis = sec_axis(~.*-1)) +
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  labs(x = "Basin", y = "Volume-weighted average CI (gCO2e MJ^-1)", fill = "Component") +
  scale_fill_manual(values = c("Upstream" = "#2B61FF", "Midstream" = "orange")) +
  theme_NAG_publication() + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.grid = element_blank()
  ) + theme(legend.position = c(0.10, 0.10), # You can adjust these values as needed
            legend.justification = c("left", "bottom")) 
# + annotate("text", x = 38, y = 150, label = "U.S. volume weighted average CI \n 15.7 gCO2e MJ^-1") 
# + annotate("segment", x = 37.5, y = 45, xend = 37.5, yend =20, arrow = arrow(type = "closed", length = unit(0.02, "npc")))




## f3 CI by basins (all basins) ----
df_v %>%
  rename(Upstream = weighted_CI, Midstream = weighted_field_mid_em) %>%
  pivot_longer(
    cols = c("Upstream", "Midstream"),
    names_to = "component",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = reorder(Basin, Gas_Production), y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  coord_flip() +
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5) +
  labs(x = "Basin", y = expression("Volume-weighted average CI (gCO"[2]*"e MJ"^"-1"*")"), fill = "Component") +
  scale_fill_manual(values = c("Upstream" = "#2B61FF", "Midstream" = "orange")) +
  #theme_minimal() +
  theme_Publication() + 
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.grid = element_blank()
  ) + theme(legend.position = c(0.95, 0.94), # You can adjust these values as needed
            legend.justification = c("right", "top")) +
  annotate("text", x = 12, y = 150, label = expression("U.S. volume weighted average CI" ~ 15.38 ~ g * CO[2] * e ~ MJ^-1)) +
  annotate("segment", x =12, y = 60, xend = 12, yend =20,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))

## f3 CI by basins (top 15 basins) ----
df_v %>%
  arrange(desc(Gas_Production)) %>%
  slice_head(n = 15) %>%
  rename(Upstream = weighted_CI, Midstream = weighted_field_mid_em) %>%
  pivot_longer(
    cols = c("Upstream", "Midstream"),
    names_to = "component",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = reorder(Basin, Gas_Production), y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_errorbar(aes(ymin = lo, ymax = up), width = 0.2) +
  coord_flip() +
  labs(x = "Basin", y = "Volume-weighted average CI (gCO2e MJ^-1)", fill = "Component") +
  scale_fill_manual(values = c("Upstream" = "#2B61FF", "Midstream" = "orange")) +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5),
    panel.grid = element_blank()
  ) + theme(legend.position = c(0.98, 0.98), # You can adjust these values as needed
            legend.justification = c("right", "top")) +
  geom_hline(yintercept = total_mean, linetype = "dotted", color = "black", size = 0.5)
#+ annotate("text", x = 10, y = 23, label = expression("U.S. volume weighted average CI \n" ~ 15.38 ~ g * CO[2] * e ~ MJ^-1)) +
#annotate("segment", x = 10, y = 17, xend = 10, yend =16, arrow = arrow(type = "closed", length = unit(0.02, "npc")))



