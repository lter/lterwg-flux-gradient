


sample.month %>% filter( gas == "CO2") %>% ggplot() + geom_point( aes(x= Month, y = Percent.Coverage.Month, col = approach, size = approach), alpha=0.3) + facet_wrap(~dLevelsAminusB )

sample.diel %>% filter( gas == "CO2") %>% ggplot() + geom_point( aes(x= hour.local, y = Percent.Coverage.Diel, col = approach, size = approach), alpha=0.3) + facet_wrap(~dLevelsAminusB )

