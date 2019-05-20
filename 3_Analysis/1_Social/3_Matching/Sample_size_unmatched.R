

sample.size.MPA.yr.tr <- match.covariate %>%
  group_by(MPAID,MonitoringYear,Treatment) %>%
  summarise (sample.size = n())


sample.size.yr.tr <- match.covariate %>%
  group_by(MonitoringYear,Treatment) %>%
  summarise (sample.size = n())