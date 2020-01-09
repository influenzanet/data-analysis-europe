# conf.R
source("../system.R")

i18n_load(platform_path("defaults/i18n"), language = 'en')

i18n_load(
  platform_path("i18n", platform = TRUE)
)

sub.text = i18n("platform.copyright")

