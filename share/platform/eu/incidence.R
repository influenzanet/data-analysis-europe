##
# Incidence parameters for eu computation
##

design.type = "age"

estimator.params = list(
  'eu'=list(active.week.before=1, active.week.after=1, active.min.surveys=2, exclude.same=T,ignore.first.delay=6, ignore.first.only.new=T)
)

age.categories = c(0, 21, 65, 200)
