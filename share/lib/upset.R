
# Symptoms grouped by topics
symptoms.groups = list(
  other=("sympt.other"),
  sensorial=c("loss.smell", "loss.taste", "nose.bleed"), 
  abdo=c("nausea", "vomiting", "diarrhea", "abdopain"),
  upper=c("rhino", "sneeze", "sorethroat", "wateryeye"),
  lower=c("cough", "dyspnea", "sputum", "chestpain"),
  fever=c("fever", "chills","pain", "headache"),
  weakness=c("asthenia", "anorexia")
)


#' Create a binay mask for each value
#' @param values character 
#' @return vector of integer value as binary mask for each input values, named by input value
create_binary_mask = function(values) {
  values.mask = c() # Binay mask for each values
  for(i in seq_along(values)) {
    n = values[i]
    values.mask = values.mask * 2L
    values.mask[n] = 1L
  }
  values.mask
}

#' Get correspondig label from a binary group value
#' @param data data.frame
#' @param mask integer vector binary mask for individual labels
#' @param group name of the column in data containing binary group value 
get_labels_from_binary = function(data, mask, group="g") {
  nn = names(mask)
  for(n in nn) {
    m = mask[n]
    data[[n]] = bitAnd(data[[group]], m) > 0
  }
  data$label = apply(data[, nn], 1, function(x) {
    paste(nn[which(x)], collapse = '+')
  })
  data
}

#' Create a binary group value from a set of logical columns. 
#' 
#' Intersection of columns will be encoded as a binary value (1 column => 1 bit)
#' 
#' @param data data.frame
#' @param mask binary mask for each column (see create_binary_mask)
#' @param column name of the column that will contain the group value
apply_binary_mask = function(data, mask, column) {
  data[[column]] = 0L
  nn = names(mask)
  for(i in seq_along(nn)) {
    n = nn[i]
    m = mask[n]
    y = as.integer(data[[n]]) * m
    y[is.na(y)] = 0L
    data[[column]] = data[[column]] + y
  }
  data
}

#' Create new groups from a set of columns
#' 
#' Names of new groups will be the name of groups entry
#' 
#' @param data data.frame
#' @param groups list of group, each group is a vector of names of columns belonging to this group
create_binary_groups = function(data, groups) {
  d = data.frame(count=data$count)
  for(i in seq_along(groups)) {
    name = names(groups[i])
    ss = groups[[i]]
    g = apply(data[, ss], 1, sum, na.rm=TRUE) > 0
    d[[name]] = g
  }
  nn = names(groups)
  mask = create_binary_mask(nn)
  d = apply_binary_mask(d, mask, 'g')
  d = d %>% group_by(g) %>% summarise(count=sum(count))
  d = get_labels_from_binary(d, mask, group="g")
  d
}

# From UpsetR package
#' @param data data.frame() with data for each sets
#' @param sets list of column names for each variable in sets
#' @param n.max number of top obs to keep, (all if na)
#' @param point_size size of point in intersection plot
#' @param name_size_scale scale of labels size in intersection plot
upset_plot <- function(data, sets, n.max=40, point_size=3, name_size_scale=1, title=NULL, subtitle=NULL, caption=NULL) {
  data = data %>% arrange(desc(count))
  data$label = reorder(data$label, order(data$count, decreasing=TRUE))
  
  if(!is.na(n.max)) {
    d = data[1:n.max, ]
  } else {
    d = data
  }
  
  left_margin = 1
  # freq_plot
  d$row = 1:nrow(d)
  freq_plot = ggplot(d, aes(x=row, y=count)) + 
    geom_bar(stat="identity") 
  
  freq_plot = freq_plot  + 
    theme(panel.background = element_rect(fill = "white"),
          plot.title = element_text(vjust = 1.5),
          panel.border = element_rect(colour = "grey83"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_text(vjust = 1.3, size = 8.3),
          axis.title.x = element_text(size = 8.3),
          axis.text.x = element_blank(),
          plot.margin=unit(c(0.5,0,0, left_margin), "cm"),
          plot.background = element_rect(fill="white")
    ) +
    scale_x_continuous(limits = c(0,(nrow(d)+1 )), expand = c(0,0), breaks = NULL) +
    labs(x=NULL, y ="Frequency", title=title, subtitle = subtitle)
  
  # Get symptoms with any associations
  n = colSums(d[, sets])
  n = names(n[n > 0])
  
  mat = as.matrix(d[, n])
  mm <- expand.grid(group=seq(nrow(mat)), symptom=seq(ncol(mat)))
  mm <- data.frame(mm, intersection = as.vector(mat))
  matrix_plot = ggplot(mm, aes(x=group, y=symptom)) +
    geom_point(color="gray83", size=point_size, shape=16) +
    geom_point(data=~filter(., intersection), size=point_size, shape=16) +
    geom_line(data=~filter(., intersection),aes(x=group, y=symptom, group=group), size = 1) +
    scale_y_continuous(breaks = 1:length(n), limits = c(0.5,(length(n) +0.5)), labels = n, expand = c(0,0)) +
    scale_x_continuous(limits = c(0,(nrow(d)+1 )), expand = c(0,0))  + 
    theme(
      panel.background = element_rect(fill = "white"),
      plot.margin=unit(c(0, 0.5, 0.5, 1), "cm"),
      panel.border = element_rect(colour = "grey83"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_text(colour = "gray0", size = 7 * name_size_scale, hjust = 0.4),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      plot.background = element_rect(fill="white", color = "white")
    ) +
    labs(x=NULL, y="", caption=caption)
  
  #g2 = ggplot_gtable(ggplot_build(matrix_plot))
  #g1 = ggplot_gtable(ggplot_build(freq_plot))
  
  #g = arrangeGrob(g1, g2)
  
  plot_grid(freq_plot, matrix_plot, align="v", ncol=1)
  
}
