smooth_and_0 <- function(data){
  data[, ':='(Value_Smoothed = rollmean(Value, 3, na.pad=T), Population_Smoothed = rollmean(Population, 3, na.pad=T)), by = c("Variable", "Income_Type", "Group")]
  data_0 <- data[Year == 2008, .(Variable, Income_Type, Group, Value_0 = Value, Value_Smoothed_0 = Value_Smoothed)]
  data <- merge(data, data_0, by = c("Variable", "Income_Type", "Group"))
  data[, ':='(Relative_Change = Value / Value_0, Smoothed_Relative_Change = Value_Smoothed / Value_Smoothed_0)]
  data[Variable == "Ratio_9010", Variable := "90/10 Ratio"]
  data[Variable == "Ratio_8020", Variable := "80/20 Ratio"]
  return(data)
}

linear_OLS <- function(data, reg_group, reg_variable, reg_income, excluded_years, middle_year) {
  regression_data <- data[Group == reg_group & Variable == reg_variable & Income_Type %like% reg_income & !(Year %in% excluded_years)]
  regression_data[, Year := Year - middle_year]
  linear_wls <- lm(Value ~ Year, regression_data)
  return(linear_wls)
}

linear_WLS <- function(data, reg_group, reg_variable, reg_income, excluded_years, middle_year) {
  regression_data <- data[Group == reg_group & Variable == reg_variable & Income_Type %like% reg_income & !(Year %in% excluded_years)]
  regression_data[, Year := Year - middle_year]
  linear_wls <- lm(Value ~ Year, regression_data, weights = 1 / (Margin_Of_Error**2))
  return(linear_wls)
}

quad_WLS <- function(data, reg_group, reg_variable, reg_income, excluded_years, middle_year) {
  regression_data <- data[Group == reg_group & Variable == reg_variable & Income_Type %like% reg_income & !(Year %in% excluded_years)]
  regression_data[, Year := Year - middle_year]
  regression_data[, Year_sq := Year ** 2]
  quadratic_wls <- lm(Value ~ Year + Year_sq, regression_data, weights = 1 / (Margin_Of_Error**2))
  return(quadratic_wls)
}


plot_trends <- function(data){
  p1 <- ggplot(data[Income_Type %like% "BHC"], 
               aes(x = Year, y = Smoothed_Relative_Change*100, color = Variable)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "BHC inequality measures, smoothed\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115),  breaks = c(80, 85, 90, 95, 100, 105, 110, 115), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  
  p2 <- ggplot(data[Income_Type %like% "AHC"], 
               aes(x = Year, y = Smoothed_Relative_Change*100, color = Variable)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "AHC inequality measures, smoothed\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115), breaks = c(80, 85, 90, 95, 100, 105, 110, 115), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_trends_unsmoothed <- function(data){
  p1 <- ggplot(data[Income_Type %like% "BHC" & Year %between% c(2008, 2022)], 
               aes(x = Year, y = Relative_Change*100, color = Variable)) +
    geom_line(linewidth=1.5) + 
    labs(subtitle = "BHC inequality measures\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115),  breaks = c(80, 85, 90, 95, 100, 105, 110, 115), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  
  
  p2 <- ggplot(data[Income_Type %like% "AHC" & Year %between% c(2008, 2022)], 
               aes(x = Year, y = Relative_Change*100, color = Variable)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "AHC inequality measures\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115), breaks = c(80, 85, 90, 95, 100, 105, 110, 115), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_trends_combo <- function(data){
  p1 <- ggplot(data[Income_Type %like% "BHC"], 
               aes(x = Year, y = Smoothed_Relative_Change*100, color = Variable)) + 
    geom_line(linewidth=1.5) + 
    geom_point(aes(x = Year, y = Relative_Change*100, color = Variable)) +
    labs(subtitle = "BHC inequality measures, smoothed\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115),  breaks = seq(80, 115, 5), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  
  p2 <- ggplot(data[Income_Type %like% "AHC" & Year], 
               aes(x = Year, y = Smoothed_Relative_Change*100, color = Variable)) + 
    geom_line(linewidth=1.5) + 
    geom_point(aes(x = Year, y = Relative_Change*100, color = Variable)) +
    labs(subtitle = "AHC inequality measures, smoothed\n(base = 100 in 2008)") + 
    custom_theme +
    scale_y_continuous(name = NULL, limits = c(80, 115), breaks = seq(80, 115, 5), expand = c(0,0)) +
    scale_x_continuous(name ="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name = NULL) 
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_measure <- function(data, selected_variable, plot_title, y_lims, y_seqs, line_color) {
  p1 <- ggplot(data[Variable == selected_variable & Group == "All" & Income_Type %like% "BHC"], 
               aes(x = Year, y = Value_Smoothed)) + 
    geom_line(linewidth=1.5, color =  line_color) + 
    geom_point(size = 2, color =  rgb(63, 64, 58, maxColorValue = 255), aes(x = Year, y = Value)) +
    geom_errorbar(linewidth = 1, color =  rgb(63, 64, 58, maxColorValue = 255), aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) + 
    labs(subtitle = paste0("BHC ", plot_title, ", smoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0))
  
  p2 <- ggplot(data[Variable == selected_variable & Group == "All" & Income_Type %like% "AHC"], 
               aes(x = Year, y = Value_Smoothed)) + 
    geom_line(linewidth=1.5, color =  line_color) + 
    geom_point(size = 2, color =  rgb(63, 64, 58, maxColorValue = 255), aes(x = Year, y = Value)) +
    geom_errorbar(linewidth = 1, color =  rgb(63, 64, 58, maxColorValue = 255), aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) + 
    labs(subtitle = paste0("AHC ", plot_title, ", smoothed")) + 
    custom_theme +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0))
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_measure_smoothed <- function(data, selected_variable, plot_title, factor_list = NULL, y_lims, y_seqs, legend_cols = 3){
  DATA <- copy(data)
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  p1 <- ggplot(DATA[Variable == selected_variable & Income_Type %like% "BHC" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = paste0("BHC ", plot_title, ", smoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(DATA[Variable == selected_variable & Income_Type %like% "AHC" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = paste0("AHC ", plot_title, ", smoothed")) + 
    custom_theme +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_measure_errors <- function(data, selected_variable, plot_title, factor_list = NULL, y_lims, y_seqs, legend_cols = 3){
  DATA <- copy(data)
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  
  p1 <- ggplot(DATA[Variable == selected_variable & Income_Type %like% "BHC" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    geom_point(size = 2, aes(x = Year, y = Value)) +
    geom_errorbar(linewidth = 1, aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) + 
    labs(subtitle = paste0("BHC ", plot_title, ", smoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(DATA[Variable == selected_variable & Income_Type %like% "AHC" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    geom_point(size = 2, aes(x = Year, y = Value)) +
    geom_errorbar(linewidth = 1,aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) + 
    labs(subtitle = paste0("AHC ", plot_title, ", smoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = y_lims, breaks = y_seqs, expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

plot_shares <- function(income_type, data, ylim_max, factor_list = NULL, legend_cols = 3) {
  DATA <- copy(data)
  total_data <- DATA[Variable == "Total" & Group != "All"]
  data_totals <- DATA[Variable == "Total" & Group == "All", .(Income_Type, Year, Total_Pop = Population, Total_Value = Value, Total_Pop_Smoothed = Population_Smoothed, Total_Value_Smoothed = Value_Smoothed)]
  total_data <- merge(total_data, data_totals, by = c("Income_Type", "Year"))
  total_data[, ":="(Population_Proportion = Population / Total_Pop, Value_Proportion = Value / Total_Value, Population_Proportion_Smoothed = Population_Smoothed / Total_Pop_Smoothed, Value_Proportion_Smoothed = Value_Smoothed / Total_Value_Smoothed)]
  
  if (!(is.null(factor_list))){
    total_data[, Group := factor(Group, levels = factor_list)]
  }
  
  p1 <- ggplot(total_data[Income_Type %like% income_type], 
               aes(x = Year, y = Population_Proportion_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "Proportion of population, smoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(total_data[Income_Type %like% income_type], 
               aes(x = Year, y = Value_Proportion_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = paste0("Proportion of ", income_type, " income, smoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}


plot_both_shares <- function(data, ylim_max, factor_list = NULL, legend_cols = 3) {
  DATA <- copy(data)
  total_data <- DATA[Variable == "Total" & Group != "All"]
  data_totals <- DATA[Variable == "Total" & Group == "All", .(Income_Type, Year, Total_Pop = Population, Total_Value = Value, Total_Pop_Smoothed = Population_Smoothed, Total_Value_Smoothed = Value_Smoothed)]
  total_data <- merge(total_data, data_totals, by = c("Income_Type", "Year"))
  total_data[, ":="(Population_Proportion = Population / Total_Pop, Value_Proportion = Value / Total_Value, Population_Proportion_Smoothed = Population_Smoothed / Total_Pop_Smoothed, Value_Proportion_Smoothed = Value_Smoothed / Total_Value_Smoothed)]
  
  if (!(is.null(factor_list))){
    total_data[, Group := factor(Group, levels = factor_list)]
  }
  
  p1 <- ggplot(total_data[Income_Type %like% "BHC"], 
               aes(x = Year, y = Population_Proportion_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "Proportion of population,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(total_data[Income_Type %like% "BHC"], 
               aes(x = Year, y = Value_Proportion_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = paste0("Proportion of BHC income,\nsmoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p3 <- ggplot(total_data[Income_Type %like% "AHC"], 
               aes(x = Year, y = Value_Proportion_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = paste0("Proportion of AHC income,\nsmoothed")) + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2 + p3)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

get_theil_between <- function(data, it, between_name) {
  DATA <- copy(data)
  data_within <- DATA[Group != "All" & Variable == "Theil_Within" & Income_Type %like% it]
  data_between <- DATA[Group != "All" & Variable == "Theil_Between" & Income_Type %like% it]
  data_between <- data_between[, .(Group = between_name, Value = sum(Value), Value_Smoothed = sum(Value_Smoothed)), by = "Year"]
  
  data_theil <- rbind(data_within, data_between, fill = TRUE)
  
  start_year <- data_theil[Year == 2008, .(Group, Variable, Income_Type, Value_Abs_0 = Value, Value_Abs_Smoothed_0 = Value_Smoothed)]
  data_theil <- merge(data_theil, start_year, by = c("Group", "Variable", "Income_Type"),)
  data_theil[, ':='(Value_Change = abs(Value) - abs(Value_Abs_0), Value_Smoothed_Change = abs(Value_Smoothed) - abs(Value_Abs_Smoothed_0))]
  
  return(data_theil)
}

plot_area_stacked <- function(data, subgroup_type, save_path, file_name_type, between_name, factor_list = NULL, ylim_max, change_max, change_gap = 0.01, total_theil_type = "New Zealand\npopulation", legend_cols = 2) {
  DATA <- copy(data)
  data_bhc <- get_theil_between(DATA, "BHC", between_name)
  data_ahc <- get_theil_between(DATA, "AHC", between_name)
  group_list <- data_bhc[!(Group %like% "Between"), unique(Group)]
  if (!(is.null(factor_list))){
    data_bhc[, Group := factor(Group, levels = factor_list)]
    data_ahc[, Group := factor(Group, levels = factor_list)]
  }
  else{
    data_bhc[, Group := factor(Group, levels = c(group_list, between_name))]
    data_ahc[, Group := factor(Group, levels = c(group_list, between_name))]
  }
  temp_pallete <- tsy_palette
  temp_pallete[length(group_list) + 1] <- rgb(63, 64, 58, maxColorValue = 255)
  p1 <- ggplot(data_bhc, 
               aes(x = Year, y = Value_Smoothed)) + 
    geom_area(aes(fill=Group)) + 
    stat_summary(fun.y = sum, show.legend = NA, aes(color="Population Theil"), geom = "line", size = 1.5, linetype = "twodash") +
    labs(subtitle = "BHC Theil contribution, smoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_manual(name ="", values = c("Population Theil" = rgb(169, 169, 169, maxColorValue = 255)), labels = paste0("Theil index:\n", total_theil_type)) +
    scale_fill_manual(name ="", values = temp_pallete) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2, ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(data_bhc, 
               aes(x = Year, y = Value_Smoothed_Change, color = Group)) + 
    geom_line(linewidth = 1.5) + 
    labs(subtitle = "Change in BHC Theil Contribution since 2008,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-1*change_max,change_max), breaks = seq(-1*change_max,change_max, change_gap), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_manual(guide = "none", values = temp_pallete) 
  
  p3 <- ggplot(data_ahc, 
               aes(x = Year, y = Value_Smoothed)) + 
    geom_area(aes(fill=Group)) + 
    labs(subtitle = "AHC Theil contribution, smoothed") + 
    stat_summary(fun.y = sum, show.legend = NA, aes(color="Population Theil"), geom = "line", size = 1.5, linetype = "twodash") +
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_manual(name ="", values = c("Population Theil" = rgb(169, 169, 169, maxColorValue = 255)), labels = paste0("Theil index:\n", total_theil_type)) +
    scale_fill_manual(name="", values = temp_pallete) +
    guides(color = guide_legend(order = 1), fill = guide_legend(order = 2, ncol = legend_cols, bycol = TRUE))

  
  p4 <- ggplot(data_ahc, 
               aes(x = Year, y = Value_Smoothed_Change, color = Group)) + 
    geom_line(linewidth = 1.5) + 
    labs(subtitle = "Change in AHC Theil Contribution since 2008,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-1*change_max,change_max), breaks = seq(-1*change_max,change_max, change_gap), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_manual(guide = "none", values = temp_pallete) 
  
  
  combine <- (p1 + p2 + p3 + p4) & theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

theil_breakdown <- function(data, group_type, factor_list = NULL, ylim, legend_cols = 3) {
  DATA <- copy(data)
  
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  p1 <- ggplot(DATA[Income_Type %like% "BHC" & Variable == "Theil_Within" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "BHC within-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(0,ylim*2), breaks = seq(0,ylim*2, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(DATA[Income_Type %like% "BHC" & Variable == "Theil_Between" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "BHC between-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-ylim,ylim), breaks = seq(-1*ylim,ylim, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p3 <- ggplot(DATA[Income_Type %like% "AHC" & Variable == "Theil_Within" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "AHC within-group Theil contribution") + 
    custom_theme + 
    scale_y_continuous(name=NULL, limits = c(0,ylim*2), breaks = seq(0,ylim*2, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p4 <- ggplot(DATA[Income_Type %like% "AHC" & Variable == "Theil_Between" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "AHC between-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-ylim,ylim), breaks = seq(-1*ylim,ylim, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  
  combine <- (p1 + p2 + p3 +p4)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect") +
    plot_annotation(
      title = paste0("Theil components by ", group_type)
    )
}

theil_breakdown_errors <- function(data, group_type, factor_list = NULL, ylim, legend_cols = 3) {
  DATA <- copy(data)
  
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  p1 <- ggplot(DATA[Income_Type %like% "BHC" & Variable == "Theil_Within" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "BHC within-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-0.04,ylim*2), breaks = seq(-0.04,ylim*2, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(DATA[Income_Type %like% "BHC" & Variable == "Theil_Between" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "BHC between-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-ylim,ylim), breaks = seq(-ylim,ylim, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p3 <- ggplot(DATA[Income_Type %like% "AHC" & Variable == "Theil_Within" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "AHC within-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-0.04,ylim*2), breaks = seq(-0.04,ylim*2, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p4 <- ggplot(DATA[Income_Type %like% "AHC" & Variable == "Theil_Between" & Group != "All"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "AHC between-group Theil contribution") + 
    custom_theme  +
    scale_y_continuous(name=NULL, limits = c(-ylim,ylim), breaks = seq(-ylim,ylim, 0.04), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  
  combine <- (p1 + p2 + p3 +p4)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}

proportion_plots_s <- function(data, ylim_max, group_type, factor_list = NULL, legend_cols = 3, y_gap = .05) {
  DATA <- copy(data)
  
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  
  p1 <- ggplot(DATA[Income_Type %like% "BHC" & Variable %like% "Proportion"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "Proportion of population,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, y_gap), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p2 <- ggplot(DATA[Income_Type %like% "BHC" & Variable %like% "Contribution"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "Proportion of BHC income,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, y_gap), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  p3 <- ggplot(DATA[Income_Type %like% "AHC" & Variable %like% "Contribution"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1.5) + 
    labs(subtitle = "Proportion of AHC income,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, y_gap), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2008, 2022), breaks = seq(2008, 2022, 2), expand = c(0,0)) +
    scale_color_discrete(name="")   +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2 + p3)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}
proportion_plots_s_errors <- function(data, ylim_max, group_type, factor_list = NULL, legend_cols = 3) {
  DATA <- copy(data)
  
  if (!(is.null(factor_list))){
    DATA[, Group := factor(Group, levels = factor_list)]
  }
  p1 <- ggplot(DATA[Income_Type %like% "BHC" & Variable %like% "Proportion"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "Proportion of population,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE)) 
  
  p2 <- ggplot(DATA[Income_Type %like% "BHC" & Variable %like% "Contribution"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "Proportion of BHC income,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE)) 
  
  p3 <- ggplot(DATA[Income_Type %like% "AHC" & Variable %like% "Contribution"], 
               aes(x = Year, y = Value_Smoothed, color = Group)) + 
    geom_line(linewidth=1) + 
    geom_point(aes(x= Year, y = Value)) +
    geom_errorbar(aes(ymin = Value - Margin_Of_Error, ymax = Value + Margin_Of_Error)) +
    labs(subtitle = "Proportion of AHC income,\nsmoothed") + 
    custom_theme  +
    scale_y_continuous(name=NULL, labels=percent_format(accuracy=1), limits = c(0, ylim_max), breaks = seq(0, ylim_max, 0.05), expand = c(0,0)) +
    scale_x_continuous(name="Years ending June", limits = c(2006, 2024), breaks = seq(2006, 2024, 2), expand = c(0,0)) +
    scale_color_discrete(name="")  +
    guides(color = guide_legend(ncol = legend_cols, bycol = TRUE))
  
  combine <- (p1 + p2 + p3)& theme(legend.position="bottom")
  combine + 
    plot_layout(guides = "collect")
}