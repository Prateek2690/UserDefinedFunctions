#***************************************************
# SPREAD SERIES VISUALIZER
#***************************************************
library(rbokeh)

## TITLE--------------------------------------
fig <- figure(title = "R bokeh") 


## LINEs-------------------------------------------
#ly_lines(fig, x = Speed, y = dist, data = cars, width = 2.5, color = "#50728b") 
#ly_lines(fig, x = speed, y = dist, data = cars, width = 2.5, color = "#ffa60c")
#"#698B69"

#ly_multi_line(fig, x = cars$speed, y = cars$dist, width = 2.5, color = "#50728b")

#ly_multi_line()

## THEME FIG----------------------------------
#theme_plot(fig, background_fill = "white",
#           border_fill = "white", outline_line_alpha = 1,
#           outline_line_cap = "butt", outline_line_color = "black",
#           outline_line_dash = NULL, outline_line_dash_offset = 0,
#outline_line_join = "miter", outline_line_width = 1,
#           title_text_align = "left", title_text_alpha = 1,
#           title_text_baseline = "bottom", title_text_color = "black",
#           title_text_font = "arial", title_text_font_size = "10pt",
#           title_text_font_style = "bold", min_border = 50,
#           min_border_bottom = 50, min_border_left = 50, min_border_right = 50,
#           min_border_top = 50)

## THEME AXIS----------------------------------
#theme_axis(fig, which = c("x", "y"), num_minor_ticks = 5,
#           axis_label_standoff = NULL, axis_label_text_align = "left",
#           axis_label_text_alpha = 1, axis_label_text_baseline = "bottom",
#           axis_label_text_color = "#444444", axis_label_text_font = "Helvetica",
#           axis_label_text_font_size = "8pt", axis_label_text_font_style = "normal",
#           axis_line_alpha = 1, axis_line_cap = "butt", axis_line_color = "black",
#           axis_line_dash = NULL, axis_line_dash_offset = 0,
#           axis_line_join = "miter", axis_line_width = 1,
#           major_label_orientation = "horizontal", major_label_standoff = NULL,
#           major_label_text_align = "left", major_label_text_alpha = 1,
#           major_label_text_baseline = "bottom", major_label_text_color = "black",
#           major_label_text_font = "Helvetica", major_label_text_font_size = "12pt",
#           major_label_text_font_style = "normal", major_tick_in = 1,
#           major_tick_line_alpha = 1, major_tick_line_cap = "butt",
#           major_tick_line_color = "black", major_tick_line_dash = NULL,
#           major_tick_line_dash_offset = 0, major_tick_line_join = "miter",
#           major_tick_line_width = 1, major_tick_out = NULL, minor_tick_in = 1,
#           minor_tick_line_alpha = 1, minor_tick_line_cap = "butt",
#           minor_tick_line_dash = NULL,
#           minor_tick_line_dash_offset = 0, minor_tick_line_join = "miter",
#           minor_tick_line_width = 1, minor_tick_out = NULL)

## THEME GRID
theme_grid(fig, which = c("x", "y"), band_fill_alpha = 1,
           band_fill_color = "gray", grid_line_alpha = 12, grid_line_cap = "butt",
           grid_line_color = "black", grid_line_dash = 5
           )




