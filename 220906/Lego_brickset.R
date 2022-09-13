#Load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(DescTools)
library(showtext)
library(ggtext)
library(pdftools)

#load fonts
font_add_google("Montserrat")
font_add_google("Angkor")
font_add_google("Viga")
showtext_auto()

#plot things 
txt.color <- '#001f2d'
line.color <- '#4d5d68'
annotate.fam <- "Montserrat" #font family for in plot annotations
annotate.size <- 4 # text size for in plot annotation 
bg.color <- '#e4e2e0'
loli.color <- '#36566a'
labLine.color <- '#5a6870'

#Load datasets of interest 
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_parts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_parts.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
themes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/themes.csv.gz')

#Combine datasets into one dataframe
combined_inventory <- inventories %>% 
  left_join(sets, by = 'set_num') %>% 
  left_join(inventory_parts, by = c('id' = 'inventory_id')) %>% 
  left_join(colors, by = c('color_id' = 'id'))

  
#Rename columns for easy callings
colnames(combined_inventory) <- c('inventory_id', 'version', 'set_num', 'set_name', 'year', 'theme_id', 'num_parts', 'set_img_url', 'part_num', 'color_id', 'quantity', 'is_spare', 'part_img_url', 'color_name', 'rgb', 'is_trans')

#drop NA
combined_inventory <- na.omit(combined_inventory)

#exclude "no color/any color' or "unknown" color 
combined_inventory <- combined_inventory[!combined_inventory$color_name %in% c('[No Color/Any Color]', "[Unknown]"),]

#Calculate color variation growth over the years 
colors_yearly <- combined_inventory %>% group_by(color_name, year, rgb) %>% summarise(color_quantity = n())

#Because lego parts also grow exponentially over the years, normalize the growth of colors by part 
sets_yearly <- combined_inventory %>% group_by(year) %>% summarise(set_quantity = length(unique(set_name)))
colors_yearly <- colors_yearly %>% left_join(sets_yearly, by = 'year')

#restrict to 1960 onwards
colors_yearly <- colors_yearly[colors_yearly$year > 1959,]

unique_colors <- colors_yearly %>% dplyr::group_by(color_name, rgb) %>% dplyr::summarise(n())

#Try to sort available colors based on hue - to create harmonious gradient as best as possible
colors_rgb <- paste('#', unique_colors$rgb, sep = "")
colors_hsv <- ColToHsv(colors_rgb)
col_hsv = c()
for (i in seq(1,dim(colors_hsv)[2],1)){
  col_hsv = rbind(col_hsv, colors_hsv[,i])
}
col_df = data.frame(color_name = unique_colors$color_name, rgb = colors_rgb, h = col_hsv[,1], s = col_hsv[,2], v = col_hsv[,3])
col_df_sorted <- col_df[with(col_df, order(h,s,v)),]

#organize factors 
colors_yearly$color_name <- factor(colors_yearly$color_name, levels = col_df_sorted$color_name)
colors_yearly$rgb <- paste('#', colors_yearly$rgb, sep = "") # add '#' to rgb values 

colors_yearly <- colors_yearly[colors_yearly$year > 1959,]


#create dataframe for year tickmarks and label 
year_marks <- data.frame(x = c(1960, 1970, 1980, 1990, 2000, 2010, 2020), y = c(-2,-2,-2,-2,-2,-2,-2),
                         xend = c(1960, 1970, 1980, 1990, 2000, 2010, 2020), yend = c(-4,-4,-4,-4,-4,-4,-4),
                         label = c("1960", "1970", "1980", "1990", "2000", "2010", "2020"))

#Dataframe for 'noteworthy' launches over the years (according to Wikipedia)
lego_moments <- data.frame(year = c(1966, 1969, 1974,1977,1978,1983,1989,1991,1995,1999,2001,2003,2006,
                                    2008,2010,2012,2014,2016,2020),
                           moment = c("Lego train","Duplo","Lego figures","Lego Technic","Minifigure (movable limbs), Lego Space",
                                      "Duplo baby","Lego Pirates","Lego Paradise","Lego Aquazone","Lego Star Wars",
                                      "Lego Harry Potter","Minifigure skintone revamp","Lego Batman, Avatar, SpongeBob",
                                      "Lego Indiana Jones, Speed Racer, Agents","Lego Atlantis, Toy Story, Cars, Prince of Persia",
                                      "Lego Disney Princess, Lord of the Rings, Hobbit",
                                      "Lego Ninjago - the comeback","Lego Stranger Things, DC Super Hero Girls, Ghostbuster",
                                      "Lego Minions, Super Mario, Trolls")) %>% 
  left_join(parts_yearly <- combined_inventory %>% group_by(year) %>% summarise(parts_quantity = sum(quantity)/length(unique(set_name))), by = "year") %>% mutate(parts_quantity_scaled = -1*parts_quantity/3 -12) %>% 
  mutate(ystart = rep(-12, length(year))) %>% mutate(point_lab = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                                                                   "K", "L", "M", "N", "O", "P", "Q", "R", "S")) %>% 
  mutate(incl = c(0,1,0,1,0,0,0,0,0,1,0,1,0,0,0,1,1,0,1))

#create dataframe for labeling moments 
moment_labs <- lego_moments[lego_moments$inc == 1,] %>% 
  mutate(xlab_start = c(1969-4, 1977-6, 1999-7, 2003-10, 2012-16, 2014-11, 2020-11), xlab_end = c(1969-1, 1977-1, 1999-1, 2003-1, 2012-1, 2014-1, 2020-0.5)) %>% 
  mutate(ylab = c(-50, -65, -80, -100, -130, -140, -150))

#create dataframe for custom legend 
legend <- data.frame(points = seq(-87, -147, length = 12), point_lab = lego_moments$point_lab[!lego_moments$point_lab %in% moment_labs$point_lab],
                     label = lego_moments$moment[!lego_moments$point_lab %in% moment_labs$point_lab])


title="<span style='color:#001f2d;font-size:40pt;font-family:Montserrat'>**LEGO COLOR & SET EVOLUTION**</span><br><br><span style='font-size:11pt;color:#001f2d;font-family:Montserrat'>
Variation in color of Lego bricks have increased exponentially over the past 40 years, along with the increse in Lego set variation. 
The bar graph (top) shows the composition of lego parts in each color adjusted for the number of sets available at each given year.
The points (bottom) shows the number of parts per set in a given year for select timepoints.
More recent years have also seen a rise in Lego sets based on popular shows (e.g., *Star Wars*, *Lord of The Rings*, *Hobbit*, *Toy Story*)</span><br>"

plot <- ggplot(colors_yearly, aes(fill=color_name, y=color_quantity/set_quantity, x=year)) + 
        geom_bar(position="stack", stat="identity")+
        scale_fill_manual(values = col_df_sorted$rgb)+
        #add dotted line to mark transition of grey color 
        geom_segment(x = 2003.5, y = 0, xend = 2003.5, yend = 120, color = "#b0bac2", size = .5, linetype = 'dashed')+
        #add annotation to top plot 
        annotate("curve", x = 1963, xend = 1964.5, y = 45, yend = 64,curvature = -.25,color = line.color,size = .4) +
        annotate("text", x = 1965, y = 65, label = "Introduction of Modulex,\nbricks intended for architects \nthat are smaller than the normal bricks.", family = annotate.fam, size = 3, color = txt.color, hjust = 0)+
        annotate("curve", x = 2003, xend = 2000, y = 85, yend = 95, curvature = -.3, color = line.color, size = .4)+
        annotate("text", x = 2002, y = 101, label = "Lego changed its gray shades\nfrom light/dark gray to light/dark bluish gray", family = annotate.fam, size = 3, color = txt.color, hjust = 1)+
        #add tick marks to mark year 
        geom_segment(data = year_marks, aes(x = x, y = y, xend = xend, yend=yend), color = line.color, inherit.aes = FALSE, color = 'black')+
        geom_text(data = year_marks, aes(x = x, y = yend-3, label = label), color = txt.color, family = "Viga", size = 6, inherit.aes = FALSE, fontface = "bold")+
        #add horizontal lines to indicate grid for lolipop
        geom_segment(data = data.frame(yint = c(0,-100,-200,-300)/3-12, xstart = rep(1958, n = 4), xend = rep(2022, n = 4)), 
                     aes(x = xstart, y = yint, xend = xend, yend = yint), color = "#f1f1f1", inherit.aes = FALSE, size = 0.8)+
        geom_text(data = data.frame(x = rep(1957.5, n = 4), y = c(0,-100,-200,-300)/3-12, label = c("0", "100", "200", "300")), aes(x = x, y = y, label = label), 
                  color = "white", family = "Viga", fontface = "bold", size = 5, inherit.aes = FALSE, hjust = 1) +
        #add the lolipops for parts/set 
        geom_segment(data = lego_moments, aes(x = year, y = ystart, xend = year, yend = parts_quantity_scaled), inherit.aes = FALSE, color = loli.color, size = 0.6)+
        geom_point(data = lego_moments, aes(x = year, y = parts_quantity_scaled), shape = 21, inherit.aes = FALSE, fill = "#f1f1f1", color = loli.color, size = 5, stroke = 1.5) +
        geom_text(data = lego_moments, aes(x = year, y = parts_quantity_scaled, label = point_lab), inherit.aes = FALSE, color = 'black', size = 3, family = annotate.fam, fontface = "bold")+
        #add label on lolipop graphs 
        geom_segment(data = moment_labs, aes(x = xlab_start, y = ylab, xend = xlab_end, yend = ylab), color = alpha(loli.color, 0.7), inherit.aes = FALSE, size = .2)+
        geom_segment(data = moment_labs, aes(x = xlab_end, y = ylab, xend = year, yend = parts_quantity_scaled -5), color = alpha(loli.color, .7), linetype = 'dashed', inherit.aes = FALSE, size = .2)+
        geom_text(data = moment_labs, aes(x = xlab_start, y = ylab + 3, label = moment), size = 3,hjust = 0, color = txt.color, family = annotate.fam, inherit.aes = FALSE)+
        #add legend
        geom_point(data = legend, aes(x = 1961, y = points), shape = 21, inherit.aes = FALSE, fill = "#f1f1f1", color = loli.color, size = 3.7, stroke = 1) +
        geom_text(data = legend, aes(x = 1961, y = points, label = point_lab), inherit.aes = FALSE, color = 'black', size = 2, family = annotate.fam, fontface = "bold")+
        geom_text(data = legend, aes(x = 1961.5, y = points, label = label), size = 3,hjust = 0, color = txt.color, family = annotate.fam, inherit.aes = FALSE)+
        #add 'axes' title 
        annotate("text", x = 2024.5, y = 60, label = "COLORS/ SET", family = annotate.fam, size = 7, color = alpha(txt.color, .8), angle = -90)+
        annotate("text", x = 2024.5, y = -60, label = "PARTS/ SET", family = annotate.fam, size = 7, color = alpha(txt.color, .8), angle = -90)+
        coord_cartesian(xlim = c(1955, 2026), ylim = c(-160, 120)) +
        labs(x = "", y = "", title = title, caption = "Graphic: Karen Sasmita | Data: Rebrickable")+
        theme(legend.position = "none",
              plot.title=element_textbox_simple(margin=margin(b=0, t = 20, r = 10, l = 10)),
              panel.background = element_rect(fill = bg.color,colour = bg.color),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank(),
              axis.text.x = element_blank(),
              axis.title = element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              plot.background = element_rect(fill = bg.color, color = NA),
              plot.caption = element_text(family = annotate.fam, color = txt.color, size = 8),
              plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))

path <- here::here("plots", "Lego_Brickset")
ggsave(glue::glue("{path}.pdf"), plot = plot, width = 16, height = 11, device = cairo_pdf)
pdf_convert(pdf = glue::glue("{path}.pdf"),
            filenames = glue::glue("{path}.png"),
            format = "png", dpi = 500)
