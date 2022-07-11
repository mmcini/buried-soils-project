library(RColorBrewer)
library(extrafont)
library(tidyverse)
library(prospectr)
library(ggpubr)
library(scales)

### PXRF ###########################################################################################

# Loading data
pxrf_ox_ufla <- read_csv("../Data/Perfis/pxrf_ox_ufla.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Depth = Lat) %>%
    replace(is.na(.), 0)
pxrf_ox_quartz <- read_csv("../Data/Perfis/pxrf_ox_quartz.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Depth = Lat) %>%
    replace(is.na(.), 0)
pxrf_glei_ufla <- read_csv("../Data/Perfis/pxrf_glei_ufla.csv") %>%
    select(-ends_with("-")) %>%
    group_by(Lat) %>%
    summarize(across(everything(), mean, na.rm = T)) %>%
    rename(Depth = Lat) %>%
    replace(is.na(.), 0)

column_names <- c("Depth", "Al", "Ca", "Mg",
                "Fe", "Si", "Ti", "Zr", "Ti/Zr")

# Plots
## Variables used in plots
plot_vars <- column_names[-1]

## Base plots
pxrf_plots_ox_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_ox_ufla,
            aes_string(x = "Depth", y = variable))
    pxrf_plots_ox_ufla[[variable]] <- plot
}

pxrf_plots_ox_quartz <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_ox_quartz,
            aes_string(x = "Depth", y = variable))
    pxrf_plots_ox_quartz[[variable]] <- plot
}

pxrf_plots_glei_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(pxrf_glei_ufla,
            aes_string(x = "Depth", y = variable))
    pxrf_plots_glei_ufla[[variable]] <- plot
}

## List with base plots
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
pxrf_plots <- list(ox_ufla = pxrf_plots_ox_ufla,
                   ox_quartz = pxrf_plots_ox_quartz,
                   glei_ufla = pxrf_plots_glei_ufla)

plot_titles <- list(ox_ufla = "Typic Udifolists (TU)",
                    ox_quartz = "Typic Dystrustepts (TD)",
                    glei_ufla = "Typic Endoaquents (TE)")

eixo_x <- rev(c(5, 20, 35, 50, 65, 80, 95, 110))

## Plot formatting
for (soil in soil_names) {
    count <- 1
    for (variable in plot_vars) {
        plot <- pxrf_plots[[soil]][[variable]] +
            geom_point() +
            geom_line() +
            coord_flip() +
            scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3)) +
            scale_x_continuous(breaks = seq(-105, 0, 15),
                               labels = eixo_x) +
            xlab("Depth (cm)") + ylab(plot_vars[count]) +
            theme_bw() +
            theme(text = element_text(family = "Times New Roman"),
                  axis.text = element_text(size = 7),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
        if (variable != "Al") {
            plot <- plot +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank())
        }
        if (variable == "Ca" | variable == "Fe" | variable == "Si") {
            plot <- plot +
                scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3),
                               labels = unit_format(unit = "K",
                                                       scale = 1e-3))
        }
        pxrf_plots[[soil]][[variable]] <- plot
        count <- count + 1
    }
}

column_names <- c("Depth", "Al", "Ca", "Mg",
                "Fe", "Si", "Ti", "Zr", "Ti/Zr")

## Plot arrange
pxrf_plot_arrange <- list()
for (soil in soil_names) {
    arrange <- ggarrange(pxrf_plots[[soil]][["Al"]],
                         pxrf_plots[[soil]][["Ca"]],
                         pxrf_plots[[soil]][["Mg"]],
                         pxrf_plots[[soil]][["Fe"]],
                         pxrf_plots[[soil]][["Si"]],
                         pxrf_plots[[soil]][["Ti"]],
                         pxrf_plots[[soil]][["Zr"]],
                         pxrf_plots[[soil]][["Ti/Zr"]],
                         nrow = 1, widths = c(1.3, 1, 1, 1, 1, 1, 1, 1))
    arrange <- annotate_figure(arrange, top = text_grob(plot_titles[soil],
                                             size = 12,
                                             family = "Times New Roman"))
    pxrf_plot_arrange[[soil]] <- arrange
}

pxrf_depth_plots <- ggarrange(pxrf_plot_arrange[["ox_ufla"]],
                         pxrf_plot_arrange[["ox_quartz"]],
                         pxrf_plot_arrange[["glei_ufla"]], nrow = 3)

ggsave("pxrf_depth.png", pxrf_depth_plots, device = "png",
       width = 250, height = 150, units = "mm", bg = "white")

### FERTILITY ATTRIBUTES ###########################################################################

# Loading data
fertility_ox_ufla <- read_csv("../Data/Fertilidade/fertilidade_ox_ufla.csv")
fertility_ox_quartz <- read_csv("../Data/Fertilidade/fertilidade_ox_quartz.csv")
fertility_glei_ufla <- read_csv("../Data/Fertilidade/fertilidade_glei_ufla.csv")

# Fixing column names
column_names_symbols <- c("ID", "Protocol", "Sample", "Depth", "pH",
                          "K⁺", "P", "Na⁺", "Ca²⁺", "Mg²⁺", "Al³⁺", "H⁺+Al³⁺",
                          "SB", "t", "T", "V", "m", "O.M.", "P.Rem")

## Version without symbols, some functions cannot handle superscripts
column_names <- c("ID", "Protocol", "Sample", "Depth", "pH", "K",
                  "P", "Na", "Ca", "Mg", "Al", "H+Al", "SB", "t",
                  "T", "V", "m", "O.M.", "P.Rem")

names(fertility_ox_ufla) <- column_names
names(fertility_ox_quartz) <- column_names
names(fertility_glei_ufla) <- column_names

# Plots
## Variables used in plots
plot_vars <- column_names[c(6, 9:11, 16:18)]
y_labels <- column_names_symbols[c(6, 9:11, 16:18)]

## Base plots
fertility_plots_ox_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_ox_ufla,
            aes_string(x = "Depth", y = variable))
    fertility_plots_ox_ufla[[variable]] <- plot
}

fertility_plots_ox_quartz <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_ox_quartz,
            aes_string(x = "Depth", y = variable))
    fertility_plots_ox_quartz[[variable]] <- plot
}

fertility_plots_glei_ufla <- list()
for (variable in plot_vars) {
    plot <- ggplot(fertility_glei_ufla,
            aes_string(x = "Depth", y = variable))
    fertility_plots_glei_ufla[[variable]] <- plot
}

## List with base plots
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
fertility_plots <- list(ox_ufla = fertility_plots_ox_ufla,
                        ox_quartz = fertility_plots_ox_quartz,
                        glei_ufla = fertility_plots_glei_ufla)

plot_titles <- list(ox_ufla = "Typic Udifolists (TU)",
                    ox_quartz = "Typic Dystrustepts (TD)",
                    glei_ufla = "Typic Endoaquents (TE)")

eixo_x <- rev(c(5, 20, 35, 50, 65, 80, 95, 110))

## Formatting
for (soil in soil_names) {
    count <- 1
    for (variable in plot_vars) {
        plot <- fertility_plots[[soil]][[variable]] +
            geom_point() +
            geom_line() +
            coord_flip() +
            scale_y_continuous(position = "right",
                               breaks = pretty_breaks(n = 3)) +
            scale_x_continuous(breaks = seq(-105, 0, 15),
                               labels = eixo_x) +
            xlab("Depth (cm)") + ylab(y_labels[count]) +
            theme_bw() +
            theme(text = element_text(family = "Times New Roman"),
                  axis.text = element_text(size = 7),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.x = element_blank())
        if (variable != "K") {
            plot <- plot +
                theme(axis.title.y = element_blank(),
                      axis.text.y = element_blank())
        }
        fertility_plots[[soil]][[variable]] <- plot
        count <- count + 1
    }
}

## Plot arrange
fertility_plot_arrange <- list()
for (soil in soil_names) {
    arrange <- ggarrange(fertility_plots[[soil]][["K"]],
                         fertility_plots[[soil]][["Ca"]],
                         fertility_plots[[soil]][["Mg"]],
                         fertility_plots[[soil]][["Al"]],
                         fertility_plots[[soil]][["V"]],
                         fertility_plots[[soil]][["m"]],
                         fertility_plots[[soil]][["O.M."]],
                         nrow = 1, widths = c(1.3, 1, 1, 1, 1, 1, 1))
    arrange <- annotate_figure(arrange, top = text_grob(plot_titles[soil],
                                             size = 12,
                                             family = "Times New Roman"))
    fertility_plot_arrange[[soil]] <- arrange
}

fertility_depth_plots <- ggarrange(fertility_plot_arrange[["ox_ufla"]],
                         fertility_plot_arrange[["ox_quartz"]],
                         fertility_plot_arrange[["glei_ufla"]], nrow = 3)

ggsave("fertility_depth.png", fertility_depth_plots, device = "png",
       width = 150, height = 130, units = "mm", bg = "white")

### TEXTURE ########################################################################################

# Loading data
texture_ox_ufla <- read_csv("../Data/Textura/textura_ox_ufla.csv")
texture_ox_quartz <- read_csv("../Data/Textura/textura_ox_quartz.csv")
texture_glei_ufla <- read_csv("../Data/Textura/textura_glei_ufla.csv")

column_names <- c("ID", "Protocol", "Sample", "Depth", "Clay",
                  "Silt", "Coarse sand", "Fine sand")

names(texture_ox_ufla) <- column_names
names(texture_ox_quartz) <- column_names
names(texture_glei_ufla) <- column_names

# Plots
## Variables used in plots
plot_vars <- column_names[5:8]

## List with data
soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
texture_data <- list(ox_ufla = texture_ox_ufla,
                        ox_quartz = texture_ox_quartz,
                        glei_ufla = texture_glei_ufla)

## Organazing data for plotting
for (soil in soil_names) {
    texture_data[[soil]] <- texture_data[[soil]] %>%
        pivot_longer(c(5:8), names_to = "Fração", values_to = "Content (%)") %>%
        mutate(Fração = factor(Fração,
                                 levels = c("Coarse sand", "Fine sand",
                                            "Silt", "Clay"),
                                 ordered = T))
}

plot_titles <- list(ox_ufla = "Typic Udifolists (TU)",
                    ox_quartz = "Typic Dystrustepts (TD)",
                    glei_ufla = "Typic Endoaquents (TE)")

colors <- c("#B83100", "#008FF5", "#DBDB95", "#9E7757")

## Creating plots
texture_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(texture_data[[soil]],
                   aes(x = Depth, y = `Content (%)`, fill = Fração)) +
        geom_bar(position = "fill", stat = "identity") +
        geom_text(aes(label = `Content (%)`),
                  position = position_fill(vjust = 0.5),
                  family = "Times New Roman",
                  size = 4) +
        coord_flip() + ggtitle(plot_titles[soil]) +
        scale_fill_manual(values = rev(colors)) +
        scale_y_continuous(position = "right",
                           breaks = seq(0, 1, 0.25),
                           labels = seq(0, 100, 25)) +
        scale_x_continuous(breaks = seq(-105, 0, 15),
                           labels = eixo_x,
                           limits = c(-112, 7)) +
        theme_bw() + xlab("Depth (cm)") +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank())
    texture_plots[[soil]] <- plot
}

texture_plots_arrange <- ggarrange(texture_plots[["ox_ufla"]],
                            texture_plots[["ox_quartz"]],
                            texture_plots[["glei_ufla"]],
                            nrow = 1, common.legend = T, legend = "bottom")


ggsave("texture_plots.png", texture_plots_arrange, device = "png",
       width = 210, height = 130, units = "mm", bg = "white")

### VIS-NIR ########################################################################################

## Loading data and calculating repetitions' means
## Adding depths
depth_order <- c("5 cm", "20 cm", "35 cm",
                 "50 cm", "65 cm", "80 cm",
                 "95 cm", "110 cm")

depth <- factor(c(rep("5 cm", 5), rep("20 cm", 5),
                  rep("35 cm", 5), rep("50 cm", 5),
                  rep("65 cm", 5), rep("80 cm", 5),
                  rep("95 cm", 5), rep("110 cm", 5)),
                  levels = depth_order, ordered = T)

visnir_ox_ufla <- read_csv("../Data/Espectral/visnir_ox_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Depth = depth, .after = "amostra")

visnir_ox_quartz <- read_csv("../Data/Espectral/visnir_ox_quartz.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Depth = depth, .after = "amostra")

visnir_glei_ufla <- read_csv("../Data/Espectral/visnir_glei_ufla.csv") %>%
    group_by(amostra) %>%
    summarize(across(c(3:2153), mean)) %>%
    add_column(Depth = depth, .after = "amostra")

## Continuum removal
oldnames <- c(1:2151)
newnames <- c(350:2500)

visnir_ox_ufla_cr <- visnir_ox_ufla %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_ox_ufla$amostra,
               Depth = depth, .before = "350")

visnir_ox_quartz_cr <- visnir_ox_quartz %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_ox_quartz$amostra,
               Depth = depth, .before = "350")

visnir_glei_ufla_cr <- visnir_glei_ufla %>%
    select(-c(1, 2)) %>%
    continuumRemoval(interpol = "linear") %>%
    as_tibble() %>%
    rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>%
    add_column(amostra = visnir_glei_ufla$amostra,
               Depth = depth, .before = "350")

## Organizing data for plotting
## Grouping data per depth
visnir_ox_ufla_cr <- visnir_ox_ufla_cr %>%
                  group_by(Depth) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Wavelength (nm)",
                               values_to = "Reflectance Factor") %>%
                  mutate("Wavelength (nm)" = as.numeric(`Wavelength (nm)`))
              
visnir_ox_quartz_cr <- visnir_ox_quartz_cr %>%
                  group_by(Depth) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Wavelength (nm)",
                               values_to = "Reflectance Factor") %>%
                  mutate("Wavelength (nm)" = as.numeric(`Wavelength (nm)`))

visnir_glei_ufla_cr <- visnir_glei_ufla_cr %>%
                  group_by(Depth) %>%
                  summarize(across(c(2:2152), mean)) %>%
                  pivot_longer(c(2:2152),
                               names_to = "Wavelength (nm)",
                               values_to = "Reflectance Factor") %>%
                  mutate("Wavelength (nm)" = as.numeric(`Wavelength (nm)`))

soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
visnir_data <- list(ox_ufla = visnir_ox_ufla_cr,
                        ox_quartz = visnir_ox_quartz_cr,
                        glei_ufla = visnir_glei_ufla_cr)

plot_titles <- list(ox_ufla = "Typic Udifolists (TU)",
                    ox_quartz = "Typic Dystrustepts (TD)",
                    glei_ufla = "Typic Endoaquents (TE)")

colors <- brewer.pal(8, "RdGy")

features <- c(480, 650, 1415, 1930, 2205, 2265, 2350, 2385)
feature_names <- c(as.character(features[1:6]), "2350, 2385")
positions <- rep(0, 7)

segment_coord <- tibble(x = c(400, 400, 700),
                      xend = c(700, 400, 700),
                      y = c(0.2, 0.18, 0.18),
                      yend = c(0.2, 0.22, 0.22))

visnir_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(visnir_data[[soil]],
                   aes(x = `Wavelength (nm)`, y = `Reflectance Factor`)) +
        geom_line(aes(color = Depth)) +
        ggtitle(plot_titles[soil]) +
        scale_y_continuous(breaks = seq(0, 1, 0.25),
                           limits = c(0, 1)) +
        scale_color_manual(values = colors) +
        guides(color = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())

    if (soil != "glei_ufla") {
        plot  <- plot +
            xlab("") +
            geom_segment(data = segment_coord,
                         aes(x = x, y = y,
                             xend = xend, yend = yend)) +
            annotate("text", label = "400-700",
                     x = 500, y = 0.15,
                     hjust = 0,
                     size = 3,
                    family = "Times New Roman")
    }

        for (i in seq_len(length(features))) {
            plot <- plot +
                geom_vline(linetype = 2, xintercept = features[i]) +
                annotate("text", label = feature_names[i],
                         x = features[i] - 35, y = positions[i], angle = 90,
                         hjust = 0,
                         size = 3,
                        family = "Times New Roman")
        }
    visnir_plots[[soil]] <- plot
}

visnir_cr_plots_arrange <- ggarrange(visnir_plots[["ox_ufla"]],
                            visnir_plots[["ox_quartz"]],
                            visnir_plots[["glei_ufla"]],
                            nrow = 3, common.legend = T,
                            legend = "bottom")

print(visnir_cr_plots_arrange)

ggsave("visnir_cr_plots.png", visnir_cr_plots_arrange, device = "png",
       width = 210, height = 220, units = "mm", bg = "white")

### MIR ############################################################################################

# Loading data and calculating repetitions' means
# Adding depths
depth_order <- c("5 cm", "20 cm", "35 cm",
                 "50 cm", "65 cm", "80 cm",
                 "95 cm", "110 cm")

depth <- factor(c(rep("5 cm", 5), rep("20 cm", 5),
                  rep("35 cm", 5), rep("50 cm", 5),
                  rep("65 cm", 5), rep("80 cm", 5),
                  rep("95 cm", 5), rep("110 cm", 5)),
                  levels = depth_order, ordered = T)

mir_ox_ufla <- read_csv("../Data/Espectral/mir_ox_ufla.csv") %>%
    add_column(Profundidade = depth, .after = "amostra")

mir_ox_quartz <- read_csv("../Data/Espectral/mir_ox_quartz.csv") %>%
    add_column(Profundidade = depth, .after = "amostra")

mir_glei_ufla <- read_csv("../Data/Espectral/mir_glei_ufla.csv") %>%
    add_column(Profundidade = depth, .after = "amostra")

# Organizing data for plotting
# Grouping data per depth
mir_ox_ufla <- mir_ox_ufla %>%
               group_by(Profundidade) %>%
               summarize(across(c(2:3402), mean)) %>%
               pivot_longer(c(2:3402),
                            names_to = "Wavenumber (cm-1)",
                            values_to = "Reflectance Factor") %>%
               mutate("Wavenumber (cm-1)" = -as.numeric(`Wavenumber (cm-1)`)) %>%
               filter(`Wavenumber (cm-1)` > -3714)
              
mir_ox_quartz <- mir_ox_quartz %>%
                 group_by(Profundidade) %>%
                 summarize(across(c(2:3402), mean)) %>%
                 pivot_longer(c(2:3402),
                              names_to = "Wavenumber (cm-1)",
                              values_to = "Reflectance Factor") %>%
                 mutate("Wavenumber (cm-1)" = -as.numeric(`Wavenumber (cm-1)`)) %>%
                 filter(`Wavenumber (cm-1)` > -3714)

mir_glei_ufla <- mir_glei_ufla %>%
                 group_by(Profundidade) %>%
                 summarize(across(c(2:3402), mean)) %>%
                 pivot_longer(c(2:3402),
                              names_to = "Wavenumber (cm-1)",
                              values_to = "Reflectance Factor") %>%
                 mutate("Wavenumber (cm-1)" = -as.numeric(`Wavenumber (cm-1)`)) %>%
                 filter(`Wavenumber (cm-1)` > -3714)

soil_names <- c("ox_ufla", "ox_quartz", "glei_ufla")
mir_data <- list(ox_ufla = mir_ox_ufla,
                        ox_quartz = mir_ox_quartz,
                        glei_ufla = mir_glei_ufla)

plot_titles <- list(ox_ufla = "Histisol",
                    ox_quartz = "Typic Dystrustepts (TD)",
                    glei_ufla = "Typic Endoaquents (TE)")

colors <- brewer.pal(8, "RdGy")

features <- -c(3694, 3622, 3529, 2924, 2852, 2233, 1993, 1870, 1788, 1527, 1220,
               1159, 1050, 800)
feature_names <- as.character(-features)
positions <- c(rep(0.04, 5), 0, rep(0.04, 20))
positions_ox <- c(rep(0.02, 5), 0, rep(0.02, 20))

mir_plots <- list()
for (soil in soil_names) {
    plot <- ggplot(mir_data[[soil]],
                   aes(x = `Wavenumber (cm-1)`, y = `Reflectance Factor`)) +
        xlab(expression("Wavenumber "~(cm^{-1}))) +
        geom_line(aes(color = Profundidade)) +
        ggtitle(plot_titles[soil]) +
        scale_y_continuous(breaks = seq(0, 0.1, 0.02),
                           limits = c(0, 0.05)) +
        scale_x_continuous(breaks = seq(-3800, -600, 400),
                           limits = c(-3800, -600),
                           labels = as.character(seq(3800, 600, -400))) +
        scale_color_manual(values = colors) +
        guides(color = guide_legend(nrow = 1)) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())
    mir_plots[[soil]] <- plot
    
    if (soil != "glei_ufla") {
      plot <- plot +
        xlab("")
    }

        for (i in seq_len(length(features))) {
            if (soil == "ox_ufla") {
                plot <- plot +
                    geom_vline(linetype = 2, size = 0.1,
                               xintercept = features[i]) +
                    annotate("text", label = feature_names[i],
                             x = features[i] - 35,
                             y = positions_ox[i], angle = 90,
                             hjust = 0,
                             size = 3,
                            family = "Times New Roman")
            } else {
                plot <- plot +
                    geom_vline(linetype = 2, size = 0.1,
                               xintercept = features[i]) +
                    annotate("text", label = feature_names[i],
                             x = features[i] - 35, y = positions[i], angle = 90,
                             hjust = 0,
                             size = 3,
                            family = "Times New Roman")
            }
        }
    mir_plots[[soil]] <- plot
}

mir_plots[["ox_ufla"]]  <- mir_plots[["ox_ufla"]] +
        scale_y_continuous(breaks = seq(0, 0.1, 0.02),
                           limits = c(0, 0.023))

# ggarrange(mir_plots[["ox_ufla"]], nrow = 1, legend = "bottom")
# ggarrange(mir_plots[["ox_quartz"]], nrow = 1, legend = "bottom")
# ggarrange(mir_plots[["glei_ufla"]], nrow = 1, legend = "bottom")

mir_plots_arrange <- ggarrange(mir_plots[["ox_ufla"]],
                            mir_plots[["ox_quartz"]],
                            mir_plots[["glei_ufla"]],
                            nrow = 3, common.legend = T,
                            legend = "bottom")
mir_plots_arrange

ggsave("mir_plots.png", mir_plots_arrange, device = "png",
       width = 210, height = 220, units = "mm", bg = "white")

# XRD ##############################################################################################

# Typic Udifolists (TU)
## Loading data
## Pivoting data and adding texture info
xrd_ox_ufla <- read_csv("../Data/DRX/xrd_ox_ufla.csv")

xrd_ox_ufla <- xrd_ox_ufla %>%
    filter(X > 7) %>%
    pivot_longer(c(2:7), names_to = "Horizon", values_to = "Counts")

xrd_ox_ufla$Texture[str_ends(xrd_ox_ufla$Horizon, "sand")] <- "Sand"
xrd_ox_ufla$Texture[str_ends(xrd_ox_ufla$Horizon, "silt")] <- "Silt"
xrd_ox_ufla$Texture[str_ends(xrd_ox_ufla$Horizon, "clay")] <- "Clay"

xrd_ox_ufla_sand <- xrd_ox_ufla %>%
    filter(Texture == "Sand")
xrd_ox_ufla_silt <- xrd_ox_ufla %>%
    filter(Texture == "Silt")
xrd_ox_ufla_clay <- xrd_ox_ufla %>%
    filter(Texture == "Clay")

xrd_ox_ufla_data <- list(Sand = xrd_ox_ufla_sand,
                         Silt = xrd_ox_ufla_silt,
                         Clay = xrd_ox_ufla_clay)

xrd_plot_names <- c("Sand", "Silt", "Clay")

colors <- brewer.pal(6, "RdGy")

xrd_ox_ufla_plots <- list()
for (texture in xrd_plot_names) {
    plot <- ggplot(xrd_ox_ufla_data[[texture]], aes(x = X, y = Counts)) +
        geom_line(aes(color = Horizon), size = 0.2) +
        ggtitle(paste("Typic Udifolists (TU) -", texture)) +
        ylab("Intensity (counts)") +
        xlab(expression(degree*2*theta~Cuk*alpha)) +
        scale_x_continuous(expand = c(0, 1),
                           breaks = seq(6, 50, 4),
                           limits = c(6, 50)) +
        scale_y_continuous(breaks = seq(0, 9000, 3000),
                           limits = c(0, 9000)) +
        scale_color_manual(labels = c("A", "Hb"), values = colors) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())

    if (texture == "Sand") {
        plot <- plot +
        scale_y_continuous(breaks = seq(0, 15000, 3000),
                           limits = c(0, 15000))
    }

    xrd_ox_ufla_plots[[texture]] <- plot
}


### Labels
mineral_names <- c("Kt", "Gb", "Kt", "Qz", "Kt + An", "Qz", rep("Qz", 5))
minerals_text_x <- c(12, 17.8, 19.5, 20.5, 22, 27, 36, 39, 40, 42, 45.3)
minerals_text_y <- c(6000, 7000, 6000, 13000, 8000, 15000, 10000, 7000, 6000,
                     6500, 5700)
xrd_ox_ufla_plots[["Sand"]] <- xrd_ox_ufla_plots[["Sand"]] +
        geom_segment(aes(x = 24.7, y = 5100, xend = 23.5, yend = 7500),
                     size = 0.2, arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Kt", "Gb", "Qz", "Kt", "Kt + An", "Qz", "Gb + Kt")
minerals_text_x <- c(12, 17.9, 20.5, 21.5, 23, 26.4, 19.3)
minerals_text_y <- c(6500, 8500, 7500, 6700, 6500, 9000, 2700)
xrd_ox_ufla_plots[["Silt"]] <- xrd_ox_ufla_plots[["Silt"]] +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Kt", "Gb", "Gb + Kt", "Kt", "Kt + An", "Kt")
minerals_text_x <- c(12, 18, 19.3, 21.4, 23.7, 37.3)
minerals_text_y <- c(6500, 8900, 7100, 6700, 6200, 6200)
xrd_ox_ufla_plots[["Clay"]] <- xrd_ox_ufla_plots[["Clay"]] +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

xrd_ox_ufla_arrange <- ggarrange(xrd_ox_ufla_plots[["Sand"]],
                        xrd_ox_ufla_plots[["Silt"]],
                        xrd_ox_ufla_plots[["Clay"]],
                        ncol = 1, common.legend = T,
                        legend = "bottom")

# Typic Dystrustepts (TD)
### Loading data
### Pivoting data and adding texture info
xrd_ox_quartz <- read_csv("../Data/DRX/xrd_ox_quartz.csv")

xrd_ox_quartz <- xrd_ox_quartz %>%
    filter(X > 7) %>%
    pivot_longer(c(2:7), names_to = "Horizon", values_to = "Counts")

xrd_ox_quartz$Texture[str_ends(xrd_ox_quartz$Horizon, "sand")] <- "Sand"
xrd_ox_quartz$Texture[str_ends(xrd_ox_quartz$Horizon, "silt")] <- "Silt"
xrd_ox_quartz$Texture[str_ends(xrd_ox_quartz$Horizon, "clay")] <- "Clay"

### Filtering data
### Changing count numbers for better visualization
xrd_ox_quartz_sand <- xrd_ox_quartz %>%
    filter(Texture == "Sand") %>%
    mutate(Counts = case_when(
                    startsWith(Horizon, "A_") ~ Counts - 1000,
                    startsWith(Horizon, "Ab_") ~ Counts - 5000))
xrd_ox_quartz_silt <- xrd_ox_quartz %>%
    filter(Texture == "Silt") %>%
    mutate(Counts = case_when(
                    startsWith(Horizon, "A_") ~ Counts + 1000,
                    startsWith(Horizon, "Ab_") ~ Counts - 2000))
xrd_ox_quartz_clay <- xrd_ox_quartz %>%
    filter(Texture == "Clay") %>%
    mutate(Counts = case_when(
                    startsWith(Horizon, "A_") ~ Counts + 3000,
                    TRUE ~ Counts))

xrd_ox_quartz_data <- list(Sand = xrd_ox_quartz_sand,
                         Silt = xrd_ox_quartz_silt,
                         Clay = xrd_ox_quartz_clay)

xrd_plot_names <- c("Sand", "Silt", "Clay")

colors <- brewer.pal(6, "RdGy")

xrd_ox_quartz_plots <- list()
for (texture in xrd_plot_names) {
    plot <- ggplot(xrd_ox_quartz_data[[texture]], aes(x = X, y = Counts)) +
        geom_line(aes(color = Horizon), size = 0.2) +
        ggtitle(paste("Typic Dystrustepts (TD) -", texture)) +
        ylab("Intensity (counts)") +
        xlab(expression(degree*2*theta~Cuk*alpha)) +
        scale_x_continuous(expand = c(0, 1),
                           breaks = seq(6, 50, 4),
                           limits = c(6, 50)) +
        scale_y_continuous(breaks = seq(0, 9000, 3000),
                           limits = c(0, 9000)) +
        scale_color_manual(labels = c("Bw", "Ab"), values = colors) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())

    if (texture == "Sand") {
        plot <- plot +
        scale_y_continuous(breaks = seq(0, 15000, 3000),
                           limits = c(0, 15000))
    }

    xrd_ox_quartz_plots[[texture]] <- plot
}

### Labels
mineral_names <- c("Qz", "Il", "An", rep("Qz", 6))
minerals_text_x <- c(19.9, 23.8, 25, 26, 36.2, 39, 40.1, 42, 45.4)
minerals_text_y <- c(15000, 7700, 7700, 15000, 8000, 8600, 7000, 8000, 6600)
arrow_coord <- tibble(x = c(24, 25.6), xend = c(24, 25.5),
                      y = c(5500, 5600), yend = c(7200, 7200))
xrd_ox_quartz_plots[["Sand"]] <- xrd_ox_quartz_plots[["Sand"]] +
        geom_segment(data = arrow_coord,
                         aes(x = x, y = y,
                         xend = xend, yend = yend),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Mc", "Kt", "Mc", "Kt", "Qz", "Il", "Qz", "Ru",
                   "Mc + 2:1", "Kt + Gb", "Qz", "Kt", rep("Qz", 4))
minerals_text_x <- c(8.6, 12.3, 17.5, 19.5, 20.6, 23.9, 26.3, 27.3,
                     29.9, 32.7, 36.4, 37.5, 39.4, 40.1, 42.3, 45.7)
minerals_text_y <- c(5500, 5000, 4900, 5500, 9000, 6200, 9000, 6400,
                     6400, 5000, 9000, 5000, 9000, 6000, 6500, 6000)
arrow_coord <- tibble(x = c(24, 27.3, 27.8, 29.8),
                      xend = c(24.1, 27.4, 30.4, 30.9),
                      y = c(4700, 4800, 4800, 4800),
                      yend = c(5800, 6000, 6000, 6000))
xrd_ox_quartz_plots[["Silt"]] <- xrd_ox_quartz_plots[["Silt"]] +
        geom_segment(data = arrow_coord,
                         aes(x = x, y = y,
                         xend = xend, yend = yend),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Kt", "Gb", "Kt + Gb", "Gt", "Kt", "Qz", "Kt + Gb",
                   "Hm", "Kt", "Gt")
minerals_text_x <- c(12, 17.8, 18.6, 21.4, 24.8, 26.4, 33.5, 35.8, 37.6, 38.9)
minerals_text_y <- c(5300, 5700, 6800, 6200, 6200, 6200, 6800, 5800, 6000, 5800)
arrow_coord <- tibble(x = c(19.8, 21.4, 24.8, 26.4, 35.8, 37.6, 38.4, 34.7),
                      xend = c(19.8, 21.6, 25.1, 26.6, 36.2, 37.9, 39.1, 34.9),
                      y = c(5500, 4700, 4700, 4600, 4400, 4500, 4400, 4800),
                      yend = c(6500, 5800, 5800, 5800, 5400, 5500, 5400, 6500))
xrd_ox_quartz_plots[["Clay"]] <- xrd_ox_quartz_plots[["Clay"]] +
        geom_segment(data = arrow_coord,
                         aes(x = x, y = y,
                         xend = xend, yend = yend),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

xrd_ox_quartz_arrange <- ggarrange(xrd_ox_quartz_plots[["Sand"]],
                        xrd_ox_quartz_plots[["Silt"]],
                        xrd_ox_quartz_plots[["Clay"]],
                        ncol = 1, common.legend = T,
                        legend = "bottom")

# Typic Endoaquents (TE)
### Loading data
### Pivoting data and adding texture info
xrd_glei_ufla <- read_csv("../Data/DRX/xrd_glei_ufla.csv")

xrd_glei_ufla <- xrd_glei_ufla %>%
    filter(X > 7) %>%
    pivot_longer(c(2:7), names_to = "Horizon", values_to = "Counts")

xrd_glei_ufla$Texture[str_ends(xrd_glei_ufla$Horizon, "sand")] <- "Sand"
xrd_glei_ufla$Texture[str_ends(xrd_glei_ufla$Horizon, "silt")] <- "Silt"
xrd_glei_ufla$Texture[str_ends(xrd_glei_ufla$Horizon, "clay")] <- "Clay"

xrd_glei_ufla_sand <- xrd_glei_ufla %>%
    filter(Texture == "Sand")
xrd_glei_ufla_silt <- xrd_glei_ufla %>%
    filter(Texture == "Silt")
xrd_glei_ufla_clay <- xrd_glei_ufla %>%
    filter(Texture == "Clay")

xrd_glei_ufla_data <- list(Sand = xrd_glei_ufla_sand,
                         Silt = xrd_glei_ufla_silt,
                         Clay = xrd_glei_ufla_clay)

xrd_plot_names <- c("Sand", "Silt", "Clay")

colors <- brewer.pal(6, "RdGy")

xrd_glei_ufla_plots <- list()
for (texture in xrd_plot_names) {
    plot <- ggplot(xrd_glei_ufla_data[[texture]], aes(x = X, y = Counts)) +
        geom_line(aes(color = Horizon), size = 0.2) +
        ggtitle(paste("Typic Endoaquents (TE) -", texture)) +
        ylab("Intensity (counts)") +
        xlab(expression(degree*2*theta~Cuk*alpha)) +
        scale_x_continuous(expand = c(0, 1),
                           breaks = seq(6, 50, 4),
                           limits = c(6, 50)) +
        scale_y_continuous(breaks = seq(0, 9000, 3000),
                           limits = c(0, 9000)) +
        scale_color_manual(labels = c("A", "Cgb"), values = colors) +
        theme_bw() +
        theme(text = element_text(family = "Times New Roman"),
              panel.grid.minor.y = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.title = element_blank())

    if (texture == "Sand") {
        plot <- plot +
        scale_y_continuous(breaks = seq(0, 15000, 3000),
                           limits = c(0, 15000))
    }

    xrd_glei_ufla_plots[[texture]] <- plot
}

### Labels
mineral_names <- c("Il", rep("Qz", 7))
minerals_text_x <- c(23.6, 21, 26.7, 36.2, 39, 40, 42, 45.4)
minerals_text_y <- c(6800, 15000, 15000, 8500, 8700, 7300, 10200, 6200)
xrd_glei_ufla_plots[["Sand"]] <- xrd_glei_ufla_plots[["Sand"]] +
        geom_segment(aes(x = 24, y = 4700,
                     xend = 24, yend = 6400),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Mc", "Ta", "Kt", "Mc", "Gb", "Kt", "Qz", "Kt", "Qz", "Ft",
                   "Ta", "Kt", "Qz", "Kt", rep("Qz", 4))
minerals_text_x <- c(7.3, 9.3, 12.6, 17.3, 18.1, 19.6, 20.6, 24.7, 26.7, 27,
                     27.8, 34.4, 36.3, 37.7, 39.2, 40.1, 42.2, 45.6)
minerals_text_y <- c(5800, 5800, 9000, 2500, 5800, 5600, 9000, 8500, 9000,
                     6500, 5300, 4900, 5200, 5000, 5300, 5000, 5000, 5000)
arrow_coord <- tibble(x = c(8.7, 9.5, 17.7, 18.2, 26.95),
                      xend = c(8.1, 9.6, 17.6, 18.3, 27.1),
                      y = c(4600, 4600, 1000, 4600, 5400),
                      yend = c(5400, 5400, 2100, 5400, 6000))
xrd_glei_ufla_plots[["Silt"]] <- xrd_glei_ufla_plots[["Silt"]] +
        geom_segment(data = arrow_coord,
                         aes(x = x, y = y,
                         xend = xend, yend = yend),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

mineral_names <- c("Gt", "Kt", "Gb", "Kt + Gb", rep("Kt", 4))
minerals_text_x <- c(21, 12.5, 18.1, 19.5, 24.6, 35.3, 38.3, 45.3)
minerals_text_y <- c(3200, 9000, 6500, 7300, 8300, 6500, 6500, 5500)
xrd_glei_ufla_plots[["Clay"]] <- xrd_glei_ufla_plots[["Clay"]] +
        geom_segment(aes(x = 21.2, y = 2300,
                     xend = 21.4, yend = 2900),
                     size = 0.2,
                     arrow = arrow(length = unit(1, "mm"))) +
        geom_segment(aes(x = 34.5, y = 6000,
                         xend = 36.5, yend = 6000),
                     size = 0.2) +
        geom_segment(aes(x = 37.5, y = 6000,
                         xend = 39.5, yend = 6000),
                     size = 0.2) +
        annotate("text", label = mineral_names,
                x = minerals_text_x, y = minerals_text_y,
                hjust = 0,
                size = 2.5,
                family = "Times New Roman")

xrd_glei_ufla_arrange <- ggarrange(xrd_glei_ufla_plots[["Sand"]],
                        xrd_glei_ufla_plots[["Silt"]],
                        xrd_glei_ufla_plots[["Clay"]],
                        ncol = 1, common.legend = T,
                        legend = "bottom")

xrd_arrange <- ggarrange(xrd_ox_ufla_arrange,
                         xrd_ox_quartz_arrange,
                         xrd_glei_ufla_arrange,
                         ncol = 3)

ggsave("xrd_plots.png", xrd_arrange, device = "png",
       width = 297 * 1.3, height = 210 * 1.3, units = "mm", bg = "white")