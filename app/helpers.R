my_theme <- function() {
    theme_minimal() +
        theme(
            legend.text=element_text(size=7),
            legend.title = element_text(size=8),
            plot.title = element_text(size = 10, hjust = 0.5),
            plot.subtitle = element_text(size = 10, hjust = 0.5),
            strip.text = element_text(size=8),
            axis.text = element_text(size = 7),
            axis.title = element_text(size = 8),
            legend.position = "bottom",
            plot.background = element_rect(color = 'white')
        )
}

get_point <- function(lat, lon, crs) {
    return(sf::st_as_sf(data.frame(lat = lat, lon = lon), coords = c('lon', 'lat'), crs = 4326) |>
        sf::st_transform(crs))
}

get_hex_id <- function(conus_hex_shp, lat, lon) {
    point <- get_point(lat, lon, sf::st_crs(conus_hex_shp))
    
    hex_id <- sf::st_filter(conus_hex_shp, point, .predicate = sf::st_intersects) |> 
        dplyr::pull(USHEXES_ID)
    return(hex_id)
}

get_best_age_match <- function(c_x_albedo, age, column) {
    c_x_albedo_age_match <- c_x_albedo |>
        dplyr::filter(!is.na({{column}})) |>
        dplyr::group_by(TYPGRPCD) |> 
        dplyr::mutate(age_diff = abs(AgeGroup - age)) |> 
        dplyr::filter(age_diff == min(age_diff, na.rm = TRUE)) |> 
        dplyr::arrange(AgeGroup) |> 
        dplyr::slice_head(n = 1) |> 
        dplyr::ungroup()
}

filter_groups_to_optimal <- function(hex_conditions, c_x_albedo, column, direction, 
                                     rank = 1) {
    hex_groups <- hex_conditions |>
        dplyr::left_join(c_x_albedo,
                         by = 'TYPGRPCD') |>
        dplyr::filter(!is.na({{column}})) |>
        dplyr::group_by(USHEXES_ID)
    
    if (direction == 'descending') {
        hex_groups <- hex_groups |> 
            dplyr::arrange(-{{column}})
    } else {
        hex_groups <- hex_groups |> 
            dplyr::arrange({{column}})
    }
    
    if(rank == -1) {
        optimal <- hex_groups |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()    
    } else if (rank != 1) {
        optimal <- hex_groups |>
            dplyr::slice_head(n = rank) |>
            dplyr::slice_tail(n = 1) |>
            dplyr::ungroup()
    } else {
        optimal <- hex_groups |>
            dplyr::slice_head(n = 1) |>
            dplyr::ungroup()
    }
    return(optimal)
    
}

get_ts_plot <- function(c_albedo_table, groups, max_age = 200, write_file = NULL) {
    data <- c_albedo_table |> 
        dplyr::filter(ForestTypeGroup %in% groups, 
                      AgeGroup <= max_age)
    c_data <- data |> 
        tidyr::pivot_longer(c('carbon', 'cumTDEE', 'joint_c'), values_to = 'c', names_to = 'type') |>
        dplyr::mutate(type = dplyr::case_when(
            type == 'cumTDEE' ~ "Carbon-equivalent Albedo Offset",
            type == 'joint_c' ~ "Joint C-albedo",
            type == 'carbon' ~ 'Non-soil Carbon'
        )) |>
        dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo')))
    
    upper_se_data <- data |>
        dplyr::mutate(carbon_upper = carbon + carbon_se) |>
        tidyr::pivot_longer(c('carbon_upper', 'cumTDEE_ubSE'), 
                            values_to = 'upper', names_to = 'type') |> 
        dplyr::mutate(type = dplyr::case_when(
            type == 'cumTDEE_ubSE' ~ "Carbon-equivalent Albedo Offset",
            type == 'carbon_upper' ~ "Non-soil Carbon",
        )) |>
        dplyr::select(upper, type, AgeGroup, ForestTypeGroup)
    
    lower_se_data <- data |>
        dplyr::mutate(carbon_lower = carbon - carbon_se) |>
        tidyr::pivot_longer(c('carbon_lower', 'cumTDEE_lbSE'), 
                            values_to = 'lower', names_to = 'type') |> 
        dplyr::mutate(type = dplyr::case_when(
            type == 'cumTDEE_lbSE' ~ "Carbon-equivalent Albedo Offset",
            type == 'carbon_lower' ~ "Non-soil Carbon",
        )) |>
        dplyr::select(lower, type, AgeGroup, ForestTypeGroup)
    
    data <- c_data |> 
        dplyr::left_join(upper_se_data, by = c('type', 'AgeGroup', 'ForestTypeGroup')) |>
        dplyr::left_join(lower_se_data, by = c('type', 'AgeGroup', 'ForestTypeGroup')) |>
        dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo')))
    
    p <- ggplot(data, aes(x = AgeGroup, y = c, color = ForestTypeGroup)) + 
        scale_color_brewer(palette = 'Dark2', name = 'Forest-type Group') +
        geom_pointrange(aes(ymin = lower, ymax = upper), size = 0.2) + 
        facet_wrap(~ type, ncol = 2, scales = 'free') +
        scale_y_continuous(expand = c(0.005, 0.005)) + 
        scale_x_continuous(expand = c(0.005, 0.005)) +
        geom_smooth(method = 'gam', se = F) +
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linewidth = 1) +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 1) +
        my_theme() +
        theme(legend.position = 'right', 
              panel.grid = element_blank()) +
        ylab(bquote("Mg"~ha^-1*"")) + 
        xlab("Age (years)")
    
    return(p)    
}

get_optimal_groups <- function(lat, 
                               lon, 
                               conus_hex_shp, 
                               hex_conditions, 
                               c_x_albedo, 
                               column, 
                               age = 20, 
                               max_rank = 3) {
    hex_id <- get_hex_id(conus_hex_shp, lat, lon)
    
    optimal <- dplyr::bind_rows(lapply(1:max_rank, \(rank) {
        filter_groups_to_optimal(hex_conditions |> 
                                     dplyr::filter(USHEXES_ID == hex_id), 
                                 get_best_age_match(c_x_albedo, 
                                                    age, 
                                                    {{column}}), 
                                 {{column}}, 
                                 'descending', 
                                 rank = rank) |>
            dplyr::mutate(rank = rank)
    }))
    
    optimal |>
        dplyr::select(-USHEXES_ID, -TYPGRPCD, -age_diff, -AgeGroup)
}

render_ts <- function(lat, lon) {
    hex_conditions <- read.csv("data/hex_conditions.csv")
    c_x_albedo <- read.csv("data/c_albedo_table.csv")
    conus_hex_shp <- sf::st_read("data/conus_hex.gpkg")
    groups <- get_optimal_groups(lat, lon, conus_hex_shp, hex_conditions, c_x_albedo, joint_c)
    get_ts_plot(c_x_albedo, groups$ForestTypeGroup)
}

render_top_three_tab <- function(lat, lon) {
    hex_conditions <- read.csv("data/hex_conditions.csv")
    c_x_albedo <- read.csv("data/c_albedo_table.csv")
    conus_hex_shp <- sf::st_read("data/conus_hex.gpkg")
    groups <- get_optimal_groups(lat, lon, conus_hex_shp, hex_conditions, c_x_albedo, joint_c) |>
        dplyr::relocate(rank, ForestTypeGroup, joint_c, carbon, carbon_se, cumTDEE, cumTDEE_ubSE, cumTDEE_lbSE) |>
        dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2)))
    
    names(groups) <- c("Rank", 
                       "Forest-type Group", 
                       "Joint C-Albedo", 
                       "Carbon", 
                       "Carbon SE", 
                       "Carbon-equivalent Albedo Offset", 
                       "Albedo Offset 1 SE Upper Bound", 
                       "Albedo Offset 1 SE Lower Bound")
    
    groups |> 
        kbl(booktabs = TRUE, align = c("r", "l", rep("r", 6)),
            linesep = "\\addlinespace", format = 'html') |>
        kableExtra::kable_styling(position = 'center')
    
}