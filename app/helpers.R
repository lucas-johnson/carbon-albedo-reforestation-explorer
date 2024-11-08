my_theme <- function() {
    theme_minimal() +
        theme(
            legend.text=element_text(size=12),
            legend.title = element_text(size=14),
            plot.title = element_blank(),
            plot.subtitle = element_blank(),
            strip.text = element_text(size=14),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            legend.position = "right",
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

get_ts_plot <- function(c_albedo_table, preds, groups, max_age = 105, write_file = NULL) {
    data <- c_albedo_table |> 
        dplyr::filter(ForestTypeGroup %in% groups, 
                      AgeGroup <= max_age)
    
    preds <- preds |> 
        dplyr::filter(ForestTypeGroup %in% groups, 
                      AgeGroup <= max_age) |>
        dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo')))
    
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
        dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo'))) |>
        dplyr::mutate(AgeGroup = AgeGroup + 5) |>
        dplyr::mutate(future = AgeGroup <= 25) |>
        dplyr::filter(AgeGroup < max_age)
    
    p <- ggplot(data, aes(x = AgeGroup, y = c, color = ForestTypeGroup)) + 
        scale_color_brewer(palette = 'Dark2', name = 'Forest-type Group') +
        geom_pointrange(aes(ymin = lower, ymax = upper, alpha = future), size = 0.2) + 
        geom_line(data = preds, aes(y = preds, x = AgeGroup, alpha = future, color = ForestTypeGroup), size = .8) +
        facet_wrap(~ type, ncol = 2, scales = 'free') +
        scale_y_continuous(expand = c(0.005, 0.005)) + 
        scale_x_continuous(expand = c(0.005, 0.005)) +
        scale_alpha_discrete(range = c(0.35, 1), guide = NULL) +
        geom_vline(xintercept = 20, linetype = 'longdash') + 
        geom_vline(xintercept = 30, linetype = 'longdash') + 
        annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linewidth = 1) +
        annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 1) +
        my_theme() +
        theme(legend.position = 'right', 
              panel.grid = element_blank()) +
        ylab(bquote("Mg"~ha^-1*"")) + 
        xlab("Age (years)")
    
    return(p)    
}

get_groups <- function(lat, lon, conus_hex_shp, hex_conditions, c_x_albedo, column, direction = 'descending', age = 20) {
    hex_id <- get_hex_id(conus_hex_shp, lat, lon)
    
    groups <- hex_conditions |> 
        dplyr::filter(USHEXES_ID == hex_id) |>
        dplyr::left_join(get_best_age_match(c_x_albedo, 
                                            age, 
                                            {{column}}),
                         by = 'TYPGRPCD') |>
        dplyr::filter(!is.na({{column}}))
    
    if (direction == 'descending') {
        groups <- groups |> 
            dplyr::arrange(-{{column}})
    } else {
        groups <- groups |> 
            dplyr::arrange({{column}})
    }
    groups |>
        dplyr::select(-USHEXES_ID, -TYPGRPCD, -age_diff, -AgeGroup) |>
        dplyr::mutate(rank = 1:dplyr::n())
        
    
}

render_ts <- function(lat_lon, conus_hex_shp, hex_conditions, c_x_albedo) {
    lat <- lat_lon$lat
    lon <- lat_lon$lon
    
    groups <- get_groups(lat, lon, conus_hex_shp, hex_conditions, c_x_albedo, joint_c)
    
    preds <- read.csv("data/gam_preds.csv")
    
    get_ts_plot(c_x_albedo, preds, groups$ForestTypeGroup)
}

render_rank_tab <- function(lat_lon, conus_hex_shp, hex_conditions, c_x_albedo) {
    lat <- lat_lon$lat
    lon <- lat_lon$lon
    
    groups <- get_groups(lat, lon, conus_hex_shp, hex_conditions, c_x_albedo, joint_c) |>
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