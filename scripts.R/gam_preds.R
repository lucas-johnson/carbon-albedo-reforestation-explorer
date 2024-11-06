c_albedo_table <- read.csv("app/data/c_albedo_table.csv") 
c_data <- c_albedo_table |> 
    tidyr::pivot_longer(c('carbon', 'cumTDEE', 'joint_c'), values_to = 'c', names_to = 'type') |>
    dplyr::mutate(type = dplyr::case_when(
        type == 'cumTDEE' ~ "Carbon-equivalent Albedo Offset",
        type == 'joint_c' ~ "Joint C-albedo",
        type == 'carbon' ~ 'Non-soil Carbon'
    )) |>
    dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo')))

upper_se_data <- c_albedo_table |>
    dplyr::mutate(carbon_upper = carbon + carbon_se) |>
    tidyr::pivot_longer(c('carbon_upper', 'cumTDEE_ubSE'), 
                        values_to = 'upper', names_to = 'type') |> 
    dplyr::mutate(type = dplyr::case_when(
        type == 'cumTDEE_ubSE' ~ "Carbon-equivalent Albedo Offset",
        type == 'carbon_upper' ~ "Non-soil Carbon",
    )) |>
    dplyr::select(upper, type, AgeGroup, ForestTypeGroup)

lower_se_data <- c_albedo_table |>
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
    dplyr::filter(!ForestTypeGroup %in% c('Nonstocked', "Western white pine", 'Other', 'Tropical hardwoods', 'Other eastern softwoods', 'Exotic softwoods', 'Exotic hardwoods', 'Other softwoods'))


preds <- data |>
    dplyr::filter(!is.na(c)) |> 
    dplyr::filter(!is.na(AgeGroup)) |>
    dplyr::group_by(ForestTypeGroup, type) |>
    dplyr::group_modify(~ data.frame(preds = predict(mgcv::gam(c ~ s(AgeGroup, bs = "cs"), data = .x), data.frame(AgeGroup = 1:100))) |> 
                            dplyr::mutate(AgeGroup = 1:100)) |>
    dplyr::ungroup() |> 
    dplyr::mutate(future = AgeGroup <= 25) |>
    dplyr::mutate(type = factor(type, levels = c('Non-soil Carbon', 'Carbon-equivalent Albedo Offset', 'Joint C-albedo')))


write.csv(preds, "app/data/gam_preds.csv", row.names = FALSE)