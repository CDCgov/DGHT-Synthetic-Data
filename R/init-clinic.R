#' Initialize the clinic dataset
#'
#' @param interactions the full interaction data frame
#' @param country string
#'
#' @importFrom dplyr filter select mutate rowwise
#'
#' @returns a dataset of clinic interaction details
#' @export
init_clinic <- function(interactions, country) {

  # find all interactions that are clinic and select, patient_id
  # interaction_id, interaction_date
  clinic_df_init <- interactions |>
    dplyr::filter(interaction_type == "clinic") |>
    dplyr::select(patient_id, interaction_id, interaction_date)

  clinic_names_df <- clinic_names |> dplyr::filter(country == country)

  # assign site_name
  clinic_df_init$site_name <- sample(clinic_names_df$sitename,
                                  size = nrow(clinic_df_init),
                                  replace = TRUE)

  # assign arv_regimen 
      
    #load("C:/Users/lpm8/OneDrive - CDC/GitHub/DGHT-Synthetic-Data/data/arv_side_effects.RData")
    #alternate approach to subsetting unique regimens
    #arv_regimen_df <- arv_side_effects |> unique(regimen_abbv, regimen_desc)
     
   #subset the unique values for regimen_abbv and regimen_desc
  arv_regimen_df <- arv_side_effects |> dplyr::distinct(regimen_abbv, regimen_desc)
  
  #	0.5% of all clinic visits will be missing arv_regimen (no arv regimen).
  # 95.5% of all clinic visits will have an arv_regimen value. Since there are multiple rows for each regimen, you will have to randomly select from the deduplicated regimen_abbv list.
  
  clinic_df_init$arv_regimen <- sample(arv_regimen_df$regimen_abbv,
                                     size = nrow(clinic_df_init),
                                     replace = TRUE)  
           
  clinic_df_init <- clinic_df_init |> dplyr::mutate(arv_regimen = if_else(row_number() %in% 
                                      sample(x = 1:n(), size = n()*0.005,
                                       replace = FALSE), NA, arv_regimen))

    #add regimen description
  clinic_df_init$arv_regimen_desc <- clinic_df_init|> left_join(arv_regimen_df, 
                                       by = c("arv_regimen", "regimen_abbv")) |>
                                        mutate(arv_regimen_desc = regimen_desc)
  
  # assign side_effects - update to pull in previous interaction visit regimen
    #depends on the possible side effects for the regimen listed on the client’s previous clinic encounter
    # Identify the regimen prescribed during the previous clinic encounter and sample from the possible side effects (side_effect). 
      #1.	Randomly determine number of side effects assigned from 0 to the max for that regimen. 
     # 2.	Rare side effects (side_effect_rarity=“Yes”) have 0.5% chance of selection vs. others.
   #Note: look at the code for pregnancy
  clinic_df_init$side_effects <- clinic_df_init|> left_join(arv_side_effects, by = c("arv_regimen", "regimen_abbv")) |>
    
    sample(arv_side_effects$side_effect,
                                        size = nrow(clinic_df_init),
                                        replace = TRUE, )
 
  
  
  ####
  #next_appointment_date
  #viral_load_result_date
  #viral_load
  #cd4_count_date
  #cd4_count
  #height
  #weight
  #bp_systolic
  #bp_diastolic
  #tb_status
  
  #record_as_of_date**
    
 

 
  return(clinic_df_init)
}
