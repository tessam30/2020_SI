# Purpose: Load Genie data and clean up to calculate net new adjustments
# Author: Tim Essam | USAID
# Date: 2020-02-27
# Note:


# Dependencies ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(ICPIutilities)
library(scales)
library(extrafont)
library(vroom)
library(keyringr)
library(llamar)
library(tidytext)
library(ggforce)

source(file.path("Scripts", "setup.R"))

# API ---------------------------------------------------------------------

  #DATIM Information
    myuser <- "tessam"
    yawn <- decrypt_kc_pw("mydb_access")
    baseurl <- "https://final.datim.org/"
    
    
  #levels in kenya
    ou_name <- "Kenya"
    ou_uid <- identify_ouuids(myuser, yawn) %>% 
        filter(displayName == ou_name) %>% 
        pull(id)
    
    
    ou_site_lvl <- identify_levels("Kenya", myuser, yawn) %>% 
        pull(facility)
    
    
  #API url 
    url <- 
      paste0(baseurl,
        "api/29/analytics.json?",
        paste0("dimension=ou:LEVEL-",ou_site_lvl,";",ou_uid,"&"), 
        "dimension=bw8KHXzxd9i:FPUgmtt8HRi;NLV6dy7BE2O&", #Funding Agency - CDC and USAID
        "dimension=SH885jaRe0o&", #Funding Mechanism
        "dimension=pe:2017Q3;2017Q4;2018Q1;2018Q2;2018Q3;2018Q4;2019Q1;2019Q2;2019Q3;2019Q4&",
        "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area: TX_CURR
        "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets / Results: Results
        "dimension=RUkVjD3BsS1:PE5QVF0w4xj&",  #Top Level Numerator
        "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
      )#pull site TX_CURR data
    
    
  #pull site TX_CURR data
    df_site_tx <- get_datim_targets(url, myuser, yawn) # - 

    

# FUNCTION - AGGREGATE BY -------------------------------------------------
    
    agg_by <- function(df, ...){
        df_agg <- df %>% 
            group_by_at(c(vars(..., period))) %>% 
            summarise_if(is.double, sum, na.rm = TRUE) %>%
            ungroup() 
        
        df_agg <- arrange_at(df_agg, vars(..., period))
        
        
        df_agg_nn <- df_agg %>%
            group_by_at(vars(...)) %>%
            mutate(tx_net_new = tx_curr - lag(tx_curr)) %>%
            ungroup() %>%
            filter(period != "FY17Q4")
        
        return(df_agg_nn)
    }
    
# FUNCTION - AGGREGATE REASSIGNING BY -------------------------------------
    
    agg_reassign_by <- function(df, ...){
        
        df_site_assignments <- df %>%
            distinct(period, orgunituid, fundingagency, mech_code, mech_name, primepartner) #%>% 
        #rename_at(vars(fundingagency, mech_code, mech_name, primepartner), ~paste0(., "_adj"))
        
        df_nn <- df %>% 
            select(-c(fundingagency, mech_code, mech_name, primepartner, sitename)) %>% 
            arrange(orgunituid, period) %>% 
            group_by(orgunituid) %>%
            mutate(tx_net_new = tx_curr - lag(tx_curr)) %>%
            ungroup() %>%
            filter(period != "FY17Q4")
        
        df_site_tx_reassigned <- df_nn %>% 
            left_join(df_site_assignments, by = c("orgunituid", "period"))
        
        df_site_tx_reassigned <- df_site_tx_reassigned %>% 
            group_by_at(c(vars(..., period))) %>% 
            summarise_if(is.double, sum, na.rm = TRUE) %>%
            ungroup() 
        
        
        return(df_site_tx_reassigned)
    }
    
  prinf <- function(df) {
    print(df, n = Inf)
  }   

# number of rows for facet
  facet_rows <- function(df) { 
    df %>% 
    distinct(fundingagency) %>% 
    count() %>% 
    pull()
  }
  
  
# MUNGE -------------------------------------------------------------------
    
    #keep select vars, drop "county" from county names, flag transition counties
    df_site_tx_clean <- df_site_tx %>% 
        select(operatingunit = orglvl_3,
            snu1 = orglvl_4,
            psnu = orglvl_5,
            ward = orglvl_6,
            sitename = `Organisation unit`,
            orgunituid,
            fundingagency = `Funding Agency`,
            mech_code = `Funding Mechanism`,
            # indicator = `Technical Area`,
            period = `Period`,
            tx_curr = Value) %>% 
      mutate(snu1 = str_remove_all(snu1, " County"),
        snu1_transflag = if_else(snu1 %in% c(
          "Garissa", "Isiolo", "Lamu", "Mandera",
          "Marsabi", "Tana River", "Wajir"
        ), 1, 0)) 
    
    #clean up mech_code and convert period to FY period
    df_site_tx_clean <- df_site_tx_clean %>% 
        mutate(mech_code = str_extract(mech_code, "[:digit:]+"),
            period = str_replace(period, "to [:alpha:]{3}", "1,") %>% 
                mdy() %>%
                quarter(with_year = TRUE, fiscal_start = 10) %>% 
                as.character %>% 
                str_replace("20", "FY") %>% 
                str_replace("\\.", "Q"))
    
    #clean agencies
    df_site_tx_clean <- df_site_tx_clean %>% 
        mutate(fundingagency = str_remove(fundingagency, "HHS/"),
            fundingagency = factor(fundingagency, c("CDC","USAID", "DOD"))) %>% 
      mutate(exclude_id = str_c(orgunituid, fundingagency, mech_code, period, tx_curr))
    
    
    #add in partner and mech names
    df_site_tx_clean <- rename_official(df_site_tx_clean) %>% 
      mutate(primepartner = str_to_title(primepartner))
    
    # #order IPs
    # ip_order <- df_site_tx_clean %>% 
    #     filter(period == "FY20Q1") %>% 
    #     agg_by(primepartner) %>% 
    #     arrange(desc(tx_curr)) %>% 
    #     pull(primepartner) 
    # 
    # df_site_tx_clean <- mutate(df_site_tx_clean, primepartner = fct_reorder(primepartner, ip_order))
    
# ADJUST SITE TX VIA INHERITING -------------------------------------------
    #are there any sites with more than one mechanism working across pds?
     multi_partner_sites <- 
      df_site_tx_clean %>% 
      distinct(period, orgunituid, mech_code) %>% 
      count(period, orgunituid)%>%
      filter(n > 1) %>%
      pull(orgunituid)

    mp_sites_df <- df_site_tx_clean %>%
      filter(orgunituid %in% multi_partner_sites) %>%
      arrange(orgunituid, period, mech_code)
   
# Exclude list below - derived from the mp_sites_df.
    mp_site_exclude <- 
    c("KATAw2bTliQCDC18208FY17Q42", "KATAw2bTliQCDC18208FY18Q15", "KATAw2bTliQCDC18208FY18Q26", 
      "KATAw2bTliQCDC18208FY18Q310", "KATAw2bTliQCDC18208FY18Q418", "krgRLpGbbJoCDC18213FY18Q1191", 
      "krgRLpGbbJoCDC18213FY18Q2202", "oof4zfbHoBNCDC18208FY17Q415", "oof4zfbHoBNCDC18208FY18Q114", 
      "oof4zfbHoBNCDC18208FY18Q214", "oof4zfbHoBNCDC18208FY18Q316", "oof4zfbHoBNCDC18208FY18Q424", 
      "Q02fJEYmyViCDC18213FY18Q1141", "Q02fJEYmyViCDC18213FY18Q2163", "Q02fJEYmyViCDC18213FY18Q3157", 
      "r9xVGmSAMepUSAID14012FY19Q251", "t5RWBPDPDxqCDC18213FY18Q216", "zMtC3WJjCYACDC18213FY18Q2103") 
    
    write_csv(mp_sites_df, file.path(dataout, "KEN_multi_partner_sites_2020_03_02.csv"))
    
# Flag sites where there was an agency shift between FY19Q4 and FY20Q1
  # folding in AMREF
    df_site_tx_clean <- 
      df_site_tx_clean %>% 
      mutate(multi_site_flag = if_else(orgunituid %in% multi_partner_sites, 1, 0)) %>% 
      group_by(operatingunit, orgunituid) %>% 
      mutate(fundingagency_lag = lag(fundingagency, n = 1, order_by = period),
        primepartner_lag = lag(primepartner, n = 1, order_by = period)) %>% 
      ungroup() %>% 
      arrange(operatingunit, orgunituid, period) %>% 
      mutate(agency_shift = if_else(period == "FY20Q1" & fundingagency != fundingagency_lag, 1, 0),
        partner_shift = if_else(period == "FY20Q1" & primepartner != primepartner_lag, 1, 0),
        agency_shift_type = case_when(
        agency_shift == 1 & period == "FY20Q1" ~ str_c("from ", fundingagency_lag, " to ", fundingagency),
          TRUE ~ NA_character_ 
        ),
        parnter_shift_type = case_when(
          partner_shift == 1 & period == "FY20Q1" ~ str_c("from", primepartner_lag, " to ", primepartner)
        )) %>% 
      group_by(operatingunit, orgunituid) %>% 
      mutate(orgunituid_shift = if_else(agency_shift == 1, 1, NA_real_)) %>% 
      fill(orgunituid_shift, .direction = c("up")) %>%
      fill(agency_shift_type, .direction = c("up")) %>% 
      ungroup()
    
    df_site_tx_clean <- 
      df_site_tx_clean %>% 
      filter(!exclude_id %in% mp_site_exclude) %>% 
      # Collapsing on occurence where an IP should have been given credit for 36 more (check on this>?!)
      mutate(tx_curr = if_else(exclude_id == "y1337MIdqZGUSAID14012FY18Q1179", tx_curr + 36, tx_curr)) %>% 
      filter(exclude_id != "y1337MIdqZGUSAID14022FY18Q136")
    
  # Show a summary of sites that switched from USAID to CDC and vice versa
  df_site_tx_clean %>% 
    filter(orgunituid_shift == 1) %>% 
    select(sitename, fundingagency, period) %>% 
    spread(period, fundingagency) %>% 
    prinf()
  
 # Plot sites where the IP changed
  df_site_tx_clean %>% 
    filter(partner_shift == 1) %>% 
    select(agency_shift, sitename, fundingagency_lag, primepartner_lag, primepartner, fundingagency, snu1) %>% 
    arrange(agency_shift, fundingagency_lag, primepartner_lag) %>% 
    prinf()

# BRING IN TARGETS
  
  
  
# SITE TRANSITION PLOTS ---------------------------------------------------
  
  df_site_tx_clean %>% 
    filter(orgunituid_shift == 1) %>% 
    group_by(snu1, fundingagency, period) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(snu1) %>% 
    mutate(max_n = max(n)) %>% 
    ungroup() %>% 
    mutate(snu1_order = fct_reorder(snu1, max_n),
      snu1_count = ifelse(period =="FY20Q1", n, NA_real_)) %>% 
    ggplot(aes(x = period, y = snu1_order, fill = fundingagency)) +
    geom_tile(colour = "white") + 
    geom_text(aes(label = str_c(snu1_count, " to ", fundingagency)), colour = "white") +
    theme_minimal() +
    scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
    theme(panel.grid.major = element_blank(),
      strip.text = element_text(hjust = 0),
      legend.position = "top",
      legend.justification = c(0, 0),
      legend.direction = "horizontal") +
    labs(x = NULL, y = NULL, 
    title = "Summary of site transitions at the county level")
    
  
  df_site_tx_clean %>% 
    filter(orgunituid_shift == 1) %>% 
    group_by(snu1) %>% 
    add_tally() %>% 
    ungroup() %>% 
    mutate(snu1_count = ceiling(n / 10),
      snu1_order = fct_reorder(str_c(snu1, " (", snu1_count, " sites)"), n, .desc = TRUE),
      site_order = tidytext::reorder_within(sitename, n, snu1_order),
      ) %>% 
    ggplot(aes(x = period, y = site_order, fill = fundingagency)) +
    geom_tile(colour = "white")  +
    ggforce::facet_row(~snu1_order, scales = "free_x", space = "free",
    labeller = label_wrap_gen(multi_line = FALSE)) +
    #facet_wrap(~snu1_order, ncol = 13) +
    scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
    #scale_fill_viridis_c(option = "D", trans = "log") +
    theme_minimal() + 
    scale_y_reordered() +
    theme(axis.text = element_text(size = 6),
      panel.grid.major = element_blank(),
      axis.text.x = element_text(angle = 90),
      strip.text = element_text(hjust = 0),
      legend.position = "top",
      legend.justification = c(0, 0),
      legend.direction = "horizontal") +
    labs(x = NULL, y = NULL, 
      title = "Seven counties transitioned from USAID to CDC, six from CDC to USAID",
      caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]"),
      fill = NULL)
  
    ggsave(file.path(graphs, "KEN_site_transition_summary_by_county.pdf"),
      plot = last_plot(),
      width = 23.4,
      height = 16.5,
      device = "pdf",
      useDingbats = FALSE, 
      units = "in"
      )
        
  
  
    
    
# FUNCTION - PLOT TX ------------------------------------------------------
    
    plot_tx <- function(df, var, title){
      
      if("fundingagency_adj" %in% names(df))
        df <- mutate(df, fundingagency = fundingagency_adj)
      
      nrows <- facet_rows(df)
      
      df %>% 
        mutate(placeholder = {{var}} * 1.2) %>% 
        ggplot(aes(period, {{var}}, fill = fundingagency)) +
        geom_hline(yintercept = 0) +
        geom_col() +
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = comma({{var}}), 
          vjust = ifelse({{var}} < 0, 1, -1)),
          size = 3.5,
          color = '#595959',
          family = "Franklin Gothic Medium Cond") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
          "FY19Q1", "", "FY19Q3", "",
          "FY20Q1")) +
        scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
        facet_wrap(~ fundingagency, nrow = nrows) +
        labs(x = NULL, y = NULL, 
          title = title,
          caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Franklin Gothic Medium Cond"),
          strip.text = element_text(size = 14, hjust = 0),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(family = "Franklin Gothic Medium"),
          plot.caption = element_text(color = "#595959"),
          legend.position = "none")
    }
    
    
    plot_tx_ip <- function(df, var, title){
      
      if("primepartner_adj" %in% names(df))
        df <- mutate(df, primepartner = primepartner_adj)
      
      df %>% 
        mutate(placeholder = {{var}} * 1.4) %>% 
        ggplot(aes(period, {{var}}, fill = fundingagency)) +
        geom_hline(yintercept = 0) +
        geom_col() +
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = comma({{var}}, accuracy = 1), 
          vjust = ifelse({{var}} < 0, 1, -1)),
          color = "#595959", size = 3,
          family = "Franklin Gothic Medium Cond") +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
          "FY19Q1", "", "FY19Q3", "",
          "FY20Q1")) +
        scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
        facet_grid(primepartner ~ ., switch = "y") +
        labs(x = NULL, y = NULL, 
          title = title,
          caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Franklin Gothic Medium Cond"),
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(family = "Franklin Gothic Medium"),
          plot.caption = element_text(color = "#595959"),
          legend.position = "none")
    }
    
    plot_nn_gr <- function(df, title){
      
      nrows <- facet_rows(df)
        
      df %>% 
        group_by(fundingagency) %>% 
        mutate(growth = tx_net_new/lag(tx_net_new)) %>% 
        ungroup() %>% 
        mutate(placeholder = growth * 1.2) %>% 
        ggplot(aes(period, growth, group = fundingagency, color = fundingagency)) +
        geom_hline(yintercept = 0) +
        geom_line(size = 1, na.rm = TRUE) +
        geom_point(aes(fill = fundingagency),
          size = 5, na.rm = TRUE, stroke = 1, colour = "white", shape = 21) + 
        geom_blank(aes(y = placeholder)) +
        geom_text(aes(label = percent(growth, 1), 
          vjust = ifelse(growth < 0, 2, -1)),
          size = 3.5,
          color = '#595959',
          family = "Franklin Gothic Medium Cond", na.rm = TRUE) +
        scale_y_continuous(label = comma) +
        scale_x_discrete(labels = c("FY18Q1", "", "FY18Q3", "",
          "FY19Q1", "", "FY19Q3", "",
          "FY20Q1")) +
        scale_color_manual(values = c("#335B8E", "#6CA18F")) +
        scale_fill_manual(values = c("#335B8E", "#6CA18F")) +
        facet_grid(fundingagency ~ ., switch = "y") +
        # facet_wrap(~fundingagency, nrow = nrows)
        labs(x = NULL, y = NULL, 
          title = title,
          caption = paste0("DATIM Genie API Pull [", format(Sys.Date(), "%Y-%m-%d"), "]")) +
        theme_minimal() +
        theme(text = element_text(family = "Franklin Gothic Medium Cond"),
          strip.text = element_text(size = 14), #, hjust = 0),
          axis.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(family = "Franklin Gothic Medium"),
          plot.caption = element_text(color = "#595959"),
          legend.position = "none")
    }
    
    
    
    # PLOT TX TRENDS ----------------------------------------------------------
    # Stopped here
    
    
    
    #AGENCY UNADJUSTED
    df_site_tx_clean %>% 
      #filter(multi_site_flag != 1) %>% 
      agg_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_curr, "TX_CURR, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_TXCURR.png",
      dpi = 330, width = 7, height = 3.6)
    
    df_site_tx_clean %>% 
      #filter(multi_site_flag != 1) %>% 
      agg_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_net_new, "TX_NET_NEW, unadjusted") 
    
    ggsave("out/plots/TZA_NNAdj_NETNEW.png",
      dpi = 330, width = 7, height = 3.6)
    
    
    df_site_tx_clean %>%
      #filter(multi_site_flag != 1) %>% 
      agg_by(operatingunit, fundingagency) %>%
      plot_nn_gr("Growth in TX_NET_NEW, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_NETNEW_gr.png",
      dpi = 330, width = 7, height = 3.6)
    
    #AGENCY REASSIGNED
    df_site_tx_clean %>% 
      #filter(multi_site_flag != 1) %>%  
      agg_reassign_by(operatingunit, fundingagency) %>% 
      plot_tx(tx_net_new, "TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ.png",
      dpi = 330, width = 7, height = 3.6)
    
    df_site_tx_clean %>% 
      #filter(multi_site_flag != 1) %>% 
      agg_reassign_by(operatingunit, fundingagency) %>% 
      plot_nn_gr("Growth in TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ_gr.png",
      dpi = 330, width = 7, height = 3.6)
    

# TX NN BY PARTNER --------------------------------------------------------


  #PARTNER UNADJUSTED
    df_site_tx_clean %>% 
      #filter(multi_site_flag != 1) %>% 
      agg_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_curr, "TX_CURR, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_TXCURR_IM.png",
      dpi = 330, width = 7, height = 7.11)
    
    df_site_tx_clean %>% 
      filter(multi_site_flag != 1) %>% 
      agg_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_net_new, "TX_NET_NEW, unadjusted")
    
    ggsave("out/plots/TZA_NNAdj_NETNEW_IM.png",
      dpi = 330, width = 7, height = 7.11)
    
    
    
    #PARTNER REASSIGNED
    df_site_tx_clean %>% 
      filter(multi_site_flag != 1) %>% 
      agg_reassign_by(operatingunit, fundingagency, primepartner) %>% 
      filter(primepartner != "THPS") %>% 
      plot_tx_ip(tx_net_new, "TX_NET_NEW, reassigned")
    
    ggsave("out/plots/TZA_NNAdj_NETNEWADJ_IM.png",
      dpi = 330, width = 7, height = 7.11)
    
    
    
 