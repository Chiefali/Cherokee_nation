# Server
# ----------------------------
server <- function(input, output, session) {
  
  output$cn_logo <- renderImage({
    list(
      src = normalizePath("www/cnimage.png"),
      contentType = "image/png",
      width = 70,
      height = 60,
      alt = "Cherokee Nation Public Health logo"
    )
  }, deleteFile = FALSE)
  
  dat <- reactive(dat_scored_sf)
  
  leaflet_ok_options <- function(sfdat) {
    max_bounds <- list(
      c(ok_bounds$ymin, ok_bounds$xmin),
      c(ok_bounds$ymax, ok_bounds$xmax)
    )
    
    leaflet(
      sfdat,
      options = leafletOptions(
        minZoom = 6,
        maxZoom = 12,
        zoomControl = TRUE,
        dragging = TRUE,
        scrollWheelZoom = TRUE,
        maxBounds = max_bounds,
        maxBoundsViscosity = 1.0
      )
    ) %>% fitBounds(ok_bounds$xmin, ok_bounds$ymin, ok_bounds$xmax, ok_bounds$ymax)
  }
  
  make_map <- function(sfdat, var, focus_county = NULL) {
    v <- sfdat[[var]]
    
    if (is.numeric(v)) {
      pal <- colorNumeric("Blues", domain = v[is.finite(v)], na.color = "#cccccc")
      fill <- pal(v)
    } else {
      pal <- colorFactor("Set2", domain = unique(v), na.color = "#cccccc")
      fill <- pal(v)
    }
    
    pretty_var <- ifelse(var %in% unname(outcome_labels),
                         names(outcome_labels)[match(var, unname(outcome_labels))],
                         label_for_indicator(var))
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      sfdat$county,
      pretty_var,
      ifelse(is.na(v), "NA", format(round(v, 3), nsmall = 0))
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(sfdat) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = fill,
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8)
    
    if (!is.null(focus_county)) {
      sel <- sfdat %>% filter(county == focus_county)
      if (nrow(sel) == 1) {
        m <- m %>% addPolygons(data = sel, fill = FALSE, color = "red", weight = 4)
      }
    }
    
    if (isTRUE(input$show_facilities)) {
      m <- add_facilities(m)
    }
    
    m
  }
  
  output$map1 <- renderLeaflet({ make_map(dat(), input$map_var1, input$county_pick) })
  output$map2 <- renderLeaflet({ make_map(dat(), input$map_var2, input$county_pick) })
  output$map3 <- renderLeaflet({ make_map(dat(), input$map_var3, input$county_pick) })
  output$map4 <- renderLeaflet({ make_map(dat(), input$map_var4, input$county_pick) })
  
  render_legend_for <- function(var) {
    sfdat <- dat()
    v <- sfdat[[var]]
    if (!is.numeric(v)) return(tags$div())
    bins <- make_bins(var, v)
    if (is.null(bins)) return(tags$div())
    
    pal <- colorBin(
      palette = "Blues",
      domain  = v,
      bins    = bins,
      na.color = "#cccccc",
      pretty   = FALSE
    )
    
    var_label <- ifelse(var %in% unname(outcome_labels),
                        names(outcome_labels)[match(var, unname(outcome_labels))],
                        label_for_indicator(var))
    
    legend_under_map(var_label, pal, bins)
  }
  
  output$legend1 <- renderUI({ render_legend_for(input$map_var1) })
  output$legend2 <- renderUI({ render_legend_for(input$map_var2) })
  output$legend3 <- renderUI({ render_legend_for(input$map_var3) })
  output$legend4 <- renderUI({ render_legend_for(input$map_var4) })
  
  output$radar <- renderPlotly({
    req(input$use_domain_view)
    focus_county <- input$county_pick_profile
    req(focus_county)
    
    d <- dat() %>% st_drop_geometry() %>% filter(county == focus_county)
    req(nrow(d) == 1)
    
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    dom_labels <- names(sdoh_domains) %>% pretty_domain() %>% wrap_2lines()
    
    radar_df <- tibble(
      domain = dom_labels,
      value  = as.numeric(d[1, dom_cols])
    )
    
    plot_ly(
      radar_df,
      type = "scatterpolar",
      r = ~value,
      theta = ~domain,
      fill = "toself",
      hovertemplate = "%{theta}<br>%{r:.1f} percentile<extra></extra>"
    ) %>%
      layout(
        title = list(text = paste0(focus_county, " — Domain percentiles (0–100)"), font = list(size = 14)),
        margin = list(l = 55, r = 55, t = 60, b = 55),
        polar = list(
          radialaxis = list(visible = TRUE, range = c(0, 100), tickfont = list(size = 11)),
          angularaxis = list(tickfont = list(size = 12))
        ),
        showlegend = FALSE
      )
  })
  
  output$drivers_tbl <- renderDT({
    focus <- input$county_pick_profile
    req(focus)
    
    all_vars <- unique(unlist(sdoh_domains))
    
    z_ind <- dat() %>%
      st_drop_geometry() %>%
      select(any_of(c("county", all_vars))) %>%
      mutate(across(all_of(all_vars), z_na))
    
    row_focus <- z_ind %>% filter(county == focus)
    req(nrow(row_focus) == 1)
    
    drivers <- tibble(
      Domain = map_chr(all_vars, ~{
        nm <- names(sdoh_domains)[map_lgl(sdoh_domains, function(vs) .x %in% vs)]
        if (length(nm) == 0) NA_character_ else nm[1]
      }),
      Indicator = label_for_indicator(all_vars),
      `Z-score (vs OK avg)` = as.numeric(row_focus[1, all_vars])
    ) %>%
      mutate(`Abs(z)` = abs(`Z-score (vs OK avg)`)) %>%
      arrange(desc(`Abs(z)`)) %>%
      select(Domain, Indicator, `Z-score (vs OK avg)`) %>%
      mutate(`Z-score (vs OK avg)` = round(`Z-score (vs OK avg)`, 4))
    
    datatable(
      head(drivers, 15),
      options = list(pageLength = 15, searching = TRUE, dom = "ftip"),
      class = "compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$profile_interp <- renderUI({
    focus_county <- input$county_pick_profile
    req(focus_county)
    
    d <- dat() %>% st_drop_geometry() %>% filter(county == focus_county)
    req(nrow(d) == 1)
    
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    vals <- as.numeric(d[1, dom_cols])
    names(vals) <- names(sdoh_domains)
    
    top_high <- names(sort(vals, decreasing = TRUE))[1:2]
    top_low  <- names(sort(vals, decreasing = FALSE))[1:2]
    
    tags$div(
      class = "well",
      tags$h4("How to interpret this County Profile"),
      tags$p("The radar chart shows domain percentiles (0–100) for ", tags$b(focus_county), " compared with other Oklahoma counties."),
      tags$p(tags$b("Highest-ranking domains: "), paste0(pretty_domain(top_high), collapse = ", "), "."),
      tags$p(tags$b("Lowest-ranking domains: "), paste0(pretty_domain(top_low), collapse = ", "), "."),
      tags$p(tags$b("Drivers table: "), "The 15 rows are indicators where this county differs most from the Oklahoma average (largest absolute z-scores)."),
      tags$div(
        style = "margin-top:6px;",
        tags$b("How to interpret z-scores (Drivers table):"),
        tags$ul(
          style = "margin-top:4px; margin-bottom:0px;",
          tags$li(tags$b("z = 0"), " means the county is at the Oklahoma average for that indicator."),
          tags$li(tags$b("z > 0"), " means the county is above the Oklahoma average; ", tags$b("z < 0"), " means below average."),
          tags$li(tags$b("|z| ≈ 1"), " means about 1 standard deviation from the Oklahoma average (a notable difference)."),
          tags$li(tags$b("|z| ≥ 2"), " means about 2+ standard deviations from the Oklahoma average (a large difference).")
        )
      )
    )
  })
  
  clustered <- reactive({
    d <- dat() %>% st_drop_geometry()
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    X <- d %>% select(all_of(dom_cols)) %>% mutate(across(everything(), as.numeric))
    Xmat <- as.matrix(X)
    
    for (j in seq_len(ncol(Xmat))) {
      bad <- !is.finite(Xmat[, j])
      Xmat[bad, j] <- 50
    }
    
    k_use <- min(input$k, nrow(Xmat))
    if (k_use < 2) {
      d$cluster <- NA_integer_
      return(d)
    }
    
    set.seed(1)
    km <- kmeans(Xmat, centers = k_use, nstart = 25)
    d$cluster <- km$cluster
    d
  })
  
  output$cluster_map <- renderLeaflet({
    d_sf <- dat() %>% left_join(clustered() %>% select(GEOID, cluster), by = "GEOID")
    
    pal <- colorFactor("Set3", domain = sort(unique(d_sf$cluster)), na.color = "#cccccc")
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>Cluster: %s",
      d_sf$county,
      ifelse(is.na(d_sf$cluster), "NA", d_sf$cluster)
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(d_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = pal(d_sf$cluster),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8)
    
    if (isTRUE(input$show_facilities)) m <- add_facilities(m)
    m
  })
  
  output$cluster_fingerprint <- renderPlotly({
    d <- clustered()
    dom_cols <- paste0("z_", names(sdoh_domains), "_pct")
    
    fp <- d %>%
      filter(!is.na(cluster)) %>%
      group_by(cluster) %>%
      summarise(across(all_of(dom_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      pivot_longer(cols = all_of(dom_cols), names_to = "domain", values_to = "mean_pct") %>%
      mutate(domain = str_remove(domain, "^z_") %>% str_remove("_pct$") %>% pretty_domain())
    
    plot_ly(fp, x = ~domain, y = ~mean_pct, color = ~factor(cluster), type = "bar") %>%
      layout(
        title = list(text = "Cluster fingerprints (mean domain percentiles)", font = list(size = 14)),
        margin = list(l = 50, r = 20, t = 60, b = 80),
        xaxis = list(title = "", tickangle = 45),
        yaxis = list(title = "Mean percentile (0–100)", range = c(0, 100))
      )
  })
  
  output$training_video_ui <- renderUI({
    req(input$training_video)
    tags$iframe(
      width = "100%",
      height = "360",
      src = input$training_video,
      frameborder = "0",
      allowfullscreen = NA
    )
  })
  
  output$trade_map <- renderLeaflet({
    d <- dat()
    x <- d[[input$xvar]]
    y <- d[[input$yvar]]
    
    if (!isTRUE(input$quad_map) || !is.numeric(x) || !is.numeric(y)) {
      v <- d[[input$xvar]]
      m <- make_map(d, input$xvar, focus_county = NULL)
      
      if (is.numeric(v)) {
        bins <- make_bins(input$xvar, v)
        if (!is.null(bins)) {
          pal_leg <- colorBin("Blues", domain = v, bins = bins, na.color = "#cccccc", pretty = FALSE)
          pretty_var <- ifelse(input$xvar %in% unname(outcome_labels),
                               names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                               label_for_indicator(input$xvar))
          m <- m %>% addLegend(
            position = "bottomright",
            pal = pal_leg,
            values = v,
            title = pretty_var,
            opacity = 0.9,
            na.label = "Missing"
          )
        }
      }
      
      return(m)
    }
    
    mx <- median(x, na.rm = TRUE)
    my <- median(y, na.rm = TRUE)
    
    d$quad <- dplyr::case_when(
      is.na(x) | is.na(y) ~ NA_character_,
      x >= mx & y >= my ~ "High X / High Y",
      x >= mx & y <  my ~ "High X / Low Y",
      x <  mx & y >= my ~ "Low X / High Y",
      TRUE ~ "Low X / Low Y"
    )
    
    quad_levels <- c("High X / High Y", "High X / Low Y", "Low X / High Y", "Low X / Low Y")
    pal <- colorFactor("Set2", domain = quad_levels, na.color = "#cccccc")
    
    xlab <- ifelse(input$xvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                   label_for_indicator(input$xvar))
    ylab <- ifelse(input$yvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$yvar, unname(outcome_labels))],
                   label_for_indicator(input$yvar))
    
    lbl <- sprintf(
      "<strong>%s</strong><br/>%s: %s<br/>%s: %s<br/>%s",
      d$county,
      xlab, ifelse(is.na(x), "NA", round(x, 3)),
      ylab, ifelse(is.na(y), "NA", round(y, 3)),
      ifelse(is.na(d$quad), "Quadrant: NA", paste0("Quadrant: ", d$quad))
    ) %>% lapply(htmltools::HTML)
    
    m <- leaflet_ok_options(d) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = pal(d$quad),
        fillOpacity = 0.8,
        color = "white",
        weight = 1,
        label = lbl,
        highlightOptions = highlightOptions(weight = 3, color = "#444444", bringToFront = TRUE)
      ) %>%
      addPolylines(data = ok, color = "#111111", weight = 2, opacity = 0.8) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = factor(d$quad, levels = quad_levels),
        title = "Quadrants (median splits)",
        opacity = 0.9,
        na.label = "Missing"
      )
    
    if (isTRUE(input$show_facilities)) m <- add_facilities(m)
    
    m
  })
  
  output$trade_interp <- renderUI({
    req(input$xvar, input$yvar)
    
    xlab <- ifelse(input$xvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$xvar, unname(outcome_labels))],
                   label_for_indicator(input$xvar))
    ylab <- ifelse(input$yvar %in% unname(outcome_labels),
                   names(outcome_labels)[match(input$yvar, unname(outcome_labels))],
                   label_for_indicator(input$yvar))
    
    tags$div(
      class = "well",
      tags$h4("How to interpret this Tradeoffs map"),
      tags$p("This map compares two measures at once using Oklahoma medians for ", YEAR0, "."),
      tags$p(tags$b("X: "), xlab, " • ", tags$b("Y: "), ylab),
      tags$p("Counties are grouped into four quadrants: High/High, High/Low, Low/High, Low/Low (relative to Oklahoma medians).")
    )
  })
  
  