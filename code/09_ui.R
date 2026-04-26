# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .well { background: #f8fafc; border: 1px solid #e5e7eb; border-radius: 10px; }
      .control-card { background:#fff; border:1px solid #e5e7eb; border-radius:12px; padding:10px; }
      .control-title { font-weight:700; font-size:13px; margin-bottom:8px; }
      .subtle { color:#555; font-size:12px; }
      #cn_logo { display:block; max-width:70px; }
      .metric-card { background:#fff; border:1px solid #e5e7eb; border-radius:12px; padding:10px; margin-bottom:10px; }
      .metric-title { font-weight:700; font-size:13px; margin-bottom:4px; }
      .metric-big { font-size:22px; font-weight:800; line-height:1.1; }
      .metric-small { font-size:12px; color:#555; }
    "))
  ),
  
  tags$div(
    style = "display:flex; align-items:center; gap:14px; padding:10px 0;",
    tags$div(style = "flex:0 0 auto; width:70px;", imageOutput("cn_logo", height = "60px")),
    tags$div(
      style = "flex:1 1 auto;",
      tags$div("Cherokee Nation Public Health", style = "font-size:24px; font-weight:700; line-height:1.1; margin:0;"),
      tags$div("Social Determinants of Health Dashboard", style = "font-size:24px; font-weight:700; line-height:1.1; margin:0;"),
      tags$div(paste0("Cherokee Nation Public Health • ", YEAR0), style = "font-size:14px; color:#555; margin-top:2px;")
    )
  ),
  
  tags$div(
    class = "well",
    style = "margin-top:10px;",
    tags$div(
      style = "font-size:14px;",
      tags$b("Welcome. "),
      "This dashboard helps Tribal public health analysts compare Oklahoma counties (2022) across six Social Determinants of Health (SDOH) domains. Use it to identify counties with similar profiles, highlight domain-specific strengths and needs, and explore relationships between SDOH conditions and health outcomes."
    ),
    tags$br(),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "What are the 6 SDOH domains in this dashboard?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$ul(
          tags$li(tags$b("Economic Instability: "), "poverty, unemployment, SNAP, low-income households"),
          tags$li(tags$b("Education Attainment: "), "overall educational attainment and AI/AN education distribution"),
          tags$li(tags$b("Housing & Transportation Barriers: "), "renting, cost burden, overcrowding, vehicle access, plumbing, housing age"),
          tags$li(tags$b("Health Care Access Barriers: "), "uninsurance and AI/AN coverage indicators (IHS, Medicaid, self-pay)"),
          tags$li(tags$b("Neighborhood & Environment Burden: "), "air quality (PM2.5) and access to parks/open space"),
          tags$li(tags$b("Social & Community Context: "), "disability, marital status, and population composition indicators")
        )
      )
    ),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "What outcomes are included in this dashboard?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$ul(
          tags$li("CN gestational diabetes rate"),
          tags$li("AI/AN gestational diabetes rate"),
          tags$li("All births rate"),
          tags$li("AI/AN births rate")
        )
      )
    ),
    tags$details(
      open = FALSE,
      tags$summary(style = "font-weight:600; font-size:14px;", "How are z-scores, domain scores, and the composite score calculated?"),
      tags$div(
        style = "margin-top:8px; font-size:13px;",
        tags$p(
          tags$b("Indicator z-score: "),
          "For each indicator, we standardize across Oklahoma counties: ",
          tags$code("z = (county value − OK mean) / OK standard deviation."),
          " A z-score of +1 means the county is ~1 standard deviation above the Oklahoma average for that indicator."
        ),
        tags$p(tags$b("Domain score: "), "Within each domain, we take the average of the indicator z-scores (equal weight)."),
        tags$p(tags$b("Composite SDOH score: "), "We take the average of the six domain z-scores (equal weight)."),
        tags$p(tags$b("Percentiles (0–100): "), "For domain and composite scores, we also compute a percentile rank across Oklahoma counties."),
        tags$p(tags$b("Important note: "), "Z-scores and percentiles are descriptive comparisons across Oklahoma counties for 2022; they are not absolute thresholds.")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      tags$div(class = "control-card",
               tags$div(class = "control-title", "Training"),
               tags$div(class = "subtle", "Pick a lesson and watch the short walkthrough.")
      ),
      
      selectInput(
        "training_video",
        "Choose a lesson",
        choices = c(
          "Welcome (1 min)" = "https://www.youtube.com/embed/YOUR_ID1",
          "Dashboard overview (3 min)" = "https://www.youtube.com/embed/YOUR_ID2",
          "Maps tab (2 min)" = "https://www.youtube.com/embed/YOUR_ID3",
          "County Profile (3 min)" = "https://www.youtube.com/embed/YOUR_ID4",
          "Clusters (3 min)" = "https://www.youtube.com/embed/YOUR_ID5",
          "Tradeoffs (2 min)" = "https://www.youtube.com/embed/YOUR_ID6",
          "Scenario Sandbox (3 min)" = "https://www.youtube.com/embed/YOUR_ID7"
        )
      ),
      uiOutput("training_video_ui"),
      
      tags$details(
        open = FALSE,
        tags$summary(style = "font-weight:700; font-size:13px; margin-top:10px;", "Data sources & definitions"),
        tags$div(
          style = "margin-top:8px; font-size:12px; color:#444;",
          tags$p(tags$b("Sources: "), "ACS 5-year county measures; OK2Share state data; Cherokee Nation health data; federal environmental/food access datasets. All measures are compiled to Oklahoma county level for ", YEAR0, "."),
          tags$p(style="font-size:12px; color:#555;", "Z-scores and percentiles are descriptive comparisons across Oklahoma counties (not causal)."),
          tags$p(style="font-size:12px; color:#555;", "Scenario Sandbox shows model-based predictions on a transformed outcome scale (associational), not guaranteed causal effects.")
        )
      ),
      
      hr(),
      helpText("Data year: ", YEAR0)
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        tabPanel(
          "Maps",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use these maps: "),
            "Select up to four variables to compare across counties. Hover counties to see values. Use “Focus county” to outline one county in red. Zoom/pan are restricted to Oklahoma."
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(6, leafletOutput("map1", height = 320), uiOutput("legend1")),
                column(6, leafletOutput("map2", height = 320), uiOutput("legend2"))
              ),
              fluidRow(
                column(6, leafletOutput("map3", height = 320), uiOutput("legend3")),
                column(6, leafletOutput("map4", height = 320), uiOutput("legend4"))
              )
            ),
            
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Map options"),
                       selectInput("map_var1", "Map 1 variable", choices = choices_all, selected = "z_SDOH_Composite_pct"),
                       selectInput("map_var2", "Map 2 variable", choices = choices_all, selected = "z_Economic_Instability_pct"),
                       selectInput("map_var3", "Map 3 variable", choices = choices_all, selected = "z_Education_Attainment_pct"),
                       selectInput("map_var4", "Map 4 variable", choices = choices_all, selected = "z_Housing_Transport_pct"),
                       
                       hr(style = "margin:10px 0;"),
                       selectInput("county_pick", "Focus county", choices = county_choices, selected = county_choices[1]),
                       
                       hr(style = "margin:10px 0;"),
                       checkboxInput("show_facilities", "Show AI/AN facility locations", value = FALSE),
                       
                       tags$div(
                         style = "margin-top:8px; font-size:12px;",
                         tags$div(style="font-weight:600; margin-bottom:4px;", "Facility affiliation"),
                         tags$table(
                           style="border-collapse:collapse;",
                           tags$tr(
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["IHS"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "IHS"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Title 1 (Tribal)"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "Title 1 (Tribal)"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Title 5 (638)"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px; padding-right:14px;", "Title 5 (638)"),
                             tags$td(style=paste0("width:10px; height:10px; background:", facility_colors["Urban"], "; border:1px solid #000;")),
                             tags$td(style="padding-left:6px;", "Urban")
                           )
                         )
                       )
              )
            )
          )
        ),
        
        tabPanel(
          "County Profile",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use County Profile: "),
            "Select a county to compare domain percentiles (0–100). The drivers table lists indicators where the county differs most from the Oklahoma average; the search box lets you quickly find an indicator by keyword."
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(6, plotlyOutput("radar", height = 420), uiOutput("profile_interp")),
                column(6, DTOutput("drivers_tbl"))
              )
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Profile options"),
                       selectInput("county_pick_profile", "County", choices = county_choices, selected = county_choices[1]),
                       checkboxInput("use_domain_view", "Show domain radar", value = TRUE),
                       hr(style = "margin:10px 0;"),
                       tags$div(class = "subtle", "Z-scores: positive = above OK average; negative = below OK average.")
              )
            )
          )
        ),
        
        tabPanel(
          "Profiles / Clusters",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to interpret clusters: "),
            "Clusters group counties with similar domain percentile patterns (k-means). The map shows where similar-profile counties are located; the fingerprint chart shows the average domain percentile profile for each cluster."
          ),
          
          fluidRow(
            column(9,
                   leafletOutput("cluster_map", height = 520),
                   tags$div(style="margin-top:10px;", plotlyOutput("cluster_fingerprint", height = 320))
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Cluster options"),
                       sliderInput("k", "Number of clusters (k)", min = 3, max = 8, value = 5, step = 1)
              )
            )
          )
        ),
        
        tabPanel(
          "Tradeoffs",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("How to use Tradeoffs: "),
            "Choose X and Y measures to view a quadrant map (median splits) and identify counties with high/high, low/low, or mixed patterns."
          ),
          
          fluidRow(
            column(9, leafletOutput("trade_map", height = 520), uiOutput("trade_interp")),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Tradeoff options"),
                       selectInput("xvar", "X variable", choices = choices_all, selected = "z_Housing_Transport_pct"),
                       selectInput("yvar", "Y variable", choices = choices_all, selected = "cn_gdm_rate"),
                       checkboxInput("quad_map", "Quadrant map (median splits)", value = TRUE)
              )
            )
          )
        ),
        
        tabPanel(
          "Scenario Sandbox",
          tags$div(
            class = "well",
            style = "margin-top:10px; font-size:13px;",
            tags$b("What this is: "),
            "This sandbox shows model-based predictions if a domain score changed, holding the county’s other domain scores fixed. Outcomes are modeled on a log scale for stability. These are associational predictions (not guaranteed causal effects)."
          ),
          tags$div(
            tags$h4("Sandbox Instructions"),
            tags$p("Use this sandbox to test how changes in community conditions may influence outcomes."),
            tags$p("Adjust the scenario inputs to simulate improvements or declines in selected factors."),
            tags$p("Results update automatically, allowing you to explore potential impacts before policy or program decisions are made."),
            tags$p("Reset inputs at any time to begin a new scenario.")
          ),
          
          fluidRow(
            column(
              9,
              fluidRow(
                column(4, uiOutput("sandbox_card_baseline")),
                column(4, uiOutput("sandbox_card_scenario")),
                column(4, uiOutput("sandbox_card_delta"))
              ),
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Predicted changes"),
                       plotlyOutput("sandbox_delta_plot", height = 420),
                       tags$div(style="margin-top:10px;", DTOutput("sandbox_tbl"))
              )
            ),
            column(
              3,
              tags$div(class = "control-card",
                       tags$div(class = "control-title", "Scenario options"),
                       selectInput(
                         "sandbox_county", "County",
                         choices = county_choices,
                         selected = county_choices[1]
                       ),
                       selectInput(
                         "sandbox_lever",
                         "Lever (domain z-score)",
                         choices = stats::setNames(domain_z_cols, pretty_domain(names(sdoh_domains))),
                         selected = "z_Education_Attainment"
                       ),
                       sliderInput("sandbox_delta", "Change in lever (SD units)", min = -2, max = 2, value = 0, step = 0.1),
                       hr(style="margin:10px 0;"),
                       tags$div(
                         class="subtle",
                         "Interpretation: Targets are predicted from the six domain z-scores. Adjusting one lever updates predictions while keeping other domains fixed."
                       )
              )
            )
          )
        )
      )
    )
  )
)

