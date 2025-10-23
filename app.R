# app.R
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(shinythemes)

# -------------------------------------------------------------------
# 1) CONFIG: labels and priors
# -------------------------------------------------------------------
class_order <- c("NI","HI","SI","DS")
class_full  <- c(NI="No Insomnia", HI="High Insomnia Risk",
                 SI="Subthreshold Insomnia",
                 DS="Predominant Daytime Symptoms")

item_order  <- c("ISI1a","ISI1b","ISI1c","ISI2","ISI3","ISI4","ISI5")
item_labels <- c("Sleep onset (ISI1a)",
                 "Sleep maintenance (ISI1b)",
                 "Early awakening (ISI1c)",
                 "Sleep dissatisfaction (ISI2)",
                 "Daytime interference (ISI3)",
                 "Noticeability (ISI4)",
                 "Worry about sleep (ISI5)")
names(item_labels) <- item_order

# ISI choice labels (generic anchors appropriate across items)
isi_choices <- c(
  "0 = none"         = 0,
  "1 = mild"         = 1,
  "2 = moderate"     = 2,
  "3 = severe"       = 3,
  "4 = very severe"  = 4
)


# Priors from LCA (will be normalized)
priors_raw <- c(NI=0.3181, HI=0.1835, SI=0.3563, DS=0.1422)

# -------------------------------------------------------------------
# 2) LOAD CONDITIONAL PROBS (probs.csv in working dir)
# Expected columns: item, category (1..5), class (NI/HI/SI/DS), prob
# -------------------------------------------------------------------
probs_long <- read_csv("probs.csv", show_col_types = FALSE) %>%
  mutate(
    item     = as.character(item),
    class    = as.character(class),
    category = as.integer(category)
  )

# validation
stopifnot(all(sort(unique(probs_long$item))     == sort(item_order)))
stopifnot(all(sort(unique(probs_long$class))    == sort(class_order)))
stopifnot(all(sort(unique(probs_long$category)) == 1:5))

# normalize per (item,class) so sums over categories == 1 exactly
probs_long <- probs_long %>%
  group_by(item, class) %>%
  mutate(prob = prob / sum(prob)) %>%
  ungroup()

# Build 3D array [item, category, class]
cond_probs_3d <- array(
  NA_real_, dim = c(length(item_order), 5, length(class_order)),
  dimnames = list(item = item_order, category = as.character(1:5), class = class_order)
)
for (it in item_order) {
  for (cl in class_order) {
    pvec <- probs_long %>% filter(item == it, class == cl) %>%
      arrange(category) %>% pull(prob)
    cond_probs_3d[it, as.character(1:5), cl] <- pvec
  }
}

# Normalize priors
priors <- priors_raw / sum(priors_raw)

safe_prob <- function(p, eps = 1e-12) pmax(p, eps)

# Posterior from a 0..4 response vector
posterior_from_ISI <- function(response_0to4, priors, cond_probs_3d,
                               item_order, class_order, eps = 1e-12) {
  resp_1to5 <- response_0to4 + 1L
  logpost <- setNames(rep(0, length(class_order)), class_order)
  for (k in seq_along(class_order)) {
    cls <- class_order[k]
    llk <- 0
    for (i in seq_along(item_order)) {
      r <- resp_1to5[i]
      if (is.na(r)) next
      p_ik <- cond_probs_3d[item_order[i], as.character(r), cls]
      llk  <- llk + log(safe_prob(p_ik, eps))
    }
    logpost[cls] <- llk + log(safe_prob(priors[cls], eps))
  }
  m <- max(logpost)
  post <- exp(logpost - m); post <- post / sum(post)
  post
}

# Expected class item means in 0..4 scale
class_item_means <- function(cond_probs_3d) {
  out <- matrix(NA_real_, nrow=length(item_order), ncol=length(class_order),
                dimnames=list(item_order, class_order))
  for (it in item_order) {
    for (cl in class_order) {
      # categories 1..5 -> convert back to 0..4 by subtracting 1
      cats <- 1:5
      p    <- cond_probs_3d[it, as.character(cats), cl]
      out[it, cl] <- sum((cats - 1) * p)
    }
  }
  out
}

# -------------------------------------------------------------------
# UI
# -------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("united"),
  
  # Styling title
  tags$head(
    tags$style(HTML("
      .title-panel h2 {
        text-align: center;
        font-weight: bold;
        padding: 20px 0;
      }
    "))
  ),
  
  # Wrap titlePanel in a div with class for styling
  div(class = "title-panel",
      titlePanel("Insomnia Latent Class Classifier based on the Insomnia Severity Index (ISI)")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Single-subject input (ISI items 0–4)"),
      lapply(seq_along(item_order), function(i) {
        selectInput(
          inputId   = item_order[i],
          label     = item_labels[item_order[i]],
          choices   = isi_choices,
          selected  = 0,
          selectize = FALSE
        )
      }),
      actionButton("score_btn", "Score subject", width="100%"),
      hr(),
      h4("Batch scoring"),
      helpText("Upload CSV with columns: ISI1a, ISI1b, ISI1c, ISI2, ISI3, ISI4, ISI5 (values 0–4)."),
      fileInput("csv", "Upload CSV"),
      downloadButton("dl_results", "Download results")
    ),
    mainPanel(
      h4("Posteriors"),
      tableOutput("post_tbl"),
      h4("Profile"),
      plotOutput("profile_plot", height="380px"),
      h4("Indices"),
      verbatimTextOutput("subscores")
    )
  )
)

# -------------------------------------------------------------------
# SERVER
# -------------------------------------------------------------------
server <- function(input, output, session) {
  
  means <- reactive({ class_item_means(cond_probs_3d) })
  
  observeEvent(input$score_btn, {
    resp <- sapply(item_order, function(id) as.numeric(input[[id]]))
    names(resp) <- item_order
    
    post <- posterior_from_ISI(resp, priors, cond_probs_3d, item_order, class_order)
    
    output$post_tbl <- renderTable({
      tibble(
        Class = names(post),
        Label = unname(class_full[names(post)]),
        Posterior = round(as.numeric(post), 3)
      ) %>% arrange(desc(Posterior))
    })
    
    # ISI total + subscales
    isi_total <- sum(resp, na.rm=TRUE)
    isi_sev   <- sum(resp[c("ISI1a","ISI1b","ISI1c")], na.rm=TRUE) # nocturnal severity
    isi_diss  <- sum(resp[c("ISI1a","ISI2","ISI5")], na.rm=TRUE)   # dissatisfaction
    isi_imp   <- sum(resp[c("ISI3","ISI4","ISI5")], na.rm=TRUE)    # impact
    
    output$subscores <- renderText({
      paste0("ISI total: ", isi_total,
             " | Severity (ISI1a+ISI1b+ISI1c): ", isi_sev,
             " | Dissatisfaction (ISI1a+ISI2+ISI5): ", isi_diss,
             " | Impact (ISI3+ISI4+ISI5): ", isi_imp)
    })
    
    # Plot: class means vs subject responses (0..4)
    output$profile_plot <- renderPlot({
      cm <- means()
      
      df_class <- as.data.frame(cm) %>%
        mutate(Item = rownames(.)) %>%
        pivot_longer(-Item, names_to="Class", values_to="Mean")
      
      df_class$Item  <- factor(df_class$Item, levels=item_order, labels=item_labels[item_order])
      df_class$Class <- factor(df_class$Class, levels=class_order,
                               labels=unname(class_full[class_order]))
      
      subj <- data.frame(
        Item    = factor(item_order, levels=item_order, labels=item_labels[item_order]),
        Subject = as.numeric(resp)
      )
      
      ggplot() +
        geom_line(data=df_class, aes(Item, Mean, group=Class, linetype=Class)) +
        geom_point(data=df_class, aes(Item, Mean, shape=Class)) +
        geom_line(data=subj, aes(Item, Subject, group=1), linewidth=1) +
        geom_point(data=subj, aes(Item, Subject), size=2) +
        coord_cartesian(ylim=c(0,4)) +
        labs(x=NULL, y="ISI item score (0–4)", title="Class profiles (means) vs. subject") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
  })
  
  # Batch scoring
  results <- reactiveVal(NULL)
  
  observeEvent(input$csv, {
    req(input$csv)
    dat <- readr::read_csv(input$csv$datapath, show_col_types = FALSE)
    stopifnot(all(item_order %in% colnames(dat)))
    score_one <- function(row) {
      resp <- as.numeric(row[item_order])
      posterior_from_ISI(resp, priors, cond_probs_3d, item_order, class_order)
    }
    posts <- t(apply(dat, 1, score_one))
    colnames(posts) <- class_order
    # most likely class and label
    mlc   <- apply(posts, 1, function(v) names(v)[which.max(v)])
    label <- unname(class_full[mlc])
    
    results(cbind(dat,
                  as.data.frame(round(posts, 3)),
                  data.frame(ml_class = mlc, ml_label = label)))
  })
  
  output$dl_results <- downloadHandler(
    filename = function() paste0("isi_lca_scores_", Sys.Date(), ".csv"),
    content  = function(file) readr::write_csv(results(), file)
  )
}

shinyApp(ui, server)
