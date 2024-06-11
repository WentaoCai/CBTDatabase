footerTagList <- list(
  tags$footer(id = "myFooter",
    shiny::includeHTML("www/footer.html")
  )
)

shinyUI(fluidPage(
tweaks,
  titlePanel(title=div(
      img(src="Simmental_beef_cattle.jpg", height="10%", width="10%"),
      span("Cattle Bodymap of Transcriptome:", style = "font-size:50px;color:orange;"), 
      span("52 tissues across three developtmental stages", style = "font-size:36px;color:white;"),
      style = "background-color:#1A5276;margin-left: -15px;margin-right: -15px;margin-top: -20px;margin-bottom: -10px;"),
    windowTitle = "Welcome to Cattle Bodymap database!"
    ),
	 
    includeCSS("www/footer.css"),
    
    shinydisconnect::disconnectMessage(
      text = "Your session timed out, reload the application!",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.75,
      top = 250,
      refreshColour = "brown"
    ),
	
	
    navbarPage(
      title = "", 
      windowTitle = "efficient compression of genotype matrix",
       theme = shinytheme("paper"),
      ## About
tabPanel(title = HTML("<strong style='font-size:20px'>Home</strong>"),icon = tags$i(class = "fa-solid fa-house", style="font-size: 20px"),
  
dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    column(
      width = 10,
      offset = 1,
	  box(width = 12, background = "maroon",
 title = span( icon("list-alt"), "CBT Database: Cattle BodyMap Transcriptome Database", style = "font-size:40px;color:white"),
  #title = span( "CBT Database: Cattle BodyMap Transcriptome Database")
)
	  
	  
	  
	  
    ),
    column(
      width = 10,
      offset = 1,
      textBox(
        width = 12,
        p("The Cattle",em("(Bos taurus)"), " BodyMap Transcriptome database (CBT Database) is an interactive web-based platform and set of analytic tools for effcient retrieve and analysis of gene expression, long noncoding RNA, RNA editing, alternative splicing among 52 tissues based on three developtmental stage.")
      , style = "font-size:20px;color:darkred"),
	  img(src="bodymap1.jpg", height="80%", width="80%"),style="text-align: center;"
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = tags$p("This database contains:", style = "font-size: 200%;color:white;"),
		
		        fluidRow(
		   valueBox( value = tags$p("3", style = "font-size: 150%;"), subtitle = tags$p("Developmental stages", style = "font-size: 250%;"),icon = tags$i(class = "fa-solid fa-cow", style="font-size: 100px;color:white"), width = 4,color = "light-blue"),
			valueBox(value = tags$p("13", style = "font-size: 150%;"), subtitle = tags$p("Tissue categories", style = "font-size: 250%;"),icon = tags$i(class = "fa-solid fa-lungs", style="font-size: 100px;color:white"), color = "purple", width = 4),
			valueBox(value = tags$p("52", style = "font-size: 150%;"), subtitle = tags$p("Tissue types", style = "font-size: 250%;"),icon = tags$i(class = "fa-solid fa-brain", style="font-size: 100px;color:white"), color = "maroon", width = 4),
        ),
        fluidRow(
		   valueBox( value = tags$p("25,530", style = "font-size: 150%;"), subtitle = tags$p("Ensemble expressed genes", style = "font-size: 250%;"), icon = tags$i(class = "fa-solid fa-dna", style="font-size: 80px;color:white"),width = 4,color = "fuchsia"),
		   valueBox( value = tags$p("28,533", style = "font-size: 150%;"), subtitle = tags$p("Novel lncRNA genes", style = "font-size: 250%;"),icon = tags$i(class = "fa-solid fa-worm", style="font-size: 100px;color:white"), width = 4,color = "teal"),
		   valueBox( value = tags$p("144,363", style = "font-size: 150%;"), subtitle = tags$p("Novel lncRNA transcripts", style = "font-size: 250%;"), icon = tags$i(class = "fa-solid fa-wave-square", style="font-size: 100px;color:white"),width = 4,color = "aqua")
        ),
	 fluidRow(
	  valueBox( value = tags$p("3,093,058", style = "font-size: 150%;"), subtitle = tags$p("RNA editing events", style = "font-size: 250%;"), icon = tags$i(class = "fa-solid fa-scissors", style="font-size: 100px;color:white"),width = 4,color = "olive"),
	      valueBox( value = tags$p("215,754", style = "font-size: 150%;"), subtitle = tags$p("Alternative splicing events", style = "font-size: 250%;"),icon = tags$i(class = "fa-brands fa-slack", style="font-size: 100px;color:white"), width = 4,color = "green")
		  
  
        )
		
      )
    ),

    column(
      width = 10,
      offset = 1,
      sectionBox(
        title = tags$p("Search modules", style = "font-size: 200%;color:white;"),
#	background = "yellow",
        messageBox(
          width = 12,
          p("We present four modules to retrieve and download the transcriptome dataset. Within each module, the user can select gene name, genomic location or Ensemble gene id to retrieve and analyze the gene expression, lncRNA, RNA editing and splicing information."),
		      style = "font-size:20px;",
          fluidRow(
            module_Box(
              width = 6,
              title = "Gene expression",
              imgSrc = "Module1.jpg",
              text = "This module provides a search box to retrieve gene expression based on gene name or Ensembl ID, then it will generate boxplot of the submited gene based your defined tissues and stages. Downloading figures and tables is free."
            ),
            module_Box(
              width = 6,
              title = "Novel lncRNA",
              imgSrc = "Module2.jpg",
              text = "This module provides a search box to retrieve lncRNA annotation, expression based on genomic location, lncRNA gene id, or lncRNA transcript ID. It will generate gene structure and expression boxplot of lncRNA gene and transcript with defined tissues and stages. Downloading figures and tables is free."
            )
          ),
          fluidRow(
            module_Box(
              width = 6,
              title = "RNA editing",
              imgSrc = "Module3.png",
              text = "This module provides RNA editing information based on genomic location, gene name, or Ensembl ID. The information contains locations, clusters, repeats, expressed tissued and stages. Download is free."
            ),
            module_Box(
              width = 6,
              title = "Alternative splicing",
              imgSrc = "Module4.png",
              text = "This module provides splicing information based on gene name, or Ensembl ID. The information contains genomic location and annotation of five splicing types. Download is free."
            )
          ),
        )
      )
    ),

    fluidRow(
      column(
        width = 10,
        offset = 1,
        box(
          title = "More about the CBT Database",
          width = 12,
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "warning",
          includeMarkdown("Please read manuscript A Cattle BodyMap of Transcriptome across Tissues and Developmental Stages Deciphers Genetic Architecture of Production Traits in Beef Cattle by Cai et al. 2024")
        )
      )
    )
  )
)
),
      
      # Genome browser
         tabPanel(title = HTML("<strong style='font-size:20px;color:#C70039'>Gene expression</strong>"),icon = tags$i(class = "fa-solid fa-magnifying-glass", style="font-size: 20px;color:#C70039"),
titlePanel("Cattle Bodymap gene expression"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(width =6,
                      class = "col-md-5",
                      style = "margin: 1px 2px 1px 1px",
                      tags$table(id = "inputs-table",
                                 style = "width: 200%",
                                 tags$tr(
                                   tags$td(style = "width: 250%",
                                           textInput("thegene", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Gene name/ID</font>'),
                                                                                    bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                                                     value = ""),
                                           bsPopover("q6", "You can input either Ensembl ID or gene name.",
                                                     trigger = "focus")
                                   ), 
                                   tags$td(style = "width: 300%; text-align: left",
                                           div(class = "form-group shiny-input-container",
                                               shinysky::actionButton("submitgene", style="color: #fff; background-color: #C70039; border-color: #C70039",strong("Submit",
                                                                                                                                                                 bsButton("q7", label="", icon=icon("question"), style="info", size="small")
                                               ), width = "100%", styleclass = "success"),
                                               conditionalPanel(condition="input.submitgene != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Plot in progress, wait...</div>"), wait = 0)),
                                               bsPopover("q7", "Whenever the gene name is updated, please click Submit!",
                                                         trigger = "focus")
                                           )
                                   )#/ column 1
                              #/ column 2

                                 ) #/ tr
                      ) #/ table
      )
      
      ),
      fluidRow(column(width =6,
                      class = "col-md-3",
                      style = "margin: 1px 2px 1px 1px",
                      tags$table(id = "inputs-table",
                                 style = "width: 200%",
                                 tags$tr(
                                   
      tags$td(style = "width: 100%; text-align: left",
              div(class = "form-group shiny-input-container",
                  shinysky::actionButton("GExam2", strong("Load gene example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  shinysky::actionButton("GExam3", strong("Load Ensemble ID example"), styleclass = "info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  shinysky::actionButton("clearG", strong("Reset"), styleclass = "warning"),
                  
              )
      )
                                 )
                      )
      )
      ),
    
      checkboxGroupInput("group", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Developmental types:</font>'),
                                                  bsButton("1qg1", label="", icon=icon("question"), style="info", size="small")),
                         choices = c("Newborn","Young","Adult"),
                         selected = c("Newborn","Young","Adult")),
      bsPopover("1qg1", "Only selected stage will be used.",
                trigger = "focus"),
      checkboxGroupInput("Tiss", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Tissue types:</font>'),
                                                   bsButton("1qg2", label="", icon=icon("question"), style="info", size="small")),
                         choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                     "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                     "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                     "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                     "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                     "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                     "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"),
                         selected = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                      "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                      "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                      "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                      "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                      "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                      "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid")),
      bsPopover("1qg2", "Only selected tissue will be used.",trigger = "focus"),
      tags$td(style = "width: 20%; text-align: left",
              div(class = "form-group shiny-input-container",
                  shinysky::actionButton("clearGB2", strong("Reset"), styleclass = "warning"),
                  shinysky::actionButton("clearGB3", strong("Select all"), styleclass = "warning")
              )
      ),
      #radioButtons("colour","Colour of histogram",choices=c("red","green","blue"),selected="red"),
     # radioButtons("Stages","Developtment stage",choices=c("Newborn","Young","Adult","All stages"),selected="All stages"),
     
      radioButtons(inputId = "FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>'),
                                                  bsButton("qg99", label="", icon=icon("question"), style="info", size="small")), choices = list("png", "pdf"), selected = "pdf"),
      width = 3, bsPopover("qg99", "Please select the figure output format.",trigger = "focus")),
    
    mainPanel(
      
      h1("Gene expression boxplot for each tissue", style = "font-size:28px;color:black;"),
      plotOutput("boxplot"),
      numericInput("plot1_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
      numericInput("plot1_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
      numericInput("plot1_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
      downloadButton(outputId = "downloadPlot",  style="color: #fff; background-color: #C70039; border-color: #C70039",label = "Download boxplot for each tissue"),
      h1("                             Gene expression boxplot for each tissue by stage", style = "font-size:28px;color:black;"),
       plotOutput("boxplot2"),
      numericInput("plot2_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
      numericInput("plot2_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
      numericInput("plot2_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
      downloadButton(outputId = "downloadPlot2", style="color: #fff; background-color: #C70039; border-color: #C70039", label = "Download boxplot for each tissue by stage"),
      h2("Gene expression table of the seached gene", style = "font-size:28px;color:black;"),
      downloadButton(outputId = "downloadTable", style="color: #fff; background-color: #C70039; border-color: #C70039", label = "Download gene expression table"),
     # tabsetPanel(type = "tabs",tabPanel("Target",tableOutput('table'))),
     mainPanel(width = 12,
               DT::dataTableOutput("mytable")),
                 #tabPanel("Full", plotOutput(outputId = "full"))),#, tableOutput('table')))
      width = 9
    )
  )
   ),
   
            tabPanel(title = HTML("<strong style='font-size:20px;color:#0C71DC'>LncRNA</strong>"),icon = tags$i(class = "fa-solid fa-worm", style="font-size: 20px;color:#0C71DC"),
titlePanel("Cattle Bodymap lncRNA expression"),
 
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow(column(width =2,
                                             class = "col-md-3",
                                             style = "margin: 1px 2px 1px 1px",
                                             tags$table(id = "inputs-table",
                                                        style = "width: 150%",
                                                        tags$tr(
                                                          tags$td(style = "width: 150%",
                                                                  textInput("regC", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Gene name/ID</font>'),
                                                                                                     bsButton("ql6", label="", icon=icon("question"), style="info", size="small")),
                                                                            value = ""),
                                                                  bsPopover("ql6", "A genomic region can be determined by chromosome positions or gene locu, chr7:29611303-29669223",
                                                                            trigger = "focus")
                                                          ), #/ column 1
                                                          tags$td(style = "width: 120%; text-align: left",
                                                                  div(class = "form-group shiny-input-container",
                                                                      shinysky::actionButton("submit4", style="color: #fff; background-color: #C70039; border-color: #C70039",strong("Submit",
                                                                                                                                                                                     bsButton("ql7", label="", icon=icon("question"), style="info", size="small")
                                                                      ), width = "100%", styleclass = "success"),
                                                                      conditionalPanel(condition="input.submit14 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                                                                      bsPopover("ql7", "Whenever the gene name is updated, please click Submit!",
                                                                                trigger = "focus")
                                                                  )
                                                          ), #/ column 2
                                                          tags$td(style = "width: 100%; text-align: left",
                                                                  div(class = "form-group shiny-input-container",
                                                                      
                                                                      shinysky::actionButton("lncExam1", strong("Load region example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                      shinysky::actionButton("lncExam2", strong("Load gene example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                      shinysky::actionButton("lncExam3", strong("Load Ensemble ID example"), styleclass = "info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                      shinysky::actionButton("clearlnc", strong("Reset"), styleclass = "warning"),
                                                                      
                                                                  )
                                                          )
                                                        ) #/ tr
                                             ) #/ table
                             )
                             
                             ),
                             
                             
                             #      br(),
                             
                             fluidRow(
                               column(3,
                                      sliderInput("lncUP", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Upstream:</font>'),
                                                                   bsButton("qgl2", label="", icon=icon("question"), style="info", size="small")
                                      ), min = 0, max = 50000, value = 0, ticks = FALSE),
                                      bsPopover("qgl2", "SNPs in the upstream of the specified genomic region will be used.",
                                                trigger = "focus"),
                                      sliderInput("lncDOWN", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Downstream:</font>'),
                                                                     bsButton("qgl4", label="", icon=icon("question"), style="info", size="small")
                                      ), min = 0, max = 50000, value = 0, ticks = FALSE),
                                      bsPopover("qgl4", "SNPs in the downstream of the specified genomic region will be used.",
                                                trigger = "focus"),
                                      
                               ),
                               
                               column(6,
                                      tags$div(align = 'left', style ="height:150px",
                                               class = 'multicol', style = "width: 80%",
                                               checkboxGroupInput("lnc_mut_tissue", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Tissue types:</font>'),
                                                                                            bsButton("qgl1", label="", icon=icon("question"), style="info", size="small")),
                                                                  choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                                                              "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                                                              "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                                                              "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                                                              "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                                                              "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                                                              "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"),
                                                                  selected =c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                                                              "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                                                              "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                                                              "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                                                              "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                                                              "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                                                              "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid")),
                                               bsPopover("qgl1", "Only selected tissue will be used.",
                                                         trigger = "focus")
                                      ),
                                      tags$td(style = "width: 20%; text-align: left",
                                              div(class = "form-group shiny-input-container",
                                                  shinysky::actionButton("clearlnc2", strong("Reset"), styleclass = "warning"),
                                                  shinysky::actionButton("clearlnc3", strong("Select all"), styleclass = "warning")
                                              )
                                      )
                               ),
                               fluidRow(column(9,
                                               tags$div(align = 'left',
                                                        class = 'multicol', style = "width: 70%",
                                                        checkboxGroupInput("lnc_mut_group", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Tissue types:</font>'),
                                                                                                    bsButton("qgl1", label="", icon=icon("question"), style="info", size="small")),
                                                                           choices = c("Newborn","Young","Adult"),
                                                                           selected = c("Newborn","Young","Adult")),
                                                        bsPopover("qgl1", "Only selected stage will be used.",
                                                                  trigger = "focus")
                                               )
                               )
                               )
                               
                             ),
                             width = 12),
                           
                           mainPanel(
                             h1("Summary of the searched lncRNA"),
                             verbatimTextOutput("sum"),
                             h1("LncRNA transcript structure figure", style = "font-size:28px;color:black;"),
                             plotOutput("anno_plot"),
                             numericInput("anno_plot_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
                             numericInput("anno_plot_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
                             numericInput("anno_plot_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
                             radioButtons(inputId = "anno_plot_FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>')), choices = list("png", "pdf"), selected = "pdf"),
                             downloadButton("download_lncRNA_annotation_plot", "Download transcript structure figure",style="color: #fff; background-color: #C70039; border-color: #2e6da4"),
                             downloadButton("download_lncRNA_annotation", "Download RNA editing information",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             
                             mainPanel(width = 12,DT::dataTableOutput("lnc_anno_table")),
                             h1("LncRNA expression boxplot", style = "font-size:28px;color:black;"),
                             plotOutput("lncRNA_boxplot1"),
                             numericInput("lncRNA_plot1_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
                             numericInput("lncRNA_plot1_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
                             numericInput("lncRNA_plot1_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
                             radioButtons(inputId = "lncRNA_plot1_FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>')), choices = list("png", "pdf"), selected = "pdf"),
                              downloadButton("download_lncRNA_plot1", "Download lncRNA gene expression",style="color: #fff; background-color: #C70039; border-color: #2e6da4"),
                             h1("LncRNA expression boxplot by stage", style = "font-size:28px;color:black;"),
                             
                             plotOutput("lncRNA_boxplot"),
                             numericInput("lncRNA_plot2_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
                             numericInput("lncRNA_plot2_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
                             numericInput("lncRNA_plot2_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
                             radioButtons(inputId = "lncRNA_plot2_FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>')), choices = list("png", "pdf"), selected = "pdf"),
                              downloadButton("download_lncRNA_plot2", "Download figure of lncRNA gene expression with stage",style="color: #fff; background-color: #C70039; border-color: #2e6da4"),
                             downloadButton("download_lncRNA_table1", "Download table of lncRNA gene expression with stage",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             mainPanel(width = 12,DT::dataTableOutput("lnc_expr_table")),
                             
                             h1("LncRNA transcript expression boxplot", style = "font-size:28px;color:black;"),
                             plotOutput("lncRNA_boxplot1_transcript",height=800),
                             numericInput("lncRNA_plot3_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
                             numericInput("lncRNA_plot3_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
                             numericInput("lncRNA_plot3_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
                             radioButtons(inputId = "lncRNA_plot3_FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>')), choices = list("png", "pdf"), selected = "pdf"),
                             downloadButton("download_lncRNA_plot3", "Download figure of lncRNA transcript expression",style="color: #fff; background-color: #C70039; border-color: #2e6da4"),
                             h1("LncRNA transcript expression boxplot by stage", style = "font-size:28px;color:black;"),
                             plotOutput("lncRNA_boxplot_transcript",height=800),
                             numericInput("lncRNA_plot4_height", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot height (inch):</font>')), value = 7),
                             numericInput("lncRNA_plot4_width", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Plot width (inch):</font>')), value = 14),
                             numericInput("lncRNA_plot4_res", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Image resolution:</font>')), value = 300),
                             radioButtons(inputId = "lncRNA_plot4_FileType", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Boxplot file type:</font>')), choices = list("png", "pdf"), selected = "pdf"),
                              downloadButton("download_lncRNA_plot4", "Download figure of lncRNA transcript expression with stage",style="color: #fff; background-color: #C70039; border-color: #2e6da4"),
                             downloadButton("download_lncRNA_table2", "Download table of lncRNA transcript expression with stage",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             mainPanel(width = 12,DT::dataTableOutput("lnc_transcript_expr_table")),  
                             
                             width = 12
                             ),
    

                           )
                         ),
                          
  
  
 tabPanel(title = HTML("<strong style='font-size:20px;color:#0B8774'>RNA editing</strong>"),icon = tags$i(class = "fa-solid fa-scissors", style="font-size: 20px;color:#0B8774"),
           sidebarLayout(
             sidebarPanel(
           fluidRow(column(width =2,
                           class = "col-md-3",
                           style = "margin: 1px 2px 1px 1px",
                           tags$table(id = "inputs-table",
                                      style = "width: 150%",
                                      tags$tr(
                                        tags$td(style = "width: 150%",
                                                textInput("regB", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Gene name/ID</font>'),
                                                                                   bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                                                          value = ""),
                                                bsPopover("q6", "A genomic region can be determined by chromosome positions or gene locu, chr07:29611303-29669223",
                                                          trigger = "focus")
                                        ), #/ column 1
                                        tags$td(style = "width: 120%; text-align: left",
                                                div(class = "form-group shiny-input-container",
                                                    shinysky::actionButton("submit1", style="color: #fff; background-color: #C70039; border-color: #C70039",strong("Submit",
                                                                                             bsButton("q7", label="", icon=icon("question"), style="info", size="small")
                                                    ), width = "100%", styleclass = "success"),
                                                    conditionalPanel(condition="input.submit1 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                                                    bsPopover("q7", "Whenever the gene name is updated, please click Submit!",
                                                              trigger = "focus")
                                                )
                                        ), #/ column 2
                                        tags$td(style = "width: 100%; text-align: left",
                                                div(class = "form-group shiny-input-container",
                                                    
                                                    shinysky::actionButton("GBExam1", strong("Load region example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                    shinysky::actionButton("GBExam2", strong("Load gene example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                    shinysky::actionButton("GBExam3", strong("Load Ensemble ID example"), styleclass = "info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                    shinysky::actionButton("clearGB", strong("Reset"), styleclass = "warning"),
                                                    
                                                )
                                        )
                                      ) #/ tr
                           ) #/ table
           )
           
           ),
           
        
           #      br(),
           
           fluidRow(
             column(3,
                    sliderInput("GBUP", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Upstream:</font>'),
                                                 bsButton("qgup2", label="", icon=icon("question"), style="info", size="small")
                    ), min = 0, max = 50000, value = 0, ticks = FALSE),
                    bsPopover("qgup2", "SNPs in the upstream of the specified genomic region will be used.",
                              trigger = "focus"),
                    sliderInput("GBDOWN", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Downstream:</font>'),
                                                   bsButton("qg4", label="", icon=icon("question"), style="info", size="small")
                    ), min = 0, max = 50000, value = 0, ticks = FALSE),
                    bsPopover("qg4", "SNPs in the downstream of the specified genomic region will be used.",
                              trigger = "focus"),
            
                    ),
            
             column(6,
                   tags$div(align = 'left', style ="height:150px",
                            class = 'multicol', style = "width: 80%",
                            checkboxGroupInput("GB_mut_tissue", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Tissue types:</font>'),
                                                                        bsButton("qgzz1", label="", icon=icon("question"), style="info", size="small")),
                                               choices = c("Abomasum","Adrenal_gland", "Antiprostate", "Bladder", "Blood_vessel","Cartilago_articularis",
                                                           "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous_cartilage",
                                                           "Heart_fat","Heart_muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney_fat",
                                                           "Liver",  "Longissimus_muscle","Lung", "Lymph_gland",  "Marrow","Medulla_oblongata","Nasal_mucosa",
                                                           "Omasum", "Parotid_gland","Penis","Pineal_body","Prostate","Rectum", "Renal_cortical","Renal_medulla",
                                                           "Reticulum","Rib_cartilage", "Rumen","Seminal_vesicle","Skin","Spinal_cord","Spleen","Subcutaneous_fat",
                                                           "Sublingual_gland", "Submandibular_gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"),
                                               selected =c("Abomasum","Adrenal_gland", "Antiprostate", "Bladder", "Blood_vessel","Cartilago_articularis",
                                                           "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous_cartilage",
                                                           "Heart_fat","Heart_muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney_fat",
                                                           "Liver",  "Longissimus_muscle","Lung", "Lymph_gland",  "Marrow","Medulla_oblongata","Nasal_mucosa",
                                                           "Omasum", "Parotid_gland","Penis","Pineal_body","Prostate","Rectum", "Renal_cortical","Renal_medulla",
                                                           "Reticulum","Rib_cartilage", "Rumen","Seminal_vesicle","Skin","Spinal_cord","Spleen","Subcutaneous_fat",
                                                           "Sublingual_gland", "Submandibular_gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid")),
                            bsPopover("qgzz1", "Only selected tissue will be used.",
                                      trigger = "focus")
                   ),
                   tags$td(style = "width: 20%; text-align: left",
                           div(class = "form-group shiny-input-container",
                               shinysky::actionButton("clearGB2", strong("Reset"), styleclass = "warning"),
                               shinysky::actionButton("clearGB3", strong("Select all"), styleclass = "warning")
                           )
                   )
                   ),
             fluidRow(column(9,
                             tags$div(align = 'left',
                                      class = 'multicol', style = "width: 70%",
                                      checkboxGroupInput("GB_mut_group", tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Stages:</font>'),
                                                                                  bsButton("ed1", label="", icon=icon("question"), style="info", size="small")),
                                                         choices = c("Newborn","Young","Adult"),
                                                         selected = c("Newborn","Young","Adult")),
                                      bsPopover("ed1", "Only selected stage will be used.",
                                                trigger = "focus")
                             )
             )
             )
         
           ),
           width = 12),

           mainPanel(
             downloadButton("download_editing", "Download RNA editing information",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             mainPanel(width = 12,
                       DT::dataTableOutput("editingtable")),
             #tabPanel("Full", plotOutput(outputId = "full"))),#, tableOutput('table')))
             width = 12
           )
           ),

           #  downloadButton("downloadGB.pdf", "Download pdf-file"),
          
  ),
 tabPanel(title = HTML("<strong style='font-size:20px;color:#840BBD '>Splicing</strong>"),icon = tags$i(class = "fa-brands fa-slack", style="font-size: 20px;color:#840BBD "),
                 sidebarLayout(
                  sidebarPanel(
                  fluidRow(column(width =2,
                            class = "col-md-3",
                              style = "margin: 1px 2px 1px 1px",
                                  tags$table(id = "inputs-table",
                                                        style = "width: 150%",
                                                        tags$tr(
                                                          tags$td(style = "width: 150%",
                                                                  textInput("Splcing_in", label = tags$div(HTML('<i class="fa fa-play" aria-hidden="true"></i> <font size="4" color="red">Gene name/ID</font>'),
                                                                                                     bsButton("q6", label="", icon=icon("question"), style="info", size="small")),
                                                                            value = ""),
                                                                  bsPopover("q6", "A genomic region can be determined by chromosome positions or gene locu, chr07:29611303-29669223",
                                                                            trigger = "focus")
                                                          ), #/ column 1
                                                          tags$td(style = "width: 120%; text-align: left",
                                                                  div(class = "form-group shiny-input-container",
                                                                      shinysky::actionButton("submit2", style="color: #fff; background-color: #C70039; border-color: #C70039",strong("Submit",
                                                                                                                                                                                     bsButton("q7", label="", icon=icon("question"), style="info", size="small")
                                                                      ), width = "100%", styleclass = "success"),
                                                                      conditionalPanel(condition="input.submit2 != '0'", shinysky::busyIndicator(HTML("<div style='color:red;font-size:30px'>Calculation In progress...</div>"), wait = 0)),
                                                                      bsPopover("q7", "Whenever the gene name is updated, please click Submit!",
                                                                                trigger = "focus")
                                                                  )
                                                          ), #/ column 2
                                                          tags$td(style = "width: 100%; text-align: left",
                                                                  div(class = "form-group shiny-input-container",
                                                                      shinysky::actionButton("SExam2", strong("Load gene example"), styleclass = "info", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                      shinysky::actionButton("SExam3", strong("Load Ensemble ID example"), styleclass = "info",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                                      shinysky::actionButton("clearS", strong("Reset"), styleclass = "warning"),
                                                                      
                                                                  )
                                                          )
                                                        ) #/ tr
                                             ) #/ table
                             )
                             
                             ),
                             width = 12),
    
                           mainPanel(
                             h2("Alternative 5' splicing site (A3SS) events", style = "font-size:28px;color:black;"),
                             mainPanel(width = 12,DT::dataTableOutput("A3SS_table")),
                             downloadButton("download_A3SS", "Download A3SS splicing events",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            
                             h2("Alternative 3' splicing site (A3SS) events", style = "font-size:28px;color:black;"),
                             mainPanel(width = 12,DT::dataTableOutput("A5SS_table")),
                             downloadButton("download_A5SS", "Download A5SS splicing events",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             
                             h2("Skipped exon (SE) events", style = "font-size:28px;color:black;"),
                             mainPanel(width = 12,DT::dataTableOutput("SE_table")),
                             downloadButton("download_SE", "Download SE splicing events",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                            
                             h2("Retained intron (RI) events", style = "font-size:28px;color:black;"),
                             mainPanel(width = 12,DT::dataTableOutput("RI_table")),
                             downloadButton("download_RI", "Download RI splicing events",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             
                             h2("Mutually exclusive exons (MXE) events", style = "font-size:28px;color:black;"),
                             mainPanel(width = 12,DT::dataTableOutput("MXE_table")),
                             downloadButton("download_MXE", "Download MXE splicing events",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                             #tabPanel("Full", plotOutput(outputId = "full"))),#, tableOutput('table')))
                             width = 12
                           )
                         )
                         
                         #  downloadButton("downloadGB.pdf", "Download pdf-file"),
                         
                ),
	 tabPanel(title = HTML("<strong style='font-size:20px'>About</strong>"),icon = tags$i(class = "fa-solid fa-circle-exclamation", style="font-size: 20px"),
	 includeMarkdown("README.md")),
      footer = footerTagList
    )
	
	
	
	
	)
	  )
  

