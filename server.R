server <- function(input, output, session) {
vals <- reactiveValues()
  observe({
    if (input$submitgene>0) {
      isolate({
        gene.expr<-data.frame()
        input_thegene <- gsub("^\\s+", "", input$thegene)
        input_thegene <- gsub("\\s+$", "", input_thegene)
        if(grepl("^ENSBTA",input_thegene)){
          gene.expr<-expr[which(expr$Gene_ID %in% input_thegene),] %>%
          dplyr::filter(grepl(paste(input$group,collapse="|"), Stage))%>%
            dplyr::filter(grepl(paste(input$Tiss,collapse="|"), Tissue))
        }else if (grepl("^[a-zA-Z]",input_thegene)||grepl("^[[:digit:]][a-zA-Z]",input_thegene)){
          
          gene.expr<-expr[which(expr$Gene_name %in% input_thegene),]  %>%
            dplyr::filter(grepl(paste(input$group,collapse="|"), Stage))%>%
            dplyr::filter(grepl(paste(input$Tiss,collapse="|"), Tissue))
          
        }else{
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input correct gene name or ensembl ID for expression search!")
        }
        if ( nrow(gene.expr) < 1) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = paste0(input$thegene, " gene was not collected in this database!"))
          }else{
  output$boxplot <- renderPlot({

    gg<- ggplot(data = gene.expr, aes(x = Tissue, y = FPKM, fill = Category)) +
      geom_boxplot(width=0.6) +
      xlab(paste0("Expression of ",input$thegene)) + ylab("Expression/FPKM") +
      scale_fill_manual(values=mycolor1)+
      theme_ydz1
    #+theme(axis.text.x = element_text(color=anno1$Color))
    vals$gg <- gg
    
    print(gg)
           # strip.text = element_text(size=15, face = "bold", color = "black")) 
     # scale_fill_manual(values = input$colour)
  })
  output$downloadPlot <- downloadHandler(
    filename =  function() {
      paste(input_thegene, input$FileType,sep=".")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file){
      if(input$FileType=="png")
        png(file, units="in", width=input$plot1_width, height=input$plot1_height, res=input$plot1_res)
      else
        pdf(file, width=input$plot1_width, height=input$plot1_height)
      print(vals$gg)
      dev.off()
    } 
  )
  output$boxplot2 <- renderPlot({
    
    gg2<- ggplot(data = gene.expr, aes(x = Tissue, y = FPKM, fill = Stage)) +
      geom_boxplot(width=0.6, position=position_dodge(0.7,preserve = "single")) +
      xlab(paste0("Expression of ",input_thegene)) + ylab("Expression/FPKM") +
      scale_fill_manual(values=mycolor1)+
    #  scale_color_manual(values=c("#FA15AA","#04A9AC","black"))+
      theme_ydz1+theme(plot.margin = margin(t = 20,  # 顶部边缘距离
                                            r = 10,  # 右边边缘距离
                                            b = 10,  # 底部边缘距离
                                            l = 20)) # 左边边缘距离
    
    #+theme(axis.text.x = element_text(color=anno1$Color))
    vals$gg2 <- gg2
    print(gg2)
    # strip.text = element_text(size=15, face = "bold", color = "black")) 
    # scale_fill_manual(values = input$colour)
  })
  output$downloadPlot2 <- downloadHandler(
    filename =  function() {
     paste(input_thegene, input$FileType,sep=".by_stage.")
  
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file){
      if(input$FileType=="png")
        png(file, units="in", width=input$plot2_width, height=input$plot2_height, res=input$plot2_res)
      else
        pdf(file, width=input$plot2_width, height=input$plot2_height)
      print(vals$gg2)
      dev.off()
    } 
  )
  
  gene.expr.table<-aggregate(FPKM ~ Stage + Tissue+Category, data =gene.expr, FUN = mean) %>%
    spread( key="Stage",value="FPKM") %>%
    mutate_if(is.numeric, ~round(., 2)) 
       
  output$downloadTable <- downloadHandler(
    filename =  function() {
      paste(input_thegene,".FPKM.csv")
    },
    content = function(file){
      write.csv(gene.expr.table, file, row.names = FALSE)
    } 
  )

  output$mytable <-  DT::renderDataTable(gene.expr.table,options = list(rowCallback = JS(rowCallback),scrollX = TRUE,pageLength = 52),
                                         rownames = FALSE)
}
  })
    }else{
      NULL
    }
  })

#  output$table <- renderTable({
#    dat()
#  })

  
  
  
  observeEvent(input$clearGB2, {
    updateCheckboxGroupInput(session, "Tiss", choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                                          "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                                          "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                                          "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                                          "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                                          "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                                          "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"),selected = NULL)})
  
  observeEvent(input$clearGB3, {
    updateCheckboxGroupInput(session, "Tiss", choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
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
                                          "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"))})

  observe({
    if (input$clearG>0) {
      isolate({
        updateTextInput(session, "thegene", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$GExam2 >0) {
      isolate({
        updateTextInput(session, "thegene", value="DGAT1")
      })
    } else {NULL}
  })
  observe({
    if (input$GExam3 >0) {
      isolate({
        updateTextInput(session, "thegene", value="ENSBTAG00000017704")
      })
    } else {NULL}
  })
  
  
observe({
    if (input$submit4>0) {
      isolate({
        lncsummary<-data.frame()
        lncRNA_info<-data.frame()
        lnc.expr<-data.frame()
        input_regC <- gsub("^\\s+", "", input$regC)
        input_regC <- gsub("\\s+$", "", input_regC)
        if(grepl(":",input_regC)){
          myPosl <- anaReg1(input_regC)
            if (!is.null(myPosl)) {
              #lncRNA_info<-lncRNA_bed[which(paste0("chr",lncRNA_bed$Chrom)==myPosl$chr & lncRNA_bed$End>=myPosl$start- input$lncUP),]
              lncRNA_info1<-dplyr::filter(klncRNA_gtf,type == "transcript" & Chrom == myPosl$chr &  end >= myPosl$start - input$lncUP & start <= myPosl$end + input$lncDOWN)
              lncRNA_transcript_id<-unique(lncRNA_info1$Transcript_id)
              lncRNA_gene_id<-unique(lncRNA_info1$Gene_id )
              lncRNA_info<-klncRNA_gtf[which(klncRNA_gtf$Transcript_id %in% lncRNA_transcript_id),]
 
            } else {
              lncRNA_info <- NULL
            }
            
            
            if (is.null(lncRNA_info)) {
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "Error input!", type = "error",
                text = "No novel lncRNA was detected in this region!"
              )
            }
        }else if(grepl("^XLOC_",input_regC)){
          lncRNA_info<-klncRNA_gtf[which(klncRNA_gtf$Gene_id %in% input_regC),]
          lncRNA_gene_id<-input_regC
          lncRNA_transcript_id<-unique(lncRNA_info$Transcript_id)
        }else if (grepl("^TCONS_",input_regC)){
          input_regC<-"TCONS_00050545"
          lncRNA_info<-klncRNA_gtf[which(klncRNA_gtf$Transcript_id %in% input_regC),]
          lncRNA_gene_id<-unique(lncRNA_info$Gene_id)
          lncRNA_transcript_id<-input_regC
          #snp.info<-editing_ann[which(editing_ann$Gene_name %in% input_regB ),] %>%
           # dplyr::filter(grepl(paste(input$GB_mut_group,collapse="|"), Stages))%>%
            #dplyr::filter(grepl(paste(input$GB_mut_tissue,collapse="|"), Tissues))
        }
        else{
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input correct lncRNA name XLOC or TCONS!")
        }
        if ( nrow(lncRNA_info) < 1) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "No novel lncRNA is detected in this specified genomic region!"
          )
        }else{
          lnc.gene.expr<-rbind(lncRNA_gene.expr[1:3,],lncRNA_gene.expr[which(lncRNA_gene.expr$ID %in% lncRNA_gene_id  ),])
          lnc.gene.expr<-as.data.frame(t(lnc.gene.expr))
          colnames(lnc.gene.expr)<-lnc.gene.expr[1,]
          lnc.gene.expr<- lnc.gene.expr[-1,]%>%
            dplyr::filter(grepl(paste(input$lnc_mut_group,collapse="|"), Stage))%>%
            dplyr::filter(grepl(paste(input$lnc_mut_tissue,collapse="|"), Tissue))
          
          lnc.gene.expr[,4]<-as.numeric(lnc.gene.expr[,4])
          lncRNA.expr_table<-aggregate(lnc.gene.expr[,4]~Stage+Tissue+Category,data=lnc.gene.expr, FUN = mean)  %>%
            spread( key="Stage",value="lnc.gene.expr[, 4]") %>%
            mutate_if(is.numeric, ~round(., 2)) 
          
          
     
          
          lnc.transcript.expr<-rbind(lncRNA_trans.expr[1:3,],lncRNA_trans.expr[which(lncRNA_trans.expr$ID %in% lncRNA_info$Transcript_id ),])
          lnc.transcript.expr<-as.data.frame(t(lnc.transcript.expr))
          colnames(lnc.transcript.expr)<-lnc.transcript.expr[1,]
          lnc.transcript.expr<- lnc.transcript.expr[-1,]%>%
            dplyr::filter(grepl(paste(input$lnc_mut_group,collapse="|"), Stage))%>%
            dplyr::filter(grepl(paste(input$lnc_mut_tissue,collapse="|"), Tissue))
          
  
          if(ncol(lnc.transcript.expr)>4){
          lnc.transcript.expr[,4:ncol(lnc.transcript.expr)] <- lapply(lnc.transcript.expr[,4:ncol(lnc.transcript.expr)], as.numeric)
          }else{
            lnc.transcript.expr[,4]<-as.numeric(lnc.transcript.expr[,4])
          }

          lncRNA.transcript_expr_table<-aggregate(.~Stage+Tissue+Category,data=lnc.transcript.expr, FUN = mean)%>%
            mutate_if(is.numeric, ~round(., 2)) 
          
          
          lncRNA.transcript_plot<-as_tibble(lnc.transcript.expr) %>%
            pivot_longer(cols=4:ncol(lnc.transcript.expr)) %>%
            as.data.frame() 
          lncRNA.transcript_plot$value=as.numeric(lncRNA.transcript_plot$value)
          if(length(lncRNA_gene_id)>1){
            shown_id<-unique(lncRNA_info[which(lncRNA_info$Gene_id== colnames(lnc.gene.expr)[4]),]$Transcript_id)
            lncRNA.transcript_plot<-lncRNA.transcript_plot[which(lncRNA.transcript_plot$name %in% shown_id),]
          }
       
        
          lncsummary<-data.frame(
            lncRNA_gene_number<-length(unique(lncRNA_info$Gene_id)),
            lncRNA_transcript_number<-length(unique(dplyr::filter(lncRNA_info,type == "exon")$Transcript_id)),
            Intergenic_lncRNA<-length(unique(dplyr::filter(lncRNA_info,type == "exon"&transcript_biotype=="Integenic_lncRNA")$Transcript_id)),
            Intron_lncRNA<-length(unique(dplyr::filter(lncRNA_info,type == "exon"&transcript_biotype=="Intron_lncRNA")$Transcript_id))
          )
          
          lncsummary<-t(lncsummary)
            rownames(lncsummary)<-c("LncRNA_gene","LncRNA_transcript","Intergenic", "Intron")
            colnames(lncsummary)<-c("Number")
            output$sum <- renderPrint({
              print(lncsummary)
            })
            
            lnc.gene.expr$Stage <- factor(lnc.gene.expr$Stage, levels = c("Newborn","Young","Adult"))
            lnc.gene.expr$Tissue <- factor(lnc.gene.expr$Tissue, levels = myorder)
            lncRNA.transcript_plot$Stage <- factor(lncRNA.transcript_plot$Stage, levels = c("Newborn","Young","Adult"))
            lncRNA.transcript_plot$Tissue <- factor(lncRNA.transcript_plot$Tissue, levels = myorder)
        lncRNA_anno_p <-dplyr::filter(lncRNA_info,type == "exon")
        output$anno_plot <- renderPlot({
          gg3<-dplyr::filter(lncRNA_anno_p,type == "exon") %>% 
            ggplot(aes(xstart = start, xend = end, y = Transcript_id)) +
            geom_range(aes(fill = transcript_biotype), height = 0.25) +
            geom_intron(data = to_intron(lncRNA_anno_p, "Transcript_id"),
                        aes(strand = strand),arrow.min.intron.length = 200
            )+theme_ydz2
          
          vals$gg3 <- gg3
          print(gg3)
        })
        output$download_lncRNA_annotation_plot <- downloadHandler(
          filename =  function() {
            paste(input_regC, input$anno_plot_FileType,sep=".transcript_structure.")
            
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file){
            if(input$anno_plot_FileType=="png")
              png(file, units="in", width=input$anno_plot_width, height=input$anno_plot_height, res=input$anno_plot_res)
            else
              pdf(file, width=input$anno_plot_width, height=input$anno_plot_height)
            print(vals$gg3)
            dev.off()
          } 
        )
        
        output$download_lncRNA_annotation <- downloadHandler(
          filename =  function() {
            paste0(input_regC,".annotation.csv")
          },
          content = function(file){
            write.csv(lncRNA_info, file, row.names = FALSE)
          } 
        )
        
        output$lnc_anno_table <-  DT::renderDataTable(lncRNA_info,options = list(rowCallback = JS(rowCallback),scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
        
        output$lncRNA_boxplot1 <- renderPlot({
          
          gg5<- ggplot(data = lnc.gene.expr, aes(x = Tissue, y = lnc.gene.expr[,4], fill = Category)) +
            geom_boxplot(width=0.6) +
            xlab(paste0("Expression of ",colnames(lnc.gene.expr)[4])) + ylab("Expression/FPKM") +
            scale_fill_manual(values=mycolor1)+
            theme_ydz1
          vals$gg5 <- gg5
          print(gg5)
        })
        
        output$download_lncRNA_plot1 <- downloadHandler(
          filename =  function() {
            paste(colnames(lnc.gene.expr)[4], input$lncRNA_plot1_FileType,sep=".gene.")
            
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file){
            if(input$lncRNA_plot1_FileType=="png")
              png(file, units="in", width=input$lncRNA_plot1_width, height=input$lncRNA_plot1_height, res=input$lncRNA_plot1_res)
            else
              pdf(file, width=input$lncRNA_plot1_width, height=input$lncRNA_plot1_height)
            print(vals$gg5)
            dev.off()
          } 
        )
        
        
        output$lncRNA_boxplot <- renderPlot({
          
          gg4<- ggplot(data = lnc.gene.expr, aes(x = Tissue, y = lnc.gene.expr[,4], fill = Stage)) +
            geom_boxplot(width=0.6, position=position_dodge(0.7,preserve = "single")) +
            xlab(paste0("Expression of ",colnames(lnc.gene.expr)[4])) + ylab("Expression/FPKM") +
            scale_fill_manual(values=mycolor1)+
            theme_ydz1
          vals$gg4 <- gg4
          print(gg4)
          
        })
        
        output$download_lncRNA_plot2 <- downloadHandler(
          filename =  function() {
            paste(colnames(lnc.gene.expr)[4], input$lncRNA_plot2_FileType,sep=".gene.by_stage.")
            
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file){
            if(input$lncRNA_plot2_FileType=="png")
              png(file, units="in", width=input$lncRNA_plot2_width, height=input$lncRNA_plot2_height, res=input$lncRNA_plot2_res)
            else
              pdf(file, width=input$lncRNA_plot2_width, height=input$lncRNA_plot2_height)
            print(vals$gg4)
            dev.off()
          } 
        )
        
        output$download_lncRNA_table1 <- downloadHandler(
          filename =  function() {
            paste0(colnames(lnc.gene.expr)[4],".gene.FPKM.csv")
          },
          content = function(file){
            write.csv(lncRNA.expr_table, file, row.names = FALSE)
          } 
        )
        output$lnc_expr_table <-  DT::renderDataTable(lncRNA.expr_table,options = list(rowCallback = JS(rowCallback),scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
        
        output$lncRNA_boxplot1_transcript <- renderPlot({
          
          gg6<- ggplot(data = lncRNA.transcript_plot, aes(x = Tissue, y = value, fill = Category)) +
            geom_boxplot(width=0.6) +
            xlab(paste0("Expression of transcript(s) in ",colnames(lnc.gene.expr)[4])) + ylab("Expression/FPKM") +
            scale_fill_manual(values=mycolor1)+facet_grid(rows = vars(name ))+
            theme_ydz1
          vals$gg6 <- gg6
          print(gg6)
        })
        
        output$download_lncRNA_plot3 <- downloadHandler(
          filename =  function() {
            paste(colnames(lnc.gene.expr)[4], input$lncRNA_plot3_FileType,sep=".transcript.")
            
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file){
            if(input$lncRNA_plot3_FileType=="png")
              png(file, units="in", width=input$lncRNA_plot3_width, height=input$lncRNA_plot3_height, res=input$lncRNA_plot3_res)
            else
              pdf(file, width=input$lncRNA_plot3_width, height=input$lncRNA_plot3_height)
            print(vals$gg6)
            dev.off()
          } 
        )
        
        output$lncRNA_boxplot_transcript <- renderPlot({
          
          gg7<- ggplot(data = lncRNA.transcript_plot, aes(x = Tissue, y = value, fill = Stage)) +
            geom_boxplot(width=0.6, position=position_dodge(0.7,preserve = "single")) +
            xlab(paste0("Expression of transcript(s) in ",colnames(lnc.gene.expr)[4])) + ylab("Expression/FPKM") +
            scale_fill_manual(values=mycolor1)+facet_grid(rows = vars(name))+
            theme_ydz1
          vals$gg7 <- gg7
          print(gg7)
        })
        output$download_lncRNA_plot4 <- downloadHandler(
          filename =  function() {
            paste(colnames(lnc.gene.expr)[4], input$lncRNA_plot4_FileType,sep=".transcript.by_stage.")
            
          },
          # content is a function with argument file. content writes the plot to the device
          content = function(file){
            if(input$lncRNA_plot4_FileType=="png")
              png(file, units="in", width=input$lncRNA_plot4_width, height=input$lncRNA_plot4_height, res=input$lncRNA_plot4_res)
            else
              pdf(file, width=input$lncRNA_plot4_width, height=input$lncRNA_plot4_height)
            print(vals$gg7)
            dev.off()
          } 
        )
        
        output$download_lncRNA_table2<- downloadHandler(
          filename =  function() {
            paste0(colnames(lnc.gene.expr)[4],".transcript.FPKM.csv")
          },
          content = function(file){
            write.csv(lncRNA.transcript_expr_table, file, row.names = FALSE)
          } 
        )
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearlnc>0) {
      isolate({
        updateTextInput(session, "regC", value="")
      })
    } else {NULL}
  })
  

  observeEvent(input$clearlnc2, {
    updateCheckboxGroupInput(session, "lnc_mut_tissue", choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
                                                          "Cecum", "Cerebellum","Cerebrum", "Colon", "Duodenum", "Epididymis",  "Esophagus","Fibrous cartilage",
                                                          "Heart fat","Heart muscle","Hypophysis", "Hypothalamus", "Ileum","Jejunum",  "Kidney fat",
                                                          "Liver",  "Longissimus muscle","Lung", "Lymph gland",  "Marrow","Medulla oblongata","Nasal mucosa",
                                                          "Omasum", "Parotid gland","Penis","Pineal body","Prostate","Rectum", "Renal cortical","Renal medulla",
                                                          "Reticulum","Rib cartilage", "Rumen","Seminal vesicle","Skin","Spinal cord","Spleen","Subcutaneous fat",
                                                          "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"),selected = NULL)})
  
  observeEvent(input$clearlnc3, {
    updateCheckboxGroupInput(session, "lnc_mut_tissue", choices = c("Abomasum","Adrenal gland", "Antiprostate", "Bladder", "Blood vessel","Cartilago articularis",
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
                                                          "Sublingual gland", "Submandibular gland", "Testis","Thymus", "Thyroid", "Tongue","Trachea","Xiphoid"))})
  
  observe({
    if (input$lncExam1 >0) {
      isolate({
        updateTextInput(session, "regC", value="chr1:2300000-2465000")
      })
    } else {NULL}
  })
  
  observe({
    if (input$lncExam2 >0) {
      isolate({
        updateTextInput(session, "regC", value="XLOC_000902")
      })
    } else {NULL}
  })
  observe({
    if (input$lncExam3 >0) {
      isolate({
        updateTextInput(session, "regC", value="TCONS_00261016")
      })
    } else {NULL}
  })

  observe({
    if (input$submit1>0) {
      isolate({
	      snp.info<-data.frame()
        input_regB <- gsub("^\\s+", "", input$regB)
        input_regB <- gsub("\\s+$", "", input_regB)
        if(grepl(":",input_regB)){
          myPos <- anaReg(input_regB)
          if (validReg(myPos)) {
            if (!is.null(myPos)) {
              snp.info<-editing_ann[which(paste0("chr",editing_ann$Chr)==myPos$chr &editing_ann$Position>=myPos$start - input$GBUP & editing_ann$Position<=myPos$end+ input$GBDOWN),]
                snp.info<-dplyr::filter(snp.info,grepl(paste(input$GB_mut_group,collapse="|"), Stages)) %>%
                  dplyr::filter(grepl(paste(input$GB_mut_tissue,collapse="|"), Tissues))
            } else {
              snp.info <- NULL
            }
            
            
            if (is.null(snp.info)) {
              shinyWidgets::sendSweetAlert(
                session = session,
                title = "Error input!", type = "error",
                text = "The specified genomic region is too large!"
              )
            }
          } else {
            shinyWidgets::sendSweetAlert(
              session = session,
              title = "Error input!", type = "error",
              text = "Please input genomic region in appropriate format! Such as: chr9:8630000-8652000"
            )
          }
        }else if(grepl("^ENSBTA",input_regB)){
          snp.info<-editing_ann[which(editing_ann$Gene_ID %in% input_regB),] %>%
            dplyr::filter(grepl(paste(input$GB_mut_group,collapse="|"), Stages))%>%
            dplyr::filter(grepl(paste(input$GB_mut_tissue,collapse="|"), Tissues))
            
        }else if (grepl("^[a-zA-Z]",input_regB)||grepl("^[[:digit:]][a-zA-Z]",input_regB)){
          snp.info<-editing_ann[which(editing_ann$Gene_name %in% input_regB ),] %>%
            dplyr::filter(grepl(paste(input$GB_mut_group,collapse="|"), Stages))%>%
            dplyr::filter(grepl(paste(input$GB_mut_tissue,collapse="|"), Tissues))
        }
        else{
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input correct gene name or ensembl ID for RNA editing search!!")
        }
        if ( nrow(snp.info) < 1) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "No RNA editing site is detected in this specified genomic region!"
          )
        }else{
          
          output$editingtable <-  DT::renderDataTable(snp.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_editing <- downloadHandler(
            filename =  function() {
              "RNA_editing.info.csv"
            },
            content = function(file){
              write.csv(snp.info, file, row.names = FALSE)
            } 
          )
        }
      })
    } else {
      NULL
    }
  })
  
  observe({
    if (input$clearGB>0) {
      isolate({
        updateTextInput(session, "regB", value="")
      })
    } else {NULL}
  })
  observeEvent(input$clearGB2, {
    updateCheckboxGroupInput(session, "GB_mut_tissue", choices = c("Abomasum",
                                                                   "Adrenal_gland",
                                                                   "Antiprostate",
                                                                   "Bladder",
                                                                   "Blood_vessel",
                                                                   "Cartilago_articularis",
                                                                   "Cecum",
                                                                   "Cerebellum",
                                                                   "Cerebrum",
                                                                   "Colon",
                                                                   "Duodenum",
                                                                   "Epididymis",
                                                                   "Esophagus",
                                                                   "Fibrous_cartilage",
                                                                   "Heart_fat",
                                                                   "Heart_muscle",
                                                                   "Hypophysis",
                                                                   "Hypothalamus",
                                                                   "Ileum",
                                                                   "Jejunum",
                                                                   "Kidney_fat",
                                                                   "Liver",
                                                                   "Longissimus_muscle",
                                                                   "Lung",
                                                                   "Lymph_gland",
                                                                   "Marrow",
                                                                   "Medulla_oblongata",
                                                                   "Nasal_mucosa",
                                                                   "Omasum",
                                                                   "Parotid_gland",
                                                                   "Penis",
                                                                   "Pineal_body",
                                                                   "Prostate",
                                                                   "Rectum",
                                                                   "Renal_cortical",
                                                                   "Renal_medulla",
                                                                   "Reticulum",
                                                                   "Rib_cartilage",
                                                                   "Rumen",
                                                                   "Seminal_vesicle",
                                                                   "Skin",
                                                                   "Spinal_cord",
                                                                   "Spleen",
                                                                   "Subcutaneous_fat",
                                                                   "Sublingual_gland",
                                                                   "Submandibular_gland",
                                                                   "Testis",
                                                                   "Thymus",
                                                                   "Thyroid",
                                                                   "Tongue",
                                                                   "Trachea",
                                                                   "Xiphoid"),selected = NULL)})
  
  observeEvent(input$clearGB3, {
    updateCheckboxGroupInput(session, "GB_mut_tissue", choices = c("Abomasum",
                                                                   "Adrenal_gland",
                                                                   "Antiprostate",
                                                                   "Bladder",
                                                                   "Blood_vessel",
                                                                   "Cartilago_articularis",
                                                                   "Cecum",
                                                                   "Cerebellum",
                                                                   "Cerebrum",
                                                                   "Colon",
                                                                   "Duodenum",
                                                                   "Epididymis",
                                                                   "Esophagus",
                                                                   "Fibrous_cartilage",
                                                                   "Heart_fat",
                                                                   "Heart_muscle",
                                                                   "Hypophysis",
                                                                   "Hypothalamus",
                                                                   "Ileum",
                                                                   "Jejunum",
                                                                   "Kidney_fat",
                                                                   "Liver",
                                                                   "Longissimus_muscle",
                                                                   "Lung",
                                                                   "Lymph_gland",
                                                                   "Marrow",
                                                                   "Medulla_oblongata",
                                                                   "Nasal_mucosa",
                                                                   "Omasum",
                                                                   "Parotid_gland",
                                                                   "Penis",
                                                                   "Pineal_body",
                                                                   "Prostate",
                                                                   "Rectum",
                                                                   "Renal_cortical",
                                                                   "Renal_medulla",
                                                                   "Reticulum",
                                                                   "Rib_cartilage",
                                                                   "Rumen",
                                                                   "Seminal_vesicle",
                                                                   "Skin",
                                                                   "Spinal_cord",
                                                                   "Spleen",
                                                                   "Subcutaneous_fat",
                                                                   "Sublingual_gland",
                                                                   "Submandibular_gland",
                                                                   "Testis",
                                                                   "Thymus",
                                                                   "Thyroid",
                                                                   "Tongue",
                                                                   "Trachea",
                                                                   "Xiphoid"),selected = c("Abomasum",
                                                                                           "Adrenal_gland",
                                                                                           "Antiprostate",
                                                                                           "Bladder",
                                                                                           "Blood_vessel",
                                                                                           "Cartilago_articularis",
                                                                                           "Cecum",
                                                                                           "Cerebellum",
                                                                                           "Cerebrum",
                                                                                           "Colon",
                                                                                           "Duodenum",
                                                                                           "Epididymis",
                                                                                           "Esophagus",
                                                                                           "Fibrous_cartilage",
                                                                                           "Heart_fat",
                                                                                           "Heart_muscle",
                                                                                           "Hypophysis",
                                                                                           "Hypothalamus",
                                                                                           "Ileum",
                                                                                           "Jejunum",
                                                                                           "Kidney_fat",
                                                                                           "Liver",
                                                                                           "Longissimus_muscle",
                                                                                           "Lung",
                                                                                           "Lymph_gland",
                                                                                           "Marrow",
                                                                                           "Medulla_oblongata",
                                                                                           "Nasal_mucosa",
                                                                                           "Omasum",
                                                                                           "Parotid_gland",
                                                                                           "Penis",
                                                                                           "Pineal_body",
                                                                                           "Prostate",
                                                                                           "Rectum",
                                                                                           "Renal_cortical",
                                                                                           "Renal_medulla",
                                                                                           "Reticulum",
                                                                                           "Rib_cartilage",
                                                                                           "Rumen",
                                                                                           "Seminal_vesicle",
                                                                                           "Skin",
                                                                                           "Spinal_cord",
                                                                                           "Spleen",
                                                                                           "Subcutaneous_fat",
                                                                                           "Sublingual_gland",
                                                                                           "Submandibular_gland",
                                                                                           "Testis",
                                                                                           "Thymus",
                                                                                           "Thyroid",
                                                                                           "Tongue",
                                                                                           "Trachea",
                                                                                           "Xiphoid"))})
  
  observe({
    if (input$GBExam1 >0) {
      isolate({
        updateTextInput(session, "regB", value="chr10:8630000-8650000")
      })
    } else {NULL}
  })
  
  observe({
    if (input$GBExam2 >0) {
      isolate({
        updateTextInput(session, "regB", value="TMED10")
      })
    } else {NULL}
  })
  observe({
    if (input$GBExam3 >0) {
      isolate({
        updateTextInput(session, "regB", value="ENSBTAG00000005694")
      })
    } else {NULL}
  })
  
  observe({
    if (input$submit2>0) {
      isolate({
        RI.info<-data.frame()
		MXE.info<-data.frame()
		SE.info<-data.frame()
		A5SS.info<-data.frame()
		A3SS.info<-data.frame()
        input_Splcing_in <- gsub("^\\s+", "", input$Splcing_in)
        input_Splcing_in <- gsub("\\s+$", "", input_Splcing_in)
  if(grepl("^ENSBTA",input_Splcing_in)){
    RI.info<-RI[which(RI$GeneID %in% input_Splcing_in),] 
    MXE.info<-MXE[which(MXE$GeneID %in% input_Splcing_in),] 
    SE.info<-SE[which(SE$GeneID %in% input_Splcing_in),] 
    A5SS.info<-A5SS[which(A5SS$GeneID %in% input_Splcing_in),] 
    A3SS.info<-A3SS[which(A3SS$GeneID %in% input_Splcing_in),] 
        }else if (grepl("^[a-zA-Z]",input_Splcing_in)||grepl("^[[:digit:]][a-zA-Z]",input_Splcing_in)){
          RI.info<-RI[which(RI$geneSymbol %in% input_Splcing_in),] 
          MXE.info<-MXE[which(MXE$geneSymbol %in% input_Splcing_in),] 
          SE.info<-SE[which(SE$geneSymbol %in% input_Splcing_in),] 
          A5SS.info<-A5SS[which(A5SS$geneSymbol %in% input_Splcing_in),] 
          A3SS.info<-A3SS[which(A3SS$geneSymbol %in% input_Splcing_in),] 
        }
        else{
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "Please input correct gene name or ensembl ID!")
        }
        if ( nrow(RI.info)+ nrow(MXE.info)+ nrow(SE.info)+ nrow(A5SS.info)+ nrow(A3SS.info) < 1) {
          shinyWidgets::sendSweetAlert(
            session = session,
            title = "Error input!", type = "error",
            text = "No Splicing event in this gene!"
          )
        }else{
          output$A5SS_table <-  DT::renderDataTable(A5SS.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_A5SS <- downloadHandler(
            filename =  function() {
              paste0(input_Splcing_in,".A5SS.info.csv")
            },
            content = function(file){
              write.csv(A5SS.info, file, row.names = FALSE)
            } 
             )
          output$A3SS_table <-  DT::renderDataTable(A3SS.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_A3SS <- downloadHandler(
            filename =  function() {
              paste0(input_Splcing_in,".A3SS.info.csv")
            },
            content = function(file){
              write.csv(A3SS.info, file, row.names = FALSE)
            } 
          )
          output$SE_table <-  DT::renderDataTable(SE.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_SE <- downloadHandler(
            filename =  function() {
              paste0(input_Splcing_in,".SE.info.csv")
            },
            content = function(file){
              write.csv(SE.info, file, row.names = FALSE)
            } 
          )
          
          output$RI_table <-  DT::renderDataTable(RI.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_RI <- downloadHandler(
            filename =  function() {
              paste0(input_Splcing_in,".RI.info.csv")
            },
            content = function(file){
              write.csv(RI.info, file, row.names = FALSE)
            } 
          )
          output$MXE_table <-  DT::renderDataTable(MXE.info,options = list(scrollX = TRUE,pageLength = 30),
                                                      rownames = FALSE) 
          output$download_MXE <- downloadHandler(
            filename =  function() {
              paste0(input_Splcing_in,".MXE.info.csv")
            },
            content = function(file){
              write.csv(MXE.info, file, row.names = FALSE)
            } 
          )
		  NULL
        }
      })
    } else {
      output$download_A5SS <- downloadHandler(
        filename =  function() {
          "All.A5SS.info.csv"
        },
        content = function(file){
          write.csv(A5SS, file, row.names = FALSE)
        }
      )
      
      output$download_A3SS <- downloadHandler(
        filename =  function() {
          "All.A3SS.info.csv"
        },
        content = function(file){
          write.csv(A3SS, file, row.names = FALSE)
        } 
      )
      
      output$download_SE <- downloadHandler(
        filename =  function() {
          "All.SE.info.csv"
        },
        content = function(file){
          write.csv(SE, file, row.names = FALSE)
        } 
      )
      
      output$download_RI <- downloadHandler(
        filename =  function() {
          "All.RI.info.csv"
        },
        content = function(file){
          write.csv(RI, file, row.names = FALSE)
        } 
      )
      
      output$download_MXE <- downloadHandler(
        filename =  function() {
          "All.MXE.info.csv"
        },
        content = function(file){
          write.csv(MXE, file, row.names = FALSE)
        } 
      )
      
    }
  })
  
  observe({
    if (input$clearS>0) {
      isolate({
        updateTextInput(session, "Splcing_in", value="")
      })
    } else {NULL}
  })
  
  observe({
    if (input$SExam2 >0) {
      isolate({
        updateTextInput(session, "Splcing_in", value="DGAT1")
      })
    } else {NULL}
  })
  observe({
    if (input$SExam3 >0) {
      isolate({
        updateTextInput(session, "Splcing_in", value="ENSBTAG00000017704")
      })
    } else {NULL}
  })
}

