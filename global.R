library(shiny)  
library(tidyr)
library(DT)
library(reshape2)
library(ggplot2)
library(data.table)
library(shinydashboard)
library(shinythemes)
library(ggtranscript)
#source("script/homepage.R")
expr<-fread("data/All.gene.fpkm2.txt.gz",sep = "\t",header = T)
anno1=read.table("data/All.tissue.annote.txt",comment.char = "",sep="\t",check.names = FALSE,header=T)
myorder=unique(anno1$Tissue)
mycolor1=unique(anno1$Color)
expr$Stage <- factor(expr$Stage, levels = c("Newborn","Young","Adult"))
expr$Tissue <- factor(expr$Tissue, levels = myorder)
klncRNA_gtf<-fread("data/novel_lncRNA1.gtf.gz",header=F)
head(klncRNA_gtf)
colnames(klncRNA_gtf)<-c("Chrom","type","start","end","strand","Transcript_id","Gene_id","transcript_biotype")
lncRNA_gene.expr<-fread("data/lncRNA.gene.expression.fpkm.txt.gz",header=T)
lncRNA_trans.expr<-fread("data/lncRNA.transcript.expression.fpkm.txt.gz",header=T)

editing_ann<-fread("data/All.AG.editing.VEP.useful7.txt.gz",header=F)
colnames(editing_ann)<-c("Chrom","Position","Region","Gene_name","Gene_ID","Transcript_ID","Gene_Category","Repeats|Category|Repeat_region","Cluster region","N of editing in cluster", "Stages","Tissues","N of Tissue")
RI<-fread("data/fromGTF.RI.txt",header=T)
MXE<-fread("data/fromGTF.MXE.txt",header=T)
SE<-fread("data/fromGTF.SE.txt",header=T)
A5SS<-fread("data/fromGTF.A5SS.txt",header=T)
A3SS<-fread("data/fromGTF.A3SS.txt",header=T)
theme_ydz1 <- theme(
  plot.title = element_text(size = rel(1.3), vjust = 2, hjust = 0.5, lineheight = 0.8),
  
  axis.title.x = element_text(face="bold", size=19),
  axis.title.y = element_text(face="bold", size=16, angle=90),
  axis.text = element_text(size = rel(1.1)),
  axis.text.x = element_text(hjust = 1, vjust = 1, angle=45,size=16),
  axis.text.y = element_text(vjust = 0.5, hjust = 1, size=16),
  axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = 'black'),
  #legend.position="none",
  strip.text=element_text(size = rel(1.3)),
  panel.background= element_blank(), 
  #aspect.ratio=1,
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  panel.grid.major=element_line(colour = NA),
  complete = T)
theme_ydz2 <- theme(
  plot.title = element_text(size = rel(1.3), vjust = 2, hjust = 0.5, lineheight = 0.8),
  
  axis.title.x = element_text(face="bold", size=19),
  axis.title.y = element_text(face="bold", size=16, angle=90),
  axis.text = element_text(size = rel(1.1)),
  axis.text.x = element_text(hjust = 0.5, vjust = 0, angle=0,size=16),
  axis.text.y = element_text(vjust = 0.5, hjust = 1, size=16),
  axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = 'black'),
  #legend.position="none",
  strip.text=element_text(size = rel(1.3)),
  panel.background= element_blank(), 
  #aspect.ratio=1,
  panel.border = element_rect(colour = "black", fill=NA, size=1),
  panel.grid.major=element_line(colour = NA),
  complete = T)

rowCallback <- c(
  "function(row, data){",
  "  for(var i=0; i<data.length; i++){",
  "    if(data[i] === null){",
  "      $('td:eq('+i+')', row).html('NA')",
  "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
  "    }",
  "  }",
  "}"  
)
anaReg1 <- function(x=NULL) {
  x <- gsub("\\s+", "", x)
  if (grepl("chr", x)) {
    myChr <- gsub(":.+", "", x) 
    myChr<-substring(myChr,4)
    myPosl <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", x),"-")[[1]]))
  } else {
    myChr <- gsub(":.+", "", x) 
    myPosl <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", x),"-")[[1]]))
  }
  
  chr.size <- c(158534110,
                136231102,
                121005158,
                120000601,
                120089316,
                117806340,
                110682743,
                113319770,
                105454467,
                103308737,
                106982474,
                87216183,
                83472345,
                82403003,
                85007780,
                81013979,
                73167244,
                65820629,
                63449741,
                71974595,
                69862954,
                60773035,
                52498615,
                62317253,
                42350435,
                51992305,
                45612108,
                45940150,
                51098607,
                139009144,
                16338
  )
  names(chr.size) <-  c(1:29,"X","MT")
  
  myPosl[1] <- max(1, myPosl[1])
  myPosl[2] <- min(myPosl[2], chr.size[myChr])
  
  return(list(chr=myChr, start=myPosl[1], end=myPosl[2]))
}

anaReg <- function(x=NULL) {
  x <- gsub("\\s+", "", x)
  if (grepl("chr", x)) {
    myChr <- gsub(":.+", "", x)
    myPos <- as.numeric(gsub("\\s","", strsplit(gsub(".+:", "", x),"-")[[1]]))
  } else {
    myChr <- paste0("chr", substr(x, 7,8))
    myPos <- c(gene.info$start[gene.info$id==x], gene.info$end[gene.info$id==x])
  }
  
  chr.size <- c(158534110,
                136231102,
                121005158,
                120000601,
                120089316,
                117806340,
                110682743,
                113319770,
                105454467,
                103308737,
                106982474,
                87216183,
                83472345,
                82403003,
                85007780,
                81013979,
                73167244,
                65820629,
                63449741,
                71974595,
                69862954,
                60773035,
                52498615,
                62317253,
                42350435,
                51992305,
                45612108,
                45940150,
                51098607,
                139009144,
                16338
  )
  names(chr.size) <- paste0("chr", c(1:29,"X","MT"))
  
  myPos[1] <- max(1, myPos[1])
  myPos[2] <- min(myPos[2], chr.size[myChr])
  
  return(list(chr=myChr, start=myPos[1], end=myPos[2]))
}



validReg <- function(myPos = list(NULL)) {
  if (myPos$chr %in% paste0("chr", c(1:29,"X","MT")) && !is.na(myPos$start) && 
      !is.na(myPos$end) && myPos$start>=1 && myPos$end>myPos$start && (myPos$end-myPos$start)<=2e6 ) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
tweaks <- list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 210px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count:5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "))))

sectionBox <- function(..., title) {
  fluidRow(
    box(...,
      width = 12,
      title = title,
      solidHeader = TRUE, status = "warning", collapsible = TRUE
    )
  )
}

textBox <- function(...) {
  box(..., status = "success")
}

messageBox <- function(...) {
  box(..., status = "danger", background = "green")
}

module_Box <- function(..., title, imgSrc, text) {
  box(
    ...,
    title = span(title, style = "font-size:18px"),
    solidHeader = TRUE, status = "primary",
    fluidRow(
      column(
        width = 4,
        div(style = "margin-left:5px;margin-right:0px;margin-top:0px;",shiny::img(src = imgSrc, width = "100%"))
      ),
      column(
        width = 8,
        p(text),style = "font-size:20px;color:black",
      )
    )
  )
}

