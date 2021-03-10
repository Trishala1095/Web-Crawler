library(RCurl)

library(XML)

library(rvest)

GetURL <- function(year){
if(year >= 2005 && year <= format(Sys.Date(), "%Y")){
input1 = "https://journals.sagepub.com/action/doSearch?field1=AllField&text1=+&field2=AllField&text2=&publication%5B%5D=cixa&Ppub=&Ppub=&AfterYear="
AfterYear = year
input2 = "&BeforeYear=&earlycite=on&access=&pageSize=20&startPage="
URL = paste(input1, AfterYear, input2, sep = "")
Page <- PageNumber(URL)
if(Page == 10)
{
Page2 <- 0
while(Page < Page2 | Page != Page2){
Page <- Page2
URL3 <- paste(URL, Page-1, sep = "")
Page2 <- PageNumber(URL3)	
}
}
print(Page)
R_Data <- data.frame()
for(i in 0:ifelse((Page-1) > 0, (Page-1), 0)){ #0:Page-1
URL2 <- AllArticleURL(paste(URL, i, sep = ""))
for(j in 1:(length(URL2))){
doc <- read_html(URL2[j])
print(URL2[j])
#R <- data.frame("Title" = Title(doc), "Authors" = Authors(doc))
#if(!Authors(doc) == ""){
R <- data.frame("Title" = Title(doc), "Authors" = Authors(doc), "Author Affiliations" = AA(doc), "Correspondence Author" = CoAuthor_Email(doc)[1], "Correspondence Author's Email" = CoAuthor_Email(doc)[2], "Publish Date" = PublishDate(doc), "Abstract" = Abstract(doc), "Keywords" = Keywords(doc), "Full Paper" = Full_Paper(doc), stringsAsFactors = FALSE)
R_Data <- rbind(R_Data, R)
#}
} 
} 
suppressWarnings(write.csv(R_Data, "Group4.csv", row.names = FALSE, sep = "\t"))
suppressWarnings(write.csv(R_Data, "Group4.txt", row.names = FALSE, sep = "\t"))
return(R_Data)
} else {
print("The Year you provide is out of range, this journal only contain articles from 2005 to present")
}
}

AllArticleURL <- function(URL){
doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'ol')
Sort2 <- Sort1[grepl("<ol class=\"search-results\"", Sort1)]
Sort3 <- html_nodes(Sort2, 'li')	
ArticleURL <- regexpr("value=\"(.)*\"><article", Sort3)
URL1 <- substr(Sort3, ArticleURL, ArticleURL + attr(ArticleURL, "match.length"))
URL2 <- gsub("value=\"|\"><article ", "", URL1)
URL3 <- "https://journals.sagepub.com/doi/full/"
URL4 <- paste(URL3, URL2, sep = "")
return(URL4)
}

PageNumber <- function(URL){
doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'ul')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "linkList centered")] 
Sort3 <- html_children(Sort2)
P <- gsub("\n| Prev|Next| …", "", html_text(Sort3))
P <- as.numeric(P)
P[is.na(P)] <- 0
return(ifelse(length(P) == 0, 0, max(P)))
}


#-------------

#Title:
Title <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'h1')
Title <- gsub("<h1>\\n|\\n</h1>", "", Sort1)
return(Title)
}


#Authors:
Authors <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'span')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "contribDegrees no-aff")]
if(!length(Sort2) == 0){
Author <- gsub("(.)*<a (.)*\"> |</a>(.)*", "", Sort2)
Author <- paste(Author, collapse = ", ")
return(Author)
} else{
return("No Author")
}
}


#Author Affiliations:
AA <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'div')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "artice-info-affiliation")] 
if(!length(Sort2) == 0){
Affiliations <- gsub("<i>|</i>|(.)*artice-info-affiliation\">|(.)*<sup>|</sup>|</div>(.)*", " ", Sort2)
Affiliations <- paste(Affiliations, collapse = "\n") 
return(Affiliations)
} else{
return("No Author Affiliations")
}
}







#Correspondence Author + E-mail(email protected issue):
CoAuthor_Email <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'div')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "artice-notes")] 
if(!length(Sort2) == 0){
Correspondence <- gsub("<div (.)*<br>|, (.)*", "", Sort2)
Sort3 <- html_text(Sort2)
email <- gsub("(.)*cfemail=|>(.)*|\"", "", Sort2) 
o = ""
a = strtoi(substr(email, 1, 2), 16)
for(i in seq(3, nchar(email), by = 2)){
o = paste(o, rawToChar(as.raw(bitwXor(strtoi(substr(email, i, i+1), 16),a))), sep = "")
}
return(c(Correspondence, o))
} else{
return(c("No Correspondence Author", "No Correspondence E-Mail"))
}
}



#Publish Date:
PublishDate <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'span')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "publicationContentEpubDate dates")] 
Publish <- gsub("(.)*</b> |\\n</span>", "", Sort2)
}


#Keywords:
Keywords <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'div')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "hlFld-KeywordText")] 
if(!length(Sort2) == 0){
Keywords <- gsub("Keywords ", "",html_text(Sort2))
} else{
return("No Key words")
}
}



#Abstract:
Abstract <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'div')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "abstractSection abstractInFull")] 
if(!length(Sort2) == 0){
Sort3 <- html_children(Sort2)
if(html_name(Sort3[1]) != "p"){
Sort4 <- html_children(Sort3)
} else{
Sort4 <- Sort3
}
Sort5 = ""
for(i in 1:(length(Sort4))){
if(html_name(Sort4[i]) == "p"){
Sort5[i] <- html_text(Sort4[i])
} else{
Sort5[i] <- gsub("<a name=(.)*</a>|(.)*sectionHeading\">|</h2>(.)*|<p>|</p>", "", Sort4[i])
}
}
Abstract <- paste(Sort5, collapse = "\n")
return(Abstract)
} else{
return("No Abstract")
}
}




#Full Paper:
Full_Paper <- function(doc){
#doc <- read_html(URL)
Sort1 <- html_nodes(doc, 'div')
Sort2 <- Sort1[which(html_attr(Sort1, "class") == "hlFld-Fulltext")] 
Sort3 <- html_children(Sort2)
if(!length(html_children(Sort3)) == 0){
Sort4 = ""
for(i in 1:(length(Sort3))){
Sort4 <- html_children(Sort3)
}
Sort5 = ""
for(i in 1:(length(Sort4))){
if(html_name(Sort4[i]) == "p"){
Sort5[i] <- html_text(Sort4[i])
} else if(html_name(Sort4[i]) == "div" | html_name(Sort4[i]) == "span"| html_name(Sort4[i]) == "tr"){
Sort6 = ""
if(length(html_children(Sort4[i])) != 0)
{
Sort6 <- html_children(Sort4[i])
b <- !is.na(html_attr(Sort6, "class"))
if(all(b) & any(html_attr(Sort6, "class") == "NLM_sec NLM_sec_level_3")){
Sort6 <- html_children(Sort6)
}
Sort7 = ""
for(j in 1:(length(Sort6))){
a <- !is.na(html_attr(Sort6[j], "class"))
if(html_name(Sort6[j]) == "table" | html_name(Sort6[j]) == "script" | html_name(Sort6[j]) == "center" | (a & html_attr(Sort6[j], "class") == "figure") | (a & html_attr(Sort6[j], "class") == "tableWrapper visuallyhidden")){
Sort7[j] <- "" 
} else {
Sort7[j] <- html_text(Sort6[j])
}
}
Sort5[i] <- paste(Sort7, collapse = " \n")
}
}
}
Sort5[max(which(is.na(Sort5)))] <- "References"
Sort5[is.na(Sort5)] <- ""
Sort5 <- gsub("\\n{2,}", " ", Sort5)
Full <- paste(Sort5, collapse = "\n")
return(Full)
} else{
return(paste(html_text(Sort3), collapse = "\n"))
}
}