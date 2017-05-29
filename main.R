rm(list = ls())
setwd("E:/gitperso/gender-french-nouns/")
#setwd("E:/to/your/directory/")

#install.packages("ggplot2")
#install.packages("reshape")
#install.packages("reshape2")
library(ggplot2)
library(reshape) #for rename
library(reshape2) #for melt

#####################################
# Retrieve and preprocess the words #
#####################################
## List of words and definitions
# source : http://www.webcontentspinning.com/langage/
words=readLines("data/FRAwords.txt")
def=readLines("data/FRAdefinition.txt")
words=as.vector(words)
def=as.vector(def)

## Take feminine and masculine words in the dictionary
# with grep/regex, "\\." means ".", and "." means any letter.
idxFeminin=grep("n\\. f\\.", def)
idxMasculin=grep("n\\. m\\.", def)

## Delete intersection between feminine and masculine words
common=intersect(idxFeminin, idxMasculin)
# Note: words[common] contains mostly job words : un acteur / une actrice.
# Contains also: amour, delice (no orgue here).

idxMasculin=idxMasculin[-common]
idxFeminin=idxFeminin[-common]

## Delete diacritics and other caracters 
# See http://fr.wikipedia.org/wiki/Diacritiques_utilis%C3%A9s_en_fran%C3%A7ais
words=tolower(words)
words=gsub("à","a",words)
words=gsub("â","a",words)
words=gsub("ä","a",words)
words=gsub("ç","c",words)
words=gsub("é","e",words)
words=gsub("è","e",words)
words=gsub("ê","e",words)
words=gsub("ë","e",words)
words=gsub("É","e",words)
words=gsub("î","i",words)
words=gsub("ï","i",words)
words=gsub("ô","o",words)
words=gsub("ö","o",words)
words=gsub("ù","u",words)
words=gsub("û","u",words)
words=gsub("ü","u",words)
words=gsub("æ","ae",words)
words=gsub("œ","oe",words)
words=gsub("!","",words)
words=gsub("-"," ",words)
words=gsub("'"," ",words)

#masculine nouns: 10246 words / feminine nouns: 6901 words.
feminin=words[idxFeminin]
masculin=words[idxMasculin]

## Delete parentheses
whichContains = function(letter = "\\(", vect) {
  which(sapply(vect, function(x) {regexpr(letter, x)[[1]]})>-1)
}

wordsWithParenthesisFem = whichContains("\\(", feminin)
feminin[which(feminin=="action de grace(s)")]="action de grace"
feminin[which(feminin=="charge  (de projet")]="charge"
feminin[which(feminin=="javel (eau de)")]="javel"
feminin[which(feminin=="lasagne(s)")]="lasagne"
feminin[which(feminin=="plupart (la)")]="plupart"
feminin[which(feminin=="tartuf(f)erie")]="tartuferie"
wordsWithParenthesisFem = whichContains("\\(", feminin)
length(wordsWithParenthesisFem)==0 #OK no more word
wordsWithParenthesisFem = whichContains("\\)", feminin)
length(wordsWithParenthesisFem)==0 #OK no more word

wordsWithParenthesisMas= whichContains("\\(", masculin)
masculin[which(masculin=="antimissile(s)")]="antimissile"
masculin[which(masculin=="antimite(s)")]="antimite"
masculin[which(masculin=="antiparasite(s)")]="antiparasite"
masculin[which(masculin=="arrache racine(s)")]="arrache racine"
masculin[which(masculin=="brule parfum(s)")]="brule parfum"
masculin[which(masculin=="charge  (de projet")]="charge"
masculin[which(masculin=="chauffe assiette(s)")]="chauffe assiette"
masculin[which(masculin=="compte( )cheques")]="compte cheques"
masculin[which(masculin=="coupe cigare(s)")]="coupe cigare"
masculin[which(masculin=="court( )metrage")]="court metrage"
masculin[which(masculin=="couvre pied(s)")]="couvre pied"
masculin[which(masculin=="exercice  (financier")]="exercice"
masculin[which(masculin=="gobe mouche(s)")]="gobe mouche"
masculin[which(masculin=="long( )metrage")]="long metrage"
masculin[which(masculin=="non( )belligerant")]="non belligerant"
masculin[which(masculin=="non( )combattant")]="non combattant"
masculin[which(masculin=="non( )conformiste")]="non conformiste"
masculin[which(masculin=="non( )figuratif")]="non figuratif"
masculin[which(masculin=="petit( )four")]="petit four"
masculin[which(masculin=="plein( )emploi")]="plein emploi"
masculin[which(masculin=="quatre vingt(s)")]="quatre vingt"
masculin[which(masculin=="repose pied(s)")]="repose pied"
masculin[which(masculin=="taille crayon(s)")]="taille crayon"
masculin[which(masculin=="tartuf(f)e")]="tartufe"
masculin[which(masculin=="trimbal(l)age")]="trimbalage"
wordsWithParenthesisMas = whichContains("\\(", masculin)
length(wordsWithParenthesisMas)==0 #OK no more word
wordsWithParenthesisMas = whichContains("\\)", masculin)
length(wordsWithParenthesisMas)==0 #OK no more word

####################
# Gender as letter #
####################
# Those function allow to know the number of masculine and feminine words
# as a function of the letter located at position 'letterPos'.
#
# If fromLast == TRUE, the position is defined from the last letters of the
# word.

# Number of masculine and feminine words as a function of the letter located
# at 'letterPos'.
genderAsLetter=function(letterPos, feminin, masculin) {
 caract=c("", " ", letters)
 rleFem=rle(sort(substr(feminin, letterPos, letterPos)))
 rleMas=rle(sort(substr(masculin, letterPos, letterPos)))

 femininLetter=rep(0,length(caract))
 #caract %in% rleFem[[2]]
 femininLetter[which(caract %in% rleFem[[2]])]=rleFem[[1]]

 masculinLetter=rep(0,length(caract))
 #caract %in% rleMas[[2]]
 masculinLetter[which(caract %in% rleMas[[2]])]=rleMas[[1]]

 return(list(c(caract), femininLetter, masculinLetter))
}

#from the last letters
revString = function(a) {
 paste(rev(substring(a, 1:nchar(a), 1:nchar(a))), collapse="") 
}
#a = "this is a string"
#revString(a)

genderAsLetterFromLast=function(letterPos, feminin, masculin) {
 revFeminin = sapply(feminin, revString)
 revMasculin = sapply(masculin, revString)
 genderAsLetter(letterPos, revFeminin, revMasculin)
}

genderAsLetterAll = function(letterPos, feminin, masculin, fromLast=FALSE) {
 if(fromLast) {
  data = genderAsLetterFromLast(letterPos, feminin, masculin)
 } else {
  data = genderAsLetter(letterPos, feminin, masculin)
 }
 return(data)
}

#######################
# Plotting parameters #
#######################
width = 560
height = 560

dpi = 130
widthHD = 1280/dpi
heightHD = 1024/dpi
# See http://stackoverflow.com/questions/16400028/

dir.create("outputs", showWarnings = FALSE)

#######################################
# Plot and save data helper functions #
#######################################
# Bar plot function indicating masculine and feminine words as a function
# of the letter located at a specified position.
#
# Defined from 
# http://learnr.wordpress.com/2009/03/17/ggplot2-barplots/
# See also: http://docs.ggplot2.org/0.9.3.1/geom_bar.html , and
# http://docs.ggplot2.org/0.9.2.1/theme.html
plotData = function(data, censorshipWords=0, title=NULL) {
 indexLetters=3:28
 numberOfNounsLetter = (data[[2]]+data[[3]])[indexLetters]
 
 #feminine censored sum
 femOut=sum(data[[2]][indexLetters[numberOfNounsLetter <= censorshipWords]])
 
 #masculine censored sum
 masOut=sum(data[[3]][indexLetters[numberOfNounsLetter <= censorshipWords]])
 
 indexLetters = indexLetters[numberOfNounsLetter > censorshipWords]
 caract=c(data[[1]])

 if(femOut>0 || masOut>0) {
  data[[2]][29]=femOut
  data[[3]][29]=masOut
  caract=c(data[[1]], "others")
  indexLetters=c(indexLetters, 29)
 }

 df <- structure(c(data[[2]][indexLetters],data[[3]][indexLetters]),
	.Dim = c(length(indexLetters), 2), 
	.Dimnames = list(c(caract[indexLetters]), c("feminine", "masculine")))
 df.m <- melt(df)
 df.m <- rename(df.m, c(X1 = "Letter", X2 = "Gender"))
 df.m$Letter <- factor(df.m$Letter, levels = c(letters, "others"))

 a <- ggplot(df.m, aes(x = Letter, y = value, fill = Gender)) +
   ggtitle(title) +
   theme(plot.title = element_text(hjust = 0.5)) +
   xlab("") + ylab("") + labs(fill = NULL)
 b <- a + geom_bar(stat = "identity", position = "dodge")
 b <- b + scale_fill_brewer(palette = "Set1")
 my_theme = theme_update(
   axis.text.x = element_text(angle = 0, hjust = 0.2, colour="#7b7d7b"),
   panel.grid.major = element_line(colour = "grey90"),
   panel.grid.minor = element_blank(), 
   panel.background = element_blank(),
   plot.title = element_text(lineheight=.8, face="bold", colour="#7b7d7b"),
   axis.ticks = element_blank(), 
   legend.position = "right")
 b
}

# Function to plot and export data to a specified location, with a normal
# output and a HD output.
save_png = function(data, outfile, censorshipWords, title, 
                    width, height, widthHD, heightHD, dpi) {
  png(outfile, width, height)
  print(plotData(data, censorshipWords, title))
  dev.off()
  
  outfileHD = gsub(".png", "_hi.png", outfile)
  plotData(data, censorshipWords, title)
  ggsave(filename = outfileHD, width=widthHD, height=heightHD, dpi=dpi)
}

###############
# Loop export #
###############
for(letterPos in 1:4) {
  print(paste("Current letter:", letterPos))
  for(fromLast in c(FALSE, TRUE)) {
    
    # Title management, a little tedious.
    titleBegin = paste(letterPos, "th", sep = "")
    if(letterPos == 1 & !fromLast) {
      titleBegin = "first"
    } else if(letterPos == 1 & fromLast) {
      titleBegin = ""
    } else if(letterPos == 2) {
      titleBegin = "second"
    } else if(letterPos == 3) {
      titleBegin = "third"
    }
    titleLast = ifelse(fromLast, "last", "")
    if(fromLast & letterPos > 1) {
      titleLast = paste(" ", titleLast, sep = "")
    }
    outfileLast = ifelse(fromLast, "_last", "")
    
    # Create data
    data=genderAsLetterAll(letterPos, feminin, masculin, fromLast)
    
    # Plotting
    for(censorshipWords in c(0,2000)) {
      title=paste("Number of French nouns as a function of gender and ",
                  titleBegin, titleLast, " letter", sep = "")
      
      outfile=paste("outputs/gender_letter", outfileLast, letterPos,
                    "_cens", censorshipWords,".png",sep="")
      save_png(data, outfile, censorshipWords, title, 
               width, height, widthHD, heightHD, dpi)
    }
  }
}

############
# Pie plot #
############
#Pie about 40/60
percentMasculin=length(idxMasculin)/(length(idxFeminin)+length(idxMasculin))
percentFeminin=1-percentMasculin
png("outputs/pieGlobal.png", width, height)
par(mar=c(0, 0, 4, 0)) 
slices <- c(percentFeminin, percentMasculin)
lbls <- c("feminine", "masculine")
colors = c("#d62c21", "#1875bd")
pie(slices, labels = lbls, main=NULL, col=colors)
mtext("French nouns gender", line=-0.5, cex=1.2, font=2) #font=2 for bold
dev.off()