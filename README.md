# pdfnamer 

I am an academic. I read a lot. Most of what I read these days comes in the form of a pdf file. I just can't manage to keep my pdfs sorted or appropriately named. Where is that article from last week? I can't find it. I will download it again.

Yes, there are many existing tools for keeping pdfs organised and in line with one's method of keeping track of bibliographical details. But I don't control how these work. And I don't like how many of these work. There must be a better way. 

The aim of `pdfnamer` is to automate the process of renaming pdfs as much as
possible. 

In the first instance `pdfnamer` will attempt to find the title and author of
a pdf file by searching the text of the pdf file against a bibliography file (a
bib file) or against a list of such files. If it is pretty confident about the
title and author of a pdf it will go ahead an rename the file using the bib
identifier field and the title. (You can ask `pdfnamer` to give you an opportunity to do a little curating before it does the renaming).    

If this fails, `pdfnamer` will ask for some help. It will display some text from the pdf file and ask you to input the title and author. As you do, it will run a search on the entries in the bib file for you. If it finds what you need, you can select it, and the pdf will be renamed. 

If all else fails, `pdfnamer` will just display some text from the pdf and ask you to manually enter a name for the pdf file. 






