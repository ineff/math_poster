Degli script per generare dei post html a partire da un formato simil
markdown capace di inserire nel testo delle formule latex.

----------------------------------------------------------------------

Per il momento non ho fatto un makefile.
Per compilare basta dare 

ghc -c Types.hs Parsers.hs PrintPost.hs
ghc Main.hs -o Poster

A questo punto è possibile usare 

Poster source

per generare la pagina html (le immagini e quant'altro serva) 
nella directory contenente source.

