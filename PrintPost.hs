{-# LANGUAGE OverloadedStrings #-}
module PrintPost where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import Data.Attoparsec.ByteString.Char8

import System.IO
import System.Cmd
import System.Directory
import System.FilePath

import Types

import Parsers

generatePost :: FilePath -> IO()
generatePost path =
  do
    let workingDir = takeDirectory path
    print workingDir
    test <- doesDirectoryExist $ workingDir ++ "/images"
    if test
      then do return ()
      else do createDirectory $ workingDir++"/images" -- we create the directory where to put images 
    source <- openFile path ReadMode
    target <- openFile (path++".html") WriteMode 
    text <- BS.hGetContents source
    case parseOnly parsePost text of
      Right post -> do
        printHtmlPost target workingDir post
      Left error -> do
        print "Problems happen"
        print error
    hClose source
    hClose target

printHtmlPost :: Handle -> FilePath -> Post -> IO() -- Print post in html file handle
printHtmlPost file workingDir post =
  do
    BS.hPutStr file "<html>\n"
    BS.hPutStr file "<head>\n"
    BS.hPutStr file "<title>"
    BS.hPutStr file $ title post
    BS.hPutStr file "</title>\n"
    BS.hPutStr file "<link rel=\"stylesheet\" type=\"text/css\" href=\"../default.css\" />\n"
    BS.hPutStr file "</head>\n"
    BS.hPutStr file "<body>\n"
    BS.hPutStr file "<h3>"
    BS.hPutStr file $ title post
    BS.hPutStr file "</h3>\n\n"
    printHtmlItems file workingDir $ items post
    BS.hPutStr file "</body>\n"
    BS.hPutStr file "</html>\n"
    
printHtmlItems :: Handle -> FilePath -> [Item] -> IO()
printHtmlItems file workingDir list =
  do
    mapM_ (printHtmlItem file workingDir) list

printHtmlItem :: Handle -> FilePath -> Item -> IO()
printHtmlItem file workingDir (Par paragraph) =
  do
    BS.hPutStr file "<p>"
    mapM (printHtmlElement file workingDir) $ paragraph
    BS.hPutStr file "</p>\n"
printHtmlItem file workingDir (Other) =
  do
    BS.hPutStr file "<!--This is still to define -->\n"
    
printHtmlElement :: Handle -> FilePath -> Element -> IO()
printHtmlElement file workingDir (PlainText string) =
  do
    BS.hPutStr file string
printHtmlElement file workingDir (Formula formula) =
  do
    idFormula <- createLatexImage workingDir formula
    hPutStr file $ "<img src=\""++"./images/"
    hPutStr file idFormula
    BS.hPutStr file ".png"
    BS.hPutStr file "\" class=\"formula\" />"

genID :: FilePath -> IO(String)
genID workingDir =
  do
    list <- getDirectoryContents (workingDir++"/images")
    return $ show $ length list

createLatexImage :: FilePath -> C.ByteString -> IO (String) -- Attempt to creata a png from a formula
createLatexImage workingDir formula  =
  do
    id_image <- genID workingDir
    log <- openFile (workingDir++"/postLogs.log") AppendMode
    latexFile <- openFile "/tmp/temp.latex" WriteMode
    BS.hPutStr latexFile header
    BS.hPutStr latexFile formula
    BS.hPutStr latexFile closure
    hClose latexFile
    system $ "latex -halt-on-error -output-directory=/tmp /tmp/temp.latex"
    system $ "dvipng -bg transparent -T tight --follow -o /tmp/temp.png /tmp/temp.dvi"
    test <- doesFileExist "/tmp/temp.png"
    if test -- If image was created we copy in ./images directory, otherwise we log an error
      then
      do 
        system $ "cp /tmp/temp.png "++workingDir++"/images/"++id_image++".png"
        hPutStrLn log $ "Formula "++(C.unpack formula)++" correctedly created."
      else
      do
        hPutStrLn log $ "An error occured why trying to create the image from the formula "++(C.unpack formula)++"."
    hClose log -- Once we finish we close the log file
    return id_image

header :: BS.ByteString
header = BS.concat $
         ["\\documentclass{article}\n",
          "\\usepackage[utf8]{inputenc}\n",
          "\\usepackage{amsmath}\n",
          "\\usepackage{amssymb}\n",
          "\\usepackage[all]{xy}\n",
          "\\DeclareMathOperator{\\dom}{dom}\n",
          "\\DeclareMathOperator{\\cod}{cod}\n",
          "\\DeclareMathOperator{\\im}{Im}\n",
          "\\DeclareMathOperator{\\id}{id}\n",  
          "\\newcommand{\\N}{\\mathbf N}\n",
          "\\newcommand{\\Z}{\\mathbf Z}\n",
          "\\newcommand{\\Q}{\\mathbf Q}\n",
          "\\newcommand{\\R}{\\mathbf R}\n",
          "\\newcommand{\\End}{\\textrm{End}}\n",
          "\\newcommand{\\Set}{\\mathbf{Set}}\n",
          "\\newcommand{\\Cat}{\\mathbf{Cat}}\n",
          "\\newcommand{\\Grp}{\\mathbf{Grp}}\n",
          "\\newcommand{\\Top}{\\mathbf{Top}}\n",
          "\\begin{document}\n",
          "\\thispagestyle{empty}\n",
          "\\begin{align*}"]
         
closure :: BS.ByteString
closure = "\n\\end{align*}\n\\end{document}\n"

  
