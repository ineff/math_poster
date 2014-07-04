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
-- Take a path of a file and try to generate a post from that file
generatePost path =
  do
    let workingDir = takeDirectory path
    -- Create the directory where to store images and formulas for the post 
    test <- doesDirectoryExist $ workingDir ++ "/images/formulas" 
    if test -- if the directory exists
      then 
      do
        system $ "rm -rf "++workingDir++"/images/formulas/*.png"-- remove the formulas created by previous compilations
        return ()
      else
      do -- otherwise create the directories needed
        system $ "mkdir -p "++workingDir++"/images/formulas" -- we create the directory where to put images
        return ()
    source <- openFile path ReadMode -- take the file containing the source of the post
    target <- openFile (path++".html") WriteMode  -- and the html to be written
    css <- openFile (workingDir++"/default.css") WriteMode -- create the default css 
    text <- BS.hGetContents source 
    case parseOnly parsePost text of -- try to parse the source post 
      Right post -> do -- if succeed continue to compile the html file
        printHtmlPost target workingDir post
      Left error -> do -- otherwise fail
        print "Problems happen" 
        print error
    printCSS css -- write the css
    hClose source
    hClose target
    hClose css

printCSS :: Handle -> IO()
printCSS css =
  do
    C.hPutStrLn css $ BS.concat[
      "background {\n",
      "color: #8a95b2;\n",
      "}\n",
      "\n",
      "p {\n",
      "color: black;\n",
      "}\n",
      "\n",
      ".formula {\n",
      "padding: 0 0.5em 0 0.5em;\n",
      "}\n"
      ]

printHtmlPost :: Handle -> FilePath -> Post -> IO() -- Print post in html file handle
printHtmlPost file workingDir post =
  do
    BS.hPutStr file "<!doctype html >\n"
    BS.hPutStr file "<html>\n"
    BS.hPutStr file "<head>\n"
    BS.hPutStr file "<title>"
    BS.hPutStr file $ title post
    BS.hPutStr file "</title>\n"
    BS.hPutStr file "<meta charset=\"UTF-8\" />\n"
    BS.hPutStr file "<link rel=\"stylesheet\" type=\"text/css\" href=\"./default.css\" />\n"
    BS.hPutStr file "</head>\n"
    BS.hPutStr file "<body>\n"
    BS.hPutStr file "<h3>"
    BS.hPutStr file $ title post
    BS.hPutStr file "</h3>\n\n"
    printHtmlItems file workingDir $ items post
    BS.hPutStr file "</body>\n"
    BS.hPutStr file "</html>\n"

-- Auxiliary functions to print the various elements parsed by the post    
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
printHtmlElement file _ (PlainText string) =
  do
    BS.hPutStr file string
printHtmlElement file workingDir (Formula formula) =
  do
    idFormula <- createLatexImage workingDir formula
    hPutStr file $ "<img src=\""++"./images/formulas/"
    hPutStr file idFormula
    BS.hPutStr file ".png\" "
    BS.hPutStr file "alt=\"$"
    BS.hPutStr file formula
    BS.hPutStr file "$\" "
    BS.hPutStr file "class=\"formula\" /> "
printHtmlElement file _ (Italic string) =
  do
    BS.hPutStr file "<em>"
    BS.hPutStr file string
    BS.hPutStr file "</em> "
printHtmlElement file _ (Bold string) =
  do
    BS.hPutStr file "<strong>"
    BS.hPutStr file string
    BS.hPutStr file "</strong> "

genID :: FilePath -> IO(String)
-- auxliary function to generate a number for the formula/image
genID workingDir =
  do
    list <- getDirectoryContents (workingDir++"/images/formulas")
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
    if test -- If image was created we copy in ./images/formulas directory, otherwise we log an error
      then
      do 
        system $ "cp /tmp/temp.png "++workingDir++"/images/formulas/"++id_image++".png"
        hPutStrLn log $ "Formula "++(C.unpack formula)++" correctedly created."
      else
      do
        hPutStrLn log $ "An error occured why trying to create the image from the formula "++(C.unpack formula)++"."
    hClose log -- Once we finish we close the log file
    return id_image


-- Default piece of code to be used for compiling formulas
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






