module ReverseFuzzy where 

-- this is a proof of concept for building a brick 
-- application that helps the user perform a fuzzy
-- search on a bibliography file for a bibliographic 
-- entry. the application needs to do several things
-- for each pdf provided. (1) it needs to display the 
-- text of the pdf in one a window. call this the
-- "pdf preview" window. (2) it needs to provide a text
-- input window for allowing the user to input a
-- search string. (3) it needs to prove a "live update"
-- of the top 10 search results on the given input. 

import BibEntries

import Control.Concurrent.Async
import Lens.Micro.Mtl
import Lens.Micro ((^.),(.~)) -- brick needs these lens libraries
import Lens.Micro.TH 
import qualified Text.BibTeX.Entry as BE (T(..)) -- bib entries
import qualified Text.BibTeX.Parse as BP -- parse a bib file (using parsec)
import qualified Data.List as List (lookup, filter, intersperse)
import qualified Data.Maybe as M (fromMaybe)
import qualified Graphics.Vty as V
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
import Brick.Widgets.Border 
import Brick.Widgets.Center
import Brick.Widgets.Border.Style
import qualified Brick.Forms as F 
import qualified Data.Text as Text
import Brick.AttrMap
import Graphics.Vty --for colours
import Brick.Util -- for 'on'
import Text.Parsec
import Text.FuzzyFind 
import Data.List
import System.Directory 
import System.Process
import qualified Text.Fuzzy as Fuzzy 
import Data.Char
import qualified Data.Text as T
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.List.Split as DLS

-- the application also needs a list of bibliography entries 
-- it will work on the data type from the bibtext library 
-- however, it will be helpful to get a string consisting 
-- of the author date and title of the entry 
-- since the library doesn't provide a function for getting 
-- title, author, and date fields we write our own 

data PDFList = PDFList {_previous :: [(String,String)]
                       ,_current :: (String,String)
		       ,_next :: [(String,String)]
		       }

makeLenses ''PDFList

nextpdf :: PDFList -> PDFList 
nextpdf p = let newcurrent = if (_next p) == [] 
                             then _current p 
                             else head (_next p)
		newnext = if (_next p) == [] 
		          then []
			  else tail (_next p) 
		newprevious = reverse (_current p : reverse (_previous p)) in 
		PDFList {_previous = newprevious 
		        ,_current = newcurrent 
			,_next = newnext}

prevpdf :: PDFList -> PDFList 
prevpdf p = let newcurrent = if (_previous p) == [] 
                             then _current p 
                             else head (reverse (_previous p))
		newnext = _current p : _next p
		newprevious = if (_previous p) == [] 
		              then [] 
			      else tail (reverse (_previous p)) in
		PDFList {_previous = newprevious 
		        ,_current = newcurrent 
			,_next = newnext
			}


-- first something generic 

getfield :: String -> BE.T -> Maybe String
getfield s e = List.lookup s (BE.fields e) 

-- then we use it to get author date and title from an entry 

getadt :: BE.T -> String 
getadt e = M.fromMaybe "" (getfield "author" e) 
                               ++ " " ++ M.fromMaybe "" (getfield "date" e) 
	                       ++ " " ++ M.fromMaybe "" (getfield "title" e)		   

getnewpath :: BE.T -> String 
getnewpath e = let adt = getadt e 
                   nopunct = filter (not . isPunctuation) adt 
		   tolower = map toLower nopunct
		   nds = nodoublespace tolower in 
		   (spacetodash nds) ++ ".pdf"

spacetodash :: String -> String
spacetodash (x:xs) = if x == ' ' then '_' : (spacetodash xs) else x : (spacetodash xs) 
spacetodash [] = []

nodoublespace :: String -> String 
nodoublespace [] = []
nodoublespace (x:y:xs) = if (x == ' ' && y == ' ') then ' ' : (nodoublespace xs) else x : nodoublespace (y:xs) 
nodoublespace x = x 

-- for testing purposes we just hardwire the bib file in 
mybib :: String
mybib = "/Users/ryan/onedrive/bibliographies/library.bib"

mybibs :: [String]
mybibs = ["/Users/ryan/onedrive/bibliographies/library.bib"
         ,"/Users/ryan/onedrive/bibliographies/philpaper_bibliographies/political_authority.bib"
	 ,"/Users/ryan/onedrive/bibliographies/philpaper_bibliographies/justice.bib"]


-- we use the bibtex library to read in the file 
parsebibfiles :: [String] -> IO [BE.T]
parsebibfiles xs = fmap concat $ mapM parsebibfile xs 


parsebibfile :: String -> IO [BE.T]
parsebibfile fp = do 
    bibfile <- readFile fp
    case (runParser (BP.skippingLeadingSpace BP.file) () "" bibfile) of 
        Left _ -> return []
	Right ents -> return ents

-- we need a function for extracting the author title date string 
getadts :: [BE.T] -> [String]
getadts = map getadt

-- now we need do our fuzzy stuff 
-- basically we want a function that takes a string 
-- cuts that string into words, so [String] 
-- and we want it to return the top 10 hits of the search 

tophits :: String -> [BE.T] -> [BE.T] 
tophits s ss = let cutstring = words s in 
            tophits' cutstring ss 

tophits' :: [String] -> [BE.T] -> [BE.T]
tophits' s ss = take 5 $ map snd $ reverse $ sortOn (score . fst) (fuzzyFindOn (getadt) s ss)

fuzzyresults :: [String] -> [String] -> [Alignment]
fuzzyresults = fuzzyFind 

-- BRICK STUFF

-- this is my data type for resource names
-- we only have one resource to keep track of, the search input field
data Name = SearchInput 
   deriving (Eq,Ord,Show)

-- this is the formstate managed by the form
data SearchString = SearchState { _search :: Text.Text }
   deriving (Show)

-- this is the apps data type 
-- it carries data about the current search string 
-- it also carries a pdf list, a list of pdf filepaths and text 
-- it actually needs to carry two more things 
-- a list of best guesses based on the pdf text 
-- and the list of search results  
data AppState e = AppState {_mysearchstate :: F.Form SearchString e Name
                         ,_mybibstate :: [String] -- 
			 ,_mypdflist :: PDFList 
			 ,_mybestguess :: [BE.T]
			 ,_mysearchresults :: [BE.T] 
			 ,_mycurrentselection :: Maybe BE.T
			 }

-- I don't think that bibstate needs to be in here. A the moment 
-- it is just providing a list of strings for the search to search 
-- on. Since this list never changes, it doesn't need to be here
-- I will to remove all references to _mybibstate below
-- then it can be removed


makeLenses ''AppState

makeLenses ''SearchString

mkForm :: SearchString -> F.Form SearchString e Name 
mkForm = F.newForm [F.editTextField search SearchInput (Just 1)]

--brick app
app :: [BE.T] -> M.App (AppState e) e Name 
app es = M.App { M.appDraw = draw es
          , M.appChooseCursor = M.showFirstCursor
	  , M.appHandleEvent = event es
	  , M.appStartEvent = return () 
	  , M.appAttrMap = const myMap -- const theMap
	  }

-- here's main function 
--
brickmain :: IO ()
brickmain = do
   pdfs <- listpdfs mypdfs
   bbs <- filestoentries mybibs
   _ <- M.defaultMain (app bbs) (initialState bbs pdfs)
   return ()

-- initial state for the app
initialState :: [BE.T] -> [(String,String)] -> AppState e
initialState es xs = AppState {_mysearchstate = mkForm $ SearchState {_search = Text.pack " "}
                          ,_mybibstate = []
			  ,_mypdflist = PDFList {_previous = [], _current = if (xs == []) then ("","") else head xs, _next = if (xs == []) then [] else tail xs}
			  ,_mybestguess = rfuzzy es (snd (head xs))

			  ,_mysearchresults = [] 
			  ,_mycurrentselection = Nothing
			  }


resetState :: AppState e -> AppState e 
resetState st = AppState {_mysearchstate = resetformstate $ _mysearchstate st
                          ,_mybibstate = _mybibstate st 
			  ,_mypdflist = _mypdflist st 
			  ,_mybestguess = _mybestguess st 
			  ,_mysearchresults = _mysearchresults st 
			  ,_mycurrentselection = _mycurrentselection st 
			  }

resetformstate :: F.Form SearchString e Name -> F.Form SearchString e Name
resetformstate = F.updateFormState $ SearchState {_search = Text.pack " "}

-- my att map 
myMap :: AttrMap 
myMap = attrMap globalDefault []
   where globalDefault = black `on` blue

-- drawing function
draw :: [BE.T] -> AppState e -> [T.Widget Name]
draw es x = let s = _mysearchstate x 
                b = _mybibstate x 
		bg = _mybestguess x
		se = _mysearchresults x 
		cs = _mycurrentselection x
	        (t,tx) = (_current (_mypdflist x)) in 
     [joinBorders $ hBox [ vBox [hBorderWithLabel (str "Search"), searchbox s, hBorderWithLabel (str "Best Guess"), bestguess bg, hBorderWithLabel (str "Search Results"), resultbox bg se, hBorderWithLabel (str "Current Selection"), currentselectionbox cs] , vBorder, vBox [hBorderWithLabel (str "File Name"), currentpdfbox t, hBorderWithLabel (str "Preview"), previewbox tx ]] ]

bestguess bg = center (padAll 2 (renderresultsl 0 (map getadt bg)))

resultbox n se = case se of 
    [] -> center (padAll 2 (str "Enter a Search Query")) 
    xs -> center (padAll 2  (renderresultsl (length n) (map getadt xs))) 

previewbox tx = center (padAll 2 (str tx))

currentpdfbox t = padAll 1 (str "file: " <+> str t) 

searchbox s = (padAll 1 (str "search: " <+> (F.renderForm s)))  

currentselectionbox ms = case ms of 
    Nothing -> (padAll 1 (str "no selection")) 
    Just x -> (padAll 1 (str (getnewpath x))) 

instructionbox = padTopBottom 1 $ padLeftRight 5 $ hBox [nextdoc, previousdoc, runsearch, selectmetadatat, selectcontentsgeuss, selecttopmanual, quit]

nextdoc = str "next document = >"
previousdoc = str "next document = <"
runsearch = str "run search = Enter"
selectmetadatat = str "select metadata guess = CTRL M" 
selectcontentsgeuss = str "select contents guess = CTRL C"
selecttopmanual = str "select from manual = 1,2,..9" 
quit = str "quit = Escape"

-- this needs improving
renderresults :: [String] -> T.Widget Name
renderresults s =  vBox $ map str s

renderresultsl :: Int -> [String] -> T.Widget Name
renderresultsl n s =  vBox $ map str (linenumbers n s)



linenumbers :: Int -> [String] -> [String] 
linenumbers n xs = let myzip = zip [n ..] xs in 
    map lns myzip 

lns :: (Int,String) -> String 
lns (x,y) = show x ++ ". " ++ y


-- event handling
event :: [BE.T] -> T.BrickEvent Name e -> T.EventM Name (AppState e) ()
event es = \ev -> do 
    case ev of 
        T.VtyEvent (EvKey KEsc []) -> M.halt
        T.VtyEvent (EvKey (KChar '+') []) -> do 
	     curr <- T.get 
	     let from = curr ^. mypdflist
	     let to = curr ^. mycurrentselection
	     liftIO $ renamePath (mypdfs ++ (fst (_current from))) ("/Users/ryan/onedrive/pdfs/sorted/" ++ (maybe "" getnewpath to)) 
        T.VtyEvent (EvKey (KChar '_') []) -> do 
	     curr <- T.get 
	     let from = curr ^. mypdflist
	     let to = curr ^. mycurrentselection
	     liftIO $ renamePath (mypdfs ++ (fst (_current from))) ("/Users/ryan/onedrive/pdfs/other/" ++ (fst (_current from))) 
        T.VtyEvent (EvKey (KChar '}') []) -> do  
	     curr <- T.get 
	     let bb = curr ^. mypdflist
	     mypdflist .= nextpdf bb 
	     mybestguess .= rfuzzy es (snd (_current (nextpdf bb)))
             mycurrentselection .= Nothing 
             T.modify $ resetState   
        T.VtyEvent (EvKey (KChar '{') []) -> do  
	     curr <- T.get 
	     let bb = curr ^. mypdflist
	     mypdflist .= prevpdf bb
	     mybestguess .= rfuzzy es (snd (_current (prevpdf bb)))
             mycurrentselection .= Nothing
        T.VtyEvent (EvKey (KChar '0') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 0 
	         then do mycurrentselection .= Just (sel !! 0)
		 else do mycurrentselection .= Nothing 
	T.VtyEvent (EvKey (KChar '1') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 1 
	         then do mycurrentselection .= Just (sel !! 1)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '2') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 2 
	         then do mycurrentselection .= Just (sel !! 2)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '3') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 3 
	         then do mycurrentselection .= Just (sel !! 3)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '4') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 4 
	         then do mycurrentselection .= Just (sel !! 4)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '5') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 5 
	         then do mycurrentselection .= Just (sel !! 5)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '6') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 6 
	         then do mycurrentselection .= Just (sel !! 6)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '7') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 7 
	         then do mycurrentselection .= Just (sel !! 7)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '8') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 8 
	         then do mycurrentselection .= Just (sel !! 8)
		 else do mycurrentselection .= Nothing
        T.VtyEvent (EvKey (KChar '9') []) -> do
	     currentstate <- T.get 
	     let bg = currentstate ^. mybestguess -- get best guesses 
	     let sr = currentstate ^. mysearchresults -- get search results 
             let sel = bg ++ sr 
	     if length sel > 9 
	         then do mycurrentselection .= Just (sel !! 9)
		 else do mycurrentselection .= Nothing 
        T.VtyEvent (EvKey (KChar '!') []) -> do
	     currentstate <- T.get
	     let bb = currentstate ^. mysearchstate
             curr <- T.get 
	     let from = curr ^. mypdflist
	     let to = curr ^. mycurrentselection
             let s = Text.unpack $ ((F.formState) bb) ^. search  
	     liftIO $ renamePath (mypdfs ++ (fst (_current from))) ("/Users/ryan/onedrive/pdfs/sorted/" ++ (spacetodash (map toLower s)) ++ ".pdf")
        T.VtyEvent (EvKey KEnter []) -> do  
             zoom mysearchstate $ F.handleFormEvent ev 
	     curr <- T.get --get the current state
	     let bb = curr ^. mysearchstate
	     let cc = ((F.formState) bb) ^. search 
             mysearchresults .= tophits (Text.unpack cc) es 
        _ -> do 
             zoom mysearchstate $ F.handleFormEvent ev 
	     -- an async function for updating the bibstate


-- here's all the pdf search and display stuff
-- 

-- just baking this in for now
mypdfs = "/Users/ryan/onedrive/pdfs/pdfs2sort/"

-- here we get all the pdfs, filtering out other shit
listpdfs :: String -> IO [(String,String)]
listpdfs x = do 
    d <- listDirectory x 
    let fp = filterpdfpaths d 
    txts <- mapM getpdftext (map (x ++) fp)   
    let z = zip fp txts
    return (z)

filterpdfpaths :: [String] -> [String] 
filterpdfpaths xs = List.filter (ispdf . reverse) xs 

ispdf :: String -> Bool 
ispdf x = (take 3 x) == "fdp"

-- here's the function where we get some of the pdf text 

getpdftext :: FilePath -> IO (String) 
getpdftext x = do 
    pdftext <- readProcess "pdftotext" ["-l", "5", x, "-"] "" 
    return (take 2000 pdftext)

-- okay so now we need a function for getting the top 
-- scoring bib entry for the pdf text 
-- this is the 'ReverseFuzzy' function that gives 
-- this module its name 
-- the function takes a bib entry and a string (the pdf string)
-- and returns the top 5 bibs

reversefuzzy :: [BE.T] -> String -> [BE.T] 
reversefuzzy es s = let strs = map getadt2 es 
                        scores = map (rff [s]) strs 
			mzip = zip scores es 
			sorted = reverse $ sortOn fst mzip in 
			map snd $ take 5 $ sorted 


rff :: [String] -> [String] -> [Alignment]
rff x y = fuzzyFind y x

--- we need this to get the "query" from the entry 
getadt2 :: BE.T -> [String]
getadt2 e = [filter (not . isPunctuation) $ M.fromMaybe "" (getfield "author" e) 
            ,M.fromMaybe "" (getfield "date" e) 
	    ,M.fromMaybe "" (getfield "title" e)]

-- for every entry, we want to generate 
-- all permutations of all subsequences 
-- once we generate these, we want to generate a 
-- list of pairs of entries with potential seach queries

getadt3 :: BE.T -> [([String],BE.T)] 
getadt3 e = undefined

entrysubs e = map (intercalate " ") $ subsequences $ words $ intercalate " " $ getadt2 e

matchEntry :: String -> BE.T -> Maybe (Fuzzy.Fuzzy BE.T String) 
matchEntry s e = Fuzzy.match s e "<" ">" getadt False 

-- FUZZY STUFF 
-- how hord can this be! 
-- remember that the aim is to get the top bib 
-- entries that match against the text of a given bib file 
-- the main function here should take a list of bib entries,
-- and a string (the pdf text) and return the top bib entries 
-- let's call it rfuzzy

rfuzzy :: [BE.T] -> String -> [BE.T]
rfuzzy es s = let alltitlesubs = getalltitlesubs es
                  fzscores = getfuzzyscores alltitlesubs s         
                  sorted = reverse $ sortOn fst fzscores in 
		  take 5 (map (fst . snd) sorted)  

rfuzzy2 :: [BE.T] -> String -> [String]
rfuzzy2 es s = let alltitlesubs = getalltitlesubs es
                   fzscores = getfuzzyscores alltitlesubs s         
                   sorted = reverse $ sortOn fst fzscores in 
		   take 5 (map getadt (map (fst . snd) sorted))

-- at some point, we are going to map 
-- all of the entries over the string 
-- to get a score for the match 
-- for simplicity, and for now, we will just 
-- get subsequences of the title 

gettitle :: BE.T -> [String]
gettitle e = [M.fromMaybe "" (getfield "title" e)]

-- but what we actually need here is a list of pairs 
-- of an entry and a title subsequence 

gettitlesubs :: BE.T -> [(BE.T,String)] 
gettitlesubs e = map (\x->(e,x)) (gettitle e)

-- and we want to get these for all entries 

getalltitlesubs :: [BE.T] -> [(BE.T,String)] 
getalltitlesubs es = concatMap gettitlesubs es 

-- now, for each of these, we will want a way of 
-- getting the score for the fuzzy search 
-- we might initially just get a score for each of these 

getfuzzyscores :: [(BE.T,String)] -> String -> [(Int,(BE.T,String))] 
getfuzzyscores es s = let fz = map (\x-> (Fuzzy.match (snd x) s "" "" id False)) es in 
    zip (map extractscore fz) es 

extractscore :: Maybe (Fuzzy.Fuzzy String String) -> Int
extractscore e = case e of 
    Just x -> Fuzzy.score x 
    Nothing -> 0




