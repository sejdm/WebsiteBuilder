{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE RankNTypes #-}

module Webpage
       (
         -- * Making the page
         -- | A website or latex CV etc can be thought of as data rendered in a particular template. With this module, you can create your own data type to carry all the data, especially as a record using the record style syntax. The following functions allow you to apply a template to the data to form a website. The purpose is to use this module with modules that define monad transformers for such structured documents. The examples here use Lucid's HtmlT monad transformer, but if there is a similar one for latex, that may be used too. Templates are monadic and therefore can be composed to form bigger templates by simply placing them in a do block.
         Template, WebTemplate, createPage, usingTemplate, saveIn, pack,

         -- * The Style functions
         Style,  usingStyle, (#), withAttr,


         -- * Common styles
         paragraphed, listed, onlyNumberStyle,

         -- * Modifying templates
         askss, sectioned, prefixed, headingStyle, headinged,
         toHtml,

         -- * Date constructors
         Date,
         -- $section1
         year, date, month, on, Month (..), getDay, getMonth, getYear,

         -- * CV data
         CVtemplate, CV (..),
         Name (..), Paper (..), JournalStatus (..), Education (..), EducationName (..), University (..), Thesis (..), Subject (..),
         --Author, author,


         -- * CV styles
         sort, sortBy,
         dobStyle, paperStyle, educationStyle, amsStyle, stdEducationStyle, dateUKstyle, fullNameStyle, abbreviatedNameStyle,
       ) where

import Lucid
--import Lucid.Base
--import Lucid.Bootstrap
import Control.Monad hiding (mapM_)
import Data.Monoid
import Data.Text hiding (map, intersperse, length)
import Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy as TL
import Prelude hiding (head, mapM_)
import Data.List (sort, intersperse, sortBy)
import Control.Monad.Reader  hiding (mapM_)
import Control.Arrow
import Data.Foldable

data Thesis = Thesis
              {
                thesisTitle :: Text,
                thesisPublication :: Paper,
                thesisSubmitted :: Date,
                thesisDefended :: Date,
                thesisAdvisor :: Text
              } deriving (Eq)

instance Ord Thesis where
  x `compare` y = thesisAdvisor x `compare` thesisAdvisor y

data Subject = Math | Phy | Stat | AppMath -- ^ Applied math
             | AlgGeo -- ^ Algebraic Geometry
             | Subject Text Text -- ^ The first argument is the name and the second is the abbreviation.
             deriving (Show, Eq, Ord)

instance ToHtml Subject where
  toHtml (Subject x _) = toHtml x
  toHtml Math = "Mathematics"
  toHtml Phy = "Physics"
  toHtml Stat = "Statistics"
  toHtml AppMath = "Applied Mathematics"


class Abbreviation a where
  abbrv :: Monad m => a -> HtmlT m ()

instance Abbreviation Subject where
  abbrv (Subject _ x) = toHtml x
  abbrv x = toHtml (show x) <> "."

instance Abbreviation Name where
  abbrv (Name x y) = (toHtml y) <> ", " <> (toHtml [head x]) <> "."

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq, Enum)

instance ToHtml Month where
  toHtml Jan = "January"
  toHtml Feb = "February"
  toHtml Mar = "March"
  toHtml Apr = "April"
  toHtml May = "May"
  toHtml Jun = "June"
  toHtml Jul = "July"
  toHtml Aug = "August"
  toHtml Sep = "September"
  toHtml Oct = "October"
  toHtml Nov = "November"
  toHtml Dec = "December"

data Date = Date {getDay :: Maybe Int, getMonth :: Maybe Month, getYear :: Int} deriving (Eq)

-- $section1
-- Use these constructors to create values of type 'Date':
year :: Int -- ^ Year
     -> Date
year = Date Nothing Nothing

month :: Month -> Int -- ^ Year
      -> Date
month n = Date Nothing (Just n)

date :: Int -- ^ Day
     -> Month -- ^ Month
     -> Int -- ^ Year
     -> Date
date d m = Date (Just d) (Just m)

-- | An alias for 'date'
on = date

instance ToHtml Date where
  toHtml (Date Nothing Nothing y) = toHtml y
  toHtml (Date Nothing (Just m) y) = toHtml m <> ", " <> toHtml y
  toHtml (Date (Just d) (Just m) y) =
    toHtml d <> " " <> toHtml m <> ", " <> toHtml y

data CV = CV
  {
    cvName :: Name,
    cvDOB :: Date,
    cvPapers :: [Paper],
    cvEducation :: [Education],
    cvUpdated :: Date
  }


data Name = Name
            {
             firstName :: Text,
             lastName :: Text
            }
          deriving (Eq)

instance ToHtml Name where
  toHtml (Name x y) = (toHtml x) <> " " <> (toHtml y)


-- | Newtype wrapper for 'Name'. The name is displayed with the first name initialized.
newtype Author = Author Name deriving (Eq, Ord)

instance ToHtml Author where
  toHtml (Author (Name x y)) = (toHtml y) <> ", " <> (toHtml [head x]) <> "."


-- | Data constructor for 'Author'.
author :: Text -- ^ First name
         -> Text -- ^ Second name
         -> Author
author x y = Author $ Name x y

-- | To convert 'Name' to 'Author'
makeAuthor :: Name -> Author
makeAuthor = Author

instance ToHtml Int where
  toHtml = toHtml . show

instance Ord Name where
  Name _ l1 `compare` Name _ l2 = l1 `compare` l2


data University = Univ {univName :: Text, univCity :: Text, univCountry :: Text} deriving (Eq)


instance ToHtml University where
  toHtml x = toHtml (univName x) <> ", " <> toHtml (univCity x) <> ", " <> toHtml (univCountry x)

data Education = Education {education :: EducationName, univ :: University, joinyear :: Date, gragetYear :: Date} deriving (Eq)

data EducationName =
  PhD Subject Thesis
  | MSc Subject
  | MS | MMath | BSc | BMath deriving (Eq, Ord)

instance Ord Education where
  x `compare` y = education x `compare` education y

instance ToHtml EducationName where
  toHtml (PhD n _) = "Ph.D. (" <> toHtml n <> ")"
  toHtml (MSc n) = "M.Sc. (" <> toHtml n <> ")"

instance ToHtml Education where
  toHtml d = strong_ (toHtml (education d)) <> ": (" <> toHtml (joinyear d) <> " - " <> toHtml (gragetYear d) <> ") " <> toHtml (univ d)

data JournalStatus = Submitted Text | Accepted Text Text | Published {jname :: Text, doi ::  Text} | Unsubmitted | InPreparation

data Paper = Paper {paperTitle :: Text, authors :: [Name], journal :: JournalStatus, arxivLink :: Maybe Text}

instance Eq Paper where
  x == y = paperTitle x == paperTitle y

--instance ToHtml Paper where
amsStyle p = do
    n <- askss cvName
    (papertitle <> ". " <> i_ (allAuthors n)) <> br_ []  <> (status (journal p)) <> br_ [] <> (arxiv (arxivLink p))
    where allAuthors m = commaSeparatedAb $ sort $ m : authors p
          papertitle = toHtml (paperTitle p)

          status (Submitted x) = i_ "Submitted: " <> toHtml x
          status (Accepted x d) = i_ "Accepted: " <>
            toHtml x <> doithing d
          status (Published x d) = i_ "Published: "
            <> toHtml x  <> doithing d
          status Unsubmitted = mempty
          status InPreparation = i_ "Manuscript in preparation"

          doithing d' = if d' == "" then mempty else toHtml (". doi: " `append` d')

askss x = lift (asks x)

nameHtml :: CVtemplate
nameHtml = askss cvName >>= h1_ [class_ "name"] . toHtml

headingStyle :: ToHtml a => WebStyle CV a
headingStyle = h1_ [class_ "name"] . toHtml

dateHtml :: CVtemplate
dateHtml = askss cvDOB >>= p_ . ("Date of birth: " <>) . i_ . toHtml

dobStyle :: WebStyle CV Date
dobStyle = p_ . ("Date of birth: " <>) . i_ . toHtml

dateUKstyle = toHtml

paperSection :: CVtemplate
paperSection = h2_ "Papers" >> askss cvPapers >>=
   mapM_ (p_ [class_ "paper"] . amsStyle)

paperStyle :: WebStyle CV [Paper]
paperStyle = mapM_ (p_ [class_ "paper"] . amsStyle)

educationSection :: CVtemplate
educationSection = do
    ps <- lift (asks cvEducation)
    h2_ "Education"
    mapM_ (p_ [class_ "education"] . toHtml) $ sort ps

educationStyle :: WebStyle CV [Education]
educationStyle ps = do
     h2_ "Education"
     mapM_ (p_ [class_ "education"] . toHtml) $ sort ps

stdEducationStyle :: WebStyle CV Education
stdEducationStyle = toHtml



arxiv :: Monad m => Maybe Text -> HtmlT m ()
arxiv Nothing = mempty
arxiv (Just x) = a_ [href_ x] "Arxiv"



lineByLine :: (Monad m, Foldable t) => t (HtmlT m a) -> HtmlT m ()
lineByLine xs = ul_ (mapM_ (li_) xs)

commaSeparated :: (Monad m, ToHtml a) => [a] -> HtmlT m ()
commaSeparated = mconcat . intersperse ", " . map toHtml

commaSeparatedAb :: (Monad m, Abbreviation a) => [a] -> HtmlT m ()
commaSeparatedAb = mconcat . intersperse ", " . map abbrv

--people = p_ $ lineByLine ["Test1", "Test2", toHtml (Name "yo" "op"), toHtml ritwikPaper, toHtml imrn]

type Style t r a = a -> Template t r
type WebStyle r a = Style HtmlT r a

usingStyle ::  (Monad (t (Reader r)), MonadTrans t) => (r -> a) -> Style t r a -> Template t r
usingStyle r s = askss r >>= s

--(#>) :: Monad (t (Reader r)) => Style t r a -> Style t r a -> Style t r a
--(#>) :: Monoid b => (a -> b) -> (a -> b) -> (a -> b)
--m1 #> m2 = uncurry (<>) . (m1 &&& m2)
--m1 #> m2 = \p -> m1 p >> m2 p
--(#>) = (<>)
--infixl 5 #>

withAttr = flip with

infixl 1 `usingStyle`

--(#) = usingStyle
(#) = (>>>)
infixl 8 #

-- | The first argument is for the type of template (Html/Latex etc) and the second argument is the datatype that represents the information to be rendered.
type Template t r = t (Reader r) ()

-- | Template specialized to Html.

type WebTemplate r = Template HtmlT r

type CVtemplate = WebTemplate CV


-- | Create an Html version of your data using the template, saved in the file path.
createPage :: FilePath -> WebTemplate r -> r -- ^ Data to be rendered as Html using the template.
           -> IO ()
createPage n t m = TI.writeFile n $ runReader (renderTextT t) m

-- | Generate Html text of the data using the template.
usingTemplate :: r -> WebTemplate r -> TL.Text
usingTemplate m t = runReader (renderTextT t) m

-- | Save the text to a file.
saveIn :: FilePath -> TL.Text -> IO ()
saveIn = TI.writeFile
--main = renderToFile "test.html" (replicateM_ 1 (people <> people))

-- | Given a style for a particular data type, generate a style for a list of that data type where each element is a rendered as a separate paragraph,  rendered in the given style.
paragraphed :: WebStyle r a -> WebStyle r [a]
paragraphed f xs = mapM_ (p_ . f) xs

sorted :: Ord a => (r -> [a]) -> r -> [a]
sorted f = sort . f

-- | Given a style for a particular data type, generate a style for a list of that data type where each element is a rendered as a separate item of a list,  rendered in the given style.
listed :: WebStyle r a -> WebStyle r [a]
listed f xs = ul_ ( mapM_ (li_ . f) xs)

-- | Wrap in a section of a given name.
sectioned :: Text -- ^ Name of the section
          -> WebTemplate r -> WebTemplate r
sectioned s = (h2_ (toHtml s) >>)

-- | Prefix with particular text. The text will be italicized and followed by a colon. An example would be to prefix a date with "/Date:/"
prefixed :: Text -> WebTemplate r -> WebTemplate r
prefixed s w = (i_ (toHtml s) <> ": " <> w)

fullNameStyle = toHtml

abbreviatedNameStyle = abbrv

-- | Wrap the template to form a heading
headinged = h1_


onlyNumberStyle :: WebStyle r [a]
onlyNumberStyle = toHtml . length
