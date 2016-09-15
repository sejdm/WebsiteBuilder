{-# LANGUAGE OverloadedStrings #-}
import Webpage
import Lucid
import Control.Arrow
import Data.Monoid
import Data.Time.Clock
import Data.Time.Calendar
import Control.Applicative
import Control.Monad
import Data.Time.Calendar.WeekDate

myCV :: CV
myCV = CV
   {
     cvName = Name "John" "Doe",
     cvDOB = on 29 Feb 1972,
     cvPapers = [paper1, paper2, paper3], -- paperN is defined below
     cvEducation = [mymsc, myphd], -- mymsc and myphd are defined below
     cvUpdated = undefined -- this field will be updated from the system clock
   }

-- | A CV template that generates the name, date of birth, list of papers, and education.
cvTemplate :: Template HtmlT CV
cvTemplate = do
  cvUpdated `usingStyle` dateUKstyle # prefixed "Last updated"
  cvName `usingStyle` fullNameStyle # headinged
  cvAge `usingStyle` dateUKstyle # prefixed "Age" # p_
  cvDOB `usingStyle` dateUKstyle # prefixed "Date of birth"
  cvPapers `usingStyle` (onlyNumberStyle # prefixed "Number of papers"
                         <> listed amsStyle) # sectioned "Papers"
  cvEducation `usingStyle` sortedEducationStyle


-- | You can define your own styles like this one which sorts the education
-- list, represents it as an html list, and places it in a section called "Education"
sortedEducationStyle :: Style HtmlT CV [Education]
sortedEducationStyle = sort # listed stdEducationStyle # sectioned "Education"


-- | You can define your own "getters" to obtain info from CV other than just
-- the record fields
cvAge :: CV -> Int
cvAge = (-) <$> (getYear . cvUpdated) <*>  (getYear . cvDOB)





main = do
  (y, m, d) <- (toGregorian . utctDay) <$> getCurrentTime
  saveIn "test.html" $ myCV {cvUpdated = on d (toEnum (m-1)) (fromIntegral y)} `usingTemplate` cvTemplate
--main = createPage "test.html" cvTemplate mine




paper1 = Paper
   {
     paperTitle = "The unstructure of Vogon poetry",
     authors = [Name "Arthur" "Dent", Name "Ford" "Prefect"],
     journal = Submitted "Journal of Unmatched Medioctrity",
     arxivLink = Just "http://arxiv/vonism/42"
   }

paper2 = Paper
   {
     paperTitle = "A pointless question on the topology of the point",
     authors = [Name "Jane" "Doe"],
     journal = Accepted "Journal of pointless topology" "0000",
     arxivLink = Just "http://arxiv/topology/24"
   }

paper3 = Paper
  {
    paperTitle = "Now what?",
    authors = [],
    journal = InPreparation,
    arxivLink = Nothing
  }

mythesis = undefined
myphd = Education (PhD Math mythesis) ohWell (year 2002) (month Dec 1997)
mymsc = Education (MSc Math) agraj (year 1995) (year 1997)


ohWell = Univ "University of Now What" "Oh Well" "Now What"
agraj = Univ "University of Agrajag" "AX1" "Betelgeuse"
