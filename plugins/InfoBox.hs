{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module InfoBox (plugin) where

-- This plugin adds a wikipedia style InfoBox.
-- Use like this:
-- ~~~ {.infobox}
-- title|=|Terra Nova
-- imageURL|=|img/bla.png
-- imageCaption|=|Image of Terra nova
-- ---
-- heading|=|Characteristics
-- field|=|Radius|=|40k
-- field|=|Inhabitants|=|40 <a href="units/million">million</a>
-- heading|=|Atmosphere
-- field|=|Surface pressure|=|2000Pa
-- ~~~

import Network.Gitit.Interface
import Data.Char (toLower)
import Data.Text (pack, unpack, isPrefixOf)
import Data.List (concat)
import Debug.Trace
import Data.List.Split

data InfoBoxData = InfoBoxData {
    title :: String,
    imageURL :: Maybe String,
    imageCaption :: Maybe String,
    tableRows :: [TableRowData]
}
data TableRowData = TableRowData {
    rowType :: String,
    label :: String,
    value :: String
}

serializeInfoBoxData :: InfoBoxData -> String
serializeInfoBoxData ibd = show (title ibd, imageURL ibd, imageCaption ibd)

-- Parses a whole infobox description into an InfoBoxData
parse :: String -> InfoBoxData
parse description =
    parseLine (head (lines description)) (InfoBoxData "" Nothing Nothing [])

-- Parses a line and adds it to existing InfoBoxData
parseLine :: String -> InfoBoxData -> InfoBoxData
parseLine lineString infoBoxData
    | rowType == "title" = infoBoxData { title = firstArg }
    | otherwise = infoBoxData
    where
        rowType = head (splitOn "|=|" lineString)
        firstArg = (splitOn "|=|" lineString) !! 1
        secondArg = (splitOn "|=|" lineString) !! 2

plugin :: Plugin
plugin = mkPageTransform transformBlock

getImageURL :: String -> String
getImageURL metaDataLines = getValueFromMetaDataLine (getMetaDataLine metaDataLines "imageURL") 

getImageCaption :: String -> String
getImageCaption metaDataLines = getValueFromMetaDataLine (getMetaDataLine metaDataLines "imageCaption") 

getTitle :: String -> String
getTitle metaDataLines = getValueFromMetaDataLine (getMetaDataLine metaDataLines "title") 

getValueFromMetaDataLine :: String -> String
getValueFromMetaDataLine metaDataLine = (splitOn "|=|" metaDataLine) !! 1

-- "title=Terra Nova\nimage=img/bla.png\n…" "image" → "img/bla.png"
getMetaDataLine :: String -> String -> String
getMetaDataLine metaDataLines metaDataType =
            unpack (head (filter
                (isPrefixOf (pack metaDataType))
                (map pack (lines metaDataLines))
            ))

-- tableRowLines into HTML table rows
getTableRows :: String -> String
getTableRows tableRowLines =
    concat (map getTableRow (filter (not . null) (lines tableRowLines)))


-- heading|=|Characteristics
-- field|=|Radius|=|40k
getTableRow :: String -> String
getTableRow tableRow
    | rowType == "heading" = "<tr><th class=\"heading\" colspan=\"2\">" ++ heading ++ "</th></tr>"
    | rowType == "field" = "<tr><th>" ++ label ++ "</th><td>" ++ value ++ "</td></tr>"
    | otherwise = "<tr><td>Could not interpret row:'" ++ tableRow ++ "'</td></tr>"
    where 
        rowType = (getTableRowType tableRow)
        heading = (splitOn "|=|" tableRow) !! 1
        label = (splitOn "|=|" tableRow) !! 1
        value = (splitOn "|=|" tableRow) !! 2

getTableRowType :: String -> String
getTableRowType tableRow = head (splitOn "|=|" tableRow)

    -- return $ Table
    --     ("ttable", [], [])
    --     (Caption (Just [(Str "some caption")]) [])
    --     [(AlignLeft,ColWidth 20), (AlignLeft,ColWidthDefault)]
    --     (TableHead ("thead", [], []) [
    --         Row ("theadrow1", [], []) [
    --             Cell ("theadcell1", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "Header col 1")]]
    --         ]
    --     ])
    --     [TableBody ("tbody1", [], []) (RowHeadColumns 0) [
    --         Row ("trow1.1", [], []) [
    --             Cell ("tcell1.1a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.1a"), (Str "2nd content cell1.1")]]
    --         ],
    --         Row ("trow1.2", [], []) [
    --             Cell ("tcell1.2a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.2a")]],
    --             Cell ("tcell1.2b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.2b")]]
    --         ],
    --         Row ("trow1.3", [], []) [
    --             Cell ("tcell1.3a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.3a")]],
    --             Cell ("tcell1.3b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell1.3b")]]
    --         ]
    --     ] [
    --         Row ("trow2.1", [], []) [
    --             Cell ("tcell2.1a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.1a")]],
    --             Cell ("tcell2.1b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.1b")]]
    --         ],
    --         Row ("trow2.2", [], []) [
    --             Cell ("tcell2.2a", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.2a")]],
    --             Cell ("tcell2.2b", [], []) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [(Str "content cell2.2b")]]
    --         ]
    --     ]]
    --     (TableFoot ("tfoot", [], []) [])
transformBlock :: Block -> Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "infobox" `elem` classes =
    traceShow (serializeInfoBoxData (parse (unpack contents)))
    (Para [Str "Here will be table"])
transformBlock x = x
