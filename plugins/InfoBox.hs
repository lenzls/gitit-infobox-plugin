{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module InfoBox (plugin) where

-- This plugin adds a wikipedia style InfoBox.
--
-- You can find the newest version of this plugin always at https://github.com/lenzls/gitit-plugins
--
-- Use like this:
-- ~~~ {.infobox}
-- title|=|Terra Nova
-- imageURL|=|img/bla.png
-- imageCaption|=|Image of Terra nova
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
} deriving Show 

data TableRowData = HeadingRowData {
    label :: String
} | FieldRowData {
    label :: String,
    value :: String
} deriving Show

-- Parses a whole infobox description into an InfoBoxData
parse :: String -> InfoBoxData
parse description = parseLines (lines description)

parseLines :: [String] -> InfoBoxData
parseLines [] = (InfoBoxData "" Nothing Nothing [])
parseLines (x:xs) = parseLine x (parseLines xs)

-- Parses a line and adds it to existing InfoBoxData
parseLine :: String -> InfoBoxData -> InfoBoxData
parseLine lineString infoBoxData
    | rowType == "title" = infoBoxData { title = firstArg }
    | rowType == "imageURL" = infoBoxData { imageURL = (Just firstArg) }
    | rowType == "imageCaption" = infoBoxData { imageCaption = (Just firstArg) }
    | rowType == "heading" = infoBoxData { tableRows = [(HeadingRowData firstArg)] ++ (tableRows infoBoxData) }
    | rowType == "field" = infoBoxData { tableRows = [(FieldRowData firstArg secondArg)] ++ (tableRows infoBoxData) }
    | otherwise = infoBoxData
    where
        rowType = head (splitOn "|=|" lineString)
        firstArg = (splitOn "|=|" lineString) !! 1
        secondArg = (splitOn "|=|" lineString) !! 2

serializeToHTML :: InfoBoxData -> Block
serializeToHTML infoBoxData =
    RawBlock "HTML" (pack (
    "<aside class=\"info-box\">\n" ++
        "<h2>" ++ title infoBoxData ++ "</h2>\n" ++
        "<figure>\n" ++
            (maybe "" (\x -> "<img src=\"" ++ x ++ "\" />\n") (imageURL infoBoxData)) ++
            (maybe "" (\x -> "<figcaption>" ++ x ++ "</figcaption>\n") (imageCaption infoBoxData)) ++
        "</figure>\n" ++
        "<table>\n" ++
            (concat (map serializeRowToHTML (tableRows infoBoxData))) ++
        "</table>" ++
    "</aside>"))

serializeRowToHTML :: TableRowData -> String
serializeRowToHTML (HeadingRowData label) = "<tr><th class=\"heading\" colspan=\"2\">" ++ label ++ "</th></tr>\n"
serializeRowToHTML (FieldRowData label value) = "<tr><th>" ++ label ++ "</th><td>" ++ value ++ "</td></tr>\n"

serializeRowsToTableBodies :: [TableRowData] -> [TableBody]
serializeRowsToTableBodies [] = []
serializeRowsToTableBodies (x:xs) = serializeRowToTableBodies x (serializeRowsToTableBodies xs)

serializeRowToTableBodies :: TableRowData -> [TableBody] -> [TableBody]
serializeRowToTableBodies (HeadingRowData label) result = result ++ [
        (
            TableBody
                nullAttr
                (RowHeadColumns 1)
                [
                    Row nullAttr [
                        Cell nullAttr AlignCenter (RowSpan 1) (ColSpan 2) [Plain [((Str . pack) label)]]
                    ]
                ]
                []
        )
    ]
serializeRowToTableBodies (FieldRowData label value) result = (init result) ++ [
        (
            addBodyRow
                (last result)
                (
                    Row nullAttr [
                        Cell nullAttr AlignLeft (RowSpan 1) (ColSpan 1) [Plain [((Str . pack) label)]],
                        Cell nullAttr AlignLeft (RowSpan 1) (ColSpan 1) [Para [(Str (pack value))]]
                    ]
                )
        )
    ]

addBodyRow :: TableBody -> Row -> TableBody
addBodyRow (TableBody attr rhc headRows bodyRows) row = TableBody attr rhc headRows (bodyRows ++ [row])

serializeToBlock :: InfoBoxData -> Block
serializeToBlock infoBoxData =
    Div ("", ["info-box"], []) [
        Header 2 nullAttr [(Str . pack) (title infoBoxData)],
        SimpleFigure 
            nullAttr
            [Str (pack (maybe "" (\x -> x) (imageCaption infoBoxData)))]
            (
                pack (maybe "" (\x -> x) (imageURL infoBoxData)),
                ""
            ),
        Table
            nullAttr
            (Caption Nothing [])
            [(AlignLeft,ColWidthDefault), (AlignLeft,ColWidthDefault)]
            (TableHead nullAttr [])
            (serializeRowsToTableBodies (reverse (tableRows infoBoxData)))
            (TableFoot nullAttr [])
    ]

plugin :: Plugin
plugin = mkPageTransform transformBlock

transformBlock :: Block -> Block
transformBlock (CodeBlock (_, classes, namevals) contents) | "infobox" `elem` classes =
    traceShow parsed
    serializeToBlock parsed
    where
        parsed = (parse (unpack contents))
transformBlock x = x
