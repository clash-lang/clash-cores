{-# OPTIONS_GHC -Wno-x-partial #-}

-- |
--  Copyright   :  (C) 2024, QBayLogic B.V.
--  License     :  BSD2
--  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
module Clash.Cores.ClashFloPoCo.Lexer
  (
    getLastInfoEntity,
    processVHDLFile,
  )
where

import Clash.Cores.ClashFloPoCo.InfoEn
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAlphaNum, isDigit, isSpace)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import System.IO
import Prelude

-- | This module contains functions to parse and get necessary information from FloPoCo VHDL file
keywords :: [String]
keywords = ["Input", "Output", "frequency", "Pipeline"]

-- Define an empty or default InfoEntity with all fields as Nothing
emptyInfoEntity :: InfoEntity
emptyInfoEntity =
  InfoEntity
    { name = Nothing,
      freq = Nothing,
      pipedep = Nothing,
      insig = Nothing,
      outsig = Nothing
    }

-- Check if the string is a valid VHDL comment
isVHDLcomment :: String -> Bool
isVHDLcomment str
  | length str > 2 = if take 2 str == "--" then True else False
  | otherwise = False

-- Get the content of a VHDL comment
getVHDLComment :: String -> Maybe String
getVHDLComment str
  | length str > 2 = if take 2 str == "--" then Just $ drop 2 str else Nothing
  | otherwise = Nothing

-- Check if string has any space
containsSpace :: String -> Bool
containsSpace str = any isSpace str

-- Check if a VHDL string contains port
containsPort :: String -> Bool
containsPort str = "port (" `L.isInfixOf` str || "port(" `L.isInfixOf` str

-- Get the string content after colon
afterColon :: Maybe [String] -> Maybe [String]
afterColon Nothing = Nothing
afterColon (Just wordList) =
  case dropWhile (notElem ':') wordList of
    [] -> Nothing -- No colon found, return Nothing
    (_ : rest) -> Just rest -- Skip the word with the colon and return the rest

-- Checks if a word contains no special characters
hasNoSpecialChars :: String -> Bool
hasNoSpecialChars = all isAllowedChar
  where
    isAllowedChar c = isAlphaNum c || elem c ['-', '_'] -- Add any extra allowed characters here

-- Update the InfoEntity to get all necessary information: input signal names, output signal names, pipeline depth, and frequency
updateInfoEntity :: String -> Maybe InfoEntity -> Maybe InfoEntity
updateInfoEntity _ Nothing = Nothing
updateInfoEntity comment (Just infoen) =
  if (containsSpace comment) == False
    then
      Just infoen
    else
      let wordList = words comment
       in if (length wordList) == 1
            then
              let singleword = head wordList
               in if hasNoSpecialChars singleword
                    then
                      Just infoen {name = Just singleword}
                    else
                      Just infoen
            else case [word | word <- keywords, elem word wordList] of
              [] -> Just infoen
              ["Input"] -> Just infoen {insig = afterColon (Just wordList)}
              ["Output"] -> Just infoen {outsig = afterColon (Just wordList)}
              ["Pipeline"] -> Just infoen {pipedep = fmap (read . filter isDigit . concat) (afterColon (Just wordList))}
              ["frequency"] -> Just infoen {freq = fmap (read . filter isDigit . concat) (afterColon (Just wordList))}
              [_, _] -> Nothing
              _ -> Nothing

-- Get all VHDL comments from the content of the VHDL file
makeListVHDLcomments :: String -> Maybe [String]
makeListVHDLcomments vhdlcontent =
  Just $ mapMaybe getVHDLComment [vhdlcomment | vhdlcomment <- (lines vhdlcontent), isVHDLcomment vhdlcomment]

-- | Function to extract the last entity from VHDL comment
getLastInfoEntity ::
  -- | List of VHDL comment
  Maybe [String] ->
  -- | InfoEntity need to be update
  Maybe InfoEntity ->
  -- | InfoEntity after update
  Maybe InfoEntity
getLastInfoEntity _ Nothing = Nothing
getLastInfoEntity Nothing _ = Nothing
getLastInfoEntity (Just []) (Just infoen) = Just infoen
getLastInfoEntity (Just (x : vhdlcomments)) (Just infoen) = getLastInfoEntity (Just vhdlcomments) (updateInfoEntity x (Just infoen))

-- Function to extract words after 'port (' and before ':'
extractPortNamespartial :: String -> [String]
extractPortNamespartial input =
  let -- Skip leading whitespace before "port ("
      trimmedInput = dropWhile (== ' ') input
      -- Check if the remaining string starts with "port ("
      afterPort =
        if "port (" `L.isPrefixOf` trimmedInput
          then drop (length ("port (" :: String)) trimmedInput
          else ""
      -- Take everything before the first occurrence of ':'
      beforeColon = takeWhile (/= ':') afterPort
      -- Replace commas with spaces to split names correctly
      names = words (map (\c -> if c == ',' then ' ' else c) beforeColon)
   in names

-- Function to update the input signal names of InfoEntity
updateInputPortInfoEntity :: Maybe [String] -> InfoEntity -> InfoEntity
updateInputPortInfoEntity inp infoen = infoen {insig = inp}

-- Function to find positions of "entity" from the tail
findEntityPositions :: [String] -> [Int]
findEntityPositions lst =
  let reversed = reverse lst -- Reverse the list to start from the tail
      positions = [i | (i, s) <- zip [0 ..] reversed, "entity" `L.isInfixOf` s] -- Get positions in reversed list
   in map (\i -> length lst - 1 - i) positions -- Convert to original indices

-- | Function to process FloPoCo VHDL file and extract the main entity and its information
processVHDLFile ::
  (MonadIO m) =>
  -- | Function to process and update InfoEntity, in this library is getLastInfoEntity
  (Maybe [String] -> Maybe InfoEntity -> Maybe InfoEntity) ->
  -- | FilePath: The path of FloPoCo VHDL file
  FilePath ->
  -- | InfoEntity
  m (Maybe InfoEntity)
processVHDLFile process path = do
  handle <- liftIO $ openFile path ReadMode -- Open the file with liftIO
  contents <- liftIO $ hGetContents handle -- Read the file's contents
  let lines_content = lines contents
  -- Initial InfoEntity (can be Nothing or some default value)
  let initialEntity = emptyInfoEntity

  -- Apply the processing function
  let result = process (makeListVHDLcomments contents) (Just initialEntity)
  -- We only need to count number of "entity SomeRandomName" and "component SomeRandomName2" not "end entity;" and "end component;"
  let ent_and_comp = filter (\line -> "component " `L.isInfixOf` line || "entity " `L.isInfixOf` line) lines_content
  let len_ent = findEntityPositions (ent_and_comp)
  let partialPortInputs = extractPortNamespartial (((filter containsPort (lines_content)) !! (head len_ent)))
  -- Process the result and return an Int

  liftIO $ print result
  liftIO $ hClose handle -- Close the file handle
  -- Analyze the result
  case result of
    Just info -> do
      -- Add remaining port input signals: clk, rst(if user enable FloPoCo to have resest signal)
      let inports = insig info
      let updateinports = fmap (partialPortInputs ++) inports
      let info1 = updateInputPortInfoEntity updateinports info
      liftIO $ print info1
      return (Just info1)
    Nothing -> return Nothing
