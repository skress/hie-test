{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , staticFiles
    ) where

import System.Directory
import Control.Monad
import qualified Data.ByteArray as ByteArray
import Crypto.Hash (MD5, Digest)
import Data.List (intercalate, foldl')
import Data.Char (isLower, isDigit)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import Control.Monad.Trans.State
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64
import Crypto.Hash.Conduit (hashFile, sinkHash)
import Conduit
import Data.Text (Text)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

--newtype Static = Static StaticSettings
newtype Static = Static Text

--type StaticRoute = Route Static
data StaticRoute = StaticRoute [Text] [(Text,Text)]

staticFiles :: FilePath -> Q [Dec]
staticFiles dir = mkStaticFiles dir

mkStaticFiles :: FilePath -> Q [Dec]
mkStaticFiles fp = mkStaticFiles' fp True

mkStaticFiles' :: FilePath -- ^ static directory
               -> Bool     -- ^ append checksum query parameter
               -> Q [Dec]
mkStaticFiles' fp makeHash = do
    fs <- qRunIO $ getFileListPieces fp
    mkStaticFilesList fp fs makeHash

mkStaticFilesList
    :: FilePath -- ^ static directory
    -> [[String]] -- ^ list of files to create identifiers for
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList fp fs makeHash = mkStaticFilesList' fp (zip fs fs) makeHash

mkStaticFilesList'
    :: FilePath -- ^ static directory
    -> [([String], [String])] -- ^ list of files to create identifiers for, where
                              -- the first argument of the tuple is the identifier
                              -- alias and the second is the actual file name
    -> Bool     -- ^ append checksum query parameter
    -> Q [Dec]
mkStaticFilesList' fp fs makeHash = do
    concat `fmap` mapM mkRoute fs
  where
    replace' c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
        | otherwise = '_'
    mkRoute (alias, f) = do
        let name' = intercalate "_" $ map (map replace') alias
            routeName = mkName $
                case () of
                    ()
                        | null name' -> error "null-named file"
                        | isDigit (head name') -> '_' : name'
                        | isLower (head name') -> name'
                        | otherwise -> '_' : name'
        f' <- [|map pack $(TH.lift f)|]
        qs <- if makeHash
                    then do hash <- qRunIO $ base64md5File $ pathFromRawPieces fp f
                            [|[(pack "etag", pack $(TH.lift hash))]|]
                    else return $ ListE []
        return
            [ SigD routeName $ ConT ''StaticRoute
            , FunD routeName
                [ Clause [] (NormalB $ (ConE 'StaticRoute) `AppE` f' `AppE` qs) []
                ]
            ]

getFileListPieces :: FilePath -> IO [[String]]
getFileListPieces = flip evalStateT M.empty . flip go id
  where
    go :: String
       -> ([String] -> [String])
       -> StateT (M.Map String String) IO [[String]]
    go fp front = do
        allContents <- liftIO $ filter notHidden `fmap` getDirectoryContents fp
        let fullPath :: String -> String
            fullPath f = fp ++ '/' : f
        files <- liftIO $ filterM (doesFileExist . fullPath) allContents
        let files' = map (front . return) files
        files'' <- mapM dedupe files'
        dirs <- liftIO $ filterM (doesDirectoryExist . fullPath) allContents
        dirs' <- mapM (\f -> go (fullPath f) (front . (:) f)) dirs
        return $ concat $ files'' : dirs'

    -- Reuse data buffers for identical strings
    dedupe :: [String] -> StateT (M.Map String String) IO [String]
    dedupe = mapM dedupe'

    dedupe' :: String -> StateT (M.Map String String) IO String
    dedupe' s = do
        m <- get
        case M.lookup s m of
            Just s' -> return s'
            Nothing -> do
                put $ M.insert s s m
                return s

base64md5File :: FilePath -> IO String
base64md5File = fmap (base64 . encode) . hashFile
    where encode d = ByteArray.convert (d :: Digest MD5)


base64 :: S.ByteString -> String
base64 = map tr
       . take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

pathFromRawPieces :: FilePath -> [String] -> FilePath
pathFromRawPieces =
    foldl' append
  where
    append a b = a ++ '/' : b

notHidden :: FilePath -> Bool
notHidden "tmp" = False
notHidden s =
    case s of
        '.':_ -> False
        _ -> True
