{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    , staticFiles
    ) where

import Debug.Trace

import Conduit
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State
import Control.Monad.Trans.Resource (runResourceT)
import Crypto.Hash (Digest, HashAlgorithm, MD5)
import qualified Crypto.Hash as Hash (hash, hashFinalize, hashUpdate, hashInit)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import qualified Data.ByteString.Base64
import Data.Char (isLower, isDigit)
import Data.Conduit
import Data.Conduit.Binary (sourceFile)
import Data.List (intercalate, foldl')
import qualified Data.Map as M
import Data.Text (Text)
import Language.Haskell.TH.Syntax as TH
import System.Directory

someFunc :: IO ()
someFunc = do
    files <- getFileListPieces "./src"
    putStrLn $ "File: " ++ intercalate ", " (map (intercalate "/") files)

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
        f' <- trace ("XXXX mkStaticFilesList' map pack for alias=" ++ show alias ++ ", f=" ++ show f) $ [|map pack $(TH.lift f)|]
        qs <- if makeHash
                    then do hash <- trace "XXXX mkStaticFilesList' qRunIO" $ qRunIO $ base64md5File $ pathFromRawPieces fp f
                            trace "XXXX mkStaticFilesList' pack etag" $ [|[(pack "etag", pack "XXX")]|]
                    else return $ ListE []
        trace "XXXX mkStaticFilesList' return" $ return
            [ SigD routeName $ ConT ''StaticRoute
            , FunD routeName
                [ Clause [] (NormalB $ (ConE 'StaticRoute) `AppE` f' `AppE` qs) []
                ]
            ]

getFileListPieces :: FilePath -> IO [[String]]
getFileListPieces = do
  result <- flip evalStateT M.empty . flip go id
  trace "XXXX getFileListPieces - return" return result
  where
    go :: String
       -> ([String] -> [String])
       -> StateT (M.Map String String) IO [[String]]
    go fp front = do
        allContents <- trace "XXXX getFileListPieces - 1" $ liftIO $ filter notHidden `fmap` getDirectoryContents fp
        let fullPath :: String -> String
            fullPath f = fp ++ '/' : f
        files <- trace "XXXX getFileListPieces - 2" $ liftIO $ filterM (doesFileExist . fullPath) allContents
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
base64md5File fp = trace "XXXX base64md5File" $ do
    hashed <- hashFile fp
    let encoded = trace ("XXXX base64md5File hashed=" ++ show hashed) $ encode hashed
    return $ trace "XXXX base64md5File return" $ base64 encoded
    where encode d = ByteArray.convert (d :: Digest MD5)


base64 :: S.ByteString -> String
base64 = trace "XXXX base64" $ map tr
       . (trace "XXXX base64 - take" $ take 8)
       . (trace "XXXX base64 - unpack" $ S8.unpack)
       . (trace "XXXX base64 - encode" $ Data.ByteString.Base64.encode)
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

pathFromRawPieces :: FilePath -> [String] -> FilePath
pathFromRawPieces = trace "XXXX pathFromRawPieces" $ 
    foldl' append
  where
    append a b = a ++ '/' : b

notHidden :: FilePath -> Bool
notHidden "tmp" = False
notHidden s =
    case s of
        '.':_ -> False
        _ -> True

-- | A 'Sink' that hashes a stream of 'B.ByteString'@s@ and
-- creates a digest @d@.
sinkHash :: (Monad m, HashAlgorithm hash) => Consumer S.ByteString m (Digest hash)
sinkHash = trace "XXXX sinkHash" $ sink Hash.hashInit
  where sink ctx = do
            b <- trace "XXXX sinkHash - await" $ await
            case b of
                Nothing -> trace "XXXX sinkHash Nothing" $ return $! Hash.hashFinalize ctx
                Just bs -> trace "XXXX! sinkHash Just" $ sink $! Hash.hashUpdate ctx bs

-- | Hashes the whole contents of the given file in constant
-- memory.  This function is just a convenient wrapper around
-- 'sinkHash' defined as:
--
-- @
-- hashFile fp = 'liftIO' $ 'runResourceT' ('sourceFile' fp '$$' 'sinkHash')
-- @
hashFile :: (MonadIO m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile fp = liftIO $ trace "XXXX hashFile" $ runResourceT (sourceFile fp $$ sinkHash)

hashFile' :: (MonadIO m, HashAlgorithm hash) => FilePath -> m (Digest hash)
hashFile' fp = do
    contents <- liftIO $ trace "XXXX hashFile" $ S.readFile fp
    return $ Hash.hash  contents
