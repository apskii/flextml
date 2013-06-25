{-# LANGUAGE ImplicitParams, ViewPatterns #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl')
import Text.XML.HXT.Core hiding (changeNode, mkTree, getChildren)
import Data.Tree.Class
import System.Environment
import Control.Applicative

type Vocab = M.Map String (S.Set String)

parseTagsAttrs :: String -> M.Map String (S.Set String)
parseTagsAttrs text = foldl' (flip insert) M.empty bindings
  where
    bindings = map (splitTagAttrs . words) (lines text)
    splitTagAttrs (tag : attrs) = (tag, attrs)
    insert (k, vs) = M.insert k (S.fromList vs)

prefixizeAttr :: (?vocab :: Vocab) => String -> XNode -> XNode
prefixizeAttr tagName attr@(XAttr (localPart -> attrName))
    | S.member attrName attrs = attr
    | otherwise               = attr'
  where
    attrs = maybe (?vocab M.! "*") id (M.lookup tagName ?vocab)
    attr' = XAttr (mkName ("data-" ++ attrName))

dataPrefixTr :: (?vocab :: Vocab) => XNode -> XNode
dataPrefixTr (XTag qTagName attrs) = XTag qTagName (map (changeNode (prefixizeAttr (localPart qTagName))) attrs)
dataPrefixTr xnode                 = xnode

ngElemToAttrTr :: (?tags :: S.Set String) => XNode -> XNode
ngElemToAttrTr tag@(XTag (localPart -> tagName) attrs)
  | S.member tagName ?tags = tag
  | otherwise              = XTag (mkName "div") (mkTree (XAttr (mkName tagName)) [] : attrs)
ngElemToAttrTr xnode       = xnode

main :: IO ()
main = do
  vocabFile : inputFile : outputFile : _ <- getArgs
  vocab <- parseTagsAttrs <$> readFile vocabFile
  input <- readFile inputFile
  let ?vocab = vocab
      ?tags  = M.keysSet vocab
  let document = readString [withParseHTML yes, withWarnings no] input
      pipeline = ngElemToAttrTr >>> dataPrefixTr
  output : _ <- runX $ do
    (document >>^ fmap pipeline)
              >>> writeDocumentToString [withOutputHTML]
  writeFile outputFile output