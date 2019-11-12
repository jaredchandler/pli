--  , __           _                       _                  _                                        
-- /|/  \         | |                   \_|_)  o             | |           |           o               
--  |___/     _|_ | |     __   _  _       |        , _|_     | | _  _    __|   _           _  _    __, 
--  |    |   | |  |/ \   /  \_/ |/ |     _|    |  / \_|    _ |/ / |/ |  /  |  |/  /\/  |  / |/ |  /  | 
--  |     \_/|/|_/|   |_/\__/   |  |_/  (/\___/|_/ \/ |_/  \_/\/  |  |_/\_/|_/|__/ /\_/|_/  |  |_/\_/|/
--          /|                                                                                      /| 
--          \|                                                                                      \| 
-- ##### Usage ##################################################################
-- You may have to run this: stack install haskell-src-meta
-- Then launch: stack repl

-- ##### Now we have to turn on quasiquotes in the repl #########################
-- > :set -XQuasiQuotes
-- > :set -XTemplateHaskell
-- > :l Pli.hs 
-- [1 of 1] Compiling Pli              ( Pli.hs, interpreted )
-- Ok, one module loaded.

-- ##### Now things get awesome #################################################
-- > let list = [1,2,3,4,5,6,7,8,9]
-- > [pli| list[4] |]
-- 5
-- > [pli| list[-3] |]
-- 7
-- > [pli| list[-3:] |]
-- [7,8,9]
-- > [pli| list[:5] |]
-- [1,2,3,4,5]
-- > [pli| list[2:5] |]
-- [3,4,5]



{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Pli where


import Text.Parsec
import Control.Monad.Identity
import qualified Language.Haskell.Meta.Parse as LHM
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Control.Monad
import qualified Control.Monad.Fail as F
import Debug.Trace
--import Prelude hiding (fail)


-- Here's the data type for my list forms... the parser will output one of these

data PliExp =
     PLIELEM Exp Exp         -- list[i]
   | PLILIST Exp Exp Exp     -- list[i:j]
   | PLILISTLO Exp Exp       -- list[i:]
   | PLILISTHI Exp Exp       -- list[:j]
   deriving (Show,Eq)


-- This looks fancy and scary. Like a shark dipped in glitter. 

type Parser a =
  ParsecT       -- ParsecT is a monad transformer i.e. a Monad parameterized by another
   String       -- the stream we will be consuming monadically is a string
   ()           -- we do not need to get or put any state to parse arithmetic
   Identity     -- let the monad we are parameterized by (evaluate to) be id
   a            -- To be a monad we still need kind :: * -> *



-- a combinator that tries the first parser, and if it failes, does not consume
-- any input and then tries the next parser
-- note, reliance on this combinator may make debugging ambiguity difficult
--
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = (try p1) <|> p2


-- Because it wouldn't be science if things always worked
failEither :: (F.MonadFail m) => Either String a -> m a
failEither (Left err) = F.fail err
failEither (Right a)  = return a


-- This tries to turn a string into a valid haskell 
parseExp :: String -> Parser Exp
parseExp = failEither . LHM.parseExp 



parsePLIELEM :: Parser PliExp 
parsePLIELEM = 

  -- list[i]
  let parseElem = do
        spaces
        -- We are getting a bunch of chars and then
        l <- (manyTill anyChar (string "["))
        -- We are trying to parse them as a haskell expression
        -- This will be the list expression
        e <- (parseExp l)
        --traceM "parsingElem" --Matt said this helps to debug whats happening
        spaces

        -- This expression will be the index expression
        l <- (manyTill anyChar (string "]"))
        i <- (parseExp l)
        -- Now we create an instance of our IR datatype
        return (PLIELEM e i)

      --list[i:]
      parseListLo = do
        spaces

        l <- (manyTill anyChar (string "["))
        e <- (parseExp l)

        spaces

        l <- (manyTill anyChar (string ":"))
        i <- (parseExp l)
        spaces
        string "]"
        return (PLILISTLO e i )

      --list[:j]
      parseListHi = do
        spaces

        l <- (manyTill anyChar (string "["))
        e <- (parseExp l)

        spaces
        
        string ":"
        
        l <- (manyTill anyChar (string "]"))
        j <- (parseExp l)

        return (PLILISTHI e j)  

      -- list[i:j]
      parseList = do
        spaces

        l <- (manyTill anyChar (string "["))
        e <- (parseExp l)
 
        spaces

        l <- (manyTill anyChar (string ":"))
        i <- (parseExp l)
        
        l <- (manyTill anyChar (string "]"))
        j <- (parseExp l)

        return (PLILIST e i j)

     in parseListHi <||> parseListLo <||> parseList <||> parseElem

-- Cause everything's gotta handle failure... 
parsePLI :: String -> PliExp
parsePLI s = 
  case runParser parsePLIELEM () "" s of
    Left errMsg -> error (show errMsg)
    Right e     -> e

 -- This takes the PLI List expressions emitted by the parser and makes them template haskell
 -- I do a bunch of dumb stuff to handle negatie indicies
 -- It's simpler without worrying about them.
 -- Look in the comments to see the simpler versions
genPLI :: PliExp -> Q Exp
genPLI e = case e of

  -- list[i]
  (PLIELEM l i) ->  let fi = [| (if $(return i) < 0 then ((length $(return l)) + ($(return i)) ) else $(return i) )|] in
                      [| ($(return l) !! $(fi)) |]
                      -- The FI thing up top is for handling negative indices.
                      -- If you just wanted the simple verison:
                      -- [| ($(return l) !! $(return i))|] which becomes: (listname !! index)
  -- list[i:j]
  (PLILIST l i j ) -> let fi = [| (if $(return i) < 0 then ((length $(return l)) + ($(return i)) ) else $(return i) )|] in
                      let fj = [| (if $(return j) < 0 then ((length $(return l)) + ($(return j)) ) else $(return j) )|] in
                      [| (drop $(fi) (take $(fj) $(return l))) |]
                      -- Simple Version
                      -- [| (drop $(return i) (take $(return j) $(return l))) |] which becomes: (drop i (take j list))

  -- list[i:]
  (PLILISTLO l i ) -> let fi = [| (if $(return i) < 0 then ((length $(return l)) + ($(return i)) ) else $(return i) )|] in
                      [| (drop $(fi) $(return l)) |]
                      -- Simple Version
                      -- [| (drop $(return i) $(return l) |] which becomes: (drop i list)
  -- list[:j]
  (PLILISTHI l j ) -> let fj = [| (if $(return j) < 0 then ((length $(return l)) + ($(return j)) ) else $(return j) )|] in
                      [|  (take $(fj) $(return l)) |]
                      -- Simple Version
                      -- [| (take $(return j) $(return l) |] which becomes: (take j list)

-- This says what to hand the string encompassed by the [pli| string goes here| ] to. 
-- First ParsePLI, which is composed with genPli
-- String --> parsePLI (emits a PliExp) --> genPLI (emits template haskell) --> Quasiquoter splices back in.
pli :: QuasiQuoter
pli = QuasiQuoter {
         quoteExp  = genPLI . parsePLI -- ##### We only use this one because we are only operating over expressions ###
        , quotePat = undefined -- quotePat :: String -> Q Pat
        , quoteDec = undefined -- quoteDec :: String -> Q Dec
        , quoteType = undefined -- quoteType :: String -> Q Type
}

-- ##### Usage ##################################################################
-- > :l Pli.hs 
-- [1 of 1] Compiling Pli              ( Pli.hs, interpreted )
-- Ok, one module loaded.
-- ##### Now we have to turn on quasiquotes in the repl #########################
-- > :set -XQuasiQuotes
-- > :set -XTemplateHaskell


