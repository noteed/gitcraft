-- | This script generates an example SVG file. (The beginning and end of the
-- generated file are copied verbatim from git.svg.)
{-# LANGUAGE RecordWildCards #-}
module Gitcraft where

import Control.Monad.State
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Digest.Pure.SHA as SHA
import Data.List (intersperse)
import System.Environment (getArgs)


--------------------------------------------------------------------------------
main = do
  args <- getArgs
  case args of
    -- Beginning of State monad -based code, to be replaced by Operational.
    ["eval"] -> renderRepository (evalState (ops script) (fst emptyRepository))
    -- Render operations on a reposiory as actual Git commands.
    ["ops"] -> mapM_ (putStrLn . git) script
    -- Show the same thing as git-state.sh.
    ["state", "0"] -> renderRepository (fst emptyRepository)
    ["state", "1"] -> renderRepository (fst initialRepository)
    ["state", "2"] -> renderRepository (fst secondRepository)
    -- Default case, render the images for the slide deck.
    _ -> do
      template <- readFile "git.svg"
      render template example1
      render template example2
      render template emptyRepository
      render template initialRepository
      render template secondRepository

render template (r, o) = do
  let Repository{..} = r
      Options{..} = o
      commits' = zip rCommits (findParents rCommits)
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat $
        [ header
        , title oName
        ] ++ map (uncurry (column r o)) oColumns ++
        [ map (renderCommit r o) rCommits
        , map (uncurry (renderArcs o)) commits'
        ] ++ map note oNotes ++
        (if oCommitLines then map (commitLine o) rCommits else []) ++
        [ footer
        ])
  writeFile ("docs/images/" ++ oName ++ ".svg") content


--------------------------------------------------------------------------------
data Op =
    OpCheckout String
  | OpCommit String

git op = case op of
  OpCheckout name -> "git checkout " ++ name
  OpCommit msg -> "git commit -m'" ++ msg ++ "' --allow-empty"

op (OpCheckout name) = checkout name
op (OpCommit msg) = commit msg

ops :: [Op] -> State Repository Repository
ops [] = get
ops (x:xs) = op x >> ops xs

script :: [Op]
script =
  [ OpCheckout "master"
  , OpCommit "Initial commit."
  , OpCommit "Add LICENSE, build file."
  ]

checkout :: String -> State Repository ()
checkout name = do
  modify (\r -> r { rHead = Ref ("refs/heads/" ++ name) })

commit :: String -> State Repository ()
commit msg = do
  r@Repository{..} <- get
  let (parents, mkHead) = nextHead r
      c = sha1Commit (Commit "" parents (5, 5) msg)
  put r { rCommits = rCommits ++ [c]
        , rHead = mkHead c
        , rRefs = if null rRefs then [("refs/heads/master", cId c)] else rRefs
        }

sha1Commit c =
  let content_ = unlines (catCommit c)
      content = "commit " ++ show (length content_) ++ "\0" ++ content_
      sha1 = SHA.showDigest (SHA.sha1 (pack content))
  in  c { cId = sha1 }

-- | Compute the current commit (i.e. parent of next commit) and new head function.
nextHead Repository{..} = case rHead of
  Ref r -> case lookup r rRefs of
    Nothing -> ([], const rHead)
    Just p -> ([p], const rHead)
  Detached p -> ([p], Detached . cId)

--------------------------------------------------------------------------------
example1 = (repository1, options1)

example2 = (repository2, options2)

emptyRepository =
  ( Repository [] [] initialHead
  , Options "empty" [] [("master", 1)] 80 60 30 120 True
  )

initialRepository =
  ( Repository [commit0]
      [("refs/heads/master", cId commit0)] initialHead
  , Options "initial" [] [("master", 1)] 80 60 30 120 True
  )

secondRepository =
  ( Repository [commit0, commit1]
      [("refs/heads/master", cId commit1)] initialHead
  , Options "second" [] [("master", 1)] 80 60 30 120 True
  )

commit0 = sha1Commit (Commit undefined [] (1, 5)
  "Initial commit.")

commit1 = sha1Commit (Commit undefined [cId commit0] (1, 4)
  "Add LICENSE, build file.")


--------------------------------------------------------------------------------
repository1 :: Repository
repository1 = Repository
  { rCommits = commits
  , rRefs = refs
  , rHead = (Ref "refs/heads/feature")
  }

repository2 :: Repository
repository2 = Repository
  { rCommits = commits
  , rRefs = refs
  , rHead = (Ref "refs/heads/feature")
  }

commits =
  [
    -- hotfix
    Commit "d20e93f" ["6a8975717fcc5e312a12041267c66513db13ae66"] (0, 3)
      "Fix build."

    -- master
  , Commit "9f52c1e" ["6a8975717fcc5e312a12041267c66513db13ae66", "d20e93f"] (1, 2)
      "Merge hotfix in master."
  , commit1
  , commit0

    -- develop
  , Commit "f12efbc" ["6a8975717fcc5e312a12041267c66513db13ae66", "9f52c1e"] (2, 1)
      "Merge hotfix in develop (via master)."

    -- feature
  , Commit "a0a0a50" ["6a8975717fcc5e312a12041267c66513db13ae66"] (3, 0)
      "CRFT-0001 Start blue dot feature."
  ]

selection = "a0a0a50"

refs =
  [ ("refs/heads/hotfix", "d20e93f")
  , ("refs/heads/master", "9f52c1e")
  , ("refs/heads/develop", "f12efbc")
  , ("refs/heads/feature", "a0a0a50")
  ]

options1 :: Options
options1 = Options "example-1" notes [] 160 60 80 120 False

notes =
  [ Note "git checkout -b feature develop" (560, 250)
  ]

options2 :: Options
options2 = Options "example-2" [] columns 80 60 30 120 True

columns =
  [ ("hotfix", 0)
  , ("master", 1)
  , ("develop", 2)
  , ("feature", 3)
  ]


--------------------------------------------------------------------------------
data Repository = Repository
  { rCommits :: [Commit]
  , rRefs :: [(String, Sha1)]
  , rHead :: Head
  }
  deriving Show

renderRepository = mapM_ putStrLn . showRepository

-- | The output of this function should be the same as git-state.sh.
showRepository Repository{..} =
  showHead rHead :
  concatMap showRef rRefs ++
  map showBranch rRefs ++
  concat (intersperse [""] (map showCommit (reverse rCommits)))

showRef (r, s) = [ ".git/" ++ r, s ]

showBranch (r, _) = "* " ++ drop 11 r

-- | Commit ID, parent IDs, (x, y) position.
data Commit = Commit
  { cId :: Sha1
  , cParents :: [Sha1]
  , cPosition :: (Int, Int)
  , cMessage :: String
  }
  deriving Show

-- | Equivalent to git log -1.
-- Such commits can be created with
--   GIT_COMMITTER_DATE="1970-01-01T00:00:00"
--   git commit -m'MESSAGE' --allow-empty --date="1970-01-01T00:00:00"
showCommit Commit{..} =
  [ "commit " ++ cId
  , "Author: Your Name <you@example.com>"
  , "Date:   Thu Jan 1 00:00:00 1970 +0000"
  , ""
  , "    " ++ cMessage
  ]

-- | Equivalent to git cat-file commit.
catCommit Commit{..} =
  [ "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904" -- Empty tree SHA1.
  ] ++ map ("parent " ++) cParents ++
  [ "author Your Name <you@example.com> 0 +0000"
  , "committer Your Name <you@example.com> 0 +0000"
  , ""
  , cMessage
  ]

-- | .git/HEAD
data Head =
    Ref String -- ^ Points to a branch
  | Detached Sha1 -- ^ Points to a commit
  deriving (Eq, Show)

-- | Equivalent to cat .git/HEAD.
showHead (Ref r) = "ref: " ++ r
showHead (Detached sha1) = sha1

-- | The value of .git/HEAD when creating a new repository.
initialHead = Ref "refs/heads/master"

type Sha1 = String


--------------------------------------------------------------------------------
data Options = Options
  { oName :: String
  , oNotes :: [Note]
  , oColumns :: [(String, Int)]
  , oSpacingX :: Int
  , oSpacingY :: Int
  , oMarginX :: Int
  , oMarginY :: Int
  , oCommitLines :: Bool
  }

data Note = Note String (Int, Int)


--------------------------------------------------------------------------------

-- | Map each commit to a list of its parents.
findParents :: [Commit] -> [[Commit]]
findParents cs = map f cs
  where
  f (Commit _ ps _ _) = filter ((`elem` ps) . cId) cs

renderCommit r@Repository{..} o (Commit sha1 ps (x, y) _) =
  "<use xlink:href=\"#" ++ xlink ++ "\" " ++ renderxy o x y  ++ " />" ++ labels
  where
  xlink = case ps of
    _ | fst (nextHead r) == [sha1] -> "selected-commit"
    [] -> "root-commit"
    _ -> "commit"
  -- Display lables only if we don't display columns.
  labels = if null (oColumns o) then concatMap label rRefs else []
  label (r, s) | sha1 == s = concat
    [ "<text text-anchor=\"end\" "
    , renderxy' o x y  (-20) 4
    , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
    , drop 11 r
    , "</text>"
    ]
  label _ = []

-- | Render arcs from c to cs (normally those are the parents of c).
renderArcs o c cs = unlines (map (renderArc o c) cs)

-- Arc going to upper-right.
renderArc Options{..} (Commit _ _ (x1, y1) _) (Commit _ _ (x2, y2) _) | x1 > x2 = concat
  [ "<path d=\"M" ++ show x2' ++ "," ++ show (y2' - 7)
  , " Q" ++ show x2' ++ "," ++ show (y2' - 30)
  , " " ++ show (x2' + 30) ++ "," ++ show (y2' - 30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  -- TODO Draw this horizontal part only if it is non-zero.
  , " <line stroke=\"blue\" x1=\"" ++ show (x2' + 30) ++ "\" y1=\"" ++ show (y2' - 30) ++ "\" x2=\"" ++ show (x1' - 30) ++ "\" y2=\"" ++ show (y2' - oSpacingY + 30) ++ "\" />"
  , " <path d=\"M" ++ show x1' ++  "," ++ show (y2' - oSpacingY + 7)
  , " Q" ++ show x1' ++ "," ++ show (y2' - oSpacingY + 30)
  , " " ++ show (x1' - 30) ++ "," ++ show (y2' - oSpacingY + 30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  -- TODO Draw this vertical part only if it is non-zero.
  , " <line stroke=\"blue\" x1=\"" ++ show x1' ++ "\" y1=\"" ++ show (y1'+ 7) ++ "\" x2=\"" ++ show x1' ++ "\" y2=\"" ++ show (y2' - oSpacingY + 7) ++ "\" />"
  ]
  where
  x1' = oSpacingX * x1 + oMarginX
  y1' = oSpacingY * y1 + oMarginY
  x2' = oSpacingX * x2 + oMarginX
  y2' = oSpacingY * y2 + oMarginY

-- Arc going to upper-left.
renderArc Options{..} (Commit _ _ (x1, y1) _) (Commit _ _ (x2, y2) _) | x1 < x2 = concat
  [ "<path d=\"M" ++ show x2' ++ "," ++ show (y2' - 7)
  , " Q" ++ show x2' ++ "," ++ show (y2' - 30)
  , " " ++ show (x2' - 30) ++ "," ++ show (y2' -30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  , " <line stroke=\"blue\" x1=\"" ++ show (x2' - 30) ++ "\" y1=\"" ++ show (y2' - 30) ++ "\" x2=\"" ++ show (x1' + 30) ++ "\" y2=\"" ++ show (y1' + 30) ++ "\" />"
  , " <path d=\"M" ++ show x1' ++  "," ++ show (y1' + 7)
  , " Q" ++ show x1' ++ "," ++ show (y1' + 30)
  , " " ++ show (x1' + 30) ++ "," ++ show (y1' + 30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  ]
  where
  x1' = oSpacingX * x1 + oMarginX
  y1' = oSpacingY * y1 + oMarginY
  x2' = oSpacingX * x2 + oMarginX
  y2' = oSpacingY * y2 + oMarginY

-- Vertical arc.
renderArc Options{..} (Commit _ _ (x1, y1) _) (Commit _ _ (x2, y2) _) =
  "<line stroke=\"blue\" x1=\"" ++ show x1' ++ "\" y1=\"" ++ show (y1' + 7) ++ "\" x2=\"" ++ show x2' ++ "\" y2=\"" ++ show (y2' - 7) ++ "\" />"
  where
  x1' = oSpacingX * x1 + oMarginX
  y1' = oSpacingY * y1 + oMarginY
  x2' = oSpacingX * x2 + oMarginX
  y2' = oSpacingY * y2 + oMarginY

renderxy Options{..} x y =
  "x=\"" ++ show (oSpacingX * x + oMarginX) ++
  "\" y=\"" ++ show (oSpacingY * y + oMarginY) ++
  "\""

renderxy' Options{..} x y dx dy =
  "x=\"" ++ show (oSpacingX * x + oMarginX + dx) ++
  "\" y=\"" ++ show (oSpacingY * y + oMarginY + dy) ++
  "\""

title name =
  [ "<text text-anchor=\"start\" x=\"" ++ show 5 ++ "\" y=\"" ++ show 20 ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , name ++ ".git"
  , "</text>"
  ]

note (Note str (x, y)) =
  [ "<rect x=\"422\" y=\"234\" width=\"280\" height=\"22\""
  , "fill=\"#f0f0f0\" fill-opacity=\"0.9\" />"
  , "<text text-anchor=\"middle\" x=\"" ++ show x ++ "\" y=\"" ++ show y ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , str
  , "</text>"
  ]

column Repository{..} Options{..} name x =
  [ "<line stroke=\"#e0e0e0\" "
  , " x1=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y1=\"" ++ show (oMarginY `div` 2 + 5) ++ "\""
  , " x2=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y2=\"" ++ show 500 ++ "\""
  , " stroke-dasharray=\"5\" />"
  , "<text text-anchor=\"middle\" x=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y=\"" ++ show (oMarginY `div` 2) ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , name ++ (if Ref ("refs/heads/" ++ name) == rHead then "*" else "")
  , "</text>"
  ]

line Options{..} name y =
  [ "<text text-anchor=\"start\" x=\"" ++ show (oSpacingX * 4 + oMarginX - (oSpacingX `div` 2)) ++ "\" y=\"" ++ show (oSpacingY * y + oMarginY + 4) ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , name
  , "</text>"
  ]

commitLine o (Commit sha1 ps (_, y) msg) = line o (take 7 sha1 ++ " " ++ msg) y
