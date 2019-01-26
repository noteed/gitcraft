-- | This script generates an example SVG file. (The beginning and end of the
-- generated file are copied verbatim from git.svg.)
{-# LANGUAGE RecordWildCards #-}
module Gitcraft where

import qualified Data.Map as M


--------------------------------------------------------------------------------
main = do
  template <- readFile "git.svg"
  let Repository{..} = repository
      commits' = zip commits (findParents rCommits)
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat
        [ header
        , ["<text text-anchor=\"start\" x=\"" ++ show 20 ++ "\" y=\"" ++ show 20 ++ "\""
        , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
        , rName ++ ".git"
        , "</text>"]
        , map (renderCommit rSelection ("00000003", "master")) rCommits
        , map (uncurry renderArcs) commits'
        , footer
        ])
  writeFile "example.svg" content

--------------------------------------------------------------------------------
repository = Repository "example" commits selection

commits =
  [ Commit "00000003" ["00000002"] (2, 0)
  , Commit "00000002" ["00000001"] (1, 1)
  , Commit "00000001" []           (1, 2)

  , Commit "00000004" ["00000002"] (2, 0)

  , Commit "00000005" ["00000002"] (3, 0)

  , Commit "00000007" ["00000002"] (0, 0)
  ]

selection = "00000002"


--------------------------------------------------------------------------------
data Repository = Repository
  { rName :: String
  , rCommits :: [Commit]
  , rSelection :: Sha1
  }


--------------------------------------------------------------------------------

-- | Commit ID, parent IDs, (x, y) position.
data Commit = Commit
  { cId :: Sha1
  , cParents :: [Sha1]
  , cPosition :: (Int, Int)
  }
  deriving Show

type Sha1 = String


--------------------------------------------------------------------------------
(marginx, marginy) = (60, 60)
spacingx = 60
spacingy = 60

--------------------------------------------------------------------------------

-- | Map each commit to a list of its parents.
findParents :: [Commit] -> [[Commit]]
findParents cs = map f cs
  where
  f (Commit _ ps _) = filter ((`elem` ps) . cId) cs

renderCommit selection refs (Commit sha1 ps (x, y)) =
  "<use xlink:href=\"#" ++
  xlink ++
  "\" x=\"" ++
  renderx x ++
  "\" y=\"" ++
  rendery y ++
  "\" />" ++ labels
  where
  xlink = case ps of
    [] -> "root-commit"
    _ | selection == sha1 -> "selected-commit"
    _ -> "commit"
  labels = concatMap label (if sha1 == fst refs then [snd refs] else [])
  label r = concat
    [ "<text text-anchor=\"end\" x=\""
    , show (spacingx * x + marginx - 20)
    , "\" y=\""
    , show (spacingy * y + marginy + 4)
    , "\""
    , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">master</text>"
    ]

-- | Render arcs from c to cs (normally those are the parents of c).
renderArcs c cs = unlines (map (renderArc c) cs)

-- Arc going to upper-right.
renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) | x1 > x2 = concat
  [ "<path d=\"M" ++ show x2' ++ "," ++ show (y2' - 7)
  , " Q" ++ show x2' ++ "," ++ show (y2' - 30)
  , " " ++ show (x2' + 30) ++ "," ++ show (y2' - 30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  , " <line stroke=\"blue\" x1=\"" ++ show (x2' + 30) ++ "\" y1=\"" ++ show (y2' - 30) ++ "\" x2=\"" ++ show (x1' - 30) ++ "\" y2=\"" ++ show (y1' + 30) ++ "\" />"
  , " <path d=\"M" ++ show x1' ++  "," ++ show (y1' + 7)
  , " Q" ++ show x1' ++ "," ++ show (y1' + 30)
  , " " ++ show (x1' - 30) ++ "," ++ show (y1' + 30) ++ "\""
  , " fill=\"none\" stroke=\"blue\" />"
  ]
  where
  x1' = spacingx * x1 + marginx
  y1' = spacingy * y1 + marginy
  x2' = spacingx * x2 + marginx
  y2' = spacingy * y2 + marginy

-- Arc going to upper-left.
renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) | x1 < x2 = concat
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
  x1' = spacingx * x1 + marginx
  y1' = spacingy * y1 + marginy
  x2' = spacingx * x2 + marginx
  y2' = spacingy * y2 + marginy

-- Vertical arc.
renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) =
  "<line stroke=\"blue\" x1=\"" ++ show x1' ++ "\" y1=\"" ++ show (y1' + 7) ++ "\" x2=\"" ++ show x2' ++ "\" y2=\"" ++ show (y2' - 7) ++ "\" />"
  where
  x1' = spacingx * x1 + marginx
  y1' = spacingy * y1 + marginy
  x2' = spacingx * x2 + marginx
  y2' = spacingy * y2 + marginy

renderx x = show (spacingx * x + marginx)
rendery y = show (spacingy * y + marginy)
