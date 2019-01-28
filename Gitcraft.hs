-- | This script generates an example SVG file. (The beginning and end of the
-- generated file are copied verbatim from git.svg.)
{-# LANGUAGE RecordWildCards #-}
module Gitcraft where

import Data.List (intersperse)


--------------------------------------------------------------------------------
main = do
  template <- readFile "git.svg"
  render template example1
  render template example2
  render template emptyRepository

render template (r, o) = do
  let Repository{..} = r
      Options{..} = o
      commits' = zip rCommits (findParents rCommits)
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat $
        [ header
        , title oName
        ] ++ map (uncurry (column o)) oColumns ++
        [ map (renderCommit r o) rCommits
        , map (uncurry (renderArcs o)) commits'
        ] ++ map note oNotes ++
        (if oCommitLines then map (commitLine o) rCommits else []) ++
        [ footer
        ])
  writeFile (oName ++ ".svg") content


--------------------------------------------------------------------------------
example1 = (repository1, options1)

example2 = (repository2, options2)

emptyRepository =
  ( Repository [] [] []
  , Options "empty" [] [("master", 1)] 80 60 30 120 True
  )

--------------------------------------------------------------------------------
repository1 :: Repository
repository1 = Repository commits selection refs

repository2 :: Repository
repository2 = Repository commits selection []

commits =
  [
    -- hotfix
    Commit "d20e93f" ["22ed737"] (0, 3)
      "Fix build."

    -- master
  , Commit "9f52c1e" ["22ed737", "d20e93f"] (1, 2)
      "Merge hotfix in master."
  , Commit "22ed737" ["f0e40f6"] (1, 4)
      "Add LICENSE, build file."
  , Commit "f0e40f6" []           (1, 5)
      "Initial commit."

    -- develop
  , Commit "f12efbc" ["22ed737", "9f52c1e"] (2, 1)
      "Merge hotfix in develop (via master)."

    -- feature
  , Commit "a0a0a50" ["22ed737"] (3, 0)
      "CRFT-0001 Start blue dot feature."
  ]

selection = "a0a0a50"

refs =
  [ ("d20e93f", ["hotfix"])
  , ("9f52c1e", ["master"])
  , ("f12efbc", ["develop"])
  , ("a0a0a50", ["feature"])
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
  , rSelection :: Sha1
  , rRefs :: [(Sha1, [String])]
  }

-- | Commit ID, parent IDs, (x, y) position.
data Commit = Commit
  { cId :: Sha1
  , cParents :: [Sha1]
  , cPosition :: (Int, Int)
  , cMessage :: String
  }
  deriving Show

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

renderCommit Repository{..} o (Commit sha1 ps (x, y) _) =
  "<use xlink:href=\"#" ++ xlink ++ "\" " ++ renderxy o x y  ++ " />" ++ labels
  where
  xlink = case ps of
    _ | rSelection == sha1 -> "selected-commit"
    [] -> "root-commit"
    _ -> "commit"
  labels = concatMap label rRefs
  label (s, r) | sha1 == s = concat
    [ "<text text-anchor=\"end\" "
    , renderxy' o x y  (-20) 4
    , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
    , concat (intersperse "," r)
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

column Options{..} name x =
  [ "<line stroke=\"#e0e0e0\" "
  , " x1=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y1=\"" ++ show (oMarginY `div` 2 + 5) ++ "\""
  , " x2=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y2=\"" ++ show 500 ++ "\""
  , " stroke-dasharray=\"5\" />"
  , "<text text-anchor=\"middle\" x=\"" ++ show (oSpacingX * x + oMarginX) ++ "\" y=\"" ++ show (oMarginY `div` 2) ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , name
  , "</text>"
  ]

line Options{..} name y =
  [ "<text text-anchor=\"start\" x=\"" ++ show (oSpacingX * 4 + oMarginX - (oSpacingX `div` 2)) ++ "\" y=\"" ++ show (oSpacingY * y + oMarginY + 4) ++ "\""
  , " font-family=\"Mono\" font-size=\"14.00\" fill=\"black\">"
  , name
  , "</text>"
  ]

commitLine o (Commit sha1 ps (_, y) msg) = line o (sha1 ++ " " ++ msg) y
