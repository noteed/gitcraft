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

render template (r, o) = do
  let Repository{..} = r
      Options{..} = o
      commits' = zip commits (findParents rCommits)
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat $
        [ header
        , title oName
        ] ++ map (uncurry (column o)) oColumns ++
        [ map (renderCommit r o) rCommits
        , map (uncurry (renderArcs o)) commits'
        ] ++ map note oNotes ++
        [ footer
        ])
  writeFile (oName ++ ".svg") content


--------------------------------------------------------------------------------
example1 = (repository1, options1)

example2 = (repository2, options2)


--------------------------------------------------------------------------------
repository1 :: Repository
repository1 = Repository commits selection refs

repository2 = Repository commits selection []

commits =
  [
    -- hotfix
    Commit "00000007" ["00000002"] (0, 3)

    -- master
  , Commit "00000008" ["00000002", "00000007"] (1, 2)
  , Commit "00000002" ["00000001"] (1, 4)
  , Commit "00000001" []           (1, 5)

    -- develop
  , Commit "00000003" ["00000002", "00000008"] (2, 1)

    -- feature
  , Commit "00000005" ["00000002"] (3, 0)
  ]

selection = "00000005"

refs =
  [ ("00000007", ["hotfix"])
  , ("00000008", ["master"])
  , ("00000003", ["develop"])
  , ("00000005", ["feature"])
  ]

options1 :: Options
options1 = Options "example-1" notes [] 160 60 80 120

notes =
  [ Note "git checkout -b feature develop" (560, 210)
  ]

options2 = Options "example-2" [] columns 80 60 80 120

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
  }

data Note = Note String (Int, Int)


--------------------------------------------------------------------------------

-- | Map each commit to a list of its parents.
findParents :: [Commit] -> [[Commit]]
findParents cs = map f cs
  where
  f (Commit _ ps _) = filter ((`elem` ps) . cId) cs

renderCommit Repository{..} o (Commit sha1 ps (x, y)) =
  "<use xlink:href=\"#" ++ xlink ++ "\" " ++ renderxy o x y  ++ " />" ++ labels
  where
  xlink = case ps of
    [] -> "root-commit"
    _ | rSelection == sha1 -> "selected-commit"
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
renderArc Options{..} (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) | x1 > x2 = concat
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
renderArc Options{..} (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) | x1 < x2 = concat
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
renderArc Options{..} (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) =
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
  [ "<rect x=\"422\" y=\"194\" width=\"280\" height=\"22\""
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
