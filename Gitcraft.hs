-- | This script generates an example SVG file. (The beginning and end of the
-- generated file are copied verbatim from git.svg.)
{-# LANGUAGE RecordWildCards #-}
module Gitcraft where

import Data.List (intersperse)


--------------------------------------------------------------------------------
main = do
  template <- readFile "git.svg"
  let Repository{..} = repository
      Options{..} = options
      commits' = zip commits (findParents rCommits)
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat $
        [ header
        , title rName
        , map (renderCommit repository options) rCommits
        , map (uncurry (renderArcs options)) commits'
        ] ++ map note oNotes ++
        [ footer
        ])
  writeFile "example.svg" content

--------------------------------------------------------------------------------
repository :: Repository
repository = Repository "example" commits selection refs

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

selection = "00000003"

refs =
  [ ("00000007", ["hotfix"])
  , ("00000008", ["master"])
  , ("00000003", ["develop"])
  , ("00000005", ["feature"])
  ]

options :: Options
options = Options notes 160 60 80 60

notes =
  [ Note "git checkout -b feature develop" (560, 210)
  ]

--------------------------------------------------------------------------------
data Repository = Repository
  { rName :: String
  , rCommits :: [Commit]
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
  { oNotes :: [Note]
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
