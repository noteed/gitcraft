-- | This script generates an example SVG file. (The beginning and end of the
-- generated file are copied verbatim from git.svg.)
module Gitcraft where

--------------------------------------------------------------------------------
main = do
  template <- readFile "git.svg"
  let commits' = zip commits (findParents commits)
      selection = "00000002"
      header = takeWhile (/= "<!-- HEADER MARKER -->") (lines template)
      footer = tail (dropWhile (/= "<!-- FOOTER MARKER -->") (lines template))
      content = unlines (concat
        [ header
        , map (renderCommit selection ("00000003", "master")) commits
        , map (uncurry renderArcs) commits'
        , footer
        ])
  writeFile "example.svg" content


--------------------------------------------------------------------------------
commits =
  [ Commit "00000003" ["00000002"] (2, 0)
  , Commit "00000002" ["00000001"] (1, 1)
  , Commit "00000001" []           (1, 2)

  , Commit "00000004" ["00000002"] (2, 0)

  , Commit "00000005" ["00000002"] (3, 0)

  , Commit "00000007" ["00000002"] (0, 0)
  ]


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
    , show (60 * x + 20)
    , "\" y=\""
    , show (60 * y + 44)
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
  x1' = 60 * x1 + 40
  y1' = 60 * y1 + 40
  x2' = 60 * x2 + 40
  y2' = 60 * y2 + 40

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
  x1' = 60 * x1 + 40
  y1' = 60 * y1 + 40
  x2' = 60 * x2 + 40
  y2' = 60 * y2 + 40

-- Vertical arc.
renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) =
  "<line stroke=\"blue\" x1=\"" ++ show x1' ++ "\" y1=\"" ++ show (y1' + 7) ++ "\" x2=\"" ++ show x2' ++ "\" y2=\"" ++ show (y2' - 7) ++ "\" />"
  where
  x1' = 60 * x1 + 40
  y1' = 60 * y1 + 40
  x2' = 60 * x2 + 40
  y2' = 60 * y2 + 40

renderx x = show (60 * x + 40)
rendery y = show (60 * y + 40)
