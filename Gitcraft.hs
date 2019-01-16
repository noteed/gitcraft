-- | This script generates an example body for the SVG file. (The body should
-- be preceded by the header and followed by the footer, both marked clearly in
-- the example git.svg file.)
module Gitcraft where


--------------------------------------------------------------------------------
main = do
  let commits' = zip commits (findParents commits)
      selection = "00000002"
  mapM_ (putStrLn . renderCommit selection) commits
  mapM_ (putStrLn . uncurry renderArcs) commits'

commits =
  [ Commit "00000003" ["00000002"] (1, 0)
  , Commit "00000002" ["00000001"] (1, 1)
  , Commit "00000001" []           (1, 2)

  , Commit "00000004" ["00000002"] (2, 0)

  , Commit "00000005" ["00000002"] (3, 0)

  , Commit "00000006" ["00000002"] (4, 0)

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

renderCommit selection (Commit sha1 ps (x, y)) =
  "<use xlink:href=\"#" ++
  xlink ++
  "\" x=\"" ++
  renderx x ++
  "\" y=\"" ++
  rendery y ++
  "\" />"
  where
  xlink = case ps of
    [] -> "root-commit"
    _ | selection == sha1 -> "selected-commit"
    _ -> "commit"

-- | Render arcs from c to cs (normally those are the parents of c).
renderArcs c cs = unlines (map (renderArc c) cs)

renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) | x1 > x2 = concat
  [ "<path d=\"M" ++ show x2' ++ "," ++ show (y2' - 7)
  , " A30,30 0 0,1 " ++ show (x2' + 30) ++ "," ++ show (y2' - 30) ++"\""
  , " style=\"stroke: blue; fill:none;\"/>"
  , " <line stroke=\"blue\" x1=\"" ++ show (x2' + 30) ++ "\" y1=\"" ++ show (y2' - 30) ++ "\" x2=\"" ++ show (x1' - 30) ++ "\" y2=\"" ++ show (y1' + 30) ++ "\" />"
  , " <path d=\"M" ++ show (x1' - 30) ++  "," ++ show (y1' + 30)
  , " A30,30 0 0,0 " ++ show x1' ++ "," ++ show (y1' + 7) ++ "\""
  , " style=\"stroke: blue; fill:none;\"/>"
  ]
  where
  x1' = 60 * x1 + 40
  y1' = 60 * y1 + 40
  x2' = 60 * x2 + 40
  y2' = 60 * y2 + 40

renderArc (Commit _ _ (x1, y1)) (Commit _ _ (x2, y2)) =
  "<line stroke=\"blue\" x1=\"" ++ show x1' ++ "\" y1=\"" ++ show (y1' + 7) ++ "\" x2=\"" ++ show x2' ++ "\" y2=\"" ++ show (y2' - 7) ++ "\" />"
  where
  x1' = 60 * x1 + 40
  y1' = 60 * y1 + 40
  x2' = 60 * x2 + 40
  y2' = 60 * y2 + 40

renderx x = show (60 * x + 40)
rendery y = show (60 * y + 40)
