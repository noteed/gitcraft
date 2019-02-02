# Gitcraft

Features (more or less):

- Render hard-coded example repositories to SVG (the default when running
  Gitcraft.hs).
- Render hard-coded example repositories to text to compare their structure to
  actual Git repositories. Actual Git repositories can be rendered in a similar
  way with the `git-state.sh` script.
- Implement a State monad with some combinators to construct example
  repositories. This should use Operational instead to have multiple
  interpretations of commands (construct the in-memory repository, output actual
  Git commands, ...).
