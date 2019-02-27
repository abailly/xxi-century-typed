#!/bin/sh

# assumes ghcid is installed
ghcid -T 'mapM_ Test.Hspec.hspec [ Minilang.REPL.HaskelineSpec.spec, Minilang.ParserSpec.spec, Minilang.EvalSpec.spec, Minilang.NormalizeSpec.spec, Minilang.TypeSpec.spec ] '
