# offset
provide a `offsetof` function for Storable in haskell 

## example
``` hs
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Shader
  ( module Shader
  )
  where

import Linear (V2 (..), V3 (..))
import GHC.Generics (Generic)
import Foreign.Storable.Generic (GStorable)

import Foreign.Storable.Offset

data ShaderInputVertex = ShaderInputVertex
  { inPosition :: !(V2 Float)
  , inColor :: !(V3 Float)
  } deriving (Generic, GStorable)

makeOffset ''ShaderInputVertex
```

```
λ> offsetof (undefined::ShaderInputVertex) (Normal 0)
0
λ> offsetof (undefined::ShaderInputVertex) (Normal 1)
8
λ> offsetof (undefined::ShaderInputVertex) (Record "inPosition")
0
λ> offsetof (undefined::ShaderInputVertex) (Record "inColor")
8
```
