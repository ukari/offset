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

### Usage
```
λ> offsetof (undefined :: ShaderInputVertex) (0 :: Int)
0
λ> offsetof (undefined :: ShaderInputVertex) (1 :: Int)
8
λ> offsetof (undefined :: ShaderInputVertex) ("inPosition" :: String)
0
λ> offsetof (undefined :: ShaderInputVertex) ("inColor" :: String)
8
```

``` hs
offsetAt0 = offsetof (undefined :: ShaderInputVertex) 0

offsetAt1 = offsetof (undefined :: ShaderInputVertex) 1

offsetInPositon = offsetof (undefined :: ShaderInputVertex) "inPosition"

offsetInColor = offsetof (undefined :: ShaderInputVertex) "inColor"
```
