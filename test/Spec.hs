import Cube

import Data.Array.IArray

superflip :: Cube
superflip = array ((0,0,0), (2,2,2)) [ (location c, cubie c) | c <- cubies ]
  where
    cubies = [
      CubieEmbed { cubie=Edge G W, location=(1,0,0) },
      CubieEmbed { cubie=Edge O W, location=(0,0,1) },
      CubieEmbed { cubie=Edge B W, location=(1,0,2) },
      CubieEmbed { cubie=Edge R W, location=(2,0,1) },
      CubieEmbed { cubie=Corner W G O, location=(0,0,0) },
      CubieEmbed { cubie=Corner W O B, location=(0,0,2) },
      CubieEmbed { cubie=Corner W B R, location=(2,0,2) },
      CubieEmbed { cubie=Corner W R G, location=(2,0,0) },
      CubieEmbed { cubie=Edge O G, location=(0,1,0) },
      CubieEmbed { cubie=Edge O B, location=(0,1,2) },
      CubieEmbed { cubie=Edge R B, location=(2,1,2) },
      CubieEmbed { cubie=Edge R G, location=(2,1,0) },
      CubieEmbed { cubie=Edge G Y, location=(1,2,0) },
      CubieEmbed { cubie=Edge O Y, location=(0,2,1) },
      CubieEmbed { cubie=Edge B Y, location=(1,2,2) },
      CubieEmbed { cubie=Edge R Y, location=(2,2,1) },
      CubieEmbed { cubie=Corner Y O G, location=(0,2,0) },
      CubieEmbed { cubie=Corner Y B O, location=(0,2,2) },
      CubieEmbed { cubie=Corner Y R B, location=(2,2,2) },
      CubieEmbed { cubie=Corner Y G R, location=(2,2,0) },
      CubieEmbed { cubie=Center W, location=(1,0,1) },
      CubieEmbed { cubie=Center G, location=(1,1,0) },
      CubieEmbed { cubie=Center O, location=(0,1,1) },
      CubieEmbed { cubie=Center B, location=(1,1,2) },
      CubieEmbed { cubie=Center R, location=(2,1,1) },
      CubieEmbed { cubie=Center Y, location=(1,2,1) },
      CubieEmbed { cubie=Inside, location=(1,1,1) }
      ]

main :: IO ()
main = print $ getFace superflip Up
