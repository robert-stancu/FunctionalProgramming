module NetpbmProcess where

import Netpbm
import qualified Greyscale as G
import qualified Color as C
import qualified Common as B

vflip :: Netpbm -> Netpbm
vflip (PGM w h m p) = PGM w h m (B.vflip p)
vflip (PPM w h m p) = PPM w h m (B.vflip p)

hflip :: Netpbm -> Netpbm
hflip (PGM w h m p) = PGM w h m (B.hflip p)
hflip (PPM w h m p) = PPM w h m (B.hflip p)

rot90r :: Netpbm -> Netpbm
rot90r (PGM w h m p) = PGM h w m (B.rot90r p)
rot90r (PPM w h m p) = PPM h w m (B.rot90r p)

rot90l :: Netpbm -> Netpbm
rot90l (PGM w h m p) = PGM h w m (B.rot90l p)
rot90l (PPM w h m p) = PPM h w m (B.rot90l p)

rot180l :: Netpbm -> Netpbm
rot180l (PGM w h m p) = PGM w h m (B.rot180l p)
rot180l (PPM w h m p) = PPM w h m (B.rot180l p)

rot180r :: Netpbm -> Netpbm
rot180r (PGM w h m p) = PGM w h m (B.rot180r p)
rot180r (PPM w h m p) = PPM w h m (B.rot180r p)


invert :: Netpbm -> Netpbm
invert (PGM w h m p) = PGM w h m (G.invert p)
invert (PPM w h m p) = PPM w h m (C.invert p)

crop :: (Int, Int, Int, Int) -> Netpbm -> Netpbm
crop f (PGM w h m p) = PGM nw nh m np
    where np = G.crop f p
          nh = length np
          nw = length . head $ np
crop f (PPM w h m p) = PPM nw nh m np
    where np = C.crop f p
          nh = length np
          nw = length . head $ np

scale :: (Float, Float) -> Netpbm -> Netpbm
scale f (PGM w h m p) = PGM nw nh m np
    where np = B.scale f p
          nh = length np
          nw = length . head $ np
scale f (PPM w h m p) = PPM nw nh m np
    where np = B.scale f p
          nh = length np
          nw = length . head $ np

brightness :: Int -> Netpbm -> Netpbm
brightness x (PGM w h m p) = PGM w h m (G.brightness x p)
brightness x (PPM w h m p) = PPM w h m (C.brightness x p)

mask :: Netpbm -> Netpbm -> Netpbm
mask (PBM _ _ msk) (PGM w h m p) = PGM w h m (G.mask msk p)
mask (PBM _ _ msk) (PPM w h m p) = PPM w h m (C.mask msk p)

superimpose :: Float -> Netpbm -> Netpbm -> Netpbm
superimpose a (PGM w h m p1) (PGM _ _ _ p2) = PGM w h m (G.superimpose a p1 p2)
superimpose a (PPM w h m p1) (PPM _ _ _ p2) = PPM w h m (C.superimpose a p1 p2)

swapColors :: Netpbm -> Netpbm
swapColors (PPM w h m p) = PPM w h m (C.swapColors p)

toPGM :: Netpbm -> Netpbm
toPGM x@PGM {} = x
toPGM (PPM w h m p) = PGM w h m (C.toGreyscale p)

toGreyscale = toPGM

contrast :: Float -> Netpbm -> Netpbm
contrast f (PGM w h m p) = PGM w h m (G.contrast f p)
contrast f (PPM w h m p) = PPM w h m (C.contrast f p)

border :: Int -> Netpbm -> Netpbm
border i (PGM w h m p) = PGM (w + 2 * i) (h + 2 * i) m (G.border i p)
border i (PPM w h m p) = PPM (w + 2 * i) (h + 2 * i) m (C.border i p)

stamp :: (Int, Int) -> (Float, Float) -> Netpbm -> Netpbm -> Netpbm
stamp c f (PGM w h m p1) (PGM _ _ _ p2) = PGM w h m (B.stamp c f p1 p2)
stamp c f (PPM w h m p1) (PPM _ _ _ p2) = PPM w h m (B.stamp c f p1 p2)

blur :: Netpbm -> Netpbm
blur (PGM w h m p) = PGM w h m (G.blur p)
blur (PPM w h m p) = PPM w h m (C.blur p)
