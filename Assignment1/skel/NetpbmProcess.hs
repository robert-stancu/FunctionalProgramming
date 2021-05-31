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

brightness :: Int -> Netpbm -> Netpbm
brightness x (PGM w h m p) = PGM w h m (G.brightness x p)
brightness x (PPM w h m p) = PPM w h m (C.brightness x p)

mask :: Netpbm -> Netpbm -> Netpbm
mask (PBM _ _ msk) (PGM w h m p) = PGM w h m (G.mask msk p)
mask (PBM _ _ msk) (PPM w h m p) = PPM w h m (C.mask msk p)

swapColors :: Netpbm -> Netpbm
swapColors (PPM w h m p) = PPM w h m (C.swapColors p)

toPGM :: Netpbm -> Netpbm
toPGM x@PGM {} = x
toPGM (PPM w h m p) = PGM w h m (C.toGreyscale p)
