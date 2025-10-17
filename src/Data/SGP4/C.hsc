{-# LANGUAGE CApiFFI
           #-}

module Data.SGP4.C where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

data SGP4 = SGP4 (

foreign import capi safe "SGP4.h sgp4init"
    sgp4init :: CInt -- gravconsttype whichconst
             -> CChar --char opsmode
             -> Ptr CChar-- const char satn[5]
             -> Double -- const double epoch
             -> Double -- const double xbstar
             -> Double -- const double xndot
             -> Double -- const double xnddot
             -> Double -- const double xecco
             -> Double -- const double xargpo
             -> Double -- const double xinclo
             -> Double -- const double xmo
             -> Double -- const double xno_kozai
             -> Double -- const double xnodeo
             -> Ptr () -- elsetrec* satrec
             -> IO CBool

foreign import capi safe "SGP4.h sgp4"
    sgp4 :: Ptr () -- elsetrec* satrec
         -> Double -- double tsince
         -> Ptr Double -- double r[3]
         -> Ptr Double --  double v[3]
         -> IO ()


