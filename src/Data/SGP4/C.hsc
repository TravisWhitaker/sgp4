{-# LANGUAGE CApiFFI
           , DerivingStrategies
           , RecordWildCards
           #-}

module Data.SGP4.C where

import Data.Time.Calendar
import Data.Time.Clock

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Linear.Geo.ECI.TEME
import Linear.Geo.PlaneAngle
import Linear.Geo.Time

import Data.SGP4.TLE

#include <SGP4.h>

data SGP4 = SGP4 {
    sgp4StructP :: (ForeignPtr ())
  , sgp4Epoch :: JulianDate
  } deriving stock (Show)

foreign import capi safe "SGP4.h sgp4init"
    sgp4init_c :: CInt -- gravconsttype whichconst
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
    sgp4_c :: Ptr () -- elsetrec* satrec
           -> Double -- double tsince
           -> Ptr v3 -- double r[3]
           -> Ptr v3 --  double v[3]
           -> IO CBool

{-
epoch notes

1440 -> minutes per Julian day

2433281.5 -> TLE epoch in Julian Days, "epoch time in days from jan 0, 1950.
0 hr"

epoch year < 57 -> SGP4 assums 2000s, else 1900s

-}

fromJD :: JulianDate -> Double
fromJD (JulianDate jd) = fromRational (jd - 2433281.5)

fromRad :: Radians a -> a
fromRad (Radians a) = a

sgp4Init :: TLE -> IO SGP4
sgp4Init TLE{..} = do
    sgp4StructP <- mallocForeignPtrBytes #{size elsetrec}
    let sgp4Epoch = tleEpoch
    withForeignPtr sgp4StructP $ \outP ->
        withCString "@@@" $ \nP -> do
            r <- sgp4init_c #{const wgs84}
                            (castCharToCChar 'i')
                            nP
                            (fromJD tleEpoch)
                            tleBStar
                            tleMeanMotionDeriv
                            tleMeanMotionDerivDeriv
                            tleEccentricity
                            (fromRad tleArgumentPerigee)
                            (fromRad tleInclination)
                            (fromRad tleMeanAnomaly)
                            tleMeanMotion
                            (fromRad tleRightAscention)
                            outP
            if (toBool r)
            then pure SGP4{..}
            else error "C++ failed"
                          
-- | SGP4 handles JulianDate and UTCTime ignoring DUT and leap seconds, so shall
--   we.
toMinutesSinceEpoch :: JulianDate -> UTCTime -> Double
toMinutesSinceEpoch epochJD (UTCTime (ModifiedJulianDay tMJD') ttod) =
    let pday = 86400
        tMJD = toRational tMJD' + (toRational ttod / pday)
        ModJulianDate epochMJD = universalTimeFromJulianDate epochJD
    in fromRational (1440 * (tMJD - epochMJD))

sgp4 :: SGP4 -> UTCTime -> IO (TEME Double)
sgp4 SGP4{..} t =
    let tsince = toMinutesSinceEpoch sgp4Epoch t
    in alloca $ \rp ->
       alloca $ \vp ->
       withForeignPtr sgp4StructP $ \satP -> do
        res <- sgp4_c satP tsince rp vp
        if toBool res
        -- km to meters
        then (TEME . fmap (* 1000)) <$> peek rp
        else error "C++ failed"

