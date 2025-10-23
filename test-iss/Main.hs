{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           #-}

module Main where

import Control.Concurrent

import Control.Monad

import qualified Data.Attoparsec.ByteString as A

import Data.IERS

import Data.Time.Clock
import Data.Time.Clock.TAI

import Linear.Geo.PlaneAngle
import Linear.Geo.ECEF
import Linear.Geo.Geodetic
import Linear.Geo.ECI.TEME
import Linear.Geo.Time

import Data.SGP4.C
import Data.SGP4.TLE

-- hardcoded TLE for convenience, update this if you want accurate results
-- comparable to any other source.
isstle = "ISS (ZARYA)             \r\n1 25544U 98067A   25295.52400961  .00014217  00000+0  26248-3 0  9999\r\n2 25544  51.6347  36.0364 0004442 324.2290  35.8401 15.49349875534914\r\n"

utcToConversionTimes :: BulletinASet -> UTCTime -> Maybe (TT, JulianDate)
utcToConversionTimes bas t = do
    EOPQueryResult{..} <- mostRecentEOP bas
    let dut = fromRational $ toRational eqrDUT
        ut1 = utcToUT1 dut t
        lsm = toLeapSecondMap bas
    at <- utcToTAITime lsm t
    let tt = ttFromTAI at
        jd = julianDateFromUniversalTime ut1
    pure (tt, jd)

fromArcSeconds :: Floating a => a -> Radians a
fromArcSeconds x = Radians ((x * pi) / (60 * 60 * 180))

printPropNow :: BulletinASet -> SGP4 -> IO ()
printPropNow bas sgp = do
    t <- getCurrentTime
    let Just (tt, jd) = utcToConversionTimes bas t
        Just EOPQueryResult{..} = mostRecentEOP bas
    teme <- sgp4 sgp t
    let ecef = fromTEME tt jd (fromArcSeconds eqrX) (fromArcSeconds eqrY) teme
        geo = ecefToGeo ecef
        (Degrees lat, Degrees lon, _) = toLatLonAlt geo
        ll = (lat, lon)
    -- print teme
    -- print ecef
    print ll

main :: IO ()
main = do
    bas <- fromListByDay baPublishedDate . (:[]) <$> fetchBulletinA
    let Right tle = A.parseOnly parseTLEWithNameLine isstle
    sgp <- sgp4Init tle
    -- forever (printPropNow bas sgp *> threadDelay 1000000)
    forever (printPropNow bas sgp)
    pure ()
