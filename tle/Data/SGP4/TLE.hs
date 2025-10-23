{-# LANGUAGE DeriveGeneric
           , DerivingStrategies
           , RecordWildCards
           , OverloadedStrings
           #-}

module Data.SGP4.TLE where

import Control.Applicative

import qualified Data.Attoparsec.ByteString.Char8 as A

import qualified Data.ByteString.Char8 as BC

import Data.Functor

import Data.Time.Calendar
import Data.Time.Calendar.MonthDay

import GHC.Generics

import Linear.Geo.PlaneAngle
import Linear.Geo.Time

-- | All quantities are converted to the units expected by SGP4, from whatever
--   units are conveyed in the TLE text.
data TLE = TLE {
    tleName :: Maybe String
  , tleSatelliteNumber :: Int
  , tleClassification :: String
  , tleDesigYear :: Int
  , tleDesigLaunchNo :: Int
  , tleDesigLaunchPiece :: String
  , tleEpoch :: JulianDate
    -- | `ndot`, rev/day^2 in TLE
  , tleMeanMotionDeriv :: Double
    -- | `nddot`, rev/day^3 in TLE
  , tleMeanMotionDerivDeriv :: Double
    -- | `bstar`, 1 / earth radius
  , tleBStar :: Double
  , tleEphemType :: Int
  , tleElementNum :: Int
  , tleChecksum1 :: Int
    -- | `inclo`, degrees in TLE
  , tleInclination :: Radians Double
    -- | `nodeo`, degrees in TLE
  , tleRightAscention :: Radians Double
    -- | `ecco`
  , tleEccentricity :: Double
    -- | `argpo`, degrees in TLE
  , tleArgumentPerigee :: Radians Double
    -- | `mo`, degrees in TLE
  , tleMeanAnomaly :: Radians Double
    -- | `no_kozai`, rev/day in TLE, converted to rev/min
  , tleMeanMotion :: Double
    -- | `revnum`
  , tleRevolutionNumber :: Int
  , tleChecksum2 :: Int
  } deriving stock ( Generic
                   , Show
                   )

parseFixedWidth :: Int -> A.Parser a -> A.Parser a
parseFixedWidth n p = do
    i <- A.take n
    case A.parseOnly (p <* A.endOfInput) i of
        Left e -> fail e
        Right x -> pure x

string_ :: BC.ByteString -> A.Parser ()
string_ = void . A.string

space1_ :: A.Parser ()
space1_ = void A.space

fracPart :: A.Parser Double
fracPart = do
    ibs' <- A.takeWhile1 A.isDigit
    let ibs = BC.dropWhileEnd (== '0') ibs'
    i <- if BC.null ibs
         then pure 0 -- all zeros, e.g. ".0"
         else case BC.readInt ibs of
                Nothing -> fail "absurd"
                Just (x, bs) | BC.null bs -> pure x
                             | otherwise -> fail "absurd"
    let pv = BC.length (BC.dropWhileEnd (== '0') ibs)
    pure (fromIntegral i / fromIntegral (10 ^ pv))

double :: A.Parser Double
double = A.signed (dub <|> A.double)
    where dub = string_ "." *> fracPart

pointAssumedExp :: A.Parser Double
pointAssumedExp = A.signed $ do
    m <- fracPart
    e <- fromIntegral <$> A.signed A.decimal
    pure (m * (10 ** e))

parseTLENoNameLine :: A.Parser TLE
parseTLENoNameLine = parseTLE Nothing

parseTLEWithNameLine :: A.Parser TLE
parseTLEWithNameLine = do
    tleName <- Just . BC.unpack <$> (A.takeWhile1 (\c -> c /= '\r' && c /= '\n') A.<?> "tleName")
    A.endOfLine A.<?> "tleName eol"
    parseTLE tleName

-- | Many sources claim that TLEs use UTC. However, how leap seconds are handled
--   is not defined, and the reference implementation ignores them. Here we
--   assume that we have the Gregorian year and fractional days of the year,
--   which is probably what people mean when they say "TLEs use UTC."
toJD :: Int -> Double -> JulianDate
toJD year' day =
    -- No good after 2056, and these Y2K style problems always sneak up on you.
    let year | year' < 57 = fromIntegral year' + 2000
             | otherwise = fromIntegral year' + 1900
        leap = isLeapYear year
        doy = floor day
        dayFrac = toRational day - fromIntegral doy
        (moy, dom) = dayOfYearToMonthAndDay leap doy
        (ModifiedJulianDay mjd) = fromGregorian year moy dom
    in JulianDate (2400000.5 + fromIntegral mjd + dayFrac)

-- | ???
xpdotp :: Floating a => a
xpdotp = 1440 / (2 * pi)

convertMeanMotion :: Floating a => a -> a
convertMeanMotion = (/ xpdotp)

convertMeanMotionDeriv :: Floating a => a -> a
convertMeanMotionDeriv = (/ (xpdotp * 1440))

convertMeanMotionDerivDeriv :: Floating a => a -> a
convertMeanMotionDerivDeriv = (/ (xpdotp * 1440 * 1440))

parseTLE :: Maybe String -> A.Parser TLE
parseTLE tleName = do
    void (parseFixedWidth 1 A.decimal A.<?> "line number 1")
    space1_ A.<?> "line number space"
    tleSatelliteNumber <- parseFixedWidth 5 A.decimal A.<?> "satellite number"
    tleClassification <- (:[]) <$> parseFixedWidth 1 A.anyChar A.<?> "classification"
    space1_ A.<?> "satellite name space"
    tleDesigYear <- parseFixedWidth 2 A.decimal A.<?> "desig year"
    tleDesigLaunchNo <- parseFixedWidth 3 A.decimal A.<?> "desig launch number"
    tleDesigLaunchPiece <- BC.unpack <$>
        parseFixedWidth 3
            (A.takeWhile (not . A.isSpace) <* A.skipSpace) A.<?> "desig piece"
    space1_ A.<?> "desig space"
    tleEpochYear <- parseFixedWidth 2 A.decimal A.<?> "epoch year"
    tleEpochFracDay <- parseFixedWidth 12 double A.<?> "epoch frac day"
    let tleEpoch = toJD tleEpochYear tleEpochFracDay
    space1_ A.<?> "epoch space"
    tleMeanMotionDeriv <- convertMeanMotionDeriv <$> parseFixedWidth 10
        (A.skipSpace *> A.signed double) A.<?> "mean motion deriv"
    space1_ A.<?> "mean motion deriv space"
    tleMeanMotionDerivDeriv <- convertMeanMotionDerivDeriv <$> parseFixedWidth 8
        (A.skipSpace *> pointAssumedExp) A.<?> "mean motion deriv deriv"
    space1_ A.<?> "mean motion deriv deriv space"
    tleBStar <- parseFixedWidth 8 (A.skipSpace *> pointAssumedExp) A.<?> "bstar"
    space1_ A.<?> "bstar space"
    tleEphemType <- parseFixedWidth 1 A.decimal A.<?> "ephem type"
    space1_ A.<?> "ephem type space"
    tleElementNum <- parseFixedWidth 4 (A.skipSpace *> A.decimal) A.<?> "element num"
    tleChecksum1 <- parseFixedWidth 1 A.decimal A.<?> "checksum1"
    A.endOfLine A.<?> "line 1 eol"
    void (parseFixedWidth 1 A.decimal) A.<?> "line number 2"
    space1_ A.<?> "line number 2 space"
    void (parseFixedWidth 5 A.decimal) A.<?> "satellite number 2"
    space1_ A.<?> "satellite number 2 space"
    tleInclination <- toRadians . Degrees <$>
        parseFixedWidth 8 (A.skipSpace *> double) A.<?> "inc deg"
    space1_ A.<?> "inc deg space"
    tleRightAscention <- toRadians . Degrees <$>
        parseFixedWidth 8 (A.skipSpace *> double) A.<?> "right ascend"
    space1_ A.<?> "right ascend space"
    tleEccentricity <- parseFixedWidth 7 (A.skipSpace *> fracPart) A.<?> "eccen"
    space1_ A.<?> "eccen space"
    tleArgumentPerigee <- toRadians . Degrees <$>
        parseFixedWidth 8 (A.skipSpace *> double) A.<?> "arg perigree"
    space1_ A.<?> "arg perigree space"
    tleMeanAnomaly <- toRadians . Degrees <$>
        parseFixedWidth 8 (A.skipSpace *> double) A.<?> "mean anom"
    space1_ A.<?> "mean anom space"
    tleMeanMotion <- convertMeanMotion <$>
        parseFixedWidth 11 (A.skipSpace *> double) A.<?> "mean motion"
    tleRevolutionNumber <- parseFixedWidth 5 (A.skipSpace *> A.decimal) A.<?> "rev num"
    tleChecksum2 <- parseFixedWidth 1 A.decimal A.<?> "checksum 2"
    A.endOfLine A.<?> "line 2 eol"
    pure TLE{..}
