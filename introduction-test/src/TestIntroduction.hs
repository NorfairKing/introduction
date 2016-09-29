{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
module TestIntroduction
    ( module X
    ) where

import           Introduction                as X hiding ((.&.))

import           Test.Hspec                  as X
import           Test.QuickCheck             as X

import           Data.GenRelativeValidity    as X
import           Data.GenValidity            as X
import           Data.GenValidity.Containers as X
import           Data.GenValidity.Text       as X
import           Test.Validity               as X

import           Path.Internal

import qualified Data.Time                   as Time
import qualified Data.Time.Clock.TAI         as Time

import           Data.Tree                   (Tree)

import qualified Data.ByteString             as SB
import qualified Data.ByteString.Lazy        as LB

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = genTreeOf arbitrary

instance Arbitrary Text where
    arbitrary = genValid

instance Arbitrary LB.ByteString where
    arbitrary = LB.fromStrict <$> arbitrary

instance Arbitrary SB.ByteString where
    arbitrary = SB.pack <$> arbitrary

instance Arbitrary Time.Day where
    arbitrary = Time.ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModifiedJulianDay <$>) . shrink . Time.toModifiedJulianDay

instance CoArbitrary Time.Day where
    coarbitrary = coarbitrary . Time.toModifiedJulianDay

instance Arbitrary Time.UniversalTime where
    arbitrary = Time.ModJulianDate <$> (2000 +) <$> arbitrary
    shrink    = (Time.ModJulianDate <$>) . shrink . Time.getModJulianDate

instance CoArbitrary Time.UniversalTime where
    coarbitrary = coarbitrary . Time.getModJulianDate

instance Arbitrary Time.DiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance CoArbitrary Time.DiffTime where
    coarbitrary = coarbitraryReal

instance Arbitrary Time.UTCTime where
    arbitrary =
        Time.UTCTime
        <$> arbitrary
        <*> (fromRational . toRational <$> choose (0::Double, 86400))
    shrink ut@(Time.UTCTime day dayTime) =
        [ ut { Time.utctDay     = d' } | d' <- shrink day     ] ++
        [ ut { Time.utctDayTime = t' } | t' <- shrink dayTime ]

instance CoArbitrary Time.UTCTime where
    coarbitrary (Time.UTCTime day dayTime) =
        coarbitrary day . coarbitrary dayTime

instance Arbitrary Time.NominalDiffTime where
    arbitrary = arbitrarySizedFractional
    shrink    = shrinkRealFrac

instance CoArbitrary Time.NominalDiffTime where
    coarbitrary = coarbitraryReal

instance Arbitrary Time.TimeZone where
    arbitrary =
        Time.TimeZone
         <$> choose (-12*60,14*60) -- utc offset (m)
         <*> arbitrary -- is summer time
         <*> (sequence . replicate 4 $ choose ('A','Z'))
    shrink tz@(Time.TimeZone minutes summerOnly name) =
        [ tz { Time.timeZoneMinutes    = m' } | m' <- shrink minutes    ] ++
        [ tz { Time.timeZoneSummerOnly = s' } | s' <- shrink summerOnly ] ++
        [ tz { Time.timeZoneName       = n' } | n' <- shrink name       ]

instance CoArbitrary Time.TimeZone where
    coarbitrary (Time.TimeZone minutes summerOnly name) =
        coarbitrary minutes . coarbitrary summerOnly . coarbitrary name

instance Arbitrary Time.TimeOfDay where
    arbitrary =
        Time.TimeOfDay
         <$> choose (0, 23) -- hour
         <*> choose (0, 59) -- minute
         <*> (fromRational . toRational <$> choose (0::Double, 60)) -- picoseconds, via double
    shrink tod@(Time.TimeOfDay hour minute second) =
        [ tod { Time.todHour = h' } | h' <- shrink hour   ] ++
        [ tod { Time.todMin  = m' } | m' <- shrink minute ] ++
        [ tod { Time.todSec  = s' } | s' <- shrink second ]

instance CoArbitrary Time.TimeOfDay where
    coarbitrary (Time.TimeOfDay hour minute second) =
        coarbitrary hour . coarbitrary minute . coarbitrary second

instance Arbitrary Time.LocalTime where
    arbitrary =
        Time.LocalTime
         <$> arbitrary
         <*> arbitrary
    shrink lt@(Time.LocalTime day tod) =
        [ lt { Time.localDay       = d' } | d' <- shrink day ] ++
        [ lt { Time.localTimeOfDay = t' } | t' <- shrink tod ]

instance CoArbitrary Time.LocalTime where
    coarbitrary (Time.LocalTime day tod) =
        coarbitrary day . coarbitrary tod

instance Arbitrary Time.ZonedTime where
    arbitrary =
        Time.ZonedTime
         <$> arbitrary
         <*> arbitrary
    shrink zt@(Time.ZonedTime lt zone) =
        [ zt { Time.zonedTimeToLocalTime = l' } | l' <- shrink lt   ] ++
        [ zt { Time.zonedTimeZone        = z' } | z' <- shrink zone ]

instance CoArbitrary Time.ZonedTime where
    coarbitrary (Time.ZonedTime lt zone) =
        coarbitrary lt . coarbitrary zone

instance Arbitrary Time.AbsoluteTime where
    arbitrary =
        Time.addAbsoluteTime
         <$> arbitrary
         <*> return Time.taiEpoch
    shrink at =
        (`Time.addAbsoluteTime` at) <$> shrink (Time.diffAbsoluteTime at Time.taiEpoch)

instance CoArbitrary Time.AbsoluteTime where
    coarbitrary = coarbitrary . flip Time.diffAbsoluteTime Time.taiEpoch

instance GenValidity (Path Abs File) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Rel File) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Abs Dir) where
  genUnchecked = Path <$> arbitrary

instance GenValidity (Path Rel Dir) where
  genUnchecked = Path <$> arbitrary

