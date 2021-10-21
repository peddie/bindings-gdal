{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module GDAL.Internal.OSR (
    SpatialReference
  , CoordinateTransformation
  , Projectable (..)
  , AxisMappingStrategy (..)

  , srsFromWkt
  , srsFromProj4
  , srsFromEPSG
  , srsFromEPSGIO
  , srsFromXML

  , srsToWkt
  , srsToProj4
  , srsToXML

  , isGeographic
  , isLocal
  , isProjected
  , isSameGeogCS
  , isSame

  , setPROJSearchPaths

  , getAngularUnits
  , getLinearUnits

  , coordinateTransformation
  , coordinateTransformationIO

  , setAxisMappingStrategy

  , cleanup
  , initialize
  , withSpatialReference
  , withMaybeSRAsCString
  , withMaybeSpatialReference
  , maybeSpatialReferenceFromCString
  , withCoordinateTransformation
  , newSpatialRefHandle
  , newSpatialRefBorrowedHandle
  , maybeNewSpatialRefHandle
  , maybeNewSpatialRefBorrowedHandle
) where

#include "ogr_srs_api.h"

{# context lib = "gdal" prefix = "OSR" #}

import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.Trans.Class (lift)
import Control.DeepSeq (NFData(..))
import Control.Monad.Catch (mask_, try)
import Control.Monad (liftM, (>=>), when, void, foldM, zipWithM_)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (useAsCString, unpack)

import qualified Data.Vector.Storable.Mutable as Stm
import qualified Data.Vector.Storable as St

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..), CDouble(..))
import Foreign.Ptr (Ptr, FunPtr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray0)
import Foreign.Marshal.Utils (toBool, with)

import Lens.Micro.Extras (view)
import Linear.V3 (V3 (..), _x, _y, _z)

import System.IO.Unsafe (unsafePerformIO)

import GDAL.Internal.Types
import GDAL.Internal.Util (fromEnumC)
import GDAL.Internal.OGRError
import GDAL.Internal.CPLError hiding (None)
import GDAL.Internal.CPLString (peekCPLString)

{#enum OSRAxisMappingStrategy as AxisMappingStrategy { } deriving (Eq, Show) #}


{#pointer OGRSpatialReferenceH as SpatialReference foreign newtype#}

instance NFData SpatialReference where
  rnf SpatialReference{} = ()

instance Show SpatialReference where
   show = unpack . srsToWkt

srsToWkt :: SpatialReference -> ByteString
srsToWkt s = exportWith fun s
  where
    fun s' p = {#call unsafe ExportToPrettyWkt as ^#} s' p 1

srsToProj4 :: SpatialReference -> ByteString
srsToProj4 = exportWith {#call unsafe ExportToProj4 as ^#}

srsToXML :: SpatialReference -> ByteString
srsToXML = exportWith fun
  where
    fun s' p = {#call unsafe ExportToXML as ^#} s' p (castPtr nullPtr)

exportWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CInt)
  -> SpatialReference
  -> ByteString
exportWith fun srs =
  unsafePerformIO $
  withSpatialReference srs $ \pSrs ->
  peekCPLString $
  checkOGRError "srsTo" . fun pSrs


foreign import ccall "ogr_srs_api.h &OSRRelease"
  c_release :: FunPtr (Ptr SpatialReference -> IO ())

newSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefHandle = maybeNewSpatialRefHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefHandle io =
  mask_ $ do
    p <- io
    if p==nullPtr
      then return Nothing
      else liftM (Just . SpatialReference) (newForeignPtr c_release p)

newSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO SpatialReference
newSpatialRefBorrowedHandle =
  maybeNewSpatialRefBorrowedHandle >=> maybe exc return
  where exc = throwBindingException NullSpatialReference

maybeNewSpatialRefBorrowedHandle
  :: IO (Ptr SpatialReference) -> IO (Maybe SpatialReference)
maybeNewSpatialRefBorrowedHandle alloc = maybeNewSpatialRefHandle $ do
  p <- alloc
  when (p /= nullPtr) (void ({#call unsafe OSRReference as ^#} p))
  return p

emptySpatialRef :: IO SpatialReference
emptySpatialRef =
  newSpatialRefHandle
  ({#call unsafe NewSpatialReference as c_newEmptySpatialRef #} nullPtr)

srsFromWkt, srsFromProj4, srsFromXML
  :: ByteString -> Either OGRException SpatialReference
srsFromWkt = fromImporter importFromWKT


srsFromProj4 = fromImporter importFromProj4

srsFromXML = fromImporter importFromXML

srsFromEPSG :: Int -> Either OGRException SpatialReference
srsFromEPSG = fromImporter importFromEPSG

srsFromEPSGIO :: Int -> IO (Either OGRException SpatialReference)
srsFromEPSGIO = fromImporterIO importFromEPSG

fromImporter
  :: (SpatialReference -> a -> IO CInt) -> a
  -> Either OGRException SpatialReference
fromImporter f = unsafePerformIO . fromImporterIO f

fromImporterIO
  :: (SpatialReference -> a -> IO CInt) -> a
  -> IO (Either OGRException SpatialReference)
fromImporterIO f s = do
  r <- emptySpatialRef
  try (checkOGRError "srsFrom" (f r s) >> return r)

importFromWKT :: SpatialReference -> ByteString -> IO CInt
importFromWKT srs bs =
  withSpatialReference srs $ \srsPtr ->
  useAsCString bs $ \bsPtr ->
  with bsPtr ({# call ImportFromWkt as ^ #} srsPtr)

useAsCStrings :: [ByteString] -> (Ptr CString -> IO a) -> IO a
useAsCStrings bss action = ($ action) . runContT $ do
  ptrs <- ContT $ allocaArray0 n
  -- The array must be null-terminated.
  lift $ pokeElemOff ptrs n nullPtr
  zipWithM_ (\buffer idx -> lift . pokeElemOff ptrs idx =<< (ContT $ useAsCString buffer)) bss [0..]
  pure ptrs
  where
    n = length bss

{#fun ImportFromProj4 as ^
   { withSpatialReference* `SpatialReference'
   , useAsCString* `ByteString'} -> `CInt' #}

{#fun ImportFromEPSG as ^
   {withSpatialReference* `SpatialReference', `Int'} -> `CInt' #}

{#fun ImportFromXML as ^
   { withSpatialReference* `SpatialReference'
   , useAsCString* `ByteString'} -> `CInt' #}

{#fun SetPROJSearchPaths as ^
  { useAsCStrings* `[ByteString]'} -> `()' #}

{#fun pure unsafe IsGeographic as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsLocal as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsProjected as ^
   {withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSameGeogCS as ^
   { withSpatialReference* `SpatialReference'
   , withSpatialReference* `SpatialReference'} -> `Bool'#}

{#fun pure unsafe IsSame as ^
   { withSpatialReference* `SpatialReference'
   , withSpatialReference* `SpatialReference'} -> `Bool'#}

instance Eq SpatialReference where
  (==) = isSame

getLinearUnits :: SpatialReference -> (Double, String)
getLinearUnits =
  unsafePerformIO . getUnitsWith {#call unsafe OSRGetLinearUnits as ^#}

getAngularUnits :: SpatialReference -> (Double, String)
getAngularUnits =
  unsafePerformIO . getUnitsWith {#call unsafe OSRGetAngularUnits as ^#}

getUnitsWith
  :: (Ptr SpatialReference -> Ptr CString -> IO CDouble)
  -> SpatialReference
  -> IO (Double, String)
getUnitsWith fun s = alloca $ \p -> do
  value <- withSpatialReference s (\s' -> fun s' p)
  ptr <- peek p
  units <- peekCString ptr
  return (realToFrac value, units)


setAxisMappingStrategy :: SpatialReference -> AxisMappingStrategy -> IO ()
setAxisMappingStrategy srs x =
  withSpatialReference srs $ \pSrs ->
    {#call unsafe OSRSetAxisMappingStrategy as ^#} pSrs (fromEnumC x)

withMaybeSRAsCString :: Maybe SpatialReference -> (CString -> IO a) -> IO a
withMaybeSRAsCString = useAsCString . maybe "" srsToWkt

maybeSpatialReferenceFromCString :: CString -> IO (Maybe SpatialReference)
maybeSpatialReferenceFromCString str | str == nullPtr = return Nothing
maybeSpatialReferenceFromCString str = do
  c <- peek str
  if c == 0 then return Nothing
    else liftM Just $
         newSpatialRefHandle $
         checkGDALCall checkIt ({#call NewSpatialReference as ^#} str)
  where
    checkIt e p
      | p==nullPtr = Just (maybe defExc toOgrExc e)
      | otherwise  = fmap toOgrExc e
    defExc = NullSpatialReference
    toOgrExc = gdalToOgrException Failure

withMaybeSpatialReference
  :: Maybe SpatialReference -> (Ptr SpatialReference -> IO a) -> IO a
withMaybeSpatialReference Nothing  = ($ nullPtr)
withMaybeSpatialReference (Just s) = withSpatialReference s

{#pointer OGRCoordinateTransformationH as CoordinateTransformation
  foreign newtype#}

coordinateTransformation
  :: SpatialReference -> SpatialReference
  -> Either OGRException CoordinateTransformation
coordinateTransformation source =
  unsafePerformIO . coordinateTransformationIO source

coordinateTransformationIO
  :: SpatialReference
  -> SpatialReference
  -> IO (Either OGRException CoordinateTransformation)
coordinateTransformationIO source target =
  try $
  liftM CoordinateTransformation $
  withSpatialReference source $ \pSource ->
  withSpatialReference target $ \pTarget ->
  mask_ $
  newForeignPtr c_destroyCT =<<
    (checkGDALCall checkIt
      ({#call unsafe OCTNewCoordinateTransformation as ^#} pSource pTarget))
  where
    checkIt e p
      | p==nullPtr = Just (maybe defExc toOgrExc e)
      | otherwise  = Nothing
    defExc = NullCoordinateTransformation
    toOgrExc = gdalToOgrException Failure

foreign import ccall "ogr_srs_api.h &OCTDestroyCoordinateTransformation"
  c_destroyCT :: FunPtr (Ptr CoordinateTransformation -> IO ())

class Projectable a where
  transformWith :: a -> CoordinateTransformation -> Maybe a

instance Projectable (St.Vector (V3 Double)) where
  transformWith = flip transformPoints

instance Projectable (St.Vector (Pair Double)) where
  transformWith = flip transformPoints2D

transformPoints2D
  :: CoordinateTransformation
  -> St.Vector (Pair Double)
  -> Maybe (St.Vector (Pair Double))
transformPoints2D transform =
  fmap (St.map v3ToPair) . transformPoints transform . St.map pairToV3
  where
    pairToV3 (x :+: y) = V3 x y 0
    v3ToPair (V3 x y _) = x :+: y

transformPoints
  :: CoordinateTransformation
  -> St.Vector (V3 Double)
  -> Maybe (St.Vector (V3 Double))
transformPoints ct v = unsafePerformIO $ withQuietErrorHandler $ do
  xs <- St.unsafeThaw (St.unsafeCast (St.map (view _x) v))
  ys <- St.unsafeThaw (St.unsafeCast (St.map (view _y) v))
  zs <- St.unsafeThaw (St.unsafeCast (St.map (view _z) v))
  pab <- Stm.replicate len (0 :: CInt)
  ok <- liftM toBool $
        withCoordinateTransformation ct $ \pCt ->
        Stm.unsafeWith xs $ \pXs ->
        Stm.unsafeWith ys $ \pYs ->
        Stm.unsafeWith zs $ \pZs ->
        Stm.unsafeWith pab $ \pabSuccess ->
          {#call unsafe OCTTransformEx as ^#} pCt (fromIntegral len) pXs pYs pZs pabSuccess
  -- We must check the individual values, since
  -- [GDAL may return `True`](https://gdal.org/api/ogrspatialref.html#_CPPv4N27OGRCoordinateTransformation9TransformEiPdPdPdPi) [ ](DONTLINTLINELENGTH)
  -- for @ok@ even if some points failed.  It's not clear under what circumstances this may occur.
  --
  --     Returns: TRUE if some or all points transform successfully, or FALSE if if none transform.
  reallyOk <- stmFoldr' (\x y -> toBool x && y) True pab
  if not (ok && reallyOk)
    then return Nothing
    else do
      fXs <- liftM St.unsafeCast (St.unsafeFreeze xs)
      fYs <- liftM St.unsafeCast (St.unsafeFreeze ys)
      fZs <- liftM St.unsafeCast (St.unsafeFreeze zs)
      return (Just (St.zipWith3 V3 fXs fYs fZs))
  where len = St.length v
        -- This is only available from vector version 0.12.3 onwards.
        stmFoldr' f z vec = foldM go z [0 .. Stm.length vec - 1]
          where
            go acc idx = (`f` acc) <$> vec `Stm.read` idx


{#fun OSRCleanup as cleanup {} -> `()'#}

-- | GDAL doesn't call ogr/ogrct.cpp:LoadProj4Library in a thread-safe way
--   (at least in 1.11.2). We indirectly make sure it is called at startup
--   in the main thread (via 'withGDAL') with this function which creates
--   a dummy 'CoordinateTransformation'
initialize :: IO ()
initialize = do
  dummy <- emptySpatialRef
  void (coordinateTransformationIO dummy dummy)
