{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Simplon.Hdf5 (getDataSetDimensions, withHdf5FileAndDataSet, withImage) where

import Control.Applicative (Applicative (pure))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.ByteString (ByteString, packCStringLen)
import Data.Foldable (Foldable (length, product))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (replicate)
import Data.Ord (Ord, (<))
import Data.Semigroup ((<>))
import Data.Text (Text, unpack)
import Foreign (sizeOf)
import Foreign.C.String (withCString)
import Foreign.C.Types (CFloat, CUShort)
import Foreign.Marshal (peekArray)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Ptr (castPtr, nullPtr)
import Simplon.Hdf5Raw (HidT, h5dclose, h5dget_space, h5dopen, h5dread, h5fAccRdOnly, h5fCloseStrong, h5fclose, h5fopen, h5pDefault, h5pFileAccess, h5pclose, h5pcreate, h5pset_fclose_degree, h5sSelectSet, h5sclose, h5screate_simple, h5sget_simple_extent_dims, h5sget_simple_extent_ndims, h5sselect_hyperslab, h5tNativeFloat, h5tNativeUShort)
import System.IO (FilePath, IO)
import Text.Show (Show (show))
import Prelude (Num ((*)), error, fromIntegral, undefined)

newtype Hdf5FileId = Hdf5FileId {ungetHdf5FileId :: HidT}

newtype Hdf5DataSpaceId = Hdf5DataSpaceId {ungetHdf5DataSpaceId :: HidT}

newtype Hdf5DataSetId = Hdf5DataSetId {ungetHdf5DataSetId :: HidT}

type Hdf5DataSetPath = Text

data Hdf5DataSet = Hdf5DataSet
  { hdf5FileId :: Hdf5FileId,
    hdf5DataSetId :: Hdf5DataSetId,
    hdf5DataSpaceId :: Hdf5DataSpaceId
  }

type DataPath = Text

checkError :: (Num a, Show a, Ord a) => IO a -> IO ()
checkError action = do
  err <- action
  when (err < 0) $ error $ "error code " <> show err

checkInvalidHid :: IO HidT -> IO HidT
checkInvalidHid action = do
  err <- action
  if err < 0
    then error $ "error code " <> show err
    else pure err

openHdf5File :: FilePath -> IO Hdf5FileId
openHdf5File fp = do
  fapl <- h5pcreate h5pFileAccess

  checkError (h5pset_fclose_degree fapl h5fCloseStrong)

  withCString fp $ \fileName -> do
    hid <- checkInvalidHid (h5fopen fileName h5fAccRdOnly fapl)
    checkError (h5pclose fapl)
    pure (Hdf5FileId hid)

closeHdf5File :: Hdf5FileId -> IO ()
closeHdf5File (Hdf5FileId fid) = checkError (h5fclose fid)

withHdf5File :: FilePath -> (Hdf5FileId -> IO a) -> IO a
withHdf5File fp = bracket (openHdf5File fp) closeHdf5File

openHdf5DataSet :: Hdf5FileId -> Hdf5DataSetPath -> IO Hdf5DataSetId
openHdf5DataSet (Hdf5FileId fileId) path =
  withCString (unpack path) $ \dsPath -> do
    dataSetId <- checkInvalidHid (h5dopen fileId dsPath h5pDefault)
    pure (Hdf5DataSetId dataSetId)

closeHdf5DataSet :: Hdf5DataSetId -> IO ()
closeHdf5DataSet (Hdf5DataSetId dataSetId) = checkError (h5dclose dataSetId)

withHdf5DataSet :: Hdf5FileId -> Hdf5DataSetPath -> (Hdf5DataSetId -> IO a) -> IO a
withHdf5DataSet fileId path = bracket (openHdf5DataSet fileId path) closeHdf5DataSet

openHdf5DataSpace :: Hdf5DataSetId -> IO Hdf5DataSpaceId
openHdf5DataSpace (Hdf5DataSetId dsid) = do
  spaceId <- checkInvalidHid (h5dget_space dsid)
  pure (Hdf5DataSpaceId spaceId)

closeHdf5DataSpace :: Hdf5DataSpaceId -> IO ()
closeHdf5DataSpace (Hdf5DataSpaceId spaceId) = checkError (h5sclose spaceId)

withHdf5DataSpace :: Hdf5DataSetId -> (Hdf5DataSpaceId -> IO a) -> IO a
withHdf5DataSpace dsId = bracket (openHdf5DataSpace dsId) closeHdf5DataSpace

withHdf5FileAndDataSet :: FilePath -> Hdf5DataSetPath -> (Hdf5DataSet -> IO a) -> IO a
withHdf5FileAndDataSet fp dataPath f =
  withHdf5File fp \fileId ->
    withHdf5DataSet fileId dataPath \dataSetId ->
      withHdf5DataSpace dataSetId (f . Hdf5DataSet fileId dataSetId)

getDataSetDimensions :: Hdf5DataSet -> IO [Int]
getDataSetDimensions ds = do
  let dataSpaceId = ungetHdf5DataSpaceId ds.hdf5DataSpaceId
  rank <- h5sget_simple_extent_ndims dataSpaceId
  withArray (replicate (fromIntegral rank) 0) $ \arrayPtr -> do
    checkError (h5sget_simple_extent_dims dataSpaceId arrayPtr nullPtr)
    carray <- peekArray (fromIntegral rank) arrayPtr
    pure (fromIntegral <$> carray)

withImage :: Hdf5DataSet -> [Int] -> [Int] -> [Int] -> (ByteString -> IO a) -> IO a
withImage ds hyperslabOffset hyperslabDim subslab f = do
  let dataSpaceId = ungetHdf5DataSpaceId ds.hdf5DataSpaceId
  withArray (fromIntegral <$> hyperslabOffset) \offset ->
    withArray (fromIntegral <$> hyperslabDim) \count -> do
      checkError
        ( h5sselect_hyperslab
            dataSpaceId
            h5sSelectSet
            offset
            nullPtr
            count
            nullPtr
        )

  withArray (fromIntegral <$> subslab) $ \memspaceDims -> do
    oneImageMemspace <-
      checkInvalidHid
        ( h5screate_simple
            (fromIntegral (length subslab))
            memspaceDims
            nullPtr
        )
    allocaArray (product subslab) $ \imageDataPtr -> do
      checkError
        ( h5dread
            (ungetHdf5DataSetId ds.hdf5DataSetId)
            -- h5tNativeFloat
            h5tNativeUShort
            oneImageMemspace
            dataSpaceId
            h5pDefault
            -- (castPtr @CFloat @() imageDataPtr)
            (castPtr @CUShort @() imageDataPtr)
        )
      -- byteString <- packCStringLen (castPtr imageDataPtr, sizeOf @CFloat undefined * product subslab)
      byteString <- packCStringLen (castPtr imageDataPtr, sizeOf @CUShort undefined * product subslab)
      f byteString
