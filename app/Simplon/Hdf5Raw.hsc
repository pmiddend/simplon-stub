{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Simplon.Hdf5Raw where

import Foreign.C.Types (CInt (CInt), CLong(CLong), CUInt(CUInt), CULong(CULong))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

#include <hdf5.h>

-- hid_t is defined as int64_t, so CLong fits hopefully
type HidT = CLong

type HSizeT = CULong

-- enum
type H5F_close_degree_t = CInt
-- enum
type H5S_seloper_t = CInt


type HerrT = CInt

foreign import capi "hdf5.h H5Pcreate" h5pcreate :: HidT -> IO HidT
foreign import capi "hdf5.h H5Pclose" h5pclose :: HidT -> IO HerrT
foreign import capi "hdf5.h H5Pset_fclose_degree" h5pset_fclose_degree :: HidT -> H5F_close_degree_t -> IO HerrT
foreign import capi "hdf5.h H5Fopen" h5fopen :: CString -> CUInt -> HidT -> IO HidT
foreign import capi "hdf5.h H5Fclose" h5fclose :: HidT -> IO HerrT
foreign import capi "hdf5.h H5Dopen" h5dopen :: HidT -> CString -> HidT -> IO HidT
foreign import capi "hdf5.h H5Dclose" h5dclose :: HidT -> IO HerrT
foreign import capi "hdf5.h H5Dget_space" h5dget_space :: HidT -> IO HidT
foreign import capi "hdf5.h H5Sclose" h5sclose :: HidT -> IO HerrT
foreign import capi "hdf5.h H5Sget_simple_extent_ndims" h5sget_simple_extent_ndims :: HidT -> IO CInt
foreign import capi "hdf5.h H5Sget_simple_extent_dims" h5sget_simple_extent_dims :: HidT -> Ptr HSizeT -> Ptr HSizeT -> IO CInt
foreign import capi "hdf5.h H5Sselect_hyperslab" h5sselect_hyperslab :: HidT -> H5S_seloper_t -> Ptr HSizeT -> Ptr HSizeT -> Ptr HSizeT -> Ptr HSizeT -> IO HerrT
foreign import capi "hdf5.h H5Screate_simple" h5screate_simple :: CInt -> Ptr HSizeT -> Ptr HSizeT -> IO HidT
foreign import capi "hdf5.h H5Dread" h5dread :: HidT -> HidT -> HidT -> HidT -> HidT -> Ptr () -> IO HerrT


h5pDefault :: HidT
h5pDefault = #const H5P_DEFAULT

h5pFileAccess :: HidT
h5pFileAccess = #const H5P_FILE_ACCESS

h5fCloseStrong :: H5F_close_degree_t
h5fCloseStrong = #const H5F_CLOSE_STRONG

h5fAccRdOnly :: CUInt
h5fAccRdOnly = #const H5F_ACC_RDONLY

h5sSelectSet :: H5S_seloper_t
h5sSelectSet = #const H5S_SELECT_SET

h5tNativeFloat :: HidT
h5tNativeFloat = #const H5T_NATIVE_FLOAT

h5tNativeUShort :: HidT
h5tNativeUShort = #const H5T_NATIVE_USHORT
