{-# LANGUAGE ForeignFunctionInterface #-}

module YubiKey(withYubiKey) where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable (peek)
import Foreign.Ptr
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)

type YkKeyPtr = ()
type YubiKey = Ptr YkKeyPtr

data YubiKeyError = YK_EUSBERR | YK_NOERROR deriving Eq
instance Enum YubiKeyError where
  toEnum i = case i of
    1 -> YK_EUSBERR
    _ -> YK_NOERROR
  fromEnum YK_EUSBERR = 1

foreign import ccall "yk_init" yk_init :: IO CInt
foreign import ccall "yk_release" yk_release :: IO CInt

foreign import ccall "yk_open_first_key" yk_open_first_key :: IO YubiKey
foreign import ccall "yk_close_key" yk_close_key :: YubiKey -> IO CInt
foreign import ccall "yk_check_firmware_version" yk_check_firmware_version :: YubiKey -> IO CInt

foreign import ccall "_yk_errno_location" _yk_errno_location :: IO (Ptr CInt)
foreign import ccall "yk_strerror" yk_strerror :: CInt -> IO CString
foreign import ccall "yk_usb_strerror" yk_usb_strerror :: IO CString

ykStrError :: IO String
ykStrError = _yk_errno_location >>= peek >>= ykCheckError >>= peekCString

ykCheckError :: CInt -> IO CString
ykCheckError i
  | toEnum (fromIntegral i) == YK_EUSBERR = yk_usb_strerror
  | otherwise = yk_strerror i

ykGetError :: a -> CInt -> ErrorT String IO a
ykGetError _   0 = lift ykStrError >>= fail
ykGetError ret _ = return ret

ykInit :: ErrorT String IO ()
ykInit = lift yk_init >>= ykGetError ()

ykOpenFirstKey :: ErrorT String IO YubiKey
ykOpenFirstKey = do
  key <- lift yk_open_first_key
  if key == nullPtr
    then ykGetError key 0
    else return key

ykCheckFirwareVersion :: YubiKey -> ErrorT String IO ()
ykCheckFirwareVersion yk = lift (yk_check_firmware_version yk) >>= ykGetError ()

ykCloseKey :: YubiKey -> ErrorT String IO ()
ykCloseKey yk = lift (yk_close_key yk) >>= ykGetError ()

ykRelease :: ErrorT String IO ()
ykRelease = lift yk_release >>= ykGetError ()

withYubiKey :: (YubiKey -> IO a) -> ErrorT String IO a
withYubiKey f = do
  ykInit
  yk <- ykOpenFirstKey
  ykCheckFirwareVersion yk
  val <- lift $ f yk
  ykCloseKey yk
  ykRelease
  return val
