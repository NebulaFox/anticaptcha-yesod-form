{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.Fields.Honeypot
    ( checkHoneypotField
    , mhoneypot
    , ahoneypot
    ) where

import Data.Bool
import Data.Eq
import Data.Either
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Control.Monad (Monad)
import Yesod.Core
import Yesod.Form

data HoneypotMessage = MsgNotEmpty
    
honeypotErrorMessage :: HoneypotMessage -> Text
honeypotErrorMessage MsgNotEmpty = "Value should be empty"
    
checkHoneypotField :: (Monad m, Eq a, RenderMessage (HandlerSite m) FormMessage) => a -> Field m a -> Field m a
checkHoneypotField def = check validateHoneypot
    where
        validateHoneypot x
            | x == def  = Right x
            | otherwise = Left $ honeypotErrorMessage MsgNotEmpty


mhoneypot :: (site ~ HandlerSite m, Eq a, MonadHandler m, RenderMessage site FormMessage) => a -> Field m a -> FieldSettings site -> Maybe (Maybe a) -> MForm m (FormResult (Maybe a), FieldView site)
mhoneypot def field = mopt (checkHoneypotField def field)

ahoneypot :: (MonadHandler m, Eq a, RenderMessage (HandlerSite m) FormMessage) => a -> Field m a -> FieldSettings (HandlerSite m) -> Maybe (Maybe a) -> AForm m (Maybe a)
ahoneypot def field = aopt (checkHoneypotField def field)