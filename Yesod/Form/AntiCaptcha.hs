{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Form.AntiCaptcha
    ( AntiCaptchaFieldSettings (..)
    , AntiCaptchaFormMessage (..)
    , antiCaptchaForm
    , acfSetAntiCaptchaFormMessageHandler 
    , acfSpinner
    , acfSecondsEpoch
    , acfReq
    , acfHoneypot
    , acfValidate
    ) where

import Prelude

import Yesod.Core
import Yesod.Form
import Yesod.Form.Fields.Honeypot
import Data.AntiCaptcha

import Data.String
import Data.Text (Text)
import Control.Applicative
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Monad.Trans.RWS (RWST, ask)

import qualified Data.Text as T

type ACMForm m = RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang]) Enctype Ints m

data AntiCaptchaFormMessage 
    = ACFMsgSpinnerInvalid
    | ACFMsgSpinnerNotGenerated
    | ACFMsgSecondsEpochBeforeMin
    | ACFMsgSecondsEpochAfterMax
    deriving (Eq, Read, Show)

data AntiCaptchaFormState digest = AntiCaptchaFormState
    { acfsAntiCaptcha :: Maybe AntiCaptcha
    , acfsAntiCaptchaConfig :: AntiCaptchaConfig digest
    , acfsRenderMessage :: ([Text] -> AntiCaptchaFormMessage -> Text)
    }

type AntiCaptchaForm digest m = StateT (AntiCaptchaFormState digest) (ACMForm m)

data AntiCaptchaFieldSettings master = AntiCaptchaFieldSettings
    { acfsLabel :: SomeMessage master
    , acfsTooltip :: Maybe (SomeMessage master)
    , acfsId :: Maybe Text
    , acfsAttrs :: [(Text, Text)]
    } 
                                     
instance IsString (AntiCaptchaFieldSettings a) where
    fromString s = AntiCaptchaFieldSettings (fromString s) Nothing Nothing []

defaultAntiCaptchaMessage
    :: [Text]
    -> AntiCaptchaFormMessage
    -> Text
defaultAntiCaptchaMessage _ ACFMsgSpinnerInvalid = T.pack "Spinner is invalid"
defaultAntiCaptchaMessage _ ACFMsgSpinnerNotGenerated = T.pack "AntiCaptcha was not generated. Spinner could be empty"
defaultAntiCaptchaMessage _ ACFMsgSecondsEpochBeforeMin = T.pack "Form is not ready to be submitted"
defaultAntiCaptchaMessage _ ACFMsgSecondsEpochAfterMax = T.pack "Form has expired"

antiCaptchaForm 
    :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => AntiCaptchaConfig digest 
    -> AntiCaptchaForm digest m (FormResult a, WidgetT site IO ())
    -> MForm m (FormResult a, WidgetT site IO ())
antiCaptchaForm config k = do
    let
        mac = Just $ antiCaptchaFromConfig config
        initalState = AntiCaptchaFormState mac config defaultAntiCaptchaMessage
    evalStateT k initalState

acfSetAntiCaptchaFormMessageHandler 
    :: (MonadHandler m)
    => ([Text] -> AntiCaptchaFormMessage -> Text)
    -> AntiCaptchaForm digest m ()
acfSetAntiCaptchaFormMessageHandler f = do
    state <- get
    let
        mac = acfsAntiCaptcha state
        config = acfsAntiCaptchaConfig state
    put $ AntiCaptchaFormState mac config f

acfSpinner
    :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => Text
    -> AntiCaptchaFieldSettings site
    -> AntiCaptchaForm digest m (FieldView site)
acfSpinner name acfs = do
    state <- get
    let
        mac = acfsAntiCaptcha state
        config = acfsAntiCaptchaConfig state
        renderMsg = acfsRenderMessage state
    (spinnerRes, spinnerView) <- lift $ mreq hiddenField (acfs2fs name acfs) (antiCaptchaSpinner <$> mac)
    let 
        nmac = case spinnerRes of
            (FormSuccess spinner) -> Just $ antiCaptchaFromConfigWithSpinner config spinner
            _                     -> mac
    put $ AntiCaptchaFormState nmac config renderMsg
    return $ spinnerView

-- TODO: pull the messages out
-- TODO: set time in config
acfSecondsEpoch
    :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) 
    => Integer 
    -> Integer 
    -> Text 
    -> AntiCaptchaFieldSettings site 
    -> AntiCaptchaForm digest m (FieldView site)
acfSecondsEpoch minse maxse name acfs = do
    state <- get
    (_, _, langs) <- lift $ ask
    let 
        config = acfsAntiCaptchaConfig state
        nowSecondsEpoch = accSecondsEpoch config
        mac = acfsAntiCaptcha state
        renderMsg = acfsRenderMessage state
        validateSecondsEpoch oldSecondsEpoch = do
            let
                diff = nowSecondsEpoch - oldSecondsEpoch
            return $ if diff <= minse
                then Left $ renderMsg langs ACFMsgSecondsEpochBeforeMin
                else if diff >= maxse
                    then Left $ renderMsg langs ACFMsgSecondsEpochAfterMax
                    else Right oldSecondsEpoch
    
    (secondsEpochRes, secondsEpochView) <- acfReq (checkM validateSecondsEpoch hiddenField) name acfs (Just $ nowSecondsEpoch)
    let 
        nSecondsEpoch = case secondsEpochRes of
            (FormSuccess se) -> se
            _                -> nowSecondsEpoch
        nconfig = AntiCaptchaConfig (accHashFunc config) (accEntry config) (accClientIpAddress config) nSecondsEpoch (accSalt config) (accSecret config)
    put $ AntiCaptchaFormState mac nconfig renderMsg
    return $ secondsEpochView
                    
acfValidate
    :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => AntiCaptchaForm digest m (Maybe Text)
acfValidate = do
    state <- get
    (_, _, langs) <- lift $ ask
    let
        mac = acfsAntiCaptcha state
        config = acfsAntiCaptchaConfig state
        renderMsg = acfsRenderMessage state
        otherAc = antiCaptchaFromConfig config
    case mac of
        Nothing   -> return $ Just $ renderMsg langs ACFMsgSpinnerNotGenerated
        (Just ac) -> if ac /= otherAc
            then return $ Just $ renderMsg langs ACFMsgSpinnerInvalid
            else return $ Nothing

acfReq 
    :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => Field m a
    -> Text
    -> AntiCaptchaFieldSettings site
    -> Maybe a
    -> AntiCaptchaForm digest m (FormResult a, FieldView site)
acfReq field name acfs mvl = do
    state <- get
    let mac = acfsAntiCaptcha state
    lift $ mreq field (fs mac name acfs) mvl
    
acfHoneypot :: (Eq a, RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
    => a 
    -> Field m a 
    -> Text 
    -> AntiCaptchaFieldSettings site
    -> Maybe (Maybe a) 
    -> AntiCaptchaForm digest m (FormResult (Maybe a), FieldView site)
acfHoneypot def field name acfs mvl = do
    state <- get
    let mac = acfsAntiCaptcha state
    lift $ mhoneypot def field (fs mac name acfs) mvl

-- TODO: add optional, mopt

-- TODO: pull the failed function out
fs 
    :: (RenderMessage site (SomeMessage site)) 
    => Maybe AntiCaptcha 
    -> Text 
    -> AntiCaptchaFieldSettings site 
    -> FieldSettings site
fs mac name acfs = case mac of
    (Just ac) -> acfs2fs (antiCaptchaHash ac $ name) acfs
    _         -> acfs2fs "" acfs

acfs2fs 
    :: (RenderMessage site (SomeMessage site))
    => Text 
    -> AntiCaptchaFieldSettings site 
    -> FieldSettings site
acfs2fs name acfs = FieldSettings (acfsLabel acfs) (acfsTooltip acfs) (acfsId acfs) (Just name) (acfsAttrs acfs)