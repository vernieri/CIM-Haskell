{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Usuario where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.Maybe (fromJust)
import Crypto.KDF.BCrypt 
import Funcs

postUsuarioR :: Handler Value
postUsuarioR = do
    maybeEmail <- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    maybeUsername <- lookupPostParam "username"
    hasReqParam <- return $ hasRequiredParameters [maybeEmail, maybePassword, maybeUsername]
    case hasReqParam of
        False -> do
            invalidArgs $ [(pack "Formato Invalido")]
        True -> do
            
            maybeUsuario <- return $ Usuario (fromJust maybeEmail) (fromJust maybePassword) (fromJust maybeUsername)
            maybeUsuarioId <- runDB $ insertUnique maybeUsuario
            case maybeUsuarioId of
                (Just usuarioId) -> do
                    setSession "ID" $ keyToText $ usuarioId
                    redirect ListaLojaR
                Nothing -> do
                    setMessage $ toHtml ("Ja existe um usuario com este email" :: String)
                    redirect SignupPageR
                    
postUserLoginR :: Handler Value
postUserLoginR = do
    maybeEmail<- lookupPostParam "email"
    maybePassword <- lookupPostParam "password"
    hasReqParam <- return $ hasRequiredParameters [maybeEmail, maybePassword]
    case hasReqParam of
        False -> do
            invalidArgs $ [(pack "Formato Valido")]
        True -> do
            maybeUsario <- runDB $ getBy $ UniqueEmail $ fromJust maybeEmail
            case maybeUsario of
                Just usuario -> do
                    --loginAttempt <- return $ validatePassword (BS.pack $ unpack $ usuarioPassword $ entityVal usuario) (BS.pack $ unpack $ fromJust maybePassword)
                    loginAttempt <- return $ validatePassword (BS.pack $ unpack $ usuarioPassword $ entityVal usuario) (BS.pack $ unpack $ fromJust maybePassword)
                    case loginAttempt of                    
                        True -> do
                            setSession "ID" $ keyToText $ entityKey usuario
                            redirect ListaLojaR
                        _ -> do
                            setMessage $ toHtml ("Login nao autorizado" :: String)
                            redirect LoginPageR
                _ -> do
                    setMessage $ toHtml ("login nao autorizado" :: String)
                    redirect LoginPageR
                    
getUserLogoutR :: Handler Value
getUserLogoutR = do
    deleteSession "ID"
    redirect LoginPageR

getLoginPageR :: Handler Html
getLoginPageR = do
    maybeMsg <- getMessage
    pc <- return $ $(widgetFile "login")
    defaultLayout $ do
        $(widgetFile "layoutlogin")
        
getSignupPageR :: Handler Html
getSignupPageR = do
    maybeMsg <- getMessage
    pc <- return $ $(widgetFile "signup")
    defaultLayout $ do
        $(widgetFile "layoutlogin")

getUsuarioByIdR :: UsuarioId -> Handler Value
getUsuarioByIdR usuarioId = do
    usuario <- runDB $ get404 usuarioId 
    username <- return $ usuarioUsername usuario
    email <- return $ usuarioEmail usuario 
    sendStatusJSON ok200 (object["user" .= (username, email)])    
