{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Loja where

import Import
import Database.Persist.Postgresql as DB
import Text.Lucius

getLojaR :: InstrumentoId -> Handler Html
getLojaR tid = do
    instrumento <- runDB $ get404 tid
    defaultLayout $ do
        [whamlet|
            <h1>
                Instrumento #{instrumentoNome instrumento}
            <h2>
                Estado: #{show $ instrumentoTipo instrumento}
            <h2>
                Valor: #{instrumentoValor instrumento}
              
        |]
