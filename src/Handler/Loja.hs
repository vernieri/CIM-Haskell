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

postLojaR :: InstrumentoId -> Handler Html
postLojaR tid = do
    runDB $ delete tid
    redirect ListaLojaR

getListaLojaR :: Handler Html
getListaLojaR = do
    instrumento <- runDB $ selectList [] [Asc InstrumentoNome]
    defaultLayout $ do
        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.2/js/materialize.min.css"
        setTitle "CIM"        
        toWidget $(luciusFile "templates/rapidao.lucius")        

        [whamlet|

            <div class="container">
                <div class="enter">
                        <div class="row">
                            <div class="col s12 m8 offset-m2 offset-l1 l10 z-depth-1 white opacity-95 border-circle">
                                <div class="section">          
                                    <table>
                                        <thead>
                                            <tr>
                                                <th>
                                                    <h2>Nome

                                                <th>
                                                    <h2>Estado

                                                <th>
                                                    <h2>Valor

                                                <th>
                                                    <form action=@{InstrumentoR} method="get">
                                                        <input type="submit" value="Adicionar!">


                                        <tbody>
                                            $forall (Entity tid instrumento) <- instrumento
                                                <tr>
                                                    <td>
                                                        <a href=@{LojaR tid}>
                                                            #{instrumentoNome instrumento}

                                                    <td>
                                                        #{show $ instrumentoTipo instrumento}

                                                    <td>
                                                        #{instrumentoValor instrumento}
                                                    
                                                    <td>
                                                        <form action=@{LojaR tid} method=post>
                                                            <input type="submit" value="Comprar!">
        |]


formInstrumento :: Form Instrumento
formInstrumento = renderDivs $ Instrumento
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Estado: " Nothing
        <*> areq intField  "Valor: " Nothing
