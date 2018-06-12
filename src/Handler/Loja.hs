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
        
getInstrumentoR :: Handler Html
getInstrumentoR = do
    (widget,enctype) <- generateFormPost formInstrumento
    defaultLayout $ do

        addStylesheetRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/css/materialize.min.css"
        addStylesheetRemote "https://fonts.googleapis.com/icon?family=Material+Icons"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/jquery/2.1.4/jquery.js"
        addScriptRemote "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0-beta/js/materialize.min.js"
        setTitle "CIM"
    
        toWidget $(luciusFile "templates/instru.lucius")        
        [whamlet|

            <div class="container">
                <div class="enter">
                        <div class="row">
                            <div class="col s12 m8 offset-m2 offset-l1 l10 z-depth-1 white opacity-95 border-circle">
                                <div class="section">
                                    <h1 class="center-align">Incluir Instrumento
                                        <div class="red lighten-4">
                                <form action=@{InstrumentoR} method="post">
                                    ^{widget}
                                    <input type="submit" value="OK">                                
                                    <div class="divider">
                                    <div class="section">
                                        <div class="input-field">
  
                                        <div class="input-field">
                                                
                                        <div class="input-field">
       
                                        <div class="center-align">

                                            <p>Deseja checar os instrumentos disponiveis?
                                            <a href=@{ListaLojaR}>Checar
            <footer class="page-footer orange lighten-4">
                <div class="row">
                    <div class="col l6 s12">
                        <h5 class="black-text">CIM
                        <p class="black-text">CIM Compra de Instrumentos Musicais
                    <div class="col l4 offset-l2 s12">
                        <h5 class="black-text">Grupo
                        <ul>
                            <li>
                                <a class="black-text"> Joao Victor Vernieri Mendes
                            <li>
                                <a class="black-text"> Gabriel Fernando Moraes

                <div class="footer-copyright grey lighten-4">
                    <p class="black-text"> 2018 CIM                                            
              

        |]

postInstrumentoR :: Handler Html
postInstrumentoR = do
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formInstrumento
    case res of
        FormSuccess instrumento -> do

            tid <- runDB $ insert instrumento 

             
            defaultLayout [whamlet|
                Instrumento #{fromSqlKey tid} inserido com sucesso!
            |]
        _ -> redirect ListaLojaR

        
