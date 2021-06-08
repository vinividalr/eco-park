{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import


formLogin :: Form Guia
formLogin = renderDivs $ Guia 
    <$> areq textField "E-mail: "  Nothing
    <*> areq passwordField "Senha:  "  Nothing


getAutR :: Handler Html
getAutR = do
    guia <- lookupSession "_ID"
    (widget,_) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do
        toWidgetHead [lucius|
            html{
                background: url(static/img/aventura.png);
                background-size: auto;
                background-repeat: no-repeat;
                background-position: center;
                background-attachment: fixed;
            }
            h1{
                text-align: left;
                color: #556B2F;
            } 
            .topnav {
              background-color: #333;
              overflow: hidden;
            }
            .topnav a {
                float: left;
                color: #f2f2f2;
                text-align: center;
                padding: 14px 16px;
                text-decoration: none;
                font-size: 17px;
            }
            .topnav a:hover {
                background-color: #ddd;
                color: black;
            }
            .topnav a.active {
              background-color: #556B2F;
              color: white;
            }  
        |]

        [whamlet|
            <div class="topnav">
                <a href="/">
                    HOME
                <a href="/guia">
                    CADASTRE-SE
                $maybe email <- guia
                    <div class="topnav">
                        <a href="/tirolesas">
                            TIROLESAS
                        <a href="/criatirolesas">
                            CRIAR TIROLESAS    
                        <a href="/passeioagua">
                            RAFTING
                        <a href="/criarafting">
                            CRIAR RAFTING
                        <a href="/guia">
                            CRIAR GUIA
                        <a>
                            <form action=@{SairR} method=post>
                                #{email}: <input type="submit" value="LOGOUT">

                $nothing
                    <div class="topnav">
                        <a class="active" href="/login">
                            LOGIN

            $maybe mensa <- msg 
                <div>
                    ^{mensa}

            <h1>
                FAÇA LOGIN!

            <form method=post action=@{AutR}>
                ^{widget}
                <input type="submit" value="Entrar">
        |]
postAutR :: Handler Html
postAutR = do
    ((result,_),_) <- runFormPost formLogin
    case result of
        FormSuccess (Guia email senha) -> do
            guiaExiste <- runDB $ getBy (UniqueEmail email)
            case guiaExiste of
                Nothing -> do
                    setMessage [shamlet|
                        GUIA NÃO CADASTRADO
                    |]
                    redirect AutR
                Just (Entity _ guia) -> do 
                    if senha == guiaSenha guia then do
                        setSession "_ID" (guiaEmail guia)
                        redirect HomeR
                    else do
                        setMessage [shamlet|
                            GUIA E/OU SENHA NÃO BATEM!
                        |]
                        redirect AutR
        _ -> redirect HomeR

postSairR :: Handler Html
postSairR = do
    deleteSession "_ID"
    redirect HomeR