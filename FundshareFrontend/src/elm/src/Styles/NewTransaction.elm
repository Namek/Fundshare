module Styles.NewTransaction
    exposing
        ( PaymentAddStyleClass(..)
        , c
        , cls
        , cs
        , newTransactionStylesheet
        )

import Material.Options as Options
import Styles.Internal exposing (..)


c : a -> String
c =
    Styles.Internal.c


type PaymentAddStyleClass
    = Window
    | Header
    | MainContainer
    | Main
    | MoneyDirection
    | MoneyDirectionArrow
    | Amount
    | TagsContainer
    | UsualTags
    | UsualTagContainer
    | UsualTag
    | UsualTagSelected
    | Description
    | BtnSaveContainer
    | SaveInfo
    | WithMargin
    | SaveErrorText


cls : PaymentAddStyleClass -> String
cls c =
    c |> t2k


cs : PaymentAddStyleClass -> Options.Property c m
cs =
    cls >> Options.cs


newTransactionStylesheet : String
newTransactionStylesheet =
    c Window ++ """{
        max-width: 550px;
        margin: 0 auto;
    }
    """ ++ c Header ++ """{
        padding: 0.7em;
        margin: 0;
        box-shadow: 0 2px 2px 0 rgba(0,0,0,.4);
        font-family: "Roboto","Helvetica","Arial",sans-serif;
        font-size: 18px;
        font-weight: 400;
        background: rgb(33,150,243);
        color: snow;
    }
    """ ++ c MainContainer ++ """{
        display: flex;
        flex-direction: row;
    }
    """ ++ c Main ++ """{
        flex: 1 0;
        padding: 10px 15px 10px 15px;
        position: relative;
        transition: opacity 0.2s ease-out;
    }

    """ ++ c MoneyDirection ++ """ {
        display: flex;
        justify-content: space-between;
    }
    """ ++ c MoneyDirection ++ """ .mdl-list {
        margin: 0;
    }
    """ ++ c MoneyDirectionArrow ++ """ {
        align-self: center;
        transform: scale(4);
        opacity: 0.5;
    }
    """ ++ c Amount ++ """{
        max-width: 170px;
        margin-left: 10px;
        font-size: 3em;
    }""" ++ c Amount ++ """::placeholder {
        font-size: 0.7em;
        vertical-align: middle;
        padding-left: 0.3em;
    }
    """ ++ c TagsContainer ++ """{
        margin-left: 10px;
    }
    """ ++ c Description ++ """{
        margin-left: 10px;
        width: 100%;
    }
    """ ++ c BtnSaveContainer ++ """ {
        padding: 15px;
    }
    """ ++ c SaveInfo ++ c WithMargin ++ """{
        padding: 20px 39px;
    }
    """ ++ c SaveInfo ++ """ > .mdl-progress {
        width: 100%;
    }
    """ ++ c SaveErrorText ++ """{
        font-weight: bold;
    }

    """ ++ c UsualTags ++ """{
        display: flex;
        flex-direction: column;
    }
    """ ++ c UsualTagContainer ++ """{
        display: flex;
        margin: 0.2em;
        height: 60px;
        width: 60px;
    }
    """ ++ c UsualTag ++ """{
       transition: all 0.2s ease-out;
       transform-origin: 50% 50%;
    }
    """ ++ c UsualTagSelected ++ """{

    }"""
