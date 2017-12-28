module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Markdown
import Regex
import Json.Decode as Decode exposing (Decoder)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL --


type alias Page =
    { name : String
    , comment : String
    , aliases : List Alias
    , types : List Type
    , values : List Value
    }


type Doc
    = AliasDoc Alias
    | TypeDoc Type
    | ValueDoc Value


type alias Alias =
    { name : String
    , comment : String
    }


type alias Type =
    { name : String
    , comment : String
    }


type alias Value =
    { name : String
    , comment : String
    , type_ : String
    }


type alias Flags =
    { docsJson : Decode.Value
    }


type alias Model =
    { selectedPage : Maybe Page
    , pages : Result String (List Page)
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { selectedPage = Nothing
      , pages = decodeDocs flags.docsJson
      }
    , Cmd.none
    )


decodeDocs : Decode.Value -> Result String (List Page)
decodeDocs docsJson =
    Decode.decodeValue (Decode.list pageDecoder) docsJson


pageDecoder : Decoder Page
pageDecoder =
    Decode.map5 Page
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "aliases" (Decode.list aliasDecoder))
        (Decode.field "types" (Decode.list typeDecoder))
        (Decode.field "values" (Decode.list valueDecoder))


aliasDecoder : Decoder Alias
aliasDecoder =
    Decode.map2 Alias
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)


typeDecoder : Decoder Type
typeDecoder =
    Decode.map2 Type
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)


valueDecoder : Decoder Value
valueDecoder =
    Decode.map3 Value
        (Decode.field "name" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "type" Decode.string)



-- UPDATE --


type Msg
    = SwitchPage Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchPage page ->
            ( { model | selectedPage = Just page }, Cmd.none )


getFirstSubmatch : Regex.Match -> Maybe String
getFirstSubmatch match =
    case match.submatches of
        [] ->
            Nothing

        s :: _ ->
            s


docList : Page -> List Doc
docList page =
    List.concat
        [ List.map AliasDoc page.aliases
        , List.map TypeDoc page.types
        , List.map ValueDoc page.values
        ]


docName : Doc -> String
docName doc =
    case doc of
        AliasDoc { name } ->
            name

        TypeDoc { name } ->
            name

        ValueDoc { name } ->
            name


docComment : Doc -> String
docComment doc =
    case doc of
        AliasDoc { comment } ->
            comment

        TypeDoc { comment } ->
            comment

        ValueDoc { comment } ->
            comment


docType : Doc -> String
docType doc =
    case doc of
        AliasDoc _ ->
            ""

        TypeDoc _ ->
            ""

        ValueDoc { type_ } ->
            " : `" ++ type_ ++ "`"


getDocString : Page -> String -> Maybe String
getDocString page name =
    let
        docs =
            docList page
                |> List.filter ((==) (String.trim name) << docName)
    in
        case docs of
            [] ->
                Nothing

            doc :: _ ->
                Just <|
                    String.concat
                        [ "### [`"
                        , docName doc
                        , "`](#"
                        , String.toLower (docName doc)
                        , ")"
                        , docType doc
                        , "\n\n"
                        , docComment doc
                        ]


replaceDocLine : Page -> Regex.Match -> String
replaceDocLine page match =
    getFirstSubmatch match
        |> Maybe.withDefault ""
        |> String.split ","
        |> List.map (getDocString page)
        |> List.filterMap identity
        |> String.join "\n\n"


replaceDocs : Page -> String
replaceDocs page =
    Regex.replace
        Regex.All
        (Regex.regex "@docs(.*)")
        (replaceDocLine page)
        page.comment



-- VIEW --


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.class "docs-container"
        , Attributes.style
            [ ( "padding", "10px" )
            , ( "width", "1000px" )
            , ( "margin", "auto" )
            , ( "font-family", "sans-serif" )
            , ( "display", "flex" )
            ]
        ]
        [ model.selectedPage
            |> Maybe.map docs
            |> Maybe.withDefault (emptyDiv "1")
        , model.pages
            |> Result.map packageNav
            |> Result.withDefault (emptyDiv "0.25")
        ]


emptyDiv : String -> Html msg
emptyDiv flex =
    Html.div
        [ Attributes.style [ ( "flex", flex ) ]
        ]
        []


packageNav : List Page -> Html Msg
packageNav pages =
    Html.div
        [ Attributes.style
            [ ( "flex", "0.25" )
            , ( "padding", "20px" )
            , ( "border-left", "1px solid #eeeeee" )
            ]
        ]
    <|
        (Html.h2
            []
            [ Html.text "Module Docs" ]
        )
            :: (List.map docLink pages)


docLink : Page -> Html Msg
docLink page =
    Html.div []
        [ Html.a
            [ Attributes.href "#"
            , Events.onClick (SwitchPage page)
            ]
            [ Html.text page.name ]
        ]


docs : Page -> Html Msg
docs page =
    Html.div
        [ Attributes.class "docs"
        , Attributes.style
            [ ( "flex", "1" )
            , ( "padding-right", "20px" )
            ]
        ]
        [ Html.div
            [ Attributes.class "docs__title" ]
            [ Html.h1 []
                [ Html.text page.name ]
            ]
        , Html.div
            [ Attributes.class "docs__comment" ]
            [ mdToHtml (replaceDocs page) ]
        ]


mdToHtml : String -> Html msg
mdToHtml md =
    let
        defaultOptions =
            Markdown.defaultOptions
    in
        Markdown.toHtmlWith
            { defaultOptions | defaultHighlighting = Just "elm" }
            []
            md



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
