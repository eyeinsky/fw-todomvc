module Main where

import qualified Data.Text as TS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Rapid
import System.Environment
import System.IO

import X.Prelude as P
import qualified HTML
import X hiding (Status, Header)
import JS.TH (deriveToExpr)
import URL.TH
import Static
import Warp_Helpers
import qualified CSS
import X.Template.V2 as T
import JS.Roundtrip (obj, Data)

-- * Item

data Item = Item
  { itemCompleted :: Bool
  , itemDescription :: TS.Text
  , itemEdit :: TS.Text
  } deriving (Show, Data)
makeFields ''Item
deriveToExpr ''Item

-- * Components

data Header = Header
data Main = Main { items :: [Item] }
data Footer = Footer

-- * Templates

instance GetTemplate Header () where
  type instance Html' Header () = Html
                          -- | Add item
  type instance In Header () = (Expr String -> Expr ())
  getTemplate addItem = do
    h1' <- cssId $ do
      color $ rgba 175 47 47 0.15
      fontWeight $ 100
      fontSize $ px 100
      top $ px $ 0 - 155
    input' <- cssId $ CSS.placeholder $ color "#e6e6e6"

    _ <- js $ attachOnLoad KeyDown (findBy input') $ \e -> do
      ifonly (e !. "which"  .== 13) $ do
        bare $ addItem (e !. "target" !. "value")
        e !. "target" !. "value" .= ""

    let
      ssr :: Header -> Html
      ssr _ = HTML.header ! Class "header" $ do
        h1 "todos" ! h1'
        input ! input'
          ! Class "new-todo"
          ! placeholder "What needs to be done?"
          ! Boolean "autofocus" True

    return $ ssrOnly ssr

-- | This section should be hidden by default and shown when there are todos
instance GetTemplate Main () where
  type instance Html' Main () = (Html -> Html)
                        -- | Items
  type instance In Main () = Expr ()
  type instance Out Main () =
    -- | Render item
    ( Expr Item -> Expr () -> Expr () -> Expr (Context Item)
    -- | Update count
    , Expr (Int -> ())
    )
  getTemplate items = mdo
    footer <- getTemplate @Footer @() clearCompleted
    item <- getTemplate @Item @() ()

    main' <- cssId $ pure ()
    ul' <- cssId $ pure ()

    cssRule (".main[data-show=completed]" :: TagName) $ do
      descendant (".todo-list li:not(.completed)" :: TagName) $ display "none"
      descendant (".filters a[href='#/completed']" :: TagName) $ do
        borderColor $ rgba 175 47 47 0.2
      descendant (".filters li a.selected" :: TagName) $ do
        borderColor $ "transparent" <> important

    cssRule (".main[data-show=active]" :: TagName) $ do
      descendant (".todo-list li.completed" :: TagName) $ display "none"
      descendant (".filters a[href='#/active']" :: TagName) $ do
        borderColor $ rgba 175 47 47 0.2
      descendant (".filters li a.selected" :: TagName) $ do
        borderColor $ "transparent" <> important

    toggleAllCompleted <- js $ newf $ \ev -> do
      compl <- const $ ev !. "target" !. "checked"
      forOf (items !/ "values") $ \(ctx :: Expr (Context Item)) -> do
        T.source ctx^.completed .= compl
        bare $ item^.T.update $ ctx

    clearCompleted <- js $ newf $ do
      forOf (items !/ "values") $ \(ctx :: Expr (Context Item)) -> do
        ifonly (T.source ctx^.completed) $ do
          nodes' <- const $ T.nodes ctx
          iterArray nodes' $ \ix -> bare $ nodes' !- ix !/ "remove"
          bare $ items !// "delete" $ ctx
      bare $ call1 updateCount $ items !. "size"

    js $ onEvent HashChange window $ do
      switch (window !. "location" !. "hash") $ do
        case_ "#/active" $ do
          findBy main' !. "dataset" !. "show" .= "active"
        case_ "#/completed" $ do
          findBy main' !. "dataset" !. "show" .= "completed"
        default_ $ do
          findBy main' !. "dataset" !. "show" .= Undefined

    let
      ssr :: Main -> Html
      ssr (Main items) =
        section ! Class "main" ! main' ! style (display "none") $ do
          let toggleAll = Id "toggle-all"
          input ! toggleAll ! Class "toggle-all" ! type_ "checkbox" ! On Change toggleAllCompleted
          label ! for toggleAll $ "Mark all as complete"
          ul ! ul' ! Class "todo-list" $ case items of
            _ : _ -> forM_ items (item^.T.ssr)
            _ -> "" -- This -- to make <ul> not self-closing

          footer^.T.ssr $ Footer

    renderItem <- js $ fn $ \item' destroy (toggle :: Expr ()) -> do
      ctx :: Expr (Context Item) <- const $ item^.T.create $ item'
      let fragment = ctx !- 2

      withNodes ctx $ \node -> do
        _ <- onEvent Click (querySelector (Class "destroy") $ Cast node)
          $ bare $ call0 destroy
        _ <- onEvent Click (querySelector (Class "toggle") $ Cast  node) $ do
          bare $ call0 toggle
          bare $ item^.T.update $ ctx
        return ()

      bare $ appendChild (Cast fragment) $ findBy ul'
      retrn ctx

    updateCount <- js $ newf $ \count -> do
      let
        hide = findBy main' !. "style" !. "display" .= "none"
        show = findBy main' !. "style" !. "display" .= "block"
      ifelse (count .> 0)
        (do bare $ call1 (footer^.T.out) count
            show
        )
        (hide)

    return $ emptyTemplate
      & T.ssr .~ ssr
      & T.out .~ (renderItem, updateCount)

instance GetTemplate Item () where
  type instance Html' Item () = Html
  getTemplate _ = mdo

    classes <- replicateM 4 $ css $ pure ()
    let (self : completed_ : descr_ : edit_) = classes

    let
      csr :: Expr Item -> Html
      csr item =
        let liStatus = Dynamic $ ternary (item^.completed) "completed" Undefined
        -- | `editing`, `completed`, <empty>
        in li ! Class liStatus ! self ! Custom "_ctx" (Dynamic $ Cast item) $ do
          div ! Class "view" $ do
            input ! Class "toggle" ! type_ "checkbox" ! Boolean "checked" False ! completed_
            label ! descr_ $ toHtml $ item^.description
            button "" ! Class "destroy"
          input ! Class "edit" ! Custom "value" (Dynamic $ Cast $ item^.description) ! edit_

    create <- js $ fn $ \(item :: Expr Item) -> do
      fragment <- createHtmls $ csr item
      -- | Make copy of document fragment content -- to pass it out
      nodes' <- const $ ex "Array" !// "from" $ fragment !. "childNodes"
      ctx <- const $ context item nodes'
      ctx !- 2 .= fragment

      withNodes ctx $ \node -> do
        view <- const $ querySelector' self $ Cast node
        edit' <- const $ querySelector' (Class "edit") $ Cast node

        void $ onEvent DblClick (querySelector' descr_ $ Cast node) $ do
          arr <- const $ querySelectorAll self $ Cast document
          iterArray (Cast arr) $ \ix -> do
            bare $ arr !- ix !. "classList" !// "remove" $ "editing"
          bare $ view !. "classList" !// "toggle" $ "editing"
          bare $ edit' !/ "focus"
        void $ onEvent KeyDown edit' $ \event -> do
          let val = event !. "target" !. "value"
          ifonly (event !. "which"  .== 13) $ do
            item^.description .= val
            item^.edit .= val
            bare $ update ctx
            bare $ view !. "classList" !// "toggle" $ "editing"

      retrn ctx

    update <- js $ fn $ \(ctx :: Expr (Context Item)) -> do
      item <- const $ T.source ctx
      withNodes ctx $ \node -> do
        let node' = Cast node
        self' <- const $ querySelector' self node'
        -- | Completed
        bare $ ternary (item^.completed)
          (self' !. "classList" !// "add" $ "completed")
          (self' !. "classList" !// "remove" $ "completed")
        querySelector' completed_ node' !. "checked" .= item^.completed
        -- | Description, Edit
        querySelector' descr_ node' !. "innerHTML" .= item^.description
        querySelector' edit_ node' !. "value" .= item^.edit

      retrn (Undefined :: Expr ())

    return $ emptyTemplate
      & T.create .~ create
      & T.update .~ update

-- | This footer should be hidden by default and shown when there are todos
instance GetTemplate Footer () where
  type instance Html' Footer () = Html
  type instance In Footer () = Expr ()
  type instance Out Footer () = Expr (Int -> ())
  getTemplate clearCompleted = do
    cls <- css $ color $ hex 0x777777
    count' <- cssId $ pure ()
    countText <- cssId $ pure ()
    updateCount <- js $ newf $ \count -> do
      findBy count' !. "innerHTML" .= count
      findBy countText !. "innerHTML" .= ternary (count .=== 1) "item left" "items left"

    let
      ssr :: Footer -> Html
      ssr _ = footer ! Class "footer" ! cls $ do
        span ! Class "todo-count" $ do -- ^ '0 items left' by default
          strong "0" ! count'
          text " "
          span ! countText $ text "items left"
        -- Remove this if you don't implement routing -->
        ul ! Class "filters" $ do
          li $ a ! Class "selected" ! HTML.href "#/" $ "All"
          li $ a                    ! HTML.href "#/active" $ "Active"
          li $ a                    ! HTML.href "#/completed" $ "Completed"
          -- Hidden if no completed items are left ↓
        button ! Class "clear-completed" ! On Click clearCompleted $ "Clear completed"

    return $ emptyTemplate
      & T.ssr .~ ssr
      & T.out .~ updateCount

-- * Site

data RunConf = RunConf { runConfDynPath :: [Segment] }
makeFields ''RunConf

instance Default RunConf where
  def = RunConf mempty

site :: FilePath -> T RunConf
site nodeModules = T $ do

  commonCss <- serveFile $ nodeModules <> "/todomvc-common/base.css"
  appCss <- serveFile $ nodeModules <> "/todomvc-app-css/index.css"
  baseJs <- serveFile $ nodeModules <> "/todomvc-common/base.js"
  appJs <- serveFile "./js/app.js"

  let
    commonHead :: Html
    commonHead = do
      meta (pure ()) ! Custom "charset" "utf-8"
      metaNC "viewport" "width=device-width, initial-scale=1"
      title "Template • TodoMVC"
      stylesheet commonCss
      stylesheet appCss

    frame :: Html -> WebT (ReaderT RunConf IO) Response
    frame content = do
      return $ htmlDoc commonHead $ body $ do
        section ! Class "todoapp" $ do
          content
        footer ! Class "info" $ do
          p "Double-click to edit a todo"
          p $ do
            text "Created by "
            a "eyeinsky" ! href [url|https://github.com/eyeinsky/|]
          p $ do
            text "Part of "
            a "TodoMVC" ! href [url|https://todomvc.com|]
        -- Scripts here. Don't remove ↓
        includeJs baseJs
        includeJs appJs

  return $ \_ -> mdo
    header <- getTemplate @Header @() addItem
    main <- getTemplate @Main @() $ Cast items
    let (renderItem, updateCount) = main^.T.out

    items <- js $ const $ call0 (New $ ex "Map")
    js $ ex "xxx" .= items

    addItem <- js $ fn $ \(descr :: Expr String) -> mdo

      item :: Expr Item <- let_ $ obj Item (lit False) (Cast descr) (Cast descr)
      ctx :: Expr (Context Item) <- let_ Null

      destroy <- newf $ do
        nodes' <- const $ nodes ctx
        iterArray nodes' $ \ix -> bare $ nodes' !- ix !/ "remove"
        bare $ items !// "delete" $ ctx
        bare $ call1 updateCount $ items !. "size"

      toggle <- newf $ do item^.completed .= X.not (item^.completed)

      ctx .= renderItem item destroy toggle
      bare $ call (items !. "set") [ctx, ctx]
      bare $ call1 updateCount $ items !. "size"
      retrn (Undefined :: Expr ())

    frame $ do
      header^.T.ssr $ Header
      main^.T.ssr $ Main []

-- * Web server

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  maybeTls <- tlsSettingsEnvIO "DEV_WEBSERVER_CERT" "DEV_WEBSERVER_KEY"
  (nodeModules : _) <- getArgs
  siteMain maybeTls def def prodUrl settings $ site nodeModules
  where settings = Warp.setPort 8081 Warp.defaultSettings

prodUrl :: URL
prodUrl = [url| https://captainhaskell.ee/todomvc |]

-- * Hot reload

hot, stop :: IO ()
hot = Rapid.rapid 0 $ \r -> do
  Rapid.restart r ("webserver" :: String) main
stop = Rapid.rapid 0 $ \r -> do
  Rapid.stop r ("webserver" :: String) main
