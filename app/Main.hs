{-# LANGUAGE RecordWildCards  #-}

module Main where

import Graphics.Win32 hiding (try)
import System.Win32.DLL (getModuleHandle)

import System.Environment (getArgs)

import Data.Bits (shift)
import Data.Maybe (isJust)

import Control.Monad.Trans.Reader (runReaderT, ReaderT, ask) 
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)

import Lib  ( createTable
            , drawTable
            , resizeTable
            , stubTable
            , getHeight
            , Table
            )

wM_MOUSEWHEEL :: WindowMessage 
wM_MOUSEWHEEL = 0x020A

wM_SIZING :: WindowMessage 
wM_SIZING = 0x0214

data Env = Env { mdc :: HDC
               , scrY :: INT
               , table :: Table
               }

main :: IO ()
main = do 
    let className = mkClassName "Fonts"
    hInstance <- getModuleHandle Nothing 
    hBrush <- getStockBrush wHITE_BRUSH
    hArrow <- loadCursor Nothing iDC_ARROW

    let wc = ( cS_DBLCLKS
             , hInstance
             , Nothing
             , Just hArrow
             , Just hBrush
             , Nothing
             , className
             )
    mAtom <- registerClass wc
    when (isJust mAtom) $ do 
        mdc <- createCompatibleDC Nothing   

        let env = Env mdc 0 stubTable
        hWnd <- createWindow
            className 
            "Table"
            wS_OVERLAPPEDWINDOW
            (Just cW_USEDEFAULT)
            (Just 0)
            (Just cW_USEDEFAULT)
            (Just 0)
            Nothing
            Nothing
            hInstance
            (wndProc env)

        _ <- sendMessage hWnd wM_CREATE 0 0
        showWindow hWnd sW_SHOWNORMAL
        updateWindow hWnd
        allocaMessage $ messagePump
        
        unregisterClass className hInstance

wndProc :: Env -> WindowClosure
wndProc env hWnd msg wParam lParam 
        | msg == wM_DESTROY = do
            postQuitMessage 0
            return 0

        | msg == wM_CREATE = runReaderT (onCreate hWnd) env

        | msg == wM_PAINT = allocaPAINTSTRUCT $ \ lpps -> do
            hdc <- beginPaint hWnd lpps 
            runReaderT (onPaint hWnd hdc) env
            endPaint hWnd lpps
            return 0


        | msg == wM_ERASEBKGND = 
            return 1

        | msg == wM_MOUSEWHEEL = runReaderT (onWheel hWnd wParam) env

        | msg == wM_SIZE = runReaderT (onSize hWnd) env

        | otherwise = defWindowProc (Just hWnd) msg wParam lParam

onCreate :: HWND -> ReaderT Env IO LRESULT
onCreate hWnd = do
    Env {..} <- ask
    liftIO $ do
        path <-  fmap head $ getArgs
        file <- try $ readFile path :: IO (Either SomeException String)
        let strs = 
                case file of 
                    Left e -> error "File does not exist / Path is not specified."
                    Right contents -> readByLines contents
        table' <- createTable hWnd mdc strs
        drawTable mdc table' 

        setWindowClosure hWnd $ wndProc (Env mdc scrY table')
        return 0

onWheel :: HWND -> WPARAM -> ReaderT Env IO LRESULT
onWheel hWnd wParam = do 
    Env {..} <- ask
    let dY = wheelMove 
    let newScrY = max 0 $ dY + scrY
    liftIO $ do
        when (scrY /= 0 || newScrY /= 0) $ do
            (_, y) <- getRealWindowRect hWnd

            let h = fst $ getHeight table     
            when (newScrY < h - y || dY < 0) $ do
                setWindowClosure hWnd $ wndProc (Env mdc newScrY table)
                invalidateRect (Just hWnd) Nothing True
        return 0 
    where 
        wheelMove = (* (-8)) $ fromIntegral wParam `shift` (-16) `div` 120

onSize :: HWND -> ReaderT Env IO LRESULT
onSize hWnd = do
    Env {..} <- ask
    liftIO $ do
        (x0', y0', x1', y1') <-  getRelativeWindowRect hWnd 
        let (x0, y0, x1, y1) = (fromIntegral x0', fromIntegral y0', fromIntegral x1', fromIntegral y1')

        let newTable = resizeTable (x1' - x0') table

        let h = fst $ getHeight newTable 
        let wH = fromIntegral (y1 - y0)
    
        let newScrY = getNewScrY h wH scrY

        setWindowClosure hWnd (wndProc (Env mdc newScrY newTable))

        resize hWnd x0 y0 (x1 - x0) (y1 - y0)
        invalidateRect (Just hWnd) Nothing True
        return 0

getNewScrY :: INT -> INT -> INT -> INT 
getNewScrY h wH scrY 
    | wH > h = 0
    | otherwise = min scrY (h - wH)

onPaint :: HWND -> HDC -> ReaderT Env IO ()
onPaint hWnd hdc = do
    Env {..} <- ask
    liftIO $ do
        (x, y) <-  getRealWindowRect hWnd

        bitmap <- createCompatibleBitmap mdc (x) (scrY + y) 
        prevBitmap <- selectBitmap mdc bitmap

        hBrush <- getStockBrush wHITE_BRUSH
        
        fillRect mdc (0, scrY, x, y + scrY) hBrush
        _ <- drawTable mdc table 

        bitBlt hdc 0 0 x y mdc 0 scrY sRCCOPY

        deleteBitmap prevBitmap
        deleteBrush hBrush

readByLines :: String -> [String]
readByLines = filter (not . null) . lines

resize :: HWND -> Int -> Int -> Int -> Int -> IO ()
resize hWnd x0 y0 dx dy 
    | dx < minX && dy < minY = moveWindow hWnd x0 y0 minX minY False
    | dx < minX              = moveWindow hWnd x0 y0 minX dy False
    | dy < minY              = moveWindow hWnd x0 y0 dx minY False
    | otherwise              = return ()
    where
        minX = 500
        minY = 300

messagePump :: LPMSG -> IO ()
messagePump lpMsg = do 
    fContinue <- getMessage lpMsg Nothing
    when fContinue $ do
        translateMessage lpMsg 
        dispatchMessage lpMsg 
        messagePump lpMsg       

getRelativeWindowRect :: HWND -> IO RECT
getRelativeWindowRect hWnd = do
    (x0, y0, x1, y1) <- getWindowRect hWnd 
    return (x0, y0, x1, y1 - 39)
    

getRealWindowRect :: HWND -> IO (LONG, LONG)
getRealWindowRect hWnd = do
    (x0, y0, x1, y1) <- getWindowRect hWnd 
    return (x1 - x0 - 14, y1 - y0 - 39)
