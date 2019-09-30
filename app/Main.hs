module Main where

import Graphics.Win32 hiding (try)
import System.Win32.DLL (getModuleHandle)
import Debug.Trace
import System.Environment (getArgs)

import Data.Bits (shift)
import Control.Monad (when, mapM_)
import Data.Maybe (isJust)
import Lib
import Control.Exception (try, SomeException)
import System.Exit (exitFailure)


wM_MOUSEWHEEL :: WindowMessage 
wM_MOUSEWHEEL = 0x020A

readByLines :: FilePath -> IO [String]
readByLines path = readFile path >>= return . filter (not . null) . lines

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
            (wndProc mdc 0 stubTable)

        _ <- sendMessage hWnd wM_CREATE 0 0
        showWindow hWnd sW_SHOWNORMAL
        updateWindow hWnd
        allocaMessage $ messagePump
        
        unregisterClass className hInstance

wndProc :: HDC -> INT -> Table -> 
        HWND -> WindowMessage -> WPARAM -> LPARAM -> IO LRESULT 
wndProc mdc scrY table
        hWnd msg wParam lParam 
        | msg == wM_DESTROY = do
            postQuitMessage 0
            return 0

        | msg == wM_CREATE = do
            args <- getArgs
            let path = head args 
            file <- try $ readFile path :: IO (Either SomeException String)
            let strs = 
                    case file of 
                        Left e -> error "something went wrong"
                        Right contents -> filter (not . null) . lines $ contents

                        
            table' <- createTable hWnd mdc strs
            drawTable mdc table' 

            setWindowClosure hWnd $ wndProc mdc scrY table'
            return 0

        | msg == wM_PAINT = allocaPAINTSTRUCT $ \ lpps -> do
            hdc <- beginPaint hWnd lpps 
            table' <- onPaint hWnd hdc mdc scrY table
            endPaint hWnd lpps

            setWindowClosure hWnd $ wndProc mdc scrY table' 
            return 0

        | msg == wM_ERASEBKGND = 
            return 1

        | msg == wM_MOUSEWHEEL = do 
            let d = (* (-5)) $ fromIntegral wParam `shift` (-16) `div` 120
            let newScrY = max 0 $ d + scrY

            when (scrY /= 0 || newScrY /= 0) $ do
                (_, y0, _, y1) <- getWindowRect hWnd

                let h = fst $ getHeight table     
                when (newScrY < h - (y1 - y0) || d < 0) $ do
                    setWindowClosure hWnd $ wndProc mdc newScrY table
                    invalidateRect (Just hWnd) Nothing True
            return 0 

        | msg == wM_SIZE = do 
            (x0', y0', x1', y1') <- getWindowRect hWnd 
            let h = fst $ getHeight table
            let (x0, y0, x1, y1) = (fromIntegral x0', fromIntegral y0', fromIntegral x1', fromIntegral y1')
            resize hWnd x0 y0 (x1 - x0) (y1 - y0)

            return 0

        | otherwise = defWindowProc (Just hWnd) msg wParam lParam

onPaint :: HWND -> HDC -> HDC -> INT -> Table -> IO (Table)
onPaint hWnd hdc mdc scrY table = do
    (x0, y0, x1, y1) <- getWindowRect hWnd

    bitmap <- createCompatibleBitmap mdc (x1 - x0 - 14) (scrY + y1 - y0) 
    prevBitmap <- selectBitmap mdc bitmap

    hBrush <- getStockBrush wHITE_BRUSH
    
    let table' = resizeTable (x1 - x0 - 14) table
    fillRect mdc (0, scrY, x1, y1 + scrY) hBrush
    _ <- drawTable mdc table' 

    bitBlt hdc 0 0 (x1 - x0 - 14) (y1 - y0) mdc 0 scrY sRCCOPY

    deleteBitmap prevBitmap
    deleteBrush hBrush

    return table'
    

resize :: HWND -> Int -> Int -> Int -> Int -> IO ()
resize hWnd x0 y0 dx dy 
    | dx < minX && dy < minY = moveWindow hWnd x0 y0 minX minY False
    | dx < minX              = moveWindow hWnd x0 y0 minX dy False
    | dy < minY              = moveWindow hWnd x0 y0 dx minY False
    | otherwise              = invalidateRect (Just hWnd) Nothing True
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
        
