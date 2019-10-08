module Lib
    ( createTable
    , drawTable
    , resizeTable
    , stubTable
    , getHeight
    , Table
    ) where

import Graphics.Win32
import Control.Monad (mapM_, foldM_)
import Debug.Trace

type Width = LONG
type Height = LONG

type Font = (Width, Height)
type Cell = String
type Row = ([Cell], Height)
                    -- of a cell
type Table = ([Row], Width, LONG, Font)

stubTable = ([(["1"], 1)], 1, 1, (1, 1)) :: Table

hack :: LONG -> [a] -> [[a]]
hack _ [] = []
hack n xs = take (fromIntegral n) xs : (hack n $ drop (fromIntegral n) xs)

-- assuming fixed 
fontDimension :: HDC -> IO Font
fontDimension hdc = do
    (w, h) <- getTextExtentPoint32 hdc "y"
    return (w, h + 0)

createTable :: HWND -> HDC -> [String] -> IO Table
createTable hWnd hdc strs = do
    hFont <- getStockFont aNSI_FIXED_FONT
    selectFont hdc hFont
    (x0, _, x1, _) <- getWindowRect hWnd
    font <- fontDimension hdc
    return $ createPureTable (x1 - x0 - 14) font strs
    
createPureTable :: Width -> Font -> [String] -> Table 
createPureTable wndW f@(charW, charH) strs = ((zip rows rowsH), cellW, charN, f)
    where 
        (cellN, cellW) = cellsInRow wndW
        rows = hack cellN strs              
        charN = cellW `div` charW 

        rowsM = fmap (fromIntegral . maximum . fmap length) rows 
        rowsH = fmap (\len -> (charH *) . head . dropWhile (\n -> n * charN <= len) $ [1..]) rowsM

cellsInRow :: Width -> (LONG, Width)
cellsInRow wndW 
    | wndW < 300 = let n = 5 in (n, wndW `div` n)
    | wndW < 500 = let n = 7 in (n, wndW `div` n)
    | wndW < 750 = let n = 10 in (n, wndW `div` n)
    | wndW < 850 = let n = 12 in (n, wndW `div` n)
    | otherwise  = let n = 15 in (n, wndW `div` n)


resizeTable :: Width -> Table -> Table 
resizeTable wndW (rows, cellW, charN, font) = createPureTable wndW font text
    where 
        text = concat . fmap fst $ rows

drawTable :: HDC -> Table -> IO ()
drawTable hdc table@(rows, cellW, charN, (_, charH)) = do
    foldM_ step 0 rows
    drawGrid hdc scrW scrH ws hs
    where 
        (scrW, ws) = getWidth table
        (scrH, hs) = getHeight table
        step = \dH row@(_, rowH) -> (drawRow hdc dH cellW charN charH row) >> return (dH + rowH) 

-- type Table = ([Row], Width, LONG, Height)
getWidth :: Table -> (Width, [Width])
getWidth (rs, dw, _, _) = (last ws, ws)
    where
        (row, _) = head rs 
        ws = take (length row) $ iterate (+ dw) dw
  
getHeight :: Table -> (Height, [Height])
getHeight (rs, _, _, _) = (last hs, hs)
    where 
        hs = scanl (\dh (_, h) -> h + dh) 0 rs

drawGrid :: HDC -> Width -> Height -> [Width] -> [Height] -> IO ()
drawGrid hdc scrW scrH ws hs =
    mapM_ lambda ws >> mapM_ lambda' hs
    where 
        lambda w = moveToEx hdc w 0 >> lineTo hdc w scrH
        lambda' h = moveToEx hdc 0 h >> lineTo hdc scrW h

drawRow :: HDC -> LONG -> LONG -> LONG -> Height -> Row -> IO ()
drawRow hdc y cellW charN charH (cells, _) = mapM_ lambda zcells
    where
        zcells = zip [0..] cells 
        lambda = \(n, str) -> drawCell hdc (n * cellW) y charN charH str

drawCell :: HDC -> LONG -> LONG -> LONG -> Height -> Cell -> IO ()
drawCell hdc x y charN charH str = mapM_ draw strs
    where 
        strs = zip [0..] $ hack charN str 
        draw = \(n, str') -> textOut hdc (fromIntegral x) (y + fromIntegral charH * n) str'