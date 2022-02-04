module Main where

  import BoardGenerate
  import Tools
  import System.Random
  import DeductiveReasoning
  
  main :: IO ()
  main = do
    putStrLn "\ESC[90m>> Cantidad de filas:"
    ns <- getLine
    putStrLn "\ESC[90m>> Cantidad de columnas:"
    ms <- getLine
    putStrLn "\ESC[90m>> Intervalo de tiempo:"
    ts <- getLine
    putStrLn ""
    do
      --generate_board
      let h = read ns :: Int
      let w = read ms :: Int
      let t = read ts :: Int

      childrensCountTemp : obsCount : robotsCount : _ <- generateCountRandom h w

      random <- randomRIO (23, 500 :: Int)

      let square = squareGet childrensCountTemp 1
      let hc:wc:_ = hAndwCorralGet childrensCountTemp square
      let childrensCount = hc * wc
      let rc0:cc0:rc1:cc1:_ = coordCorralGet hc wc h w

      let board = boardGenerateAll  h w childrensCount obsCount robotsCount rc0 cc0 rc1 cc1 random

      --printear
      putStrLn "\ESC[96m Objetos en el ambiente :"

      putStrLn "\ESC[90mx -> casilla vacia"
      putStrLn "\ESC[96mx -> borde del corral"
      putStrLn "\ESC[92m* -> ninno en la casilla"
      putStrLn "\ESC[93mx -> casilla sucia"
      putStrLn "\ESC[91mx -> obstaculo en la casilla"
      putStrLn "\ESC[94m0 -> robot en la casilla sin ninno cargado"
      putStrLn "\ESC[92m0 -> robot en la casilla con ninno cargado"



      putStrLn "\ESC[91m++++++++++++++++++++++++++++++++++++++++++++++++++++"
      putStrLn "\ESC[90m>> Ambiente inicial . . . "
      draw board h w
      putStrLn "\ESC[91m++++++++++++++++++++++++++++++++++++++++++++++++++++"

      init__ board h w rc0 cc0 rc1 cc1 childrensCount obsCount robotsCount t t 20 []

    return()

  
  init__ :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int  -> Int -> [[Int]] -> IO() 
  init__ board h w rc0 cc0 rc1 cc1 childrensCount obstCount robotsCount t0 t count childsDown | count == 0 = return()
                                                                                              | t0 == 1 = do
                                                                                                --agent activities 
                                                                                                let board__: childsDown__ : _ = play board h w rc0 cc0 rc1 cc1 childsDown
                                                                                                --childrens move
                                                                                                let childrens = childrensGet board__ h h w w rc0 cc0 rc1 cc1
                                                                                                count1 <- randomRIO (0, length childrens :: Int)
                                                                                                let boardWithMoves__ = boardWithChildrensMove board__ h w rc0 cc0 rc1 cc1 childrens (length childrens)
                                                                                                let clean = environmentClean boardWithMoves__ h w
                                                                                                let write | clean = "\ESC[90m La casa esta limpia . . ."
                                                                                                          | otherwise = "\ESC[93m La casa esta sucia . . ."
                                                                                                
                                                                                                putStrLn ""
                                                                                                putStrLn "**************************************************"
                                                                                                putStrLn "\ESC[94m Robots de casa trabajando . . . "
                                                                                                putStrLn ""
                                                                                                draw board__ h w
                                                                                                putStrLn ""
                                                                                                putStrLn "\ESC[92m Movimiento de los ninnos y suciedad . . ."
                                                                                                putStrLn ""
                                                                                                draw boardWithMoves__ h w
                                                                                                putStrLn ""
                                                                                                putStrLn  write
                                                                                                putStrLn ""
                                                                                                putStrLn "**************************************************"


                                                                                                init__ boardWithMoves__ h w rc0 cc0 rc1 cc1 childrensCount obstCount robotsCount t t (count - 1) childsDown__

                                                                                              | otherwise = do
                                                                                                --agent activities 
                                                                                                let board__: childsDown__ : _ = play board h w rc0 cc0 rc1 cc1 childsDown
                                                                                                let clean = environmentClean board__ h w
                                                                                                let write | clean = "\ESC[90m La casa esta limpia . . ."
                                                                                                          | otherwise = "\ESC[93m La casa esta sucia . . ."
                                                                                                putStrLn ""
                                                                                                putStrLn "**************************************************"
                                                                                                putStrLn "\ESC[94m Robots de casa trabajando . . . "
                                                                                                putStrLn ""
                                                                                                draw board__ h w
                                                                                                putStrLn ""
                                                                                                putStrLn  write
                                                                                                putStrLn ""
                                                                                                putStrLn "**************************************************"

                                                                                                init__ board__ h w rc0 cc0 rc1 cc1 childrensCount obstCount robotsCount (t0 - 1) t (count - 1) childsDown__