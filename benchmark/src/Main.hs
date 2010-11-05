
module Main where


-- ParseArgs library
import System.Console.ParseArgs

import System.CPUTime (cpuTimePrecision)
import Data.List (groupBy, sortBy, intersperse)
import System.FilePath ((</>), takeExtension)
import System.Directory (getDirectoryContents, removeFile, createDirectoryIfMissing)
import System.Cmd (system)
import System.IO
  (hPutStrLn, Handle, IOMode(..), stdout, hFlush, hIsEOF, hGetChar, hClose,
   openFile, hFileSize)
import System.Exit (ExitCode(..))
import System.Info (os, arch, compilerVersion)
import Control.Monad (when)

data Library = Hand          -- Handwritten code
             | HandGenericDeriving  -- Handwritten code but with the 
                                    -- GenericDeriving flag on
             | EMGM          -- emgm-0.3.1
             | SYB           -- syb-0.1
             | SYBInline     -- syb-0.1 with inlining tricks
             | SYB3          -- syb-with-class-0.5.1
             | MultiRec      -- multirec-0.4
             | Regular       -- regular-0.2.1
             | RegularInline -- regular-0.2.1 with INLINE pragmas
             | RegularDeep   -- a version of regular-0.1 with deep encodings
             | Instant       -- instant-generics-0.1
             | InstantInline -- instant-generics-0.1 with INLINE pragmas
             | Derived       -- derived instances in UHC
             deriving (Eq, Ord, Show)

data TestName = Eq 
              | Map 
              | Read
              | Show
              | Update      -- Traversals
              | Arbitrary   -- QuickCheck's (1.2)
              | Enum
              | Decode
              | Id
              deriving (Eq, Ord, Show)

data Test = Test { lib      :: Library,
                   testName :: TestName,
                   datatype :: Datatype
                 } deriving (Eq, Ord, Show)

data Datatype = Tree    -- Labelled binary trees
              | Logic   -- Logic expressions
              deriving (Eq, Ord, Show)

derivedTests = [
                 Test Derived Eq Tree
               , Test Derived Eq Logic
--               , Test Derived Id Tree
               ]

ungenericTests = [
                   Test Hand Eq Tree
                 , Test Hand Eq Logic
--                 , Test Hand Id Tree
                 ]

tests = [t | t <- ungenericTests] -- test THAT benchmark!

inCommas :: [String] -> String
inCommas = concat . intersperse ","

printGroupStats :: (Enum a, Fractional a, Floating a, Num a)
                => Handle -> IO [(Test, Int, a)] -> IO ()
printGroupStats h l = do
  l' <- l
  let --group1 :: [[(Test, Int, a)]]
      group1 = groupBy g (sortBy f l')
      f (t1,_,_) (t2,_,_) = compare t1 t2
      g (t1,_,_) (t2,_,_) = t1 == t2
      
--      calcAvgStdDev :: [(Test, Int, a)] -> (Test, a, a)
      calcAvgStdDev x = (fst' (head x), avg x, stddev (avg x))
        where
          avg  l   = sum' l / toEnum (length l)
          avg' l   = sum' l / (toEnum (length l) - 1) -- sample standard deviation
          stddev a = sqrt (avg' [ (t,d,(y - a)^2) | (t,d,y) <- x ])
          fst' (a,_,_) = a
          --sum' :: [(Test, Int, a)] -> a
          sum' []  = 0
          sum' ((_,_,d):ts) = d + sum' ts
      
      --group2 :: [(Test, a, a)] -> [[(Test, a, a)]]
      group2 = groupBy g' . sortBy f'
      f' (t1,_,_) (t2,_,_) = compare (testName t1, datatype t1) 
                                     (testName t2, datatype t2)
      g' (t1,_,_) (t2,_,_) =    testName t1 == testName t2 
                             && datatype t1 == datatype t2
  sequence_ $ map (handleGroup h) $ group2 $ map calcAvgStdDev group1

handleGroup :: (Show a) => Handle -> [(Test, a, a)] -> IO ()
handleGroup _ [] = error "handleGroup []"
handleGroup h g  = do
                    let (t,a,d) = head g
                        name = show (testName t) ++ "/" ++ show (datatype t)
                    -- First line is different
                    hPutStrLn h (inCommas [name, show (lib t), show a, show d])
                    -- Then the rest
                    sequence_ $ map (hPutStrLn h)
                      [ inCommas ["", show (lib t), show a, show d] | (t,a,d) <- tail g ]

-- Arguments
data MyArgs = N | O | F | P | B | C | H deriving (Eq, Ord, Show)

myArgs :: [Arg MyArgs]
myArgs = [
          Arg { argIndex = N,
                argAbbr = Just 'n',
                argName = Just "number-times",
                argData = argDataDefaulted "int" ArgtypeInt 1,
                argDesc = "Number of times to run the input program"
              },
          Arg { argIndex = O,
                argAbbr = Just 'o',
                argName = Just "output",
                argData = argDataOptional "file" ArgtypeString,
                argDesc = "Output report file"
              },
          Arg { argIndex = F,
                argAbbr = Just 'f',
                argName = Just "uhc-flags",
                argData = argDataDefaulted "uhcflags" ArgtypeString "",
                argDesc = "Flags to pass to the compiler"
              },
          Arg { argIndex = P,
                argAbbr = Just 'p',
                argName = Just "profiling",
                argData = Nothing,
                argDesc = "Profile, do not benchmark"
              },
          Arg { argIndex = B,
                argAbbr = Just 'b',
                argName = Just "binary-size",
                argData = Nothing,
                argDesc = "Compute binary sizes (Win only), do not benchmark"
              },
          Arg { argIndex = C,
                argAbbr = Just 'c',
                argName = Just "path-to-ghc",
                argData = argDataDefaulted "path" ArgtypeString "uhc ",  -- uhc
                argDesc = "Path to GHC (defaults to \"ghc\")"
              },
          Arg { argIndex = H,
                argAbbr = Just 'h',
                argName = Just "help",
                argData = Nothing,
                argDesc = "Display usage instructions"
              }
         ]

sequenceProgress_ :: [IO ExitCode] -> IO ()
sequenceProgress_ [] = return ()
sequenceProgress_ l  = do
  let seq :: [IO ExitCode] -> Int -> IO ()
      seq []    _ = putStrLn "done."
      seq (h:t) n = do
                      putStr ((show n) ++ " ") >> hFlush stdout
                      sequenceError_ [h]
                      seq t (n + 1)
  putStr ("Total number of elements: " ++ show (length l) ++ ". ")
  seq l 1

-- sequence_ accounting for errors
sequenceError_ :: [IO ExitCode] -> IO ()
sequenceError_ []    = return ()
sequenceError_ (h:t) = do
                         e <- h
                         case e of
                           ExitSuccess   -> sequenceError_ t
                           ExitFailure n -> error ("Execution returned exit code "
                                                    ++ show n ++ ", aborted.")

-- Stricter readFile
hGetContents' hdl = do e <- hIsEOF hdl
                       if e then return []
                         else do c <- hGetChar hdl
                                 cs <- hGetContents' hdl
                                 return (c:cs)

readFile' fn = do hdl <- openFile fn ReadMode
                  xs <- hGetContents' hdl
                  hClose hdl
                  return xs


main :: IO ()
main = do
        args <- parseArgsIO ArgsComplete myArgs
        
        -- Some variables
        let profiling = gotArg args P
            binsize   = gotArg args B
            help      = gotArg args H
            n :: Int
            n          = if profiling then 1 else (getRequiredArg args N)
            uhc        = getRequiredArg args C
            flags      = " -fforce-recomp --make " ++ getRequiredArg args F ++ " "
                       ++ (if profiling then " -prof -auto-all " else "")
                       ++ " -outputdir out "
            uhcflags   =  " -v=4 "               -- be verbose
                       ++ "--no-recomp "         -- force recompilation
                       ++ getRequiredArg args F  -- get additonal cmd line flags
                       ++ " "
                          -- odir implies --compile-only which makes it useless for
                          -- our purposes
                          -- ++ "--odir=out "     -- set the output directory to out
            mainis t   = "-main-is " ++ show (lib t) ++ "." 
                       ++ show (testName t) 
                       ++ ".Main.main" ++ show (datatype t)
                       ++ " -o " ++ path t ++ show (lib t) ++ show (testName t) ++ show (datatype t) ++ " "
            path t     = "src" </> show (lib t) </> show (testName t) </> "Main"
            testPath t = "src" </> show (lib t) </> show (testName t) </> show (datatype t) ++ ".hs"
            out t      = "out" </> show (lib t) ++ "." ++ show (testName t) ++ "." 
                       ++ show (datatype t) ++ ".compileout"
            redirect t = " > " ++ out t ++ " 2>&1 "
            -- command-line call to ghc/uhc is placed here
            -- cmd t = ghc ++ flags ++ mainis t ++ path t ++ redirect t

            cmd t =  uhc
                  ++ uhcflags    -- some flags
                  ++ testPath t  -- path to the test file
                  ++ redirect t  -- put compiler output into out/
        
        -- Display usage information
        when help $ usageError args ""
        
        -- Sanity check
        when (profiling && binsize) $ do
          usageError args "Cannot profile and compute binary sizes."
        
        -- Create an output directory
        putStrLn "Create output directory if necessary..." >> hFlush stdout
        createDirectoryIfMissing True "out"
        
        -- Compilation
        putStrLn "Compiling..." >> hFlush stdout
        --sequence_ [ putStrLn (cmd t) | t <- tests ]
        sequenceProgress_ [ system (cmd t) | t <- tests ]
        
        -- Remove old outputs
        putStrLn "Removing old outputs..." >> hFlush stdout
        files <- getDirectoryContents "out"
        let filesToDelete = filter ((==) ".out" . takeExtension) files
        mapM removeFile (map ("out" </>) filesToDelete)
        
        -- Running tests
        let newout t m   = "out" </> show (lib t) ++ "." ++ show (testName t) 
                         ++ "." ++ show (datatype t) ++ "." ++ show m ++ ".out"
            newpath t    = "src" </> show (lib t) </> show (testName t) </> "Main" 
                         ++ show (lib t) ++ show (testName t) ++ show (datatype t)
            uhcnewpath t = "src" </> show (lib t) </> show (testName t) </> show (datatype t)
            run t m = uhcnewpath t 
                --  ++ " +RTS -K32M " ++ if profiling then " -p " else "" ++ " -RTS"
                    ++ " > " 
                    ++ newout t m
        when (not binsize) $ do
          putStrLn "Running tests..." >> hFlush stdout
          --sequence_ [ putStrLn (run t m) | t <- tests, m <- [1..n]]
          sequenceProgress_ [ system (run t m) | t <- tests, m <- [1..n]]
       
        -- Results output
        h <- getArgStdio args O WriteMode
        putStrLn ("-------------------------------------")
        hPutStrLn h "\nResults:"
        hPutStrLn h ("Number of repetitions: " ++ show n)
        hPutStrLn h ("Compiler flags: " ++ (getRequiredArg args F :: String))
        hPutStrLn h ("Environment: " ++ inCommas [os, arch, show compilerVersion])
        hPutStrLn h ("CPU time precision: " ++ show (fromInteger cpuTimePrecision / (1000000000 :: Double)) ++ " (ms)")
        hPutStrLn h ""
        let parse :: Test -> Int -> IO Double
            parse t m = readFile' (newout t m) >>= return . read . tail . dropWhile (/= '\t')
            liftIOList :: [(a, b, IO c)] -> IO [(a, b, c)]
            liftIOList [] = return []
            liftIOList ((a,b,c):t) = do c' <- c
                                        t' <- liftIOList t
                                        return ((a,b,c'):t')
        case (profiling, binsize) of
          (True , False) -> hPutStrLn h ("Profiling run, no benchmarking results.")
          (False, True)  -> printGroupStats h (liftIOList [ (t, m, parse t m) | t <- tests, m <- [1..n]])
          (False, False) -> printGroupStats h (liftIOList [ (t, m, parse t m) | t <- tests, m <- [1..n]])
          (True , True)  -> error "Internal error #1 (can never happen)"
        hPutStrLn h ("-------------------------------------")
        hClose h
