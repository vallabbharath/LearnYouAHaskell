-- Part of 09_input_and_output.hs (but independent on its own)
-- Learn You a Haskell For Great Good
-- Chapter 9: Input and Output
-- http://learnyouahaskell.com/input-and-output

-- create an executable file out of this .hs file
-- Compile into an "exe" and run
    -- > ghc --make byteStringCopy.hs  

-- ./byteStringCopy sourceFile destinationFile    (on linux machines)
-- byteStringCopy.exe sourceFile destinationFile  (on windows machines)

import System.Environment  
import qualified Data.ByteString.Lazy as B  

main = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile fileName1 fileName2  
  
copyFile :: FilePath -> FilePath -> IO ()  
copyFile source dest = do  
    contents <- B.readFile source  
    B.writeFile dest contents  
    