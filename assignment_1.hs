{- This is the first assignment main code file for COM2108, started on October 4th and to be 
    sumbitted on October 28th, written by Maxime Fontana, every function description is mentioned above it,
    everything related to testing for main functions can be found below those-}
module Ciphers where
    import AssignmentHelp
    import Data.List -- needed for use of elemIndex
    import Data.Maybe -- needed for use of "fromJust"
    import Data.Tuple -- needed for use of "swap"
    import Data.Char -- needed for use of toLower/toUpper
    -----------------------------------------------------------------------------------------------
    -- (everything in relation with validateCipher)

    -- Function from the slides that helps, output is a list which represents the number of times a character have been used in it
    isRepeated :: Cipher -> [Int] 
    isRepeated my_cipher = map f my_cipher
                              where f x = length [ y | y <- my_cipher, x == y]  
    -- We use the help function to return a string of characters, '1' if this is valid or '0' if it is not, valid means Upper Case Letters
    isaLetter :: Char -> Char 
    isaLetter c = if alphaPos c >= 0 && alphaPos c <= 26 then '1' else '0'

    -- Applying the function above to every character in a cipher, we check if these are are all real Upper Case letters
    areAllValidLetters :: Cipher -> String 
    areAllValidLetters xs = map isaLetter xs 

    -- Validating a Cipher first making sure there are 26 upper case letters than making sure any is repeated 
    validateCipher :: Cipher -> Bool 
    validateCipher the_cipher = (areAllValidLetters the_cipher == "11111111111111111111111111" &&
         all (==1) (isRepeated the_cipher)) 
     {- I have tested this function by making sure every condition was met, i.e testing without 26 letters,
     with lower case letters and 26 letters and repeated letters -}

    ----------------------------------------------------------------------------------------------- 
    -- (everything in relation with encoding/reverseEncoding

    -- adds the last element to the head of the list
    sliding :: Cipher -> String 
    sliding xs = (last xs) : xs

    -- removes the last element to create this right sliding effect
    adjusting :: Cipher -> String 
    adjusting xs = init xs

     {- encode a character, right sliding occurs on ciper as many times as the offset is repeated by incrementation of 1, 
    at the end returns the character in the cipher that matches the position of the letter -}
    encode :: Cipher -> Int -> Char -> Char  
    encode my_cipher offset my_char = if offset > 0 then encode (adjusting (sliding my_cipher)) (offset-1) (my_char) else my_cipher !! (alphaPos my_char)
    {- I have tested encode with ciphers of my own and ciphers provided in the assignment, primarly, I had errors due to "Out of Bounds" cases in which
    I wanted to encode either A or Z, but it has been now resolved and have not found any problem with it, the offset can take large numbers it will not fail 
    so the wrapping part works fine. I have not included the validateCipher, so in this this case as well as in the other functions below, 
    I consider Ciphers are indeed Ciphers, but these can actually be just a string-}

    -- applies "encode" to each element in the cipher, returning the full encoded string
    encodeMessage :: Cipher -> Int -> String -> String 
    encodeMessage my_cipher offset my_message =  map f my_message
                                                  where f x = encode my_cipher offset x
    {- I have tested this function with different lengths of my_message and it works perfectly for every letter we can have. However, it holds the same
    "validateCipher" limitation.-}

    plain = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 
    {- right sliding occurs on cipher as many times as the offset is repeated by incrementation of 1, 
    at the end returns the character in the plain alphabet that matches the index of the element in the cipher  -}
    reverseEncode :: Cipher -> Int -> Char -> Char 
    reverseEncode my_cipher offset my_char = if offset > 0 then reverseEncode (adjusting (sliding my_cipher)) (offset-1) (my_char) else plain !! (fromJust $ elemIndex my_char my_cipher)
    {- I have tested reverseEncode the same way I tested its twin function "encode" and this end up having the same limitation but no further errors have been noticed,
    the only part changing being the elem index and the created plain alphabet variable, it has not brought any other problem -}

    -- applies "reverseEncode" to each element in the cipher, returning the full string
    reverseEncodeMessage :: Cipher -> Int -> String -> String 
    reverseEncodeMessage my_cipher offset my_message =  map f my_message
                                                         where f x = reverseEncode my_cipher offset x
     {-I have tested this function the same way I tested encodeMessage, no particular noticed errors-}

    ----------------------------------------------------------------------------------------------- 
    -- (everything in relation with letterStats)
    
    {- A function that looks life isRepeated but where a character may be taken into parameters 
    and only returns the number of times this characters appears in the string -}
    count :: Eq a => Integral b => a -> [a] -> b 
    count e [] = 0
    count e (a:xs) = (count e xs +) $ if a == e then 1 else 0 

    -- applies count to every element in the list and compute the percentage for each letter without taking care if an element is repeated
    getFre :: String -> [Int] 
    getFre xs = map f xs
                 where f x = ((count x xs) * 100) `div` length xs

    -- a function that takes off all elements that are repeated therefore, in the output, every element appears once
    uniq :: Eq a => [a] -> [a] 
    uniq [] = []
    uniq (x:xs) = (if x `elem` xs then id else (x:)) $ uniq xs

    -- a function that sorts a list of tuples (by zipping getFre's output and and the input String) + using uniq to make sure we dont have duplicates
    preLetterStats :: String -> [(Int, Char)] 
    preLetterStats xs = mergesort (>) (uniq (zip (getFre xs) xs))

    -- same as preLetterStats but swapping elements within tuples
    letterStats :: String -> [(Char,Int)] 
    letterStats xs = map swap (preLetterStats xs)
    {-I have tested this function with differents length of the message, from made up relatively small length to mystery provided in the help file, it turned out
    the function holds a small limitation which is, the computing part in getFre is not rounding up or down, so sometimes it may be possible to end up having 
    statistiques that don't add up to 100% but like 98%/99% due to this problem. For instance, with the example provided in the assignment handout with "STDDWSD"
    the output add up to 98% due to rounding issue : [('D',42),('S',28),('W',14),('T',14)] -}


    -----------------------------------------------------------------------------------------------     
    -- (last function)
    
    -- Pulls out the first element of the tuples in the input list
    getAllFst :: [(Char,Char)] -> String 
    getAllFst xs = map f xs
                    where f x = snd x

    {- If the character belongs to one of the first element of the tuples, 
     then we return the second element of that index in the reflector as a lower case otherwise we make sure we return the uppercase X -}
    partialDecode :: [(Char,Char)] -> String -> String
    partialDecode my_reflector my_message = map f my_message 
                                             where f x = if x `elem` (getAllFst my_reflector) then toLower(fst (my_reflector !! (fromJust $ elemIndex x (getAllFst my_reflector)))) else toUpper x
     {- I have tested this function using two examples, first the example given in the handout : "DXPWXW" with the reflector [(’E’,’X’),(’S’,’W’)], which means it 
     correctly reads the reflector in the right order "E ciphers to X" etc... It gives the correct output. And now with reflectorB and mystery this gives the following 
     output : "eJAvjAbJFBEvjtjADBAJeJDJeFageVCvEVEFKedvdbFDCjKvjkFaMvaFDMCivAAjMvAJFVkvJAijtvJCeAFavjkeJJkvBeJkFaMvEAJFVFteJACFDkdBvJCvEeMCJAFEJFTAecvaFfAJFVijbBvaFJkv
     JAeagEvjAvJCvivAAjMvkvaMJCjBeJiFEvAJFVtvvVJCeAivAAjMvAvgEvJFEACjEveTbFDfjaJJCvfCFkvgkjAAJFMvJJCvBFaDAijEtAAJFV" which I have briefly reviewed by hand and I have not
     noticed anything goig wrong with the decoding here. -}
                                             
    

     