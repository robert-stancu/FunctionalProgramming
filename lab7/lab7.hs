import Data.List

type Image = [String]

img1 = [l1, l2, l3]
    where l1 = "  *"
          l2 = "  *"
          l3 = "  *"

img2 = [l1, l2, l3]
    where l1 = "* *"
          l2 = "*  "
          l3 = "* *"

mask = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="                       *****************"
          l2 ="                       *****************"
          l3 ="                       *****************"
          l4 ="                       *****************"
          l5 ="                       *****************"
          l6 ="                       *****************"
          l7 ="                       *****************"
          l8 ="                       *****************"
          l9 ="                       *****************"
          l10="                       *****************"
          l11="                       *****************"
          l12="                       *****************"
          l13="                       *****************"
          l14="                       *****************"
          l15="                       *****************"
          l16="                       *****************"
          l17="                       *****************"
          l18="                       *****************"
          l19="                       *****************"

logo = [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19]
    where l1 ="        ***** **            ***** **    "
          l2 ="     ******  ****        ******  ****   "
          l3 ="    **   *  *  ***      **   *  *  ***  "
          l4 ="   *    *  *    ***    *    *  *    *** "
          l5 ="       *  *      **        *  *      ** "
          l6 ="      ** **      **       ** **      ** "
          l7 ="      ** **      **       ** **      ** "
          l8 ="    **** **      *      **** **      *  "
          l9 ="   * *** **     *      * *** **     *   "
          l10="      ** *******          ** *******    "
          l11="      ** ******           ** ******     "
          l12="      ** **               ** **         "
          l13="      ** **               ** **         "
          l14="      ** **               ** **         "
          l15=" **   ** **          **   ** **         "
          l16="***   *  *          ***   *  *          "
          l17=" ***    *            ***    *           "
          l18="  ******              ******            "
          l19="    ***                 ***             "

toStringImg :: Image -> String 
toStringImg = foldr (\l acc -> l ++ "\n" ++ acc) []

displayImg = putStrLn . toStringImg

flipH :: Image -> Image
flipH = map reverse

flipV :: Image -> Image
flipV = reverse

rotate90r :: Image -> Image
rotate90r = map reverse . transpose

rotate90l :: Image -> Image
rotate90l = reverse . transpose

invert :: Image -> Image
invert = map (map (\x -> if x == '*' then ' ' else '*'))

maskKeep :: Image -> Image -> Image
maskKeep = zipWith (zipWith (\a b -> if a == '*' && b == '*' then '*' else ' ')) 

maskDiscard :: Image -> Image -> Image
maskDiscard = zipWith (zipWith (\a b -> if a == '*' && b == '*' then ' ' else a)) 

unionp :: Image -> Image -> Image
unionp = zipWith (zipWith (\a b -> if a == '*' || b == '*' then '*' else ' '))

transformationSequence :: [Image -> Image] -> Image -> Image
transformationSequence trans image = foldr (\x acc -> x acc) image trans

type Matrix = [[Integer]]

vprod :: Integer -> Matrix -> Matrix
vprod v = map (map (*v))

hjoin :: Matrix -> Matrix -> Matrix
hjoin = zipWith (++)

vjoin :: Matrix -> Matrix -> Matrix
vjoin = (++)

msum :: Matrix -> Matrix -> Matrix
msum = zipWith (zipWith (+))

mainDiagonal [] = []
mainDiagonal (x:xs) = head x : mainDiagonal (map tail xs)