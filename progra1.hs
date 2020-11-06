
import qualified Data.Text as T
import qualified Data.Map
import qualified Data.Tuple
import qualified Data.Char

{- PROGRA 1 -}

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)
data Options = NOSEPARAR | SEPARAR | AJUSTAR | NOAJUSTAR
			 deriving (Eq,Show)


{- Convierte un String en un Line / NO HAY HYPWORDS -}
string2line :: String -> Line
string2line text = let temp = words text in 
			[ Word y | y <- temp]



{- Convierte un Line en un String / HAY UN ESPACIO EN BLANCO LUEGO DE CADA PALABRA MENOS EN LA ULTIMA -}
line2string :: Line -> String
line2string line = let temp = [ case tkn of 
									Word wrd -> wrd 
									Blank -> ""  
									HypWord hwrd -> hwrd ++ "-"
								| tkn <- line] in 
					let palabra = T.strip (T.pack (unwords temp)) in
					T.unpack palabra



{- Largo de un Token -}
tokenLength :: Token -> Int
tokenLength tkn = case tkn of 
					Word wrd -> length wrd 
					Blank -> 1  
					HypWord hwrd -> length hwrd + 1



{- Largo de una Line / HAY UN ESPACIO EN BLANCO ENTRE LAS PALABRAS -}
lineLength :: Line -> Int
lineLength [] = 0
lineLength line = length (line2string line)



{- Parte lineas de manera en que en la primera solo pueden caber un n especificado y 
   en la segunda lo que sobra -}
breakLine :: Int -> Line -> (Line,Line)
breakLine 0 (x:xs) = ([],(x:xs))
breakLine _ [] = ([],[])
breakLine n (x:xs) | n >= lineLength (x:xs) = ((x:xs),[]) 
				   | otherwise = (breakLineLeft n (x:xs) 0, breakLineRight n (x:xs))

{- Hace primer parentesis / LOS QUE CABEN -}
breakLineLeft :: Int -> Line -> Int -> Line
breakLineLeft n [] cont = []
breakLineLeft n line cont = let size = tokenLength (head line) in
						(if cont > 0 then
							(if (size+1) <= n then (head line) : breakLineLeft (n-(size+1)) (tail line) (cont+1)
					 		 else [])
						else 
							(if size <= n then (head line) : breakLineLeft (n-size) (tail line) (cont+1)
					 		 else []))

{- Hace el segundo parentesis / LOS QUE SOBRAN -}
breakLineRight :: Int -> Line -> Line
breakLineRight n line = let elemLeft = length (breakLineLeft n line 0) in
							drop elemLeft line



{- Genera todas las formas de cocatenar los string / DEJA SIEMPRE DOS ESPACIOS ACUMULADOS -}
mergers :: [String] -> [(String, String)]
mergers [] = []
mergers [x] = []
mergers list = mergersAux list ""

{- Concatena todas las posibles combinaciones (String, String) en una lista -}
mergersAux :: [String] -> String -> [(String, String)]
mergersAux [x] prev = []
mergersAux (x:xs) prev = mergersAux2 (x:xs) prev : mergersAux xs (prev ++ x)

{- Me genera una sola posibilidad (String, String) segun el prev y la lista restante -}
mergersAux2 :: [String] -> String -> (String, String)
mergersAux2 list prev = (prev ++ (head list), (mergersAux3 (drop 1 list)))

{- Genera un string de todo el resto que se pone en el segundo espacio de (String, String) -}
mergersAux3 :: [String] -> String
mergersAux3 [] = ""
mergersAux3 (x:xs) = x ++ mergersAux3 xs

{- GUIA DE FUNCIONAMIENTO
"a" + "bcd"
"ab" + "cd"
string ("" + head), tail 
string ("previous head" + head), tail  (drop 1 line)-}


type HypMap = Data.Map.Map String [String]
enHyp :: HypMap
enHyp = Data.Map.fromList [ ("controla",["con","tro","la"]), 
                            ("futuro",["fu","tu","ro"]),
                            ("presente",["pre","sen","te"])]

{- Separa un token Word de todas las formas posibles que se pueden derivar del Data.Map de arriba. -}
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate enHyp wrd = let sinPnt = (deletePunctuation (line2string [wrd])) in 
					 (if Data.Map.member (sinPnt) (enHyp) then hyphenateAux enHyp wrd sinPnt
					  else [])

{- Vengo a buscar la separacion de la palabra en el enHyp y le hago mergers y llamo a Aux2. -}
hyphenateAux :: HypMap -> Token -> String -> [(Token,Token)]
hyphenateAux enHyp wrd sinPnt = let tom = Data.Map.findWithDefault ["no","hay"] (sinPnt) (enHyp) in
						hyphenateAux2 (mergers tom) (givePunctuation (line2string [wrd]))

{- Tomo cada una de las merge posibles y le coloco Hypword a la primera parte y Word a la segunda. -}
hyphenateAux2 :: [(String, String)] -> String -> [(Token,Token)]
hyphenateAux2 [] pnt = []
hyphenateAux2 (x:xs) pnt = 
		((HypWord (Data.Tuple.fst x)), (Word ((Data.Tuple.snd x) ++ pnt))) : hyphenateAux2 xs pnt

{- Elimina los signos de puntuacion de un string. -}
deletePunctuation :: String -> String
deletePunctuation str = [l | l <- str, not(Data.Char.isPunctuation l)]

{- Da los signos de puntuacion de un string. -}
givePunctuation :: String -> String
givePunctuation str = [l | l <- str, (Data.Char.isPunctuation l)]



{- Toma las entradas y se las pasa a una funcion auxiliar que ademas le pasa la ultima palabra con sus 
posibles separaciones -}
lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks enHyp n line = lineBreaksAux n line (hyphenate enHyp (last line)) 

{- Toma la linea original y le realiza su debido breakline, añadiendole tambien los breaklines de las palabras
   que la funcion Aux2 forman -}
lineBreaksAux :: Int -> Line -> [(Token, Token)] -> [(Line,Line)]
lineBreaksAux n line lws = (breakLine n line) : [breakLine n l | l <- (lineBreaksAux2 line lws)]

{- Toma las posibles separaciones de la ultima palabra y genera nuevas lineas que contengan cada una de esas
   separaciones como palabra final -}
lineBreaksAux2 :: Line -> [(Token, Token)] -> [Line]
lineBreaksAux2 line [] = []
lineBreaksAux2 line (x:xs) = (reverse(tail(reverse line)) ++ [Data.Tuple.fst x] ++ [Data.Tuple.snd x]) : lineBreaksAux2 line xs



{- Toma una linea y averigua la cantidad de posibles lineBreak utilizando la herramienta para separar palabras,
   esta función en particular llama a la Aux pero calcula antes el breakLine original sin separaciones de la linea -}
lineBreaksEach :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaksEach enHyp n line = let sep = breakLine n line in 
								(lineBreaksEachAux enHyp n line sep)

{- Toma los datos de entrada y se los pasa a Aux2, pero antes realiza el cálculo sobre la cantidad de 
   posibles divisiones que tiene la palabra siguiente a la última del breakLine original sin separaciones -}
{- NO CONSIDERA UNA LINEA CON PALABRAS QUE NO ESTÁN EN EL DICCIONARIO CUIDADO!!! -}
lineBreaksEachAux :: HypMap -> Int -> Line -> (Line,Line) -> [(Line,Line)]
lineBreaksEachAux enHyp n line sep | Data.Tuple.snd sep == [] = [breakLine n line]
								   | otherwise = lineBreaksEachAux2 n line (hyphenate enHyp (head (Data.Tuple.snd sep)))

{- lineBreaksEachAux :: HypMap -> Int -> Line -> (Line,Line) -> [(Line,Line)]
lineBreaksEachAux enHyp n line sep = let hyp = hyphenate enHyp (head (Data.Tuple.snd sep)) in
										(if (hyp/=[]) then lineBreaksEachAux2 n line (hyp)
										 else [breakLine n line]) -}

{- Toma la linea original y le realiza su debido breakline, añadiendole tambien los breaklines de las lineas
   que la funcion Aux3 forma -}
lineBreaksEachAux2 :: Int -> Line -> [(Token, Token)] -> [(Line,Line)]
lineBreaksEachAux2 n line lws = let org = (breakLine n line) in org : [breakLine n l | l <- (lineBreaksEachAux3 n lws (splitAt (length (Data.Tuple.fst org)) line))]

{- Toma las posibles separaciones de la palabra que se puede separar y genera nuevas lineas que contengan cada 
   una de esas separaciones -}
lineBreaksEachAux3 :: Int -> [(Token, Token)] -> (Line, Line) -> [Line]
lineBreaksEachAux3 n [] index = []
lineBreaksEachAux3 n (x:xs) split | (lineLength (Data.Tuple.fst split) == n) = []
lineBreaksEachAux3 n (x:xs) split | (n - lineLength (Data.Tuple.fst split)) < (tokenLength (Data.Tuple.fst x)+1) = lineBreaksEachAux3 n xs split
									   | otherwise = ((Data.Tuple.fst split) ++ [Data.Tuple.fst x] ++ [Data.Tuple.snd x] ++ (tail (Data.Tuple.snd split))) : lineBreaksEachAux3 n xs split




{- Tomo la linea y descarto los 2 casos de descarte, si no son estos entonces llamo una función aux -}
insertBlanks :: Int -> Line -> Line
insertBlanks _ [] = []
insertBlanks n line | (length line) < 2 = line
					| otherwise = insertBlanksAux n line (length line)

{-  Tomo la linea y el número de espacios que se requieren, si ya no quedan espacios que colocar entonces
	devuelvo la linea resultante. Para colocar espacios voy a colocarlo por rondas, se colocan todos los 
	que se puedan 1 por 1 y luego se repiten esas rondas, esto hasta que se hayan colocado todos -}
insertBlanksAux :: Int -> Line -> Int -> Line
insertBlanksAux n line size = if n <= 0 
							then line
						 else let ltemp = insertBlanksAux2 n line in insertBlanksAux (n-(size-1)) ltemp size

{-  Tomo la linea y luego de cada token que no es un blank coloco un blank, de esta forma se colocan todos
	los posibles, pero solo se colocan los necesarios así que antes se pregunta si aún hay que colocar más -}
insertBlanksAux2 :: Int -> Line -> Line
insertBlanksAux2 n [x] = [x]
insertBlanksAux2 n (x:xs) = if n /= 0 
								then 
									if (x /= Blank) 
										then x : Blank : insertBlanksAux2 (n-1) xs 
									else x : insertBlanksAux2 n xs
							else x:xs



{- Reciba un string y un tamaño de línea, y devuelve una lista de strings que no sean más largos que el tamaño 
   especificado -}
separarYalinear :: Int -> Options -> Options -> String -> [String]
separarYalinear n cond1 cond2 txt = case cond1 of
										SEPARAR -> separarYalinearAuxS n cond2 (string2line txt)
										NOSEPARAR -> separarYalinearAuxNS n cond2 (string2line txt)

{- Para una Line se va a acomodar en una linea de cierto tamaño las palabras que entren, en este caso las
   palabras NO deben de tener separaciones -}
separarYalinearAuxNS :: Int -> Options -> Line -> [String]
separarYalinearAuxNS n cond [] = []
separarYalinearAuxNS n cond line = case cond of
									AJUSTAR -> let tmp1 = line2string (separarYalinearAuxNSA n line) in 
												   (if (deleteNFirst (length (words tmp1)) line) /= [] then (tmp1 : separarYalinearAuxNS n cond (deleteNFirst (length (words tmp1)) line))
												    else (unwords(words tmp1)) : separarYalinearAuxNS n cond (deleteNFirst (length (words tmp1)) line))
									
									NOAJUSTAR -> let tmp2 = line2string (separarYalinearAuxNSNA n line) in 
												   tmp2 : separarYalinearAuxNS n cond (deleteNFirst (length (words tmp2)) line)

{- Para una Line se va a acomodar en una linea de cierto tamaño las palabras que entren, en este caso las
   palabras deben de tener separaciones de ser posible -}
separarYalinearAuxS :: Int -> Options -> Line -> [String]
separarYalinearAuxS n cond [] = []
separarYalinearAuxS n cond line = case cond of 
									AJUSTAR -> let tmp = getMaxLine n line in 
											 	(let tmp1 = line2string (insertBlanks (n-(lineLength (Data.Tuple.fst tmp))) (Data.Tuple.fst tmp)) in
											  	(if (Data.Tuple.snd tmp) /= [] then tmp1 : separarYalinearAuxS n cond (Data.Tuple.snd tmp)
												  else (unwords(words tmp1)) : separarYalinearAuxS n cond (Data.Tuple.snd tmp)))
									NOAJUSTAR -> let tmp2 = getMaxLine n line in 
												(line2string(Data.Tuple.fst tmp2)) : separarYalinearAuxS n cond (Data.Tuple.snd tmp2)


{- Nos brinda una sola linea con las palabras que cabían -}
separarYalinearAuxNSNA :: Int -> Line -> Line
separarYalinearAuxNSNA n line =  Data.Tuple.fst (breakLine n line)

{- Nos brinda una sola linea con las palabras que cabían CON AJUSTE (espacios) -}
separarYalinearAuxNSA :: Int -> Line -> Line
separarYalinearAuxNSA n line =  let tmp = Data.Tuple.fst (breakLine n line) in
											insertBlanks (n-(lineLength tmp)) tmp

{- Elimina los primeros n de una lista -}
deleteNFirst :: Int -> Line -> Line
deleteNFirst n txtl = drop n txtl

{- Brinda el elemento que llena más espacios de todos los posibles -}
getMaxLine :: Int -> Line -> (Line, Line)
getMaxLine n line = last (lineBreaksEach enHyp n line)


{- Al [x] digale que quite todos los espacios y luego se los volvemos a poner xd 
	*Main> words "Hola          que          tal"
	["Hola","que","tal"]
	*Main> unwords ["Hola","que","tal"]          
	"Hola que tal"
-}
