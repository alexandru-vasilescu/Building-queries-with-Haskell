module Query where

import UserInfo
import Rating
import Movie

type Column = String
type TableSchema = [Column]
type Field = String
type Entry = [Field]

data Table = Table TableSchema [Entry]

type ColSeparator = Char
type LnSeparator = Char

-- TODO 1
--functia primeste 2 separatori si un string si apeleaza 2 functii care parseaza stringul
read_table :: ColSeparator -> LnSeparator -> String -> Table
read_table cs ls l = Table (header cs ls l) (entries cs ls l)

--header separa Stringul dupa separatorul de linii
--ia primul element din lista de stringuri rezultata
--separa din nou strinul dupa separatorul de coloane
--intoarce o lista de stringuri
header :: ColSeparator -> LnSeparator -> String -> [String]
header cs ls l = splitBy cs (head (splitBy ls l))

--entries separa Stringul dupa separatorul de linii
--se prelucreaza toata lista mai putin primul element reprezentat de header
--se aplica functia map cu operatia de separare cu separatorul de coloane
--intoarce o lista cu liste de stringuri
--aplic functia init pentru a scapa de ultimul element care este o lista goala 
entries :: ColSeparator -> LnSeparator -> String -> [Entry]
entries cs ls l = init (map (splitBy cs) (tail (splitBy ls l)))

--functie care separa un String dupa un caracter 
splitBy :: Char -> String ->[String]
splitBy _ [] = [""] 
splitBy c (x:xs)
    | x == c = "":(splitBy c xs)
    | otherwise = (x : (head (splitBy c xs))) : (tail (splitBy c xs))

--tabelele initializate
user_info = read_table '|' '\n' user_info_str
rating = read_table ' ' '\n' rating_str
movie = read_table '|' '\n' movie_str

--functie care primeste un tabel si returneaza headerul acestuia
getHeader :: Table -> [String]
getHeader (Table header entries) = header

--functie care primeste un tabel si returneaza lista de intari a acestuia
getEntries :: Table -> [Entry]
getEntries (Table header entries) = entries

--functie care primeste intrarile o lista de intari
--aplica functia head pentru a ramane doar primul element din fiecare lista
--intoarce o lista care contine lungimea primului string din fiecare intrare
length_line :: [[String]] -> [Int]
length_line l = map length (map head l)

--functie care primeste un tabel si intoarce lungimea maxima din prima coloana acestuia
--in acumulator este pastrata lungimea din header
--se aplica length_line pe lista de intrari pentru a o transforma in lista de inturi
--cu ajutorul unui fold se parcurge toata lista de intrari si aplica functia max
column_length :: Table -> Int
column_length (Table header entries) = foldr max (length (head header)) (length_line entries)

--functia tail_table primeste un tabel si returneaza tabelul mai putin prima coloana
--se aplica tail pe header si folosind map se aplica tail pe fiecare intrare
tail_table :: Table -> Table
tail_table (Table header entries)= Table (tail header) (map tail entries)

--functia care primeste un tabel si returneaza o lista cu lungimea maxima a fiecarei coloane
--calculeaza lungimea maxima a primei coloane folosind column_length  
--se apeleaza recursiv functia folosind tail_table 
lengths :: Table -> [Int]
lengths (Table [] ([]:_)) = []
lengths t= (column_length t):(lengths (tail_table t))

--functia primeste un tabel si returneaza latimea acestuia 
--se aduna numarul de coloane la inceput care reprezinta un delimitator "|" dupa fiecare coloana
--se incepe de la 1 pentru avea un delimitator "|" la inceput
width :: Table -> Int
width t =(length (lengths t)) + (foldr (+) 1 (lengths t))


-- TODO 2
--functie care primeste un numar si afiseaza un numar egal de liniute
print_liniute :: Int -> [Char]
print_liniute 0 = ['\n']
print_liniute x = '-':print_liniute (x-1)

--functie care primeste o linie de tabel si lista cu lungimile fiecarei coloane
--intoarce un string cu linia din tabel cu delimitatori
--la ficare element din lista de stringuri verifica lungimea pentru a adauga spatii
--se adauga spatii pentru a avea coloane egale
--se apeleaza recursiv pana la finalul liniei de tabel
print_line :: [String] -> [Int] -> String
print_line [] [] = "|\n"
print_line (s:ls) (x:xs) = "|"++s++(replicate (x-(length s)) ' ')++(print_line ls xs)

--functie care primeste un tabel si creaza un string cu headerul acestuia
--apeleaza print_line cu lista de Stringuri reprezentativa pentru numele de coloane
--adauga o linie de liniute de latimea tabelului inainte si dupa
print_header :: Table -> String
print_header t = margine ++ (print_line (getHeader t) (lengths t)) ++ margine
    where margine = (print_liniute (width t))

--functia primeste lista de intrari in tabel, tabelul si lista cu lungime maxima a ficarei coloane
--apeleaza print_line pentru a afisa o linie
--se apeleaza recursiv modificandu-se lista de intari
--cand nu mai sunt intari se adauga o linie de liniute ca final de tabel
print_entries :: [Entry] -> Table -> [Int] -> String
print_entries [] t l= (print_liniute (width t))
print_entries entries t l= (print_line (head entries) l)++(print_entries (tail entries) t l)

--adaugarea tabelului in clasa Show
--se apeleaza print_header si print_entries care creaza stringul pentru a fi afisat
instance Show Table where
   show t = (print_header t) ++ (print_entries (getEntries t) t (lengths t))

data FilterCondition = Lt Field Integer 
                        | Eq Field String 
                        | In Field [String] 
                        | Not FilterCondition

--functia multiple_head primeste un numar si o lista si intoarce elementul x din lista
--primul element are indicele 1
multiple_head :: Int -> [a] -> a
multiple_head x l=head (drop (x-1) l)


-- TODO 3
--functia getFilter este implementata pentru fiecare tip din FilterCondition
getFilter :: FilterCondition -> TableSchema -> (Entry -> Bool)
getFilter (Eq column str) header = \y -> (multiple_head (lookfor2 column header) y)==str
getFilter (In column list) header = \y -> (multiple_head (lookfor2 column header) y) `myIncl` list
getFilter (Not fc) header= \y-> not (getFilter fc header y)
getFilter (Lt column int) header = \y -> (toInt (multiple_head (lookfor2 column header) y))<int

--functia myIncl primeste un element si o lista de acelasi tip
--verifica daca se gaseste elementul in lista
--intoarce true daca il gaseste si false daca lista ajunge la final
myIncl :: Eq a => a -> [a] -> Bool
myIncl x []=False
myIncl x list=if (x==(head list)) then True else (myIncl x (tail list))

--primeste un string si intoarce un Int din acel integer
--am gasit functia online nu am facut-o singur
toInt :: String -> Integer
toInt str= read str :: Integer

-- TODO 4
data Query = Filter FilterCondition Query |  
             Select [String] Query |
             SelectLimit [String] Int Query |
             Cosine Query |
             Query :|| Query |
             Atom Table

--functia eval primeste un Query si intoarce un tabel
eval :: Query -> Table
eval (Atom table) = table
eval (Select l query) = createnew_table l (-1) query
eval (SelectLimit l i query) = createnew_table l i query
eval (Filter fc query) = filter_table fc query
eval (q1 :|| q2) = concat_table q1 q2

--functia createnew_table primeste o lista cu Stringuri, un int si un Query
--lista de Stringuri reprezinta coloanele care trebuie pastrate din tabelul din query
--int-ul reprezinta cate valori din tabelul initial din query trebuiesc pastrate
--daca int-ul este -1 este apelata de un query Select si trebuie pastrat tot tabelul
--daca i-ul este mai mare ca 0 este apelat de un SelectLimit si pastreaza doar primele i linii
createnew_table :: [String] -> Int -> Query -> Table
createnew_table l i (Atom table)  
    |i<0 = Table l (createnew_entries l [] table)
    |otherwise = Table l (take i (createnew_entries l [] table))
createnew_table l i (Filter fc query)  
    |i<0 = Table l (createnew_entries l [] (filter_table fc query))
    |otherwise = Table l (take i (createnew_entries l [] (filter_table fc query)))
createnew_table l i (Select l1 query)  
    |i<0 = Table l (createnew_entries l [] (createnew_table l1 (-1) query))
    |otherwise = Table l (take i (createnew_entries l [] (createnew_table l1 (-1) query)))

--primeste o lista cu stringuri, un tabel si un acumulator
--se apeleaza recursiv pana lista de stringuri devine nula
--se cauta indicele primului element din l in headerul tabelului 
--se parcurge lista de intrari folosind newstr si se modifica sa ramana doar elementul de indice
--se adauga in acc folosind cclista intarile necesare pe rand
--se intoarce acumulatorul la final cu o lista noua de intrari
createnew_entries :: [String] -> [Entry] -> Table -> [Entry]
createnew_entries [] acc _ = acc
createnew_entries l acc t = createnew_entries (tail l) (cclist acc (newstr (lookfor (head l) t) t)) t

--primeste un string si un tabel
--verifica daca se gaseste stringul in headerul tabelului
--intoarce indexul stringului din lista daca s-a gasit sau un numar mare daca nu s-a gasit stringul
lookfor :: String -> Table -> Int
lookfor str (Table [] _) = 10000
lookfor str (Table header entries)  
    |(head header)==str = 1 
    |otherwise = (1 + (lookfor str (Table (tail header) entries)))

--primeste un string si o lista de stringuri(un header)
--intoarce indexul stringului din lista daca exista
--Am avut situatii in care am avut nevoie de tot tabelul si situatii in care aveam doar headerul
--Am creat 2 functii care fac aproape acelasi lucru dar primesc altii parametri
lookfor2 :: String -> [String] -> Int
lookfor2 str []=100
lookfor2 str header=if (head header)==str then 1 else (1+ (lookfor2 str (tail header)))

--primeste un numar si un tabel
--daca numarul este mai mare ca numarul de coloane din tabel se intoarce o lista goala
--altfel se aplica functia map pastrand din lista de intrari doar elementul de pe coloana x
--intoarce o lista cu liste continand un singur String 
newstr :: Int -> Table -> [Entry] 
newstr x t  
    |(x <= (length (getHeader t))) = (map (\y -> [multiple_head x y]) (getEntries t))
    |otherwise = []

--primeste o lista de liste de stringuri (o lista de entry)
--concateneaza prima lista din x1 cu prima lista din x2
--se apeleaza recursiv pana cand o lista se termina
cclist :: [Entry] -> [Entry] -> [Entry]
cclist [] x2 = x2
cclist x1 [] = x1
cclist x1 x2 = ((head x1)++(head x2)):(cclist (tail x1) (tail x2))

--primeste un FilterCondition si un query
--creaza un tabel doar cu intarile care respecata conditia din filter condition
filter_table :: FilterCondition -> Query -> Table
filter_table fc (Atom table) = Table header (filter (getFilter fc header) entries)
                                        where
                                            header = getHeader table
                                            entries = getEntries table                                            
filter_table fc (Select l query) = Table header (filter (getFilter fc header) entries)
                                        where
                                            header = getHeader table
                                            entries = getEntries table
                                            table = createnew_table l (-1) query
filter_table fc (Filter fc1 query) = Table header (filter (getFilter fc header) entries)
                                        where
                                            header = getHeader table
                                            entries = getEntries table
                                            table = filter_table fc1 query
--Face reuniunea a doua tabele
--Daca headerele sunt diferite intoarce un tabel cu linii si header gol
--Altfel adauga la intarile primului tabel, intrarile celui de-al doilea tabel 
concat_table :: Query -> Query -> Table                                       
concat_table (Atom t1) (Atom t2) 
    |(getHeader t1)/=(getHeader t2) = Table [] []
    |otherwise = (Table (getHeader t1) (add_entries (getEntries t1) (getEntries t2)))
--Daca primesc un filter creeaza tabelele cu filter_table si le face (Atom table)
concat_table (Filter f q1) q2 = concat_table (Atom table) q2
                                            where
                                                table = filter_table f q1
concat_table q2 (Filter f q1) = concat_table q2 (Atom table)
                                            where
                                                table = filter_table f q1                                                
--Daca primeste un Select creeaza tabele cu createnew_table si le face (Atom table)
concat_table (Select l q1) q2 = concat_table (Atom table) q2
                                            where
                                                table = createnew_table l (-1) q1
concat_table q2 (Select l q1) = concat_table q2 (Atom table)
                                            where
                                                table = createnew_table l (-1) q1                                              
--Primeste un acumulator si un element x
--Daca elementul x se gaseste in lista de acumulatori se intoarce acumulator
--Daca elementul nu se gaseste in acumulator se adauga in acumulator la final
verificare :: Eq a => [a] -> a -> [a]
verificare acc x 
    |((filter (==x) acc)==[]) = (acc++[x]) 
    |otherwise = acc

--Primeste 2 liste de intari
--Intoarce reuniunea functiilor cu fiecare element identic doar o data
add_entries :: [Entry] -> [Entry] -> [Entry]
add_entries entries1 entries2 = foldl verificare entries1 entries2

--Primeste numele unei coloane si o valoare de pe aceasta, alt nume de coloana si un Tabel
--Cauta in lista de intari din tabel valoarea de pe coloana i si intoarce Stringul de pe coloana j
--Am folosit pentru a gasi zona unui user care are un anumit id
getValue:: String -> String -> String -> Table -> String
getValue column1 value column2 t = returnValue (lookfor column1 t) (lookfor column2 t) value (getEntries t)
    where
        returnValue i j value [] = []
        returnValue i j value list
            |((multiple_head i (head list))==value) = (multiple_head j (head list))
            |otherwise = returnValue i j value (tail list)

-- TODO 5
--Filtrez tabelul in functie de zona egala cu utilizatorul cu user_id=str
--Scot din tabel linia care contine str pe user_id
--Selectez doar coloanele "user_id" si "occupation"
same_zone :: String -> Query
same_zone str=Select ["user_id","occupation"] 
                      (Filter (Not (Eq "user_id" str))
                              (Filter (Eq "zone" (getValue "user_id" str "zone" user_info))
                                      (Atom user_info)))

--Filtrez tabelul sa am doar sex-ul mascul
--Filtrez tabelul sa am doar oameni mai tineri ca old
--Filtrez tabelul sa am doar oameni mai batrani sau la fel de batrani cu young+1
--Selectez doar coloanele "occupation" si "zone"
male_within_age :: Integer -> Integer -> Query
male_within_age young old= Select  ["occupation","zone"]
                                    (Filter (Not (Lt "age" (young+1)))
                                            (Filter (Lt "age" old) 
                                                    (Filter (Eq "sex" "M") 
                                                            (Atom user_info))))

--Filtrez tabelul sa fie doar oameni mai tineri ca age
--Filtrez in functie de lista de zone 
--Filtrez in functie de lista de ocupatii
--Selectez doar coloana "user_id"
mixed :: [String] -> [String] -> Int -> Query
mixed zones occupations age = Select ["user_id"]
                                     (Filter (In "occupation" occupations)
                                            (Filter(In "zone" zones) 
                                                    (Filter (Lt "age" (toInteger age))
                                                            (Atom user_info))))