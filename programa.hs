--kintamųjų ir simbolių reikšmės:
--'matrica' -tai įvesties matrica, kuri sudaryta iš sąrašų sąrašo bool tipo
--'!!'indeksavimo operatorius, leidžia pasiekti elementą pagal konkretų indeksą sąraše
--'i','j','k' tai kintamieji, einantys per intervalą [0..n], įgyja kiekvieną intervalo reikšmę iš eilės ir leidžia pasiekti atatinkamų indeksų matricos elementus
--'[0..n]' intervalo sąrašas nuo 0 iki n
--'length matrica-1' tai išraiška, kuri apskaičiuoja matricos 'matrica' eilučių skaičių ir atima 1(indeksavimas nuo nulio). 
--'implies p q = not p||q' implikacijos išraiška

-- Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra refleksyvi  
refleksyvus :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
refleksyvus matrica = and [matrica !! i !! i | i <- [0..n]]
  where n = length matrica - 1
--'refleksyvus' funkcijos veikimas :
--1.nustatomas matricos dydis 'n'(prieinamų indeksų nuo 0 iki 'n')
--2.Naudojama sąlyginė sąrašo išraiška [matrica !! i !! i | i <- [0..n]],
--kuri grąžina naują sąrašą, sudarytą iš įstrižainės elementų reikšmių.
--Tai padaryta naudojant sąrašo generatorių(list comrehension), kuris eina per indeksus nuo 0 iki n ir gauna įstrižainės elementų reikšmes (matrica !! i !! i).
--3.naudojama 'and' funkcija, kuri patikrina, ar visi sąrašo elementai yra True. 
--Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.

-- Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra antirefleksyvi
antirefleksyvus :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
antirefleksyvus matrica = and [not (matrica !! i !! i) | i <- [0..n]]
  where n = length matrica - 1
--'antirefleksyvus' funckijos veikimas:
--1.Nustatomas matricos dydis n (prieinamų indeksų nuo 0 iki n).
--2.Naudojama sąlyginė sąrašo išraiška [not (matrica !! i !! i) | i <- [0..n]], kuri grąžina naują sąrašą, sudarytą iš priešingų reikšmių įstrižainės elementams.
--Tai padaryta naudojant sąrašo generatorių, kuris eina per indeksus nuo 0 iki n ir gauna įstrižainės elementų reikšmes (matrica !! i !! i), 
--pakeičia jas naudojant 'not' funkciją, kuri grąžina priešingą reikšmę (False tampa True, o True tampa False).
--3.Naudojama 'and' funkcija, kuri patikrina, ar visi sąrašo elementai yra True. Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.

-- Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra simetrinė.
simetrinis :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
simetrinis matrica = and [matrica !! i !! j == matrica !! j !! i | i <- [0..n], j <- [0..n]]
  where  n = length matrica - 1
--'simetrinis' funkcijos veikimas:
--1.Nustatomas matricos dydis n (prieinamų indeksų nuo 0 iki n).
--2.Naudojama sąlyginė sąrašo išraiška [matrica !! i !! j == matrica !! j !! i | i <- [0..n], j <- [0..n]],
--kuri grąžina naują sąrašą, sudarytą iš sąlygos matrica !! i !! j == matrica !! j !! i rezultatų.
--Tai padaryta su dviem sąrašų generatoriais - i nuo 0 iki n ir j nuo 0 iki n.
--Sąlyga tikrina, ar matricos elemento reikšmė stulpelyje j ir eilutėje i yra lygi elemento reikšmei stulpelyje i ir eilutėje j.
--3.Naudojama 'and' funkcija, kuri patikrina, ar visi sąrašo elementai yra True. Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.

-- Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra antisimetrinė.
antisimetrinis :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
antisimetrinis matrica = and [not (matrica !! i !! j) || not (matrica !! j !! i) | i <- [0..n], j <- [0..n], i /= j]
  where n = length matrica - 1
--'antisimetrinis' funkcijos veikimas:
--1.Nustatomas matricos dydis n (prieinamų indeksų nuo 0 iki n).
--2.Naudojama sąlyginė sąrašo išraiška [not (matrica !! i !! j) || not (matrica !! j !! i) | i <- [0..n], j <- [0..n], i /= j],
--kuri grąžina naują sąrašą, sudarytą iš sąlygos not (matrica !! i !! j) || not (matrica !! j !! i) rezultatų.
--Tai padaryta su dviem sąrašų generatoriais - i nuo 0 iki n ir j nuo 0 iki n, bet tik tais atvejais, kai i nelygu j.
--Sąlyga tikrina, ar matricos elemento reikšmė stulpelyje j ir eilutėje i yra False arba ar elemento reikšmė stulpelyje i ir eilutėje j yra False.
--3.Naudojama 'and' funkcija, kuri patikrina, ar visi sąrašo elementai yra True. Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.

-- Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra pilnoji.
pilnasis :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
pilnasis matrica = and [matrica !! i !! j || matrica !! j !! i | i <- [0..n], j <- [0..n], i /= j]
  where n = length matrica - 1
--'pilnasis' funkcijos veikimas:
--1.Nustatomas matricos dydis n (prieinamų indeksų nuo 0 iki n).
--2.Naudojama sąlyginė sąrašo išraiška [matrica !! i !! j || matrica !! j !! i | i <- [0..n], j <- [0..n], i /= j],
--kuri grąžina naują sąrašą, sudarytą iš sąlygos matrica !! i !! j || matrica !! j !! i rezultatų.
--Tai padaryta su dviem sąrašų generatoriais - i nuo 0 iki n ir j nuo 0 iki n, bet tik tais atvejais, kai i nelygu j. 
--Sąlyga tikrina, ar matricos elemento reikšmė stulpelyje j ir eilutėje i yra True arba ar elemento reikšmė stulpelyje i ir eilutėje j yra True.
--3.Naudojama 'and' funkcija, kuri patikrina, ar visi sąrašo elementai yra True. Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.

--Funkcija, kuri tikrina, ar matrica(sąrašas sąrašų) yra pilnoji.
tranzityvus :: [[Bool]] -> Bool--Ši funkcija priima sąrašą sąrašų, kurio elementai yra 'Bool' tipo 
tranzityvus matrica = and [implies (matrica !! i !! j && matrica !! j !! k) (matrica !! i !! k) | i <- [0..n], j <- [0..n], k <- [0..n]]
  where
    n = length matrica - 1
    implies p q = not p || q
--'tranzityvus' funckijos veikimas:
--1. Nustatomas matricos dydis n (prieinamų indeksų nuo 0 iki n).
--2.Naudojama sąlyginė sąrašo išraiška [implies (matrica !! i !! j && matrica !! j !! k) (matrica !! i !! k) | i <- [0..n], j <- [0..n], k <- [0..n]],
--kuri grąžina naują sąrašą, sudarytą iš sąlygos implies (matrica !! i !! j && matrica !! j !! k) (matrica !! i !! k) rezultatų.
--Tai padaryta naudojant tris sąrašų generatorius - i nuo 0 iki n, j nuo 0 iki n ir k nuo 0 iki n.
--Sąlyga tikrina, ar matricos elemento reikšmė stulpelyje j ir eilutėje i yra True, arba matricos elemento reikšmė stulpelyje k ir eilutėje j yra True,
--ir jei taip, arba matricos elemento reikšmė stulpelyje k ir eilutėje i yra True.
--Funkcija implies atlieka implikacijos operaciją: jei p yra False, arba q yra True, tai grąžina True, kitu atveju grąžina False.
--Naudojama and funkcija, kuri patikrina, ar visi sąrašo elementai yra True. Jei visi elementai yra True, funkcija grąžina rezultatą True, kitaip - False.
main :: IO ()--pagrindinė funckija, kuri bus iškviečiama iš terminalo
main = do
  putStrLn "Įveskite charakteristinės matricos eilučių skaičių: "--ši eilutė išveda pranešimą vartotojui, kad jis turėtų įvesti charakteristinės matricos eilučių skaičių.
  numRowsStr <- getLine--Ši eilutė nuskaito vartotojo įvestą tekstą iš konsolės ir priskiria jį kintamajam numRowsStr (kaip simbolių eilutę).
  let numRows = read numRowsStr :: Int --Ši eilutė konvertuoja simbolių eilutę numRowsStr į sveikąjį skaičių ir priskiria jį kintamajam numRows. read 
  --funkcija naudojama konvertuoti simbolių eilutę į norimą tipą. :: Int nurodo, kad norime, jog rezultatas būtų sveikasis skaičius.

  putStrLn "Įveskite charakteristinės matricos stulpelių skaičių: "--- Ši eilutė išveda pranešimą vartotojui, kad jis turėtų įvesti charakteristinės matricos stulpelių skaičių.
  numColsStr <- getLine--- Ši eilutė nuskaito vartotojo įvestą tekstą iš konsolės ir priskiria jį kintamajam numColsStr (kaip simbolių eilutę).
  let numCols = read numColsStr :: Int-- Ši eilutė konvertuoja simbolių eilutę numColsStr į sveikąjį skaičių ir priskiria jį kintamajam numCols.
  --read funkcija naudojama konvertuoti simbolių eilutę į norimą tipą, o :: Int nurodo, kad norime, jog rezultatas būtų sveikasis skaičius.

  putStrLn "Įveskite charakteristinės matricos elementus (kiekvienas elementas yra True arba False):" --Ši eilutė išveda pranešimą vartotojui, kad jis turėtų įvesti kiekvieno charakteristinės matricos elemento reikšmę (True arba False).
  matrica <- getMatrix numRows numCols-- Ši eilutė kviečia funkciją getMatrix, kuri nuskaito vartotojo įvestus charakteristinės matricos elementus ir
  -- sukuria matricą iš True arba False reikšmių. 
  --numRows ir numCols yra jau anksčiau įvesti ir nusako, kiek eilučių ir stulpelių turės matrica. 
  -- <- operatorius naudojamas priskirti matrix rezultatą (matricą) kintamajam.

  putStrLn "Characteristinė matrica:"-- - Ši eilutė išveda pranešimą, kad bus rodoma charakteristinė matrica.
  printMatrix matrica--- Ši eilutė kviečia funkciją printMatrix, kuri atspausdina matricą matrica. 
  --Matrica yra pateikta kaip argumentas, ir funkcija atspausdins ją ekrane.

  putStrLn $ "Refleksyvus: " ++ show (refleksyvus matrica)
  putStrLn $ "Antirefleksyvus: " ++ show (antirefleksyvus matrica)
  putStrLn $ "Simetrinis: " ++ show (simetrinis matrica)
  putStrLn $ "Antisimetrinis: " ++ show (antisimetrinis matrica)
  putStrLn $ "Pilnasis: " ++ show (pilnasis matrica)
  putStrLn $ "Tranzityvus: " ++ show (tranzityvus matrica)
--Kiekviena iš šių eilučių išveda pranešimą apie sąryšio charakteristiką ir prie jo prideda rezultatą iš atitinkamos funkcijos.
-- show funkcija konvertuoja rezultatą į simbolių eilutę, kad jis būtų rodomas ekrane.

getMatrix :: Int -> Int -> IO [[Bool]]
getMatrix numRows numCols = sequence [getRow numCols rowNum | rowNum <- [0..numRows-1]]
--'getMatrix' funkcijos veikimas:
--1. getMatrix :: Int -> Int -> IO [[Bool]] - Tai funkcijos tipo deklaracija, kuri nurodo, kad getMatrix funkcija priima du sveikuosius skaičius 
--(numRows ir numCols) kaip įvesties parametrus ir grąžina IO veiksmą, kuris rezultate duoda dvimatį Bool tipo sąrašą ([[Bool]]).
--2. sequence [getRow numCols rowNum | rowNum <- [0..numRows-1]] -  vykdo sekančius veiksmus:
--[0..numRows-1] yra sąrašas, kuris sudarytas iš skaičių nuo 0 iki numRows-1. Tai nustato, kiek eilučių turėsime charakteristinėje matricoje.
--getRow numCols rowNum yra funkcija, kuri grąžina IO veiksmą, kuris rezultate duoda eilutę su numCols ilgiu ([Bool] tipo sąrašą).
-- Ši funkcija kviečiama su kiekviena rowNum reikšme iš sąrašo [0..numRows-1].
--[getRow numCols rowNum | rowNum <- [0..numRows-1]] yra sąrašas, sudarytas iš rezultatų, gautų kviečiant getRow funkciją su skirtingomis rowNum reikšmėmis.
--sequence funkcija sujungia sąrašą IO veiksmų į vieną IO veiksmą. Tai leidžia vykdyti kiekvieną veiksmą ir surinkti visų jų rezultatus į vieną sąrašą
getRow :: Int -> Int -> IO [Bool]
getRow numCols rowNum = do
  putStrLn $ "Įveskite elementus " ++ show (rowNum + 1) ++ " eilutei: "
  sequence [getCell rowNum colNum | colNum <- [0..numCols-1]]
--'getRow' funkcijos veikimas:
--1. getRow :: Int -> Int -> IO [Bool] - Tai funkcijos tipo deklaracija, kuri nurodo, kad getRow funkcija priima du sveikuosius skaičius (numCols ir rowNum) 
--kaip įvesties parametrus ir grąžina IO veiksmą, kuris rezultate duoda Bool tipo sąrašą ([Bool]).
--2. putStrLn $ "Įveskite elementus " ++ show (rowNum + 1) ++ " eilutei: " - Ši eilutė išveda pranešimą, kuriame prašoma vartotojo įvesti elementus tam tikrai eilutei.
-- Pranešimas yra sudarytas iš teksto "Įveskite elementus ", show (rowNum + 1) (eilutės numeris padidintas vienetu) ir teksto " eilutei: ".
--3. sequence [getCell rowNum colNum | colNum <- [0..numCols-1]] - vykdo sekančius veiksmus:
--[0..numCols-1] yra sąrašas, kuris sudarytas iš skaičių nuo 0 iki numCols-1. Tai nustato, kiek stulpelių turėsime charakteristinėje matricoje.
--getCell rowNum colNum yra funkcija, kuri grąžina IO veiksmą, kuris rezultate duoda bool tipo reikšmę (Bool).
-- Ši funkcija kviečiama su kiekviena colNum reikšme iš sąrašo [0..numCols-1].
--[getCell rowNum colNum | colNum <- [0..numCols-1]] yra sąrašas, sudarytas iš rezultatų, gautų kviečiant getCell funkciją su skirtingomis colNum reikšmėmis.
--sequence funkcija sujungia sąrašą IO veiksmų į vieną IO veiksmą. Tai leidžia vykdyti kiekvieną veiksmą ir surinkti visų jų rezultatus į vieną sąrašą

getCell :: Int -> Int -> IO Bool
getCell rowNum colNum = do
  putStrLn $ "Įveskite stulpelio " ++ show (colNum + 1) ++ " elementą iš eilės" ++ show (rowNum + 1) ++ " :"
  valueStr <- getLine
  let value = read valueStr :: Bool
  return value
--'getCell' funkcijos veikimas:
--1. getCell :: Int -> Int -> IO Bool - Tai funkcijos tipo deklaracija, kuri nurodo, kad getCell funkcija priima du sveikuosius skaičius (rowNum ir colNum)
--kaip įvesties parametrus ir grąžina IO veiksmą, kuris rezultate duoda Bool tipo reikšmę (Bool).
--2. putStrLn $ "Įveskite stulpelio " ++ show (colNum + 1) ++ " elementą iš eilės" ++ show (rowNum + 1) ++ " :" - Ši eilutė išveda pranešimą, kuriame prašoma vartotojo įvesti tam tikro stulpelio elemento reikšmę.
--Pranešimas yra sudarytas iš teksto "Įveskite stulpelio ", show (colNum + 1) (stulpelio numeris padidintas vienetu), teksto " elementą iš eilės" ir show (rowNum + 1) (eilutės numeris padidintas vienetu).
--3.valueStr <- getLine - Ši eilutė nuskaito vartotojo įvestą reikšmę iš įvesties srauto (getLine) ir priskiria ją valueStr kintamajam.
--4.let value = read valueStr :: Bool - Ši eilutė konvertuoja valueStr į Bool tipo reikšmę ir priskiria ją value kintamajam. 
--read funkcija naudojama konvertuoti tekstą į reikšmę pagal nurodytą tipą (Bool).
--5.return value - Ši eilutė grąžina value reikšmę IO veiksme. Tai leidžia perduoti gautą reikšmę kaip rezultatą iš funkcijos.
printMatrix :: [[Bool]] -> IO ()
printMatrix = mapM_ (putStrLn . concatMap showBool)
  where
      showBool True  = "1 "
      showBool False = "0 "
--'printMatrix' funkcijos veikimas:
--1.printMatrix :: [[Bool]] -> IO () - Tai funkcijos tipo deklaracija, kuri nurodo, kad printMatrix funkcija priima Bool tipo sąrašų sąrašą ([[Bool]]) kaip įvesties parametrą 
--ir grąžina IO veiksmą be rezultato (IO ()).
--2. mapM_ (putStrLn . concatMap showBool) - Ši eilutė vykdo veiksmus kiekvienai charakteristinės matricos eilutei:
--mapM_ funkcija taiko pateiktą veiksmą kiekvienai sąrašo elementų eilutei ir ignoruoja rezultatus. Tai leidžia vykdyti veiksmą su kiekviena eilute.
--(putStrLn . concatMap showBool) yra veiksmas, kuris atlieka sekančius veiksmus:
--concatMap showBool yra veiksmas, kuris taiko showBool funkciją kiekvienam sąrašo elementui ir sudeda gautas reikšmes į vieną sąrašą ([String]).
--putStrLn funkcija išveda kiekvieną sąrašo elementą į naują eilutę.
--Taigi, mapM_ (putStrLn . concatMap showBool) vykdo veiksmą kiekvienai charakteristinės matricos eilutei ir išveda ją į ekraną.
--3. showBool True = "1 " ir showBool False = "0 " - Tai yra funkcijos showBool apibrėžimas, kuris konvertuoja Bool tipo reikšmę į tekstą. 
--Jei reikšmė yra True, ji konvertuojama į "1 ", o jei reikšmė yra False, ji konvertuojama į "0 ".
  