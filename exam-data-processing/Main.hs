{-# LANGUAGE ExistentialQuantification #-}

{-# LANGUAGE FlexibleInstances #-}
module Tasks where

import Dataset ( exam_grades, exam_grades_csv, hw_grades,
        hw_grades_csv, lecture_grades, lecture_grades_csv, email_map_csv)
import Text.Printf
import Data.Array
import Data.List ( sortBy )

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]


{-
    TASK SET 1
-}

-- Task 1
    -- ["Nume","Q1","Q2","Q3","Q4","Q5","Q6","Ex. Scris"],
    -- ["Olivia Noah","0","0","2","2","2","2","0.93"],

olivia_exam :: [String]
olivia_exam = ["Olivia Noah","0","0","2","2","2","2","0.93"]
exam_grades_dataset = Dataset.exam_grades



stringToFloat :: Value -> Float
stringToFloat grade
    | null grade = 0.0
    | otherwise = read grade :: Float

computeExamGrade :: Row -> Float
computeExamGrade student = (  stringToFloat (student!!1)
                            + stringToFloat (student!!2)
                            + stringToFloat (student!!3)
                            + stringToFloat (student!!4)
                            + stringToFloat (student!!5)
                            + stringToFloat (student!!6) )
                            / 4
                            + (read (student!!7) :: Float)

examGradesToRow :: Row -> Row
examGradesToRow student = head student : [printf "%.2f" (computeExamGrade student)]


compute_exam_grades :: Table -> Table
compute_exam_grades table = ["Nume", "Punctaj Exam"] : foldr op [] (tail table)   --eliminam randul cu informatii
                        where
                            op student [] = [examGradesToRow student]
                            op student acc = examGradesToRow student : acc

-- Task 2
-- Number of students who have passed the exam:
passedExam :: Row -> Bool
passedExam student
    | computeExamGrade student >= 2.5 = True
    | otherwise = False

get_passed_students_num table = foldl op 0 (tail table)
                                    where
                                        op acc student
                                            | passedExam student = 1 + acc
                                            | otherwise = acc

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = fromIntegral passed / fromIntegral (length table - 1)
                            where
                                passed = get_passed_students_num table

percentagePassedExamToInteger :: Float -> Integer
percentagePassedExamToInteger percentage = round (percentage * 100)

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg table = foldl op 0 (tail table) / (fromIntegral (length table) - 1)
                            where
                                op acc student = acc + computeExamGrade student

    -- ["Nume","Lab (1p)","T1 (0.5p)","T2 (1p)","T3 (1.5p)","Ex1 (0.25p)","Ex2 (0.25p)","Ex3 (0.25p)","Ex4 (0.25p)"],
    -- ["Olivia Noah","0.42","0.49","1","1.05","","","0",""],
    -- ["Riley Jackson","0.85","0.5","1","1.5","0.25","0.13","0","0"],
homework_grades_dataset = Dataset.hw_grades
olivia_homework = homework_grades_dataset!!1

computeHomeworkGrade :: Row -> Float
computeHomeworkGrade student = stringToFloat (student!!2)
                                +stringToFloat (student!!3)
                                +stringToFloat (student!!4)

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num table = foldl op 0 (tail table)
                            where
                                op acc student
                                    | computeHomeworkGrade student >= 1.5 = acc + 1
                                    | otherwise = acc

-- Task 3
    -- ["Nume","Q1","Q2","Q3","Q4","Q5","Q6","Ex. Scris"],
    -- ["Olivia Noah","0","0","2","2","2","2","0.93"],

exam_questions_string :: [String]
exam_questions_string = ["0.0", "0.0", "0.0", "0.0", "0.0", "0.0"]
exam_questions_float :: [Float]
exam_questions_float = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
-- olivia_exam = ["Olivia Noah","0","0","2","2","2","2","0.93"]

--operator for fold
--adds to Q1-Q6 row (float) a student's exam results
addExamQuestions :: [Float] -> Row -> [Float]
addExamQuestions questions student = ( head questions + stringToFloat (student!!1))
                                    :( questions!!1 + stringToFloat (student!!2))
                                    :( questions!!2 + stringToFloat (student!!3))
                                    :( questions!!3 + stringToFloat (student!!4))
                                    :( questions!!4 + stringToFloat (student!!5))
                                    :[questions!!5 + stringToFloat (student!!6)]

addAllExamQuestions :: Table -> [Float]
addAllExamQuestions table = foldl op acc (tail table)
                                where
                                    acc = exam_questions_float
                                    op acc [] = acc
                                    op acc student = addExamQuestions acc student

sumOfAllQuestions :: [Float]
sumOfAllQuestions = [201.0,178.0,220.0,194.0,203.0,173.0]

averageExamQuestions :: Table -> [Float] -> [Float]
averageExamQuestions table = map (/ numberOfStudents)
                                        where
                                            numberOfStudents = fromIntegral (length table - 1) :: Float

averageExamQuestionsToString :: [Float] -> Row
averageExamQuestionsToString = map (printf "%.2f")



get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"]:
                                    [averageExamQuestionsToString (averageExamQuestions table (addAllExamQuestions table))]

-- Task 4
examSummarySample :: [[Int]]
examSummarySample = [[0,0,0], [0,0,0], [0,0,0], [0,0,0], [0,0,0] , [0,0,0]]
addQuestionN :: Int -> Table -> [Int]
addQuestionN questionNumber table = markedZero : markedOne : [markedTwo]
                                    where
                                        markedZero = foldl opZero 0 table
                                        opZero acc student
                                            | stringToFloat (student!!questionNumber) == 0.0 = acc + 1
                                            | otherwise = acc
                                        markedOne = foldl opOne 0 table
                                        opOne acc student
                                            | stringToFloat (student!!questionNumber) == 1.0 = acc + 1
                                            | otherwise = acc
                                        markedTwo = foldl opTwo 0 table
                                        opTwo acc student
                                            | stringToFloat (student!!questionNumber) == 2.0 = acc + 1
                                            | otherwise = acc


get_exam_summary :: Table -> Table
get_exam_summary table = ["Q", "0", "1", "2"]
                        :("Q1" : map show (addQuestionN 1 (tail table)))
                        :("Q2" : map show (addQuestionN 2 (tail table)))
                        :("Q3" : map show (addQuestionN 3 (tail table)))
                        :("Q4" : map show (addQuestionN 4 (tail table)))
                        :("Q5" : map show (addQuestionN 5 (tail table)))
                        :[("Q6" : map show (addQuestionN 6 (tail table)))]

-- Task 5
--this task was made in the future
get_ranking :: Table -> Table
get_ranking table = tsort "Punctaj Exam" newTable
                    where
                        newTable = compute_exam_grades table

-- Task 6
examGradesToDifference :: Row -> Row
examGradesToDifference student = (head student)
                                :(printf "%.2f" interviewGrade)
                                :(printf "%.2f" writtenGrade)
                                :[(printf "%.2f" difference)]
                                where
                                    interviewGrade = computeExamGrade student - writtenGrade
                                    writtenGrade = (stringToFloat $ student!!7)
                                    difference = abs $ interviewGrade - writtenGrade

computeExamDifferences :: Table -> Table
computeExamDifferences table =  ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]
                                : foldr op [] (tail table)
                        where
                            op student [] = [examGradesToDifference student]
                            op student acc = (examGradesToDifference student):acc

--also using tsort from the future
get_exam_diff_table :: Table -> Table
get_exam_diff_table table = tsort "Diferenta" (computeExamDifferences table)


--Task Set 2
    -- \Nume,Q1,Q2,Q3,Q4,Q5,Q6,Ex. Scris\n\
    -- \Olivia Noah,0,0,2,2,2,2,0.93\n\
exam_grades_dataset_csv = Dataset.exam_grades_csv

--Prerequisite
olivia_homework_csv = ",Olivia Noah,0.42,0.49,1,1.05,,,0,,,,"
splitBy :: Char -> String -> [String]
splitBy del = foldr op [""]
                where
                    op char []
                        | char == del = []
                        | otherwise = [[char]]
                    op char list@(x:xs)
                        | char == del = [] : list
                        | otherwise = (char:x) : xs


read_csv :: CSV -> Table
read_csv csv = map op (splitBy '\n' csv)
                where
                    op row = splitBy ',' row

combineValues :: Char -> [String] -> String
combineValues del row
    | head row == "" = "" ++ function (tail row) True
    | otherwise = function row False
        where
            function table beginsWithNull = foldl op "" table
                        where
                            op [] value
                                | value == "" = [del]
                                | beginsWithNull = "," ++ value
                                | otherwise = value
                            op acc value
                                | value == "" = acc ++ [del]
                                | otherwise = acc ++ [del] ++ value

combineRows :: Char -> [String] -> String
combineRows del = foldr op ""
                        where
                            op string [] = string
                            op string acc =  string ++ [del] ++ acc
lecture_grade :: Table
lecture_grade = [["","","","1","","1","1","1","1","1","0","0","0","1","0","","1","1","1","1","1","1","","1","","1","1","",""]]

write_csv :: Table -> CSV
write_csv table = combineRows '\n' $ map (combineValues ',') table

-- Task 1
columnIndex :: Value -> Row -> Int
columnIndex columnName [] = 0
columnIndex columnName row
    | head row == columnName = 0
    | otherwise = 1 + columnIndex columnName (tail row)

columnIndexTable :: Value -> Table -> Int
columnIndexTable columnName table = columnIndex columnName (head table)

as_list :: String -> Table -> [String]
as_list columnName table = foldr op [] (tail table)
                            where
                                op row [] = [ row!!index ]
                                op row acc = (row!!index):acc
                                index = columnIndexTable columnName table

as_list_full :: String -> Table -> [String]
as_list_full columnName table = foldr op [] table
                            where
                                op row [] = [ row!!index ]
                                op row acc = (row!!index):acc
                                index = columnIndexTable columnName table


as_list_full_to_table :: String -> Table -> Table
as_list_full_to_table columnName table = foldl op [] (as_list_full columnName table)
                                        where
                                            op [] string = [[string]]
                                            op acc string = acc ++ [[string]]

--Task 2
-- insert :: Int -> [Int] -> Int
-- insert x [] = [x]
-- insert x (y:ys)
--     | x < y = (x:y:ys)
--     | otherwise = (y : (insert x ys))
stringOrdering :: String -> String -> Ordering
stringOrdering s1 s2
    | s1 > s2 = GT
    | s1 < s2 = LT
    | otherwise = EQ

-- Olivia_exam = ["Olivia Noah","0","0","2","2","2","2","0.93"]
riley_exam :: Row
riley_exam =   ["Riley Jackson","2","2","2","2","2","1","1.2"]

rowOrdering :: Int -> Row -> Row -> Ordering
rowOrdering index r1 r2
    | v1 > v2  = GT
    | v1 < v2 = LT
    | head r1 > head r2 = GT
    | head r1 < head r2 = LT
    | otherwise = EQ
        where
            v1 = if null (r1!!index) then -1.0 else read (r1!!index) :: Float
            v2 = if null (r2!!index) then -1.0 else read (r2!!index) :: Float

insertRow :: Int -> Row -> Table -> Table
insertRow _ row [] = [row]
insertRow index row (y:ys)
    | rowOrdering index row y == GT = y : insertRow index row ys
    | otherwise = row : y : ys

insertionSortTable :: Int -> Table -> Table
insertionSortTable index = foldr op []
                            where
                                op row acc = insertRow index row acc

tsort :: String -> Table -> Table
tsort columnName table = head table:insertionSortTable index (tail table)
                            where
                                index = columnIndex columnName (head table)

--Task 3
-- vrow ::

vmap :: (Value -> Value) -> Table -> Table
vmap function = map (map function)

correct_exam_table = vmap (\x -> if x == "" then "0" else x) exam_grades

-- Task 4
selectRowsFromTable :: [String] -> Table -> Table
selectRowsFromTable strings table = foldr op [] strings
                                    where
                                        op columnName [] = as_list_full_to_table columnName table
                                        op columnName acc = hunion (as_list_full_to_table columnName table) acc

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f strings table = strings : map f (tail table)

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = head row : [printf "%.2f" total]
                        where
                            total = stringToFloat (row!!2)
                                  +stringToFloat (row!!3)
                                  +stringToFloat (row!!4)
                                  +stringToFloat (row!!5)
                                  +stringToFloat (row!!6)
                                  +stringToFloat (row!!7)
                                  +stringToFloat (row!!8)

--Task 5
columnsCoincide :: Table -> Table -> Bool
columnsCoincide table1 table2 = all (==True) $ zipWith (==) (head table1) (head table2)

vunion :: Table -> Table -> Table
vunion table1 table2
    | columnsCoincide table1 table2 = table1 ++ tail table2
    | otherwise = table1



--Task 6
-- remainingRows :: Table -> Table -> Table
-- remainingRows table1 table2 = reverse . take number . reverse table2
--                                 where
--                                     number = length table1 - length table2

hunion2 :: Table -> Table -> Table
hunion2 table1 table2 = zipWith (++) table1 table2 ++ remainingTable
    where
        remainingTable
            | length table1 == length table2 = []
            | length table1 > length table2 = drop (length table2) table1
            | otherwise = drop (length table1) table2

hunion :: Table -> Table -> Table
hunion table1 table2
    | length table1 > length table2 = zipWith (++) table1 table2 ++ remainingTable1
    | length table1 < length table2 = zipWith (++) table1 table2 ++ remainingTable2
    | otherwise = zipWith (++) table1 table2
        where
            remainingTable1 = map (\row -> row ++ replicate number1 "") table1left
            table1left = drop (length table2) table1
            number1 = length (head table2)

            remainingTable2 = map (\row -> replicate number2 "" ++ row) table2left
            table2left = drop (length table1) table2
            number2 = length (head table1)

--Task 7

--for each column in table 1 search if there are identical columns in table 2 
--if there are, overwrite the value from table 2

--then add the remaining columns of row2
mergeRows :: String -> Row -> Row -> Table -> Table -> Row
mergeRows colname row1 row2 table1 table2 = row1 ++ part2
                                            where
                                              part2 = foldr op [] (head table2)
                                              op col []
                                                | col == colname = []
                                                | otherwise = [row2!!(columnIndexTable col table2)]
                                              op col acc
                                                | col == colname = acc
                                                | otherwise = row2!!(columnIndexTable col table2) : acc


canBeMerged :: String -> Row -> Row -> Table -> Table -> Bool
canBeMerged colname row1 row2 table1 table2 = (row1!!index1) == (row2!!index2)
    where
        index1 = columnIndexTable colname table1
        index2 = columnIndexTable colname table2

correspondentRow :: String -> Row -> Table -> Table -> Row
correspondentRow colname row1 table1 table2 = foldr op [] table2
                                            where
                                              op row2 []
                                                | canBeMerged colname row1 row2 table1 table2 = mergeRows colname row1 row2 table1 table2
                                                | otherwise = row1 ++ (take ((length $ head table2)-1) (repeat ""))
                                              op row2 acc
                                                | canBeMerged colname row1 row2 table1 table2 = mergeRows colname row1 row2 table1 table2
                                                | otherwise = acc



tjoin :: String -> Table -> Table -> Table
tjoin colname table1 table2 = foldr op1 [] table1
                            where
                                op1 row1 [] = [correspondentRow colname row1 table1 table2]
                                op1 row1 acc = (correspondentRow colname row1 table1 table2) : acc



--Task 8
cartesianOneRow :: (Row -> Row -> Row) -> Row -> Table -> Table
cartesianOneRow f row1 table2 = foldr op [] (tail table2)
                              where
                                op row2 [] = [f row1 row2]
                                op row2 acc = (f row1 row2) : acc

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian f strings table1 table2 = strings : (foldr op [] (tail table1))
                                  where
                                    op row1 [] = cartesianOneRow f row1 table2
                                    op row1 acc = (cartesianOneRow f row1 table2) ++ acc

--Task 9

projection :: [String] -> Table -> Table
projection = selectRowsFromTable


--------------Task Set 3---------------------
data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value-> Value) Query
    | RowMap (Row->Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool


--Task 1
qResultCSV = CSV exam_grades_csv
qResultTable = Table exam_grades
qResultString = List ["String1", "String2", "String3"]

instance Show QResult where
    show (CSV csv) = show csv
    show (Table table) = write_csv table
    show (List list) = show list

--Task 2
class Eval a where
    eval :: a -> QResult

query1 = FromCSV lecture_grades_csv
query2 = ToCSV $ FromCSV lecture_grades_csv
query3 = AsList "Nume" query1
query4 = Sort "Q1" query1
query5 = ValueMap (++"!") query1

qResultToTable :: QResult -> Table
qResultToTable (Table table) = table

qResultToCSV :: QResult -> CSV
qResultToCSV (CSV csv) = csv

instance Eval Query where
    eval (FromCSV str) = Table (read_csv str)
    eval (ToCSV query) = CSV $ write_csv $ qResultToTable $ eval query
    eval (AsList colname query) = List (as_list colname table)
                                 where
                                    table = qResultToTable $ eval query
    eval (Sort colname query) = Table (tsort colname table)
                                 where
                                    table = qResultToTable $ eval query
    eval (ValueMap op query) = Table (map (map op) table)
                                where
                                    table = qResultToTable $ eval query
    eval (RowMap op colnames query) = Table (rmap op colnames table)
                                where
                                    table = qResultToTable $ eval query
    eval (VUnion query1 query2) = Table (vunion table1 table2)
                                where
                                    table1 = qResultToTable $ eval query1
                                    table2 = qResultToTable $ eval query2
    eval (HUnion query1 query2) = Table (hunion table1 table2)
                                where
                                    table1 = qResultToTable $ eval query1
                                    table2 = qResultToTable $ eval query2
    eval (TableJoin string query1 query2) = Table (tjoin string table1 table2)
                                where
                                    table1 = qResultToTable $ eval query1
                                    table2 = qResultToTable $ eval query2
    eval (Cartesian f strings query1 query2) = Table (cartesian f strings table1 table2)
                                where
                                    table1 = qResultToTable $ eval query1
                                    table2 = qResultToTable $ eval query2
    eval (Projection strings query) = Table (projection strings table)
                                where
                                    table = qResultToTable $ eval query
    eval (Filter filterCondition query) = Table ((head table):filteredTable)
                                where
                                    filteredTable = (filter (\row -> (feval row filterCondition) row) (tail table))
                                    table = qResultToTable $ eval query
    eval (Graph edgeop query) = Table (evalGraph edgeop table)
                                where
                                  table = qResultToTable $ eval query


--in all feval operations, i cannot pass a table to find a value in a row, based by the colname
--so i'll search the dataset
findValueByColname :: Row -> Value -> Value
findValueByColname row colname
  | searchExamGrades /= [] = searchExamGrades
  | searchHomeworkGrades /= [] = searchHomeworkGrades
  | searchLectureGrades /= [] = searchLectureGrades
  | otherwise = []
    where
      searchExamGrades = foldr op [] exam_grades
                          where
                            op r []
                              | row == r = row !! (columnIndexTable colname exam_grades)
                              | otherwise = []
                            op r acc
                              | row == r = row !! (columnIndexTable colname exam_grades)
                              | otherwise = acc
      searchHomeworkGrades = foldr op [] hw_grades
                          where
                            op r []
                              | row == r = row !! (columnIndexTable colname hw_grades)
                              | otherwise = []
                            op r acc
                              | row == r = row !! (columnIndexTable colname hw_grades)
                              | otherwise = acc
      searchLectureGrades = foldr op [] lecture_grades
                          where
                            op r []
                              | row == r = row !! (columnIndexTable colname lecture_grades)
                              | otherwise = []
                            op r acc
                              | row == r = row !! (columnIndexTable colname lecture_grades)
                              | otherwise = acc





class FEval a where
    feval :: [String] -> FilterCondition a -> FilterOp

raelynn_anthony = ["Raelynn Anthony","2","2","2","2","2","2","2.0"]


instance FEval Float where
    feval row (Eq colname ref) = (\row -> if value == ref then True else False)
                            where
                                value = stringToFloat (findValueByColname row colname)
    feval row (Lt colname ref) = (\row -> if value < ref then True else False)
                            where
                                value = stringToFloat (findValueByColname row colname)
    feval row (Gt colname ref) = (\row ->  ((stringToFloat (findValueByColname row colname) > ref)))
    feval row (In colname list) = (\row -> foldl op False list)
                                    where
                                        op acc elem
                                            | elem == value = True
                                            | otherwise = acc
                                        value = stringToFloat (findValueByColname row colname)
    feval row (FNot filterCondition) = (not.feval row filterCondition)
    feval row (FieldEq colname1 colname2) = (\row -> if value1 == value2 then True else False)
                                            where
                                                value1 = stringToFloat (findValueByColname row colname1)
                                                value2 = stringToFloat (findValueByColname row colname2)

instance FEval String where
    feval row (Eq colname ref) = (\row -> if value == ref then True else False)
                            where
                                value = findValueByColname row colname
    feval row (Lt colname ref) = (\row -> if value < ref then True else False)
                            where
                                value = findValueByColname row colname
    feval row (Gt colname ref) = (\row -> ((stringToFloat $ findValueByColname row colname) > (stringToFloat ref)))

    feval row (In colname list) = (\row -> foldl op False list)
                                    where
                                        op acc elem
                                            | elem == value = True
                                            | otherwise = acc
                                        value = findValueByColname row colname
    feval row (FNot cond) = not.feval row cond
    feval row (FieldEq colname1 colname2) = (\row -> if value1 == value2 then True else False)
                                            where
                                                value1 = findValueByColname row colname1
                                                value2 = findValueByColname row colname2

--Task3

functionIndex2 :: Int -> Table -> [(Int, Int)]
functionIndex2 index1 table = foldr op [] [(index1+1) .. (length table - 1)]
                            where
                                op index2 [] = [(index1, index2)]
                                op index2 acc = (index1, index2) : acc

--if size of graph is 4,
--for (index1 = 1..2)
--  for(index2= index1+1..3)
--      create pair(index1,index2)
functionIndex1 :: Table -> [(Int, Int)]
functionIndex1 graph = foldl op [] [1 .. ((length graph) - 2)]
                            where
                                op [] index1  = functionIndex2 index1 graph
                                op acc index1  = acc ++ (functionIndex2 index1 graph)

maybeValue :: Int -> Int -> Table -> EdgeOp -> Maybe Value
maybeValue i j table edgeop = edgeop (table!!i) (table!!j) 

fromMaybe :: Value -> Maybe Value -> Value
fromMaybe d Nothing = d
fromMaybe d (Just a) = a

graphValue :: Int -> Int -> Table -> EdgeOp -> Value
graphValue i j table edgeop = fromMaybe "" (edgeop (table!!i) (table!!j))

graphRow :: Int -> Int -> Table -> EdgeOp -> Row
graphRow i j table edgeop = (table!!i!!0):(table!!j!!0):[(graphValue i j table edgeop)]

evalGraph :: EdgeOp -> Table -> Table
evalGraph edgeop table = ["From", "To", "Value"] : (foldr op [] indexPairs)
                        where
                          op (i,j) []
                            | (maybeValue i j table edgeop/= Nothing) && (table!!i!!0) <= (table!!j!!0) = [graphRow i j table edgeop]
                            | (maybeValue i j table edgeop/= Nothing) && (table!!i!!0) > (table!!j!!0) = [graphRow j i table edgeop]
                            | otherwise = []
                          op (i,j) acc
                            | (maybeValue i j table edgeop/= Nothing) && (table!!i!!0) <= (table!!j!!0) = (graphRow i j table edgeop):acc
                            | (maybeValue i j table edgeop/= Nothing) && (table!!i!!0) > (table!!j!!0) = (graphRow j i table edgeop):acc
                            | otherwise = acc
                          indexPairs = functionIndex1 table 

--Task4
mason_lectures = ["Mason.Zoe@stud.cs.pub.ro","1","","1","0","1","1","1","1","1","1","1","1","1","1","0","1","1","1","1","1","1","1","1","1","1","1","1","1"]
mason_lectures2 = ["Mason.Zoe@stud.cs.pub.ro","1","","1","0","1","1","1","1","1","1","1","1","1","1","0","1","1","1","1","1","1","1","1","1","1","1","1","1"]
ian_lectures = ["Ian.Brooklyn@stud.cs.pub.ro","0","0","1","0","1","1","1","0","1","","1","1","1","1","","1","1","0","1","1","1","1","1","1","1","1","1","0"]

lectureEdgeOp :: Row -> Row -> Maybe Value
lectureEdgeOp row1 row2
  | head row1 == "" = Nothing
  | head row2 == "" = Nothing
  | distance >= 5 = Just (show distance)
  | otherwise = Nothing
    where
      sameResultList = zipWith op1 (tail row1) (tail row2)
      op1 = (\value1 value2 -> value1 == value2)
      distance = foldl op2 0 sameResultList
      op2 0 isTheSame
        | isTheSame == True = 1
        | otherwise = 0
      op2 acc isTheSame
        | isTheSame == True = 1 + acc
        | otherwise = acc

similarities_query = Sort "Value" $ Graph lectureEdgeOp $ FromCSV lecture_grades_csv


--Etapa 4
-- hw_grades = [
--     ["Nume","Lab (1p)","T1 (0.5p)","T2 (1p)","T3 (1.5p)","Ex1 (0.25p)","Ex2 (0.25p)","Ex3 (0.25p)","Ex4 (0.25p)"],
--     ["Olivia Noah","0.42","0.49","1","1.05","","","0",""],

-- exam_grades = [
--     ["Nume","Q1","Q2","Q3","Q4","Q5","Q6","Ex. Scris"],
--     ["Olivia Noah","0","0","2","2","2","2","0.93"],

-- lecture_grades = [
--     ["Email","12.1","4.2","5.1","12.2","5.2","3","5.3","7.1","7.2","4.1","7.3","8.1","2.2","8.2","16.1","8.3","9.1.","9.2.","10.1","2.1","10.2","10.3","11.1","11.3","11.2","13.1","13.2","13.3"],["Mason.Zoe@stud.cs.pub.ro","1","","1","0","1","1","1","1","1","1","1","1","1","1","0","1","1","1","1","1","1","1","1","1","1","1","1","1"],
--     ["Ian.Brooklyn@stud.cs.pub.ro","0","0","1","0","1","1","1","0","1","","1","1","1","1","","1","1","0","1","1","1","1","1","1","1","1","1","0"],

-- email_map_csv = "\
--     \Nume,Email\n\
--     \Olivia Noah,Olivia.Noah@stud.cs.pub.ro\n\

hw_names = as_list "Nume" hw_grades
email_names = as_list "Nume" (read_csv email_map_csv)
email_map = read_csv email_map_csv
typo_correct_lists = filterPerfectMatches email_names hw_names
typo_list = fst typo_correct_lists
correct_list = snd typo_correct_lists
typo_correct_pairs = makeTypoCorrectPairs typo_list correct_list
correct_table_data = correctAllMistakes email_map "Nume" typo_correct_pairs
correct_table_csv = write_csv correct_table_data

better a b = d m n
  where (m, n) = (length a, length b)
        a'     = listArray (1, m) a
        b'     = listArray (1, n) b

        d i 0 = i
        d 0 j = j
        d i j
          | a' ! i ==  b' ! j = ds ! (i - 1, j - 1)
          | otherwise = minimum [ ds ! (i - 1, j)     + 1
                                , ds ! (i, j - 1)     + 1
                                , ds ! (i - 1, j - 1) + 1
                                ]

        ds = listArray bounds
               [d i j | (i, j) <- range bounds]
        bounds = ((0, 0), (m, n))

deleteAtPosition :: [a] -> Int -> [a]
deleteAtPosition list pos = (take pos list) ++ (drop (pos + 1) list)

deleteElement :: (Eq a) => [a] -> a -> [a]
deleteElement list elem = foldr op [] list
                        where
                            op x [] 
                                | x == elem = []
                                | otherwise = [x]
                            op x acc
                                | x == elem = acc
                                | otherwise = x:acc

hasPerfectMatch :: Value -> [Value] -> Bool
hasPerfectMatch value = foldl op False 
                        where
                            op False x = (x == value)
                            op acc x
                                | x == value = True
                                | otherwise = acc

filterPerfectMatches :: [String] -> [String] -> ([String], [String])
filterPerfectMatches t ref = foldr op pairs t
                            where
                                pairs = (t, ref)
                                op word acc
                                    | hasPerfectMatch word ref = (deleteElement (fst acc) word, deleteElement (snd acc) word)
                                    | otherwise = acc


smallestDistance :: Value -> [Value] -> Int
smallestDistance value = foldl op 0
                        where
                            op 0 word = better value word
                            op acc word = min acc (better value word)
first :: (Value, Value, Int) -> Value
first (typo,correct,distance) = typo
second :: (Value, Value, Int) -> Value
second (typo,correct,distance) = correct                          
third :: (Value, Value, Int) -> Int
third (typo,correct,distance) = distance

findCorrectMatch :: Value -> [Value] -> (Value, Value, Int)
findCorrectMatch typo list = foldl op (typo, "", 10000) list
                            where
                                op acc x
                                    | (better x typo) < third acc = (typo, x, (better x typo))
                                    | otherwise = acc

makeTypoCorrectPairs :: [Value] -> [Value] -> [(Value, Value, Int)]
makeTypoCorrectPairs typos correctWords = foldr op [] typos
                                        where
                                          op typo [] = [findCorrectMatch typo correctWords]
                                          op typo acc = (findCorrectMatch typo correctWords):acc

correctMistake :: (Eq a) => [a] -> a -> a -> [a]
correctMistake list typo correct = map op list
                                        where
                                            op x
                                                | x == typo = correct
                                                | otherwise = x

correctMistakeTable :: Table -> String -> Value -> Value -> Table
correctMistakeTable table colname typo correct = map op table
                                                where
                                                    op row
                                                        | row!!index == typo = correctMistake row typo correct
                                                        | otherwise = row
                                                    index = columnIndexTable colname table


correctAllMistakes :: Table -> String -> [(Value, Value, Int)] -> Table
correctAllMistakes table colname typoCorrectPairs = foldl op table typoCorrectPairs
                                                    where
                                                        op acc pair = (correctMistakeTable acc colname (first pair) (second pair))

-- -- exemplu
-- hw_names = as_list "Nume" hw_grades
-- email_names = as_list "Nume" (read_csv email_map_csv)
-- email_map = read_csv email_map_csv
-- typo_correct_lists = filterPerfectMatches email_names hw_names
-- typo_list = fst typo_correct_lists
-- correct_list = snd typo_correct_lists
-- typo_correct_pairs = makeTypoCorrectPairs typo_list correct_list
-- correctTable = correctAllMistakes email_map "Nume" typo_correct_pairs
-- correct_table_csv = write_csv correct_table


correct_table :: String -> CSV -> CSV -> CSV
correct_table colname wrongTableCSV refTableCSV = correct_table_csv
                                                    where
                                                      wrongTable = read_csv wrongTableCSV
                                                      refTable = read_csv refTableCSV
                                                      wrongColumn = as_list colname wrongTable
                                                      refColumn = as_list colname refTable
                                                      typoCorrectLists = filterPerfectMatches wrongColumn refColumn
                                                      typoList = fst typoCorrectLists
                                                      correctList = snd typoCorrectLists
                                                      typoCorrectPairs = makeTypoCorrectPairs typoList correctList
                                                      correctTable = correctAllMistakes wrongTable colname typoCorrectPairs
                                                      correct_table_csv =  write_csv correctTable

--4.2
-- hw_grades = [
--     ["Nume","Lab (1p)","T1 (0.5p)","T2 (1p)","T3 (1.5p)","Ex1 (0.25p)","Ex2 (0.25p)","Ex3 (0.25p)","Ex4 (0.25p)"],
--     ["Olivia Noah","0.42","0.49","1","1.05","","","0",""],

-- exam_grades = [
--     ["Nume","Q1","Q2","Q3","Q4","Q5","Q6","Ex. Scris"],
--     ["Olivia Noah","0","0","2","2","2","2","0.93"],

-- lecture_grades = [
--     ["Email","12.1","4.2","5.1","12.2","5.2","3","5.3","7.1","7.2","4.1","7.3","8.1","2.2","8.2","16.1","8.3","9.1.","9.2.","10.1","2.1","10.2","10.3","11.1","11.3","11.2","13.1","13.2","13.3"],["Mason.Zoe@stud.cs.pub.ro","1","","1","0","1","1","1","1","1","1","1","1","1","1","0","1","1","1","1","1","1","1","1","1","1","1","1","1"],
--     ["Ian.Brooklyn@stud.cs.pub.ro","0","0","1","0","1","1","1","0","1","","1","1","1","1","","1","1","0","1","1","1","1","1","1","1","1","1","0"],

-- email_map_csv = "\
--     \Nume,Email\n\
--     \Olivia Noah,Olivia.Noah@stud.cs.pub.ro\n\

-- ian_lectures = ["Ian.Brooklyn@stud.cs.pub.ro","0","0","1","0","1","1","1","0","1","","1","1","1","1","","1","1","0","1","1","1","1","1","1","1","1","1","0"]
olivia_lectures = ["Olivia.Noah@stud.cs.pub.ro","","1","0","","1","","1","","","0","","","1","","","","","","","1","","","","","","","",""]

getHwGrade :: Row -> Float
getHwGrade row = foldl op 0.0 (drop 1 row)
                where
                    op acc value = acc + (stringToFloat value)

getLectureGrade :: Row -> Float
getLectureGrade row = 2 * (foldl op 0.0 (drop 1 row)) / (fromIntegral (length $ drop 1 row))
                    where
                        op acc value = acc + (stringToFloat value)

--see task 1.1
-- computeExamGrade :: Row -> Float

--searches which of the 3 tables is the hw one
addHwGrade :: String -> Table -> Table -> Table -> String
addHwGrade name table1 table2 table3
    | table1 == hw_grades = if (searchStudentInTable table1 name) == [] 
                              then ""
                              else printf "%.2f" (getHwGrade $ searchStudentInTable table1 name)
    | table2 == hw_grades = if (searchStudentInTable table2 name) == [] 
                              then "" 
                              else printf "%.2f" (getHwGrade $ searchStudentInTable table2 name)
    | otherwise = if (searchStudentInTable table3 name) == []
                              then ""
                              else printf "%.2f" (getHwGrade $ searchStudentInTable table3 name)

addLectureGrade :: String -> Table -> Table -> Table  -> String
addLectureGrade email table1 table2 table3
    | table1 == lecture_grades = if (searchStudentInTable table1 email) == [] 
                                  then ""
                                  else printf "%.2f" (getLectureGrade $ searchStudentInTable table1 email)
    | table2 == lecture_grades = if (searchStudentInTable table2 email) == [] 
                                  then ""
                                  else printf "%.2f" (getLectureGrade $ searchStudentInTable table2 email)
    | otherwise = if (searchStudentInTable table3 email) == [] 
                                  then ""
                                  else printf "%.2f" (getLectureGrade $ searchStudentInTable table3 email)

addExamGrade :: String -> Table -> Table -> Table -> String
addExamGrade name table1 table2 table3
    | table1 == exam_grades = if (searchStudentInTable table1 name) == [] 
                                then ""
                                else printf "%.2f" (computeExamGrade $ searchStudentInTable table1 name)
    | table2 == exam_grades = if (searchStudentInTable table2 name) == [] 
                                then ""
                                else printf "%.2f" (computeExamGrade $ searchStudentInTable table2 name)
    | otherwise = if (searchStudentInTable table3 name) == [] 
                                then ""
                                else printf "%.2f" (computeExamGrade $ searchStudentInTable table3 name)

getTotal :: Float -> Float -> Float -> Float
getTotal hwGrade lectureGrade examGrade
    | (hwGrade + lectureGrade) < 2.5 = 4.00
    | examGrade < 2.5 = 4.00
    | otherwise = (min (hwGrade + lectureGrade) 5) + examGrade

-- decides if row we're searching is in hw_grades, lecture_grades or exam_grades
--returns the whole row based on the name or email given
searchStudentInTable :: Table -> String -> Row
searchStudentInTable table nameOrEmail
    | head table == head hw_grades = foldr op1 [] table
    | head table == head exam_grades = foldr op2 [] table
    | otherwise = foldr op3 [] table
        where
            op1 row1 [] 
                | (head row1) == nameOrEmail = row1
                | otherwise = []
            op1 row1 acc1
                | (head row1) == nameOrEmail = row1
                | otherwise = acc1

            op2 row2 []
                | (head row2) == nameOrEmail = row2
                | otherwise = []
            op2 row2 acc2
                | (head row2) == nameOrEmail = row2
                | otherwise = acc2

            op3 row3 []
                | (head row3) == nameOrEmail = row3
                | otherwise = []
            op3 row3 acc3
                | (head row3) == nameOrEmail = row3
                | otherwise = acc3
            
getStudent'sGrades :: String -> String -> Table -> Table -> Table -> [String]
getStudent'sGrades name email table1 table2 table3 = hwGrade:lectureGrade:examGrade:[total]
                                                    where
                                                        hwGrade = addHwGrade name table1 table2 table3
                                                        lectureGrade = addLectureGrade email table1 table2 table3
                                                        examGrade = addExamGrade name table1 table2 table3
                                                        total = printf "%.2f" $ getTotal (stringToFloat hwGrade) (stringToFloat lectureGrade) (stringToFloat examGrade)                                       

-- gradesToRow :: String -> [Float] -> Row
-- gradesToRow name allGrades = name:(foldr op [] allGrades)
--                             where
--                               op grade [] = [printf "%.2f" grade]
--                               op grade acc = (printf "%.2f" grade) : acc

mergeGrades :: Table -> Table -> Table -> Table -> Table
mergeGrades emailMap hwGrades examGrades lectureGrades = foldr op [] (tail emailMap)
                                                        where
                                                          op nameEmail [] = [(nameEmail!!0) : (getStudent'sGrades (nameEmail!!0) (nameEmail!!1) hwGrades lectureGrades examGrades)]
                                                          op nameEmail acc = ( (nameEmail!!0) : (getStudent'sGrades (nameEmail!!0) (nameEmail!!1) hwGrades lectureGrades examGrades)):acc
-- given that the first table is emails
grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades emailMapCSV hwGradesCSV
 examGradesCSV lectureGradesCSV = sortedGradesTableCSV
                                  where
                                      -- emailMap = read_csv emailMapCSV
                                      emailMap = read_csv (correct_table "Nume" emailMapCSV hwGradesCSV)
                                      hwGrades = read_csv hwGradesCSV
                                      examGrades = read_csv examGradesCSV
                                      lectureGrades = read_csv lectureGradesCSV

                                      mergedGrades = mergeGrades emailMap hwGrades examGrades lectureGrades
                                      gradesTable = ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"]:mergedGrades
                                      sortedGradesTable = sortTableByName gradesTable
                                      sortedGradesTableCSV = write_csv sortedGradesTable


--tsort can't sort tables by name using the rowOrdering comparator above, since names are strings, not floats                                      
nameComparator :: Row -> Row -> Ordering
nameComparator row1 row2
  | head row1 == "" = LT
  | head row2 == "" = GT
  | head row1 < head row2 = LT
  | otherwise = GT

sortTableByName :: Table -> Table
sortTableByName table = (head table):(sortBy nameComparator (tail table))
