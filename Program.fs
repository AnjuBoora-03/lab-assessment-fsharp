type Student = {
    Id: int
    Name: string
    Age: int
    Marks: float
}

type resultstatus =
    | Pass
    | Fail

let checkResult (student: Student) : resultstatus =
    if student.Marks >= 40.0 then
        Pass
    else
        Fail
let displayResult (student: Student) =
    let result = checkResult student
    match result with
    | Pass -> printfn "Student %s has passed." student.Name
    | Fail -> printfn "Student %s has failed." student.Name

let processStudents (students: Student list) =
    let names =
        students
        |> List.map (fun s -> s.Name)   
    printfn "/nList of Student Names: %A" names
    students |> List.iter displayResult
let topper  (students: Student list) =
    let topStudent =
        students
        |> List.maxBy (fun s -> s.Marks)
    printfn "Topper is %s with marks %.2f" topStudent.Name topStudent.Marks
    printfn "\nStudent Results:"
    students |> List.iter displayResult
let createtable connectionString =
    use connection = new SqlConnection(connectionString)
    connection.Open()
    let createTableQuery = """
        CREATE TABLE Students (
            Id INT PRIMARY KEY,
            Name NVARCHAR(100),
            Age INT,
            Marks FLOAT
        )
    """
    use command = new SqlCommand(createTableQuery, connection)
    command.ExecuteNonQuery() |> ignore
    