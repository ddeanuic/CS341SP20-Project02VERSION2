//
// F# program to analyze payroll data.
//
// DANIEL DEAN ASUNCION
// dasunc2
// U. of Illinois, Chicago
// CS 341, Spring 2020
// Project #02
//

module Project02


//
// doubleOrNothing
//
// Given a string containing a double numeric value
// or being an empty string
// returns the double equivalent, with the empty string
// treated as the value 0.0
//
let doubleOrNothing s =
    match s with
    | "" -> 0.0
    | x -> double x
#nowarn "25"
//
// ParseCSVLine and ParseCSVDatabase
//
// Given a sequence of strings representing payroll data,
// parses the strings and returns a list of tuples.  Each
// sub-list denotes one employee.  Example:
//
//[ ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0); ... ]
//
// The values are first name, last name, occupation,
// department, salary type, hours per week, annual salary,
// and hourly wage.
// Depending on the salary type,
// either hours per week and hourly wage
// or annual salary will be filled in with 0.0,
// since the field is empty in the csv
//
// First Name, Last Name, Occupation, Dept Name, Fulltime or Part time, Typical Hours, Annual Salary, Hourly Rate
let ParseCSVLine (line:string) =
    let tokens = line.Split(',')
    let listOfValues = Array.toList tokens
    //match listOfValues with  // alternative implmentation to remove warnings
    //| fName::lName::occupation::department::salaryType
    //    ::hoursPerWeek::annualSalary::hourlyWage::[] -> (fName,lName,occupation,department,salaryType,
    //                                                      (doubleOrNothing hoursPerWeek),
    //                                                      (doubleOrNothing annualSalary),
    //                                                      (doubleOrNothing hourlyWage)
    //                                                    )
    //| _ -> failwith "Insufficient values in line of csv file"

    let fName::lName::Occupation::Department::SalaryType
        ::HoursPerWeek::AnnualSalary::HourlyWage::[] = listOfValues
    (fName,lName,Occupation,Department,SalaryType,
      (doubleOrNothing HoursPerWeek),
      (doubleOrNothing AnnualSalary),
      (doubleOrNothing HourlyWage)
    )

let rec ParseCSVDatabase lines =
    let employees = Seq.map ParseCSVLine lines
    //printfn "%A" employees
    Seq.toList employees


// Returns whether the employee is salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// true
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// false
//
let isSalary employee =
    match employee with
    | (_, _, _, _, salary, _, _, _) -> if salary = "Salary" then true
                                       else false

// Returns whether the employee is paid hourly, not salaried
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// false
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// true
//
let isHourly employee =
    match employee with
    | (_, _, _, _, hourly, _, _, _) -> if hourly = "Hourly" then true
                                       else false

// Returns the full name of the employee
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "JESSE A ACOSTA"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "JOHN R KEATING"
//
let getName employee =
    match employee with
    | (fName, lName, _, _, _, _, _, _) -> fName + " " + lName

// Returns name of the department the employee belongs to
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// "POLICE"
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// "GENERAL SERVICES"
//        
let getDepartment employee =
    match employee with
    | (_, _, _, department, _, _, _, _) -> department


//Gets the salary of the employee.
let getSalary employee =
    match employee with
    | (_, _, _, _, _, _, salaryVal, _) -> let payType = isSalary employee
                                          if payType = true then salaryVal
                                          else 0.0

//
//The function calcSalary calculates the annual salary,
// either by directly returning the recorded annual salary
// or calculating it by finding the average weekly salary
// by multiplying the hours per week by hourly wage,
// and then multiplying that by 52 to get the average
// annual salary for that hourly worker.    
//
// Example: on Input of
// ("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0)
// Returns
// 93354.0
//
// Example: on Input of
// ("JOHN R", "KEATING", "FOREMAN OF ELECTRICAL MECHANICS", "GENERAL SERVICES",  "Hourly", 40.0, 0.0, 52.35)
// Returns
// 52.0*40.0*52.35 = 108888.0
//
let calcSalary employee =
    let hourlyToAnnual employee =
        match employee with
        | (_, _, _, _, _, hours, _, wage) -> let result = (float hours) * wage * (float 52)
                                             result

    if (isSalary employee) = true then getSalary employee
    elif (isHourly employee) = true then hourlyToAnnual employee
    else (float 0.0)
       
// The function getNumberOfEmployees returns the number of employees in the data set.
let rec getNumberOfEmployees allData =
    match allData with
    | [] -> 0
    | element::rest -> 1 + getNumberOfEmployees rest

// The function getNumberOfSalariedEmployees returns the number of employees who have an annual salary in the data set.
let getNumberOfSalariedEmployees allData =
    (*
    //Recursive implementation. Add "rec" back to function name.
    match allData with
    | [] -> 0
    | element::rest -> if (isSalary element) = true then 1 + getNumberOfSalariedEmployees rest
                       else 0 + getNumberOfSalariedEmployees rest
    *)
    let salariedEmployees = List.filter (isSalary) allData

    List.length salariedEmployees

// The function getNumberOfSalariedEmployees returns the number of employees who are paid hourly in the data set.
let getNumberOfHourlyEmployees allData =
   (*
   //Recursive implementation.  Add "rec" back to function name.
   match allData with
   | [] -> 0
   | element::rest -> if (isHourly element) = true then 1 + getNumberOfHourlyEmployees rest
                      else 0 + getNumberOfHourlyEmployees rest
   *)
   let hourlyEmployees = List.filter (isHourly) allData
   
   List.length hourlyEmployees

// The function findHighestPaidEmployee returns the name and salary
// of the highest paid employee.
// Use the computed salary for hourly employees.
let rec findHighestPaidEmployee allData =
    let helper emp1 emp2 = let val1 = calcSalary emp1
                           let val2 = calcSalary emp2
                           if val1 > val2 then emp1 else emp2

    match allData with
    | [] -> invalidArg "allData" "Input sequence is empty."
    | [oneElement] -> (getName oneElement, calcSalary oneElement)
    | element1::element2::rest -> findHighestPaidEmployee((helper element1 element2)::rest)

// The function findHighestPaidEmployee returns the salary
// of the highest paid employee within a specific department.
// Use the computed salary for hourly employees.
let findHighestPaidEmployeeInDept allData deptName =
    let helper employee deptName = if (getDepartment employee) = deptName then true else false

    let deptList = List.filter (fun x -> helper x deptName) allData
    let highestPaidInDept = findHighestPaidEmployee deptList

    snd highestPaidInDept

// The function getAverageSalary calculates
// the average of the computed salary field.
let getAverageSalary allData =
    let numEmployees = float (getNumberOfEmployees allData)
    let rec helper accum allData =
        match allData with
        | [] -> accum
        | element::rest -> let curSalary = calcSalary element
                           curSalary + helper accum rest
    let sumSalary = helper 0.0 allData
    let avgSalary = sumSalary / numEmployees
    avgSalary

// The function getAverageSalary calculates
// the average of the computed salary field for a specific department.
let getAverageSalaryInDept allData deptName =
  let helper employee deptName = if (getDepartment employee) = deptName then true else false
  let deptList = List.filter (fun x -> helper x deptName) allData
  let deptAvgSalary = getAverageSalary deptList

  deptAvgSalary

// Searches through the data set to generate the list of all unique department names.
let getUniqueDepartmentNames allData =
    allData
    |> List.map getDepartment
    |> List.distinct
    |> List.sort
    //without piping
    //let allDepts = List.map getDepartment allData
    //let uniqueDepts = List.distinct allDepts
    //uniqueDepts


// The function howManyEmployeesInEachDepartment computes the number of employees in every department.  
// This function should return a list of tuples, pairs between the department name and number of employees.
let rec howManyEmployeesInEachDepartment allData deptNames =
  let rec deptCounter curDept allData = 
    match allData with
    | [] -> 0
    | element::rest -> if (getDepartment element) = curDept then 1 + deptCounter curDept rest
                       else 0 + deptCounter curDept rest

  match deptNames with
  | [] -> [] 
  | element::rest -> let deptCount = deptCounter element allData
                     [(element, deptCount)] @ howManyEmployeesInEachDepartment allData rest

//Helper? to get the number of employees in a department
let rec numEmployeesInGivenDepartment deptName deptCount =
  match deptCount with
  | [] -> 0
  | element::rest -> if fst element = deptName then snd element
                     else numEmployeesInGivenDepartment deptName rest
  


// The function findTotalSalaryByDepartment computes the overall annual salary budget for every department.
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a list of tuples, pairs between the department name and total annual salary.
let rec findTotalSalaryByDepartment allData deptNames =
  let rec deptSalarySum curDept allData = 
    match allData with
    | [] -> 0.0
    | element::rest-> let curSalary = calcSalary element
                      if (getDepartment element) = curDept then curSalary + deptSalarySum curDept rest
                      else 0.0 + deptSalarySum curDept rest
  
  match deptNames with
  | [] -> []
  | element::rest -> let deptSum = deptSalarySum element allData
                     [(element, deptSum)] @ findTotalSalaryByDepartment allData rest
       
// The function findHighestPaidDeptOverall returns the name and total annual salary
// of the department with the largest overall annual salary paid to employees in that department.
// The calculated salary should include the average annual salary for hourly employees.
// This function should return a single tuple, containing the department name and total annual salary.
let rec findHighestPaidDeptOverall allData deptNames =
  let helper curDept allData = 
    let found = findHighestPaidEmployeeInDept allData curDept

    found
    
  match deptNames with
  | [] -> []
  | element::rest -> let highestSalaryInDept = helper element allData
                     [(element, highestSalaryInDept)] @ findHighestPaidDeptOverall allData rest

// The function withinSalaryRange returns the number of employees whose calculated salary
// is greater than the lower bound and less than or equal to the upper bound.
let withinSalaryRange lower upper L =
  let rangeHelper sal = if sal > lower && sal <= upper then true else false

  let rec _withinSalaryRange lower upper L = 
    match L with
    | [] -> 0
    | element::rest -> let curSalary = calcSalary element
                       let isInRange = rangeHelper curSalary
                       if isInRange = true then 1 + _withinSalaryRange lower upper rest
                       else 0 + _withinSalaryRange lower upper rest
  _withinSalaryRange lower upper L

//
// printOneHistogram
//
// prints one histogram value, the format is
//   label: *****value
// where the # of stars is value / amountPerStar.
//
let printOneHistogram label value amountPerStar =
  //
  // helper function to print # of stars:
  //
  let rec printstars n =
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printstars (n-1)
  //
  // print label, stars, and histogram value:
  //
  printf " %16s: " label    // Enough space for all the departments in the file
  printstars (value / amountPerStar)
  printfn "%A" value




[<EntryPoint>]
let main argv =
    //
    // input file name, then input employee data and build
    // a list of tuples:
    //
    printf "Enter name of the csv file containing employee data: "

    let filename = System.Console.ReadLine() //"payroll_02.csv" //
    let contents = System.IO.File.ReadLines(filename)
    let data = ParseCSVDatabase contents
    //let data = [List.head data]

    //printfn "This is the data you have loaded."
    //List.iter (printfn "%A") data

    printf "Enter name of the department to be analyzed: "

    let deptname = System.Console.ReadLine()
    let allDepts = getUniqueDepartmentNames data

    let N = getNumberOfEmployees data
    printfn ""
    printfn "# of employees: %A" N
    printfn ""

    let deptNs = howManyEmployeesInEachDepartment data allDepts
    //printfn ""
    let curDeptCount = numEmployeesInGivenDepartment deptname deptNs

    printfn "# of employees in %s: %d" deptname curDeptCount       // Figure out how to get the number for one department
    printfn ""

    //
    // % of employees salaried:
    //
    let numSalaried = getNumberOfSalariedEmployees data
    let percentSalaried = (double numSalaried) / (double N) * 100.0
    let numHourly = getNumberOfHourlyEmployees data
    let percentHourly = (double numHourly) / (double N) * 100.0

    printfn "%% of employees Salaried: %d (%.2f%%)" numSalaried percentSalaried
    printfn "%% of employees Hourly: %d (%.2f%%)" numHourly percentHourly
    printfn ""

    //
    // average salary:
    //
    let avgSalary = getAverageSalary data
    printfn "Average salary: %.2f" avgSalary
    //printfn ""

    //
    // average salary in department:
    //
    let avgSalary = getAverageSalaryInDept data deptname
    printfn "Average salary in %s: %.2f" deptname avgSalary
    printfn ""
   
    //
    // highest salary:
    //
    let (maxName,maxSalary) = findHighestPaidEmployee data
    printfn "Largest salary: %s paid %.2f annually" maxName maxSalary
    //printfn ""

    //
    // highest salary in department:
    //
    let maxSalary = findHighestPaidEmployeeInDept data deptname
    printfn "Largest salary in %s: %.2f" deptname maxSalary
    printfn ""

    printfn "** Histogram of employees by department (each star represents 5 employees):"    
    // Use printOneHistogram to build the histogram
    let histogram1 = List.map (fun x -> printOneHistogram (fst x) (snd x) 5) deptNs
    printfn ""
    //
    // categorize salaries into 5 groups:
    //   0       < salary <= 60000
    //   60000   < salary <= 80000
    //   80000   < salary <= 100000
    //   100000  < salary <= 120000
    //   120000  < salary <= 10,000,000  // arbitrary upper bound to reuse function
    //

    let count60korless = withinSalaryRange 0.0 60000.0 data
    let percent60korless = (double count60korless) / (double N) * 100.0

    let count60kto80k = withinSalaryRange 60000.0 80000.0 data
    let percent60kto80k = (double count60kto80k) / (double N) * 100.0

    let count80kto100k = withinSalaryRange 80000.0 100000.0 data
    let percent80kto100k = (double count80kto100k) / (double N) * 100.0

    let count100kto120k = withinSalaryRange 100000.0 120000.0 data
    let percent100kto120k = (double count100kto120k) / (double N) * 100.0

    let countgreater120k = withinSalaryRange 120000.0 10000000.0 data
    let percentgreater120k = (double countgreater120k) / (double N) * 100.0

    printfn "** Salary Ranges:"
    printfn " 0-60000 : %A (%.2f%%)" count60korless percent60korless
    printfn " 60000-80000 : %A (%.2f%%)" count60kto80k percent60kto80k
    printfn " 80000-100000: %A (%.2f%%)" count80kto100k percent80kto100k
    printfn " 100000-120000: %A (%.2f%%)" count100kto120k percent100kto120k
    printfn " > 120000: %A (%.2f%%)" countgreater120k percentgreater120k
    printfn ""

    printfn "** Histogram of Salary Ranges (each star represents 10 employees):"    
    let salaryGroups = [("<60000", count60korless);("60-80k", count60kto80k);("80-100k", count80kto100k);("100-120k", count100kto120k);(">120000",countgreater120k)]
    let histogram2 = List.map (fun x -> printOneHistogram (fst x) (snd x) 10) salaryGroups



    0