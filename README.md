# F# program to analyze payroll data

-Main point of project is to practice functional programming with F#.\
-Parses payroll data that is in .csv format.\
-Given a sequence of strings representing payroll data, parses the strings and returns a list of tuples. Each sub-list denotes one employee.  Example:\
[("JESSE A", "ACOSTA", "POLICE OFFICER", "POLICE", "Salary", 0.0, 93354.0, 0.0); ... ]

-The values are first name, last name, occupation, department, salary type, hours per week, annual salary, and hourly wage.
Depending on the salary type, either hours per week and hourly wage or annual salary will be filled in with 0.0, since the field is empty in the csv.
