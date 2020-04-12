# Overview

A command line tool to generate reports used to manage the budget for a product development organization. Currently, two types
of reports are available: *budget* and *ratio*.

## Budget Report

A budget report tells a manager how much money the organization is spending each quarter on each product priority.

Spend for a quarter is calculated by summing the quarterly costs of everyone on a team assigned to a product priority. If
someone is working on multiple teams, their cost is divided evenly between these teams. If someone is not assigned to a team
in a particular quarter, their team for the quarter is assumed to be "None" to ensure that their cost is still accounted for
in the budget.

The cost of a teammate is `(salary + perPersonCost) * salaryCostFactor`. In words, we take the person's salary (after
splitting it based on the number of teams they are assigned to) add some constant to account for per-person costs (such as
software licenses and team events) and then we scale this amount to account for costs that grow with base salary such as a
401K plan.

You can manually change the constants used in the cost calculation by updating the values directly in `CostCalculator.hs`. In
the future, we may make these values configurable via the command line or separate file.

The report is printed to `stdout` in CSV format. The report has the following structure:

| Priority | Spend Q1 | Spend Q2 | Spend Q3 | Spend Q4 | Spend FY |
|----------|----------|----------|----------|----------|----------|
| Foobar   | 40.10    | 20.45    | 33.80    | 40.10    | 134.45   |
| Barfoo   | 5.12     | 8.12     | 1.12     | 1.14     | 15.5     |

Dollar values are shown in _Millions_.

## Ratio Report

A ratio report helps a manager understand whether the roles in the organization are balanced, and can inform hiring needs for
the future. For this report, we assume several standard roles on each team:

* Product Manager
* User Experience Designer
* Software Engineer
* Quality Assurance Engineer

The number of software engineers in an organization typically drives hiring for the other non-SE roles, so we report the
ratio of software engineers to each of these roles. A manager can compare this against target values for each role to better
understand hiring needs for the future.

The report is printed to `stdout` in CSV format. The report has the following structure:

| Role                       | SEs Per Role Q1 | SEs Per Role Q2 | SEs Per Role Q3 | SEs Per Role Q4 |
|----------------------------|-----------------|-----------------|-----------------|-----------------|
| Product Manager            | 6.4             | 6.8             | 7.1             | 6.2             |
| User Experience Designer   | 6.0             | 6.7             | 7.4             | 6.5             |
| Quality Assurance Engineer | 4.5             | 5.0             | 4.4             | 4.3             |

If the input files do not contain any software engineer teammates, the ratio values will be _-1.0_ to indicate that the values
could not be computed.

# Usage

## Requirements

This program is written in Haskell and requires the `ghc` compiler as well as the `stack` build tool.

## Installation

Clone this repo with:

```
git clone https://github.com/jyoder/budgeting.git
```

In the top-level repo directory, compile the program with:

```
stack build
```

Once compiled, you can execute the program with:

```
stack exec budgeting-exe <budget|ratios> <priorities-file> <salaries-file> <teammates-file>
```

The first command line argument specifies which kind of report to generate and can be either `budget` or `ratios`. The
second argument is the `priorities-file`. This file contains a list of teams and which priorities they are assigned to each
quarter of the year. The third argument is the `salaries-file`. This contains salary information for each person in the
organization. The fourth argument is the `teammates-file`. This contains the list of people in the organization and which
teams they are assigned to each quarter. More details on the format of these files can be found in the following sections.

# Input Files

All input files use the CSV format. This section describes the required structure of each input file and how fields in these
files are related to one another.

## Priorities File

This file contains a list of teams and which priorities they are assigned to each quarter of the year. It has the following
structure:

| Team        | Priority Q1    | Priority Q2    | Priority Q3    | Priority Q4    |
|-------------|----------------|----------------|----------------|----------------|
| Goobers     | Infrastructure | Security       | Gumballs       | Communication  |
| Hex Pistols | Communication  | Communication  | Infrastructure | Infrastructure |
| None        | Overhead       | Overhead       | Overhead       | Overhead       |

Every team that appears in the teammates file _must_ have an entry in the priorities file or data validations will fail. The
`None` team is a special team that teammates will have by default if no team is specified. As such, the priorities file should
always contain an entry for `None` to ensure that all costs are accounted for.

## Salaries File

This file contains salary information for each person in the organization. It has the following structure:

| Bhc   | Name           | Salary Q1 | Salary Q2  | Salary Q3  | Salary Q4  |
|-------|----------------|-----------|------------|------------|------------|
| 100   | Bob Bobberson  | 70000.00  | 75000.00   | 75000.00   | 80000.00   |
| 2000  | Amy Amerson    | 75000.00  | 75000.00   | 83000.00   | 83000.00   |

Salary numbers for each quarter are specifiec as _yearly salaries_ (they are converted to quarterly salaries internally).

The `Bhc` column contains the _Budgeted Headcount_ ID for each person. This is a unique value used to identify a person. Each
BHC that shows up in the salaries file _must_ have a corresponding teammate in the teammates file. Data validations will fail
if any BHCs are duplicated or do not show up in the teammates file. The `Name` column is only used for reference and is
ignored by validations.

## Teammates File

This file contains the list of people in the organization and which teams they are assigned to each quarter. It has the
following structure:

| Bhc   | Name           | Department  | Role                   | Teams Q1               | Teams Q2           | Teams Q3           | Teams Q4           |
|-------|----------------|-------------|------------------------|------------------------|--------------------|--------------------|--------------------|
| 100   | Bob Bobberson  | Development | Product Manager        | "Hex Pistols,Goobers"  | Hex Pistols        | Hex Pistols        | None               |
| 2000  | Amy Amerson    | Development | Software Engineer      | Goobers                | Goobers            |                    | Goobers            |

The `Bhc` column contains the _Budgeted Headcount_ ID for each person. This is a unique value used to identify a person. Each
BHC that shows up in the teammates file _must_ have a corresponding entry in the salaries file.

### Teams Columns

In the teams columns, it is possible to specify that a person is on multiple teams. In this case, teams should be enclosed in
quotes and separated by commas. Each team in the teams columns _must_ have a corresponding entry in the priorities file,
otherwise data validations will fail.

In the example above, no teams were specified for `Amy Amerson` in Q4. By default, she will be assigned to a special team
called `None` to ensure her cost is always accounted for. In this case, the `None` team _must_ appear in the priorities file,
otherwise data validations will fail. It is also possible to assign people to the special `None` team as shown in the case of
`Bob Bobberson` in Q4. 

### Role Column

The role column is used by the ratio report. Any value is allowed in this column, but the following values are treated
specially for the purposes of the ratio report:

* Product Manager
* User Experience Designer
* Software Engineer
* Quality Assurance Engineer

### Department Column

Input files undergo a bit of _preprocessing_ prior to validation. One purpose of the preprocessing stage is to remove any
teammates from the file who are not part of the product development organization. Current departments that are treated
specially and removed from the input include:

* Web Operations
* Security

It is not necessary for people with these roles to have a `Bhc` or to have a corresponding entry in the salaries file.
