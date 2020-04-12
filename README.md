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

The report is printed to `stdout` in CSV format. The report has the following structure:

| Priority | Spend Q1 | Spend Q2 | Spend Q3 | Spend Q4 |
|----------|----------|----------|----------|----------|
| Foobar   | 40.10    | 20.45    | 33.80    | 40.10    |
| Barfoo   | 5.12     | 8.12     | 1.12     | 1.14     |

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
