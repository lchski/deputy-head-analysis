Questions this project should answer:

- By position:
	- What’s the DM level of the position?
	- Has that DM level ever changed?
- By person:
	- What DM levels has that person held?
	- How have they progressed through the DM levels?

To do that, this project needs:

- Who held deputy positions, when (OICs, optionally also “Change in the ranks” announcements)
	- [Raw OIC data, all OICs](https://github.com/lchski/oic-data)
	- [Script to extract deputy appointments](https://github.com/lchski/oic-analysis/blob/main/analysis/deputy-appointments.R)
	- [TODO: link to folder of extracted OICs in this repo]
- What the pay ranges for those deputy positions was (OICs)
	- [Raw OIC data, all OICs](https://github.com/lchski/oic-data)
	- [Script to extract deputy appointments](https://github.com/lchski/oic-analysis/blob/main/analysis/deputy-appointments.R)
	- [TODO: link to folder of extracted OICs in this repo]
- What the pay ranges correspond to in deputy levels (PCO “[Salary ranges and maximum pay ranges for Governor in Council appointees](https://www.canada.ca/en/privy-council/programs/appointments/governor-council-appointments/compensation-terms-conditions-employment/salary-ranges-performance-pay.html)”)
  - [TODO: link to folder of extracted salary ranges in this repo]
	- [TODO: find years before 2014-15, going back to 2003-04 if possible, though lower priority]

Parsed data structure:

```
position,person,start_date,salary_min,salary_max
```

Processed data structure:

```
position,person,start_date,end_date,group_level,salary_min,salary_max
```

Signal phrases:

"appoints [name] of [location] to be [position] to hold office during pleasure ... "

"appoints [name] of [location] to be [position] to be styled [position_style] to hold office during pleasure"

"designates [name] to be [position] to be styled [position_style]" (“designates” appears sometimes referring to the PSEA, see e.g., `2003-0528`, sometimes / more frequently when the person has a clarifying “DM” title on top of being “Associate DM”, e.g., at HRSDC/ESDC or DFAIT)

- "which (salary|remuneration) is within the range" which captures:
  - "remuneration as set out in the annexed schedule, which salary is within the range [range] effective [date]"
  - "salary at the rate set out in the schedule hereto, which salary is within the range [range]"
  - "set out in the schedule hereto, which salary is within the range [range]"


## Notes

- Salary ranges shift over time, as executive pay is updated (following rounds of general collective bargaining).
	- e.g., DM-2 in 2015–16 made 224,700 to 264,300, per _current_ version of the page. But go back to the [version from 2018](https://web.archive.org/web/20180223163506/https://www.canada.ca/en/privy-council/programs/appointments/governor-council-appointments/compensation-terms-conditions-employment/salary-ranges-performance-pay.html), and the range shows as 221,300 to 260,300.
	- e.g., until recently, all executives were appointed and paid at the 2017-18 levels (where an EX-5 runs 181,000 to 212,900); these mass revisions are affected by “Salary Orders” (which, upon reflection, may be the easiest way to capture much of the pay, position, and dates for most people; they just exclude more recent appointments, and need to check how standard the form is)
