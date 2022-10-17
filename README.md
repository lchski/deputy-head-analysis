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
