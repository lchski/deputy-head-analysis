Questions this project should answer:

- By position:
	- What’s the DM level of the position?
	- Has that DM level ever changed?
	- Is DM level related to size ($, FTEs) of department?
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
- Positions are sometimes "and concurrently" (and, sometimes, they’re concurrent without that flag—usually related to "Personal Representative for the G# Summit" or similar)—could split into a "position_secondary"

- Older salary ranges available from previous PCO site. 2018 scrape, for:
	- 2013: https://web.archive.org/web/20180201222842/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2013-eng.htm
	- 2012: https://web.archive.org/web/20180201222844/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2012-eng.htm
	- 2011: https://web.archive.org/web/20180201222845/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2011-eng.htm
  2015 scrape, for:
	- 2010: https://web.archive.org/web/20150613040456/http://www.pco-bcp.gc.ca/index.asp?lang=eng&page=secretariats&sub=spsp-psps&doc=sal/sal2010-eng.htm

- Would definitely be fun to look at appointment authorities, use of term or not, from appointment orders.
- also the “from” locations (might be easier to use French version, as it puts the province in parentheses)




- Other salary orders:
  - "Order fixing the salary of certain persons appointed by the Governor in Council" (sometimes includes qualifier, e.g., CEO), e.g., 2009-1576, 2007-2023 (includes one former DM), 2007-0357 (a few DMs)
  - "Fix the salary and employment conditions" (sometimes with “Order fixing...” ?)
  - "Fixing of the remuneration payable to certain persons appointed by the Governor in Council" (some non-DM), e.g., 2010-1167, 2007-1333, 2006-1316, 2005-2007 (includes one former DM), 2005-1331, 2005-0729 (a few DM), 2005-0034, 2004-1318
  - various individual salary orders, e.g., 2009-0773, 2009-0778, 2008-1513, 2022-1093, 2007-0262
- These include non-DMs, but may be interesting to capture to show movement from / to DM ranks, if any (lower priority)




Limitations:
- doesn't capture DMs appointed or promoted after the latest salary order (currently October 2022), nor much movement within 2022
- there doesn't seem to be a salary order for FY 2012-13, only 7 revisions for that year (may be able to fill in the blanks by looking at appointment orders directly)



Appointment orders:
  
  - check for presence in both appointments and salary datasets using this code, followed by `filter(is.na(appointments))` (or `salary`), depending which you want to look at:
  ```appointments_classified %>%
    count(name_standardized, name = "appointments") %>%
    full_join(
        salary_revisions_classified %>%
            count(name_standardized, name = "salary")) %>%
    mutate(in_both = ! is.na(appointments) & ! is.na(salary))```
      - for `is.na(salary)`, meaning they appear in an appointment but not a salary order, (16 as of writing), we'd expect they're either: recently appointed (~after October 2022, so there's no salary order yet); not a deputy head (e.g., Secretary to the GG, or other GC/GCQ positions)
      - for `is.na(appointments)`, meaning they appear in a salary order but not an appointment order (146 as of writing), they're likely: not captured from the OIC scoping for appointment orders (e.g., President of ACOA)
        - we can prioritize these by arranging by the number of salary entries
  





Reflections
- never (?) demoted, from one level to another (steps / salary progression, unknown)
- but the level of a given position can change, depending on the person occupying it
- which challenges the core idea of classification, used everywhere else in the public service: the duties of the role are X, Y, Z, so it's a level #, and you either are or aren't qualified for that level
- sometimes, apparently (but rarely), this is treated as an actual reclassification: https://open.canada.ca/en/search/reclassification/reference/8b0f00c80a378631cf0df8c77d0e12f1
- otherwise, it seems to largely be "this person was a 3, they're moved to this post [possibly for other reasons, unrelated to particular competence in that field!], they must remain a 3"
