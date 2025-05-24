---
editor_options: 
  markdown: 
    wrap: 72
---

# NSCH 2022 Survey Data Dictionary

**MHealthConcern** - Mental Health Concern (Composite Variable)
- **Description:** Composite variable indicating if child has current mental health concerns (anxiety or depression)
- **Construction:** Created from K2Q33B OR K2Q32B = 1 (either currently has anxiety OR currently has depression)
- **Values:**
  - 0 = No current mental health concerns
  - 1 = Has current mental health concerns
- **Data Type:** Binary (0/1)

### Outcome Component Variables

**K2Q33A** - Anxiety (Ever Diagnosed)
- **Description:** Has a doctor or healthcare provider EVER told you that this child has anxiety?
- **Values:**
  - 1 = Yes
  - 2 = No
- **Data Type:** Categorical
- **Usage:** Component variable used to construct MHealthConcern target variable

**K2Q33B** - Anxiety (Currently)
- **Description:** Does this child CURRENTLY have anxiety?
- **Values:**
  - 1 = Yes
  - 2 = No
- **Data Type:** Binary
- **Usage:** Primary component variable used to construct MHealthConcern target variable

**K2Q32A** - Depression (Ever Diagnosed)
- **Description:** Has a doctor or healthcare provider EVER told you that this child has depression?
- **Values:**
  - 1 = Yes
  - 2 = No
- **Data Type:** Categorical
- **Usage:** Component variable used to construct MHealthConcern target variable

**K2Q32B** - Depression (Currently)
- **Description:** Does this child CURRENTLY have depression?
- **Values:**
  - 1 = Yes
  - 2 = No
- **Data Type:** Binary

---

### Demographic Variables

**SC_AGE_YEARS** - Child's Age
- **Description:** Age of the child in years
- **Values:** Continuous (6-17 years for this analysis)
- **Data Type:** Numeric

**sex_22** - Child's Sex
- **Description:** Sex of the child
- **Values:**
  - 1 = Male
  - 2 = Female
- **Data Type:** Categorical

**SC_RACE_R** - Race/Ethnicity
- **Description:** Race as described in 7 categories
- **Values:**
  - 1 = White alone
  - 2 = Black or African American alone
  - 3 = American Indian or Alaska Native alone
  - 4 = Asian alone
  - 5 = Native Hawaiian and Other Pacific Islander alone
  - 7 = Two or More Races
- **Data Type:** Categorical

**age3_22** - Age Group Categories
- **Description:** Categorical age groupings
- **Values:** Age categories (specific ranges need verification from data)
- **Data Type:** Categorical

**BORNUSA** - Born in USA
- **Description:** Binary variable representing if the individual was born in the USA
- **Values:**
  - 0 = No
  - 1 = Yes
- **Data Type:** Binary
- **Usage:** Used in analysis as demographic control variable

---

### Family Environment Variables

**FAMILY_R** - Family Structure
- **Description:** The family composition/parent situation of the individual
- **Values:**
  - 1 = Two biological/adoptive parents, currently married
  - 2 = Two biological/adoptive parents, not currently married
  - 3 = Two parents (at least one not biological/adoptive), currently married
  - 4 = Two parents (at least one not biological/adoptive), not currently married
  - 5 = Single mother
  - 6 = Single father
  - 7 = Grandparent household
  - 8 = Other relation
- **Data Type:** Categorical

**HHCOUNT** - Household Size
- **Description:** Number of people living in the household
- **Values:** Continuous (count)
- **Data Type:** Numeric
- **Usage:** Used as family environment control variable in analysis

**MotherMH_22** - Mother's Mental Health
- **Description:** The status/severity of the mother's mental health
- **Values:**
  - 1 = Excellent
  - 2 = Good
  - 3 = Fair or Poor
  - 95 = No mother reported in household as primary caregiver
  - 99 = Missing values
- **Data Type:** Categorical

**FatherMH_22** - Father's Mental Health
- **Description:** The status/severity of the father's mental health
- **Values:**
  - 1 = Excellent
  - 2 = Good
  - 3 = Fair or Poor
  - 95 = No father reported in household as primary caregiver
  - 99 = Missing values
- **Data Type:** Categorical

---

### Social Support Variables

**K8Q35** - Emotional Support Available
- **Description:** Child has someone to turn to for emotional support
- **Values:**
  - 0 = No
  - 1 = Yes
- **Data Type:** Binary

**ShareIdeas_22** - Communication with Parents
- **Description:** How well children share ideas or talk about things that really matter with their parents
- **Values:**
  - 1 = Very Well
  - 2 = Somewhat Well
  - 3 = Not at all or Not very well
- **Data Type:** Categorical

**mentor_22** - Adult Mentor Availability
- **Description:** Children have at least one adult mentor
- **Values:**
  - 1 = Yes
  - 2 = No
  - 90 = Children age 0-5 years (not applicable)
  - 99 = Missing
- **Data Type:** Categorical

**EventPart_22** - Parent Participation in Activities
- **Description:** Parent participation in the child's activities
- **Values:**
  - 1 = Always
  - 2 = Usually
  - 3 = Sometimes
  - 4 = Rarely or never
  - 90 = Children age 0-5 years (not applicable)
  - 99 = Missing
- **Data Type:** Categorical

---

### Activity and Lifestyle Variables

**PHYSACTIV** - Physical Activity Level
- **Description:** How many days did this child exercise, play a sport, or participate in physical activity for at least 60 minutes
- **Values:**
  - 1 = 0 days
  - 2 = 1-3 days
  - 3 = 4-6 days
  - 4 = Every day
- **Data Type:** Categorical

**AftSchAct_22** - After-School Activity Participation
- **Description:** Whether the child participates in organized activities outside school
- **Values:**
  - 0 = No participation
  - 1 = Participates
- **Data Type:** Binary

**ScreenTime_22** - Daily Screen Time
- **Description:** The number of hours spent on screens daily
- **Values:** Categorical hours (specific categories need verification)
- **Data Type:** Categorical

---

### Adverse Experiences Variables

**ACE12** - Sexual Orientation/Gender Identity Discrimination
- **Description:** Child experienced "Treated or judged unfairly because of their sexual orientation or gender identity"
- **Values:**
  - 0 = No
  - 1 = Yes
- **Data Type:** Binary

**ACEct11_22** - Total ACE Count
- **Description:** The number of adverse childhood experiences out of 11 possible
- **Values:** Count (0-11)
- **Data Type:** Numeric

**ACE4ctCom_22** - Community-Based ACEs
- **Description:** Whether the child experienced 1 or more community-based adverse events
- **Values:**
  - 1 = No community-based adverse childhood experiences
  - 2 = Experienced 1 or more community-based ACEs
- **Data Type:** Binary

**ACE6ctHH_22** - Household-Based ACEs
- **Description:** The number of household-based adverse childhood experiences based on 6 household items
- **Values:** Count (0-6)
- **Data Type:** Numeric

---

### Bullying Variables

**bully_22** - Bullying Others
- **Description:** Whether the child bullies others
- **Values:**
  - 1 = Never (in the past 12 months)
  - 2 = 1-2 times (in the past 12 months)
  - 3 = 1-2 times per month
  - 4 = 1-2 times per week
  - 5 = Almost every day
- **Data Type:** Categorical

**bullied_22** - Being Bullied
- **Description:** Whether the child was bullied by others
- **Values:**
  - 1 = Never (in the past 12 months)
  - 2 = 1-2 times (in the past 12 months)
  - 3 = 1-2 times per month
  - 4 = 1-2 times per week
  - 5 = Almost every day
- **Data Type:** Categorical

---

### Neighborhood Environment Variables

**NbhdSafe_22** - Neighborhood Safety
- **Description:** How safe is the neighborhood where the child lives
- **Values:**
  - 1 = Definitely agree (safe)
  - 2 = Somewhat agree (safe)
  - 3 = Somewhat/Definitely disagree (not safe)
- **Data Type:** Categorical

**NbhdSupp_22** - Neighborhood Support
- **Description:** Whether children live in a supportive neighborhood
- **Values:**
  - 0 = No
  - 1 = Yes
- **Data Type:** Binary

---

### Data Processing Notes

1. **Missing Data Handling:** Multiple imputation was performed using the MICE (Multiple Imputation by Chained Equations) method with 5 imputations and 50 iterations.

2. **Variable Transformations:** Many categorical variables were recoded from their original format (e.g., changing 2s to 0s) to create binary representations where 1 indicates the event occurred/condition present.

3. **Age Filtering:** Analysis focused on children aged 6-17 years.

4. **Target Variable Construction:** MHealthConcern was created as a composite measure combining current anxiety (K2Q33B=1) OR current depression (K2Q32B=1).