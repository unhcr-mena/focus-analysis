# focus-analysis

## See also 

https://github.com/unhcr/axis/wiki/GFI-Algorithms

## Achivement

Achievement attempts to capture the question, "Are you doing what set out to do?"

## Single Indicator

### Outputs
* Number from 0 to 1
* MISSING

### Inputs
Either one impact or performance indicator.

### Algorithm

**Legend**

`reported_value`: MYR or YER

`goal_value`: Standard or Target

**Pseudo-code**

```
function achievement(indicator):
  achievement = NULL
  
  if (not reported_value or not goal_value or not ol_budget)
    achievement = 'MISSING'
    return

  if (indicator is performance)
    achievement = minimum(reported_value / goal_value, 1)
    return
  else (indicator is impact)
    if (reported_value is equal to goal_value)
      achievement = 1
      return
    
    if (indicator is not reversed)
      if (reported_value < goal_value)
        if (baseline >= reported_value)
          achievement = 0
          return
        else (baseline < reported_value)
          achievement = (reported_value - baseline) / (goal_value - baseline)
          return
      else (reported_value >= goal_value)
        achievement = 1
        return
    else (indicator is reversed)
      if (reported_value > goal_value)
        if (baseline <= reported_value)
          achievement = 0
          return
        else (baseline > reported_value)
          achievement = (baseline - reported_value) / (baseline - goal_value)
          return
      else (reported_value <= goal_value)
        achievement = 1
        return
```

    Impact Criticality - Computes the values of the indicators based on the criticality algorithm. The algorithm uses impact indicators and computes values based on the standard. -  Acceptable, Not Acceptable and Critical (respectively).
    Impact Achievement - Computes the values of the indicators based on the achievement algorithm. The algorithm uses impact indicators and computes values based on the impact targets.Non-reported, Below Target, Approaching Target, Met Target
    Performance Achievement - Computes the values of the indicators based on the achievement algorithm. The algorithm uses performance indicators and computes values based on the impact targets. Non-reported, Below Target, Approaching Target, Met Target



    standard - Measures the achievement of indicators in relation to the standard
    comp target - Measures the achievement of indicators in relation to the Comprehensive Target
    imp target - Measures the achievement of indicators in relation to the Impact Target

Impact Criticality



    Baseline to MYR - Will sort the indicators based on the progress made from the Baseline value to the Mid Year value
    Baseline to YER - Will sort the indicators based on the progress made from the Baseline value to the Year End value
    MYR to YER - Will sort the indicators based on the progress made from Mid Year value to the Year End value.
    # of inconsistencies - Will sort the indicators based on the number of inconsistencies that are present in the indicator



# Focus Analysis

## Reference 

OperationGroupID -- for instance:  “I1LJ”
OperationGroupname -- for instance:  “Africa”
OperationsubGroupID -- for instance:  “I1L1”
OperationsubGroupname -- for instance:  “Central Africa and the Great Lakes”
OperationHeaderID -- for instance:  “7XS”
operationID -- for instance: “7TI”
operationname -- for instance:  “Burundi”
PlanHeaderID -- for instance:  “d6ee0f9f-cf58-d6ce-e040-a8c06d342905”
planID -- for instance:  “9e17a6d1-9eeb-401f-b5fd-78b07689e4ca”
name -- for instance:  “Detailed Budget Target Plan”
planningPeriod -- for instance:  “2010”
type -- for instance:  “DETAILED”

## Budget 

PlanID -- for instance:  “001aa07f-9726-670e-e053-6d34a8c093c2”
operation -- for instance:  “Jordan“
operationID -- for instance:  “7VL”
year -- for instance:  “2015“
plantype -- for instance:  “ExCom Revised”
PPGID -- for instance:  “001aa07f-97be-670e-e053-6d34a8c093c2”
POPGRPID -- for instance:  “b6564625-525e-4d1e-9dc7-d666bc8cb7db“
PPGname -- for instance:  “Refugees and asylum seekers in Jordan“
PPGtypeID -- for instance:  “LYPK“
PPGtypeName -- for instance:  “Refugee“
GoalID -- for instance:  “001aa07f-99cf-670e-e053-6d34a8c093c2“
GoalRFID -- for instance:  “EM“
Goalname -- for instance:  “Protection and mixed solutions“
RightsGroupID -- for instance:  “001aa07f-9f06-670e-e053-6d34a8c093c2“
RightsGroupRFID -- for instance:  “126240dd-276d-482f-aef0-dc62acd94011“
RightsGroupname -- for instance:  “Basic Needs and Essential Services“
ProblemObjectiveID -- for instance:  “001aa07f-ab00-670e-e053-6d34a8c093c2“
ProblemObjectiveRFID -- for instance:  “d13a589a-fdb1-4913-90c5-d67fb3d21fd1“
problemName -- for instance:  “Health status of the population is unsatisfactory or needs constant attention“
objectiveName -- for instance:  “Health status of the population improved“
OutputID -- for instance:  “001aa07f-dd51-670e-e053-6d34a8c093c2“
---
OutputRFID -- for instance:  “e3191653-46ca-4887-a2d7-bf90321ac6e2“
Outputname -- for instance:  “Access to primary health care services provided or supported“
Outputpriority -- for instance:  “PARTIAL“
---
BudgetLineID -- for instance:  “001aa081-e470-670e-e053-6d34a8c093c2“
scenario -- for instance:  “Above Operating Level“
type -- for instance:  “PARTNER“
costCenter -- for instance:  “32061“
implementerCode -- for instance:  “0000001“
implementerName -- for instance:  “ALL IMPLEMENTING PARTNERS“
accountCode -- for instance:  “699997“
accountName -- for instance:  “NBS RESULT STREAM OPS BUDGET“
quantity -- for instance:  “1“
currency -- for instance:  “USD“
unitCost -- for instance:  “300000“
localCost -- for instance:  “300000“
amount -- for instance:  “300000“
comment


# Indicator

PlanID -- for instance:  “001aa07f-9726-670e-e053-6d34a8c093c2“
operation -- for instance:  “Jordan“
operationID -- for instance:  “7VL“
year -- for instance:  “2015“
plantype -- for instance:  “ExCom Revised“
PPGID -- for instance:  “001aa07f-97be-670e-e053-6d34a8c093c2“
POPGRPID -- for instance:  “b6564625-525e-4d1e-9dc7-d666bc8cb7db“
PPGname -- for instance:  “Refugees and asylum seekers in Jordan“
PPGtypeID -- for instance:  “LYPK“
PPGtypeName -- for instance:  “Refugee“
GoalID -- for instance:  “001aa07f-99cf-670e-e053-6d34a8c093c2“
GoalRFID -- for instance:  “EM“
Goalname -- for instance:  “Protection and mixed solutions“
RightsGroupID -- for instance:  “001aa07f-9f06-670e-e053-6d34a8c093c2“
RightsGroupRFID -- for instance:  “126240dd-276d-482f-aef0-dc62acd94011“
RightsGroupname -- for instance:  “Basic Needs and Essential Services“
ProblemObjectiveID -- for instance:  “001aa07f-ab00-670e-e053-6d34a8c093c2“
ProblemObjectiveRFID -- for instance:  “d13a589a-fdb1-4913-90c5-d67fb3d21fd1“
problemName -- for instance:  “Health status of the population is unsatisfactory or needs constant attention“
objectiveName -- for instance:  “Health status of the population improved“
OutputID -- for instance:  “001aa07f-dd51-670e-e053-6d34a8c093c2“
---
IndicatorID -- for instance:  “cc8b2fed-fa2e-453d-a27f-561b0bceb5a3“
IndicatorRFID -- for instance:  “68fbacd0-82c9-4958-b94a-f111661ef881“
IndicatorisPerformance -- for instance:  “true“
IndicatorisGSP -- for instance:  “false“
Indicatorname -- for instance:  “# of health facilities equipped/constructed/rehabilitated“
Indicatorstandard -- for instance:  “0“
Indicatorreversal -- for instance:  “false“
IndicatorimpTarget -- for instance:  “0“
IndicatorcompTarget -- for instance:  “16“
IndicatormidYearValue -- for instance:  “47“
IndicatoryearEndValue -- for instance:  “47“



## Human Ressources


PlanID -- for instance:  “001aa07f-9726-670e-e053-6d34a8c093c2“
operation -- for instance:  “Jordan“
operationID -- for instance:  “7VL“
year -- for instance:  “2015“
plantype -- for instance:  “ExCom Revised“
--
officeID -- for instance:  “001aa080-e7b9-670e-e053-6d34a8c093c2“
officename -- for instance:  “Amman“
officecostCenter -- for instance:  “32061“
officecostParent -- for instance the branch office cost center – if not null
headPositionID -- for instance:  “001aa081-0ee4-670e-e053-6d34a8c093c2“
positionIDParent -- for instance the position ID of the supervisor if not NULL
positionID -- for instance:  “10020461“
positiontitle -- for instance:  “Head of Field Office P4“
positionincumbent -- for instance:  “CASTEL, Bernadette Raymonde“
positiontype -- for instance:  “Standard“
positiongrade -- for instance:  “P4“
positionepmJobCode -- for instance:  “JORP4“
positionhrJobCode -- for instance:  “000271“
positionfastTrack -- for instance:  “N“
positiontimedCost -- for instance:  “134164.26“

