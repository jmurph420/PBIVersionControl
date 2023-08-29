// f_DateRange
// f_DateRange
let
    Source = Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText("i45WMtY31DcyMDJSitWJVjLTNzIC8UyUYmMB", BinaryEncoding.Base64), Compression.Deflate)), let _t = ((type nullable text) meta [Serialized.Text = true]) in type table [DateRange = _t]),
    ChgType = Table.TransformColumnTypes(Source,{{"DateRange", type date}})
in
    ChgType
//-----------------------------------------------------------------------------------

// pBeginDate
// pBeginDate
let
    Source = f_DateRange,
    ChangeToDate = Table.TransformColumnTypes(Source,{{"DateRange", type date}}),
    FilterToEarliest = Table.SelectRows(ChangeToDate, let earliest = List.Min(ChangeToDate[DateRange]) in each [DateRange] = earliest),
    BeginYear = Table.AddColumn(FilterToEarliest,"BeginYear", each Date.Year([DateRange]),Int64.Type),
    BeginMonth = Table.AddColumn(BeginYear,"BeginMonth", each Date.Month([DateRange]),Int64.Type),
    BeginDay = Table.AddColumn(BeginMonth,"BeginDay", each Date.Day([DateRange]), Int64.Type)
in
    BeginDay
//-----------------------------------------------------------------------------------

// pEndDate
// pEndDate
let
    Source = #"f_DateRange",
    ChangeToDate = Table.TransformColumnTypes(Source,{{"DateRange", type date}}),
    FilterToLatest = Table.SelectRows(ChangeToDate, let latest = List.Max(ChangeToDate[DateRange]) in each [DateRange] = latest),
    BeginYear = Table.AddColumn(FilterToLatest,"BeginYear", each Date.Year([DateRange]),Int64.Type),
    BeginMonth = Table.AddColumn(BeginYear,"BeginMonth", each Date.Month([DateRange]),Int64.Type),
    BeginDay = Table.AddColumn(BeginMonth,"BeginDay", each Date.Day([DateRange]), Int64.Type)
in
    BeginDay
//-----------------------------------------------------------------------------------

// f_FiscalYearStartEnd
//-----------------------------------------------------------------------------------

// f_FiscalYearStartEnd
let
    Source = Table.FromRows(Json.Document(Binary.Decompress(Binary.FromText("i45WMjTQN1SK1YlWstQ3NlCKjQUA", BinaryEncoding.Base64), Compression.Deflate)), let _t = ((type nullable text) meta [Serialized.Text = true]) in type table [DateRange = _t])
in
    Source
//-----------------------------------------------------------------------------------

// fnSuperDateTableCreator
//-----------------------------------------------------------------------------------
// fnSuperDateTableCreator
let
// ----------------------- Documentation -----------------------
documentation_ = [
Documentation.Name =  " fnSuperDateTableCreator
", Documentation.Description = " Creates a date table according to the chosen interval between Start and End. Allowed values for 3rd parameter: ""Year"", ""Quarter"", ""Month"", ""Week"" or ""Day"" ORIGINAL fnCreateDateIntervalTable written by Imke Feldmann.
" , Documentation.LongDescription = " Creates a date table according to the chosen interval between Start and End. Allowed values for 3rd parameter: ""Year"", ""Quarter"", ""Month"", ""Week"" or ""Day"" ORIGINAL fnCreateDateIntervalTable written by Imke Feldmann.
", Documentation.Category = " Table
", Documentation.Source = " Original Source - http://www.thebiccountant.com/2017/12/11/date-datesbetween-retrieve-dates-between-2-dates-power-bi-power-query/ .
", Documentation.Author = " Imke Feldmann: www.TheBIccountant.com  / u/JM420A.
", Documentation.Examples = {[Description =  " see http://www.thebiccountant.com/2017/12/11/date-datesbetween-retrieve-dates-between-2-dates-power-bi-power-query/ .
" , Code = "
 ", Result = "
"]}],

// ----------------------- Function Code -----------------------
function_ =  
(From as date, To as date, optional Selection as text ) =>
let
// Create default-value "Day" if no selection for the 3rd parameter has been made
TimeInterval = if Selection = null then "Day" else Selection,

// Table with different values for each case
CaseFunctions = #table({"Case", "LastDateInTI", "TypeOfAddedTI", "NumberOfAddedTIs"},
{   {"Day", Date.From, Date.AddDays, Number.From(To-From)+1},
   {"Week", Date.EndOfWeek, Date.AddWeeks, Number.RoundUp((Number.From(To-From)+1)/7)},
   {"Month", Date.EndOfMonth, Date.AddMonths, (Date.Year(To)*12+Date.Month(To))-(Date.Year(From)*12+Date.Month(From))+1},
   {"Quarter", Date.EndOfQuarter, Date.AddQuarters, (Date.Year(To)*4+Date.QuarterOfYear(To))-(Date.Year(From)*4+Date.QuarterOfYear(From))+1},
   {"Year", Date.EndOfYear, Date.AddYears,Date.Year(To)-Date.Year(From)+1}
} ),

// Filter table on selected case
Case = CaseFunctions{[Case = TimeInterval]},
   
// Create list with dates: List with number of date intervals -> Add number of intervals to From-parameter -> shift dates at the end of each respective interval
DateFunction = List.Transform({0..Case[NumberOfAddedTIs]-1}, each Function.Invoke(Case[LastDateInTI], {Function.Invoke(Case[TypeOfAddedTI], {From, _})})) ,
     ConvertToTable = Table.FromList(DateFunction, Splitter.SplitByNothing(), {"DateField"}, null, ExtraValues.Error),
    DateInput = Table.TransformColumnTypes(ConvertToTable,{{"DateField", type datetime}}) ,


////////////////////////////////////////////////////////////////
////////////   UPDATE YOUR FISCAL YEAR PARAMETERS BELOW ////////
////////////////////////////////////////////////////////////////

    FYStartDay =f_FiscalYearStartEnd[DateRange]{0} & "/", // Update per your Fiscal Year Start Day
    FYEndDay = f_FiscalYearStartEnd[DateRange]{1} & "/", // Update per your Fiscal Year End Day

////////////////////////////////////////////////////////////////
////////////   UPDATE YOUR FISCAL YEAR PARAMETERS ABOVE ////////
////////////////////////////////////////////////////////////////

     DateSplitter = Table.AddColumn(DateInput,".", each    
     [
         
         
         CurrentDate = DateTime.LocalNow(),
         DaysSinceInputDate = Number.Round(Number.From(CurrentDate) - Number.From([DateField]),0),

         CurrentTime = Time.From(DateTime.LocalNow()),
         CurrentDay = Date.Day(CurrentDate),
         CurrentDayNum = Date.DayOfWeek(CurrentDate),
         CurrentDayName = Date.DayOfWeekName(CurrentDate),
         CurrentMonthNum = Date.Month(CurrentDate),
         CurrentMonthName = Date.MonthName(CurrentDate),
         CurrentQuarter = Date.QuarterOfYear(CurrentDate),
         CurrentFYDaysRemaining = if Date.Month(CurrentDate) >=10 then // if it is October or higher
              Number.From(
              Date.FromText(FYStartDay &
                        Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))  
                          - Date.StartOfYear(Date.FromText(FYStartDay &  Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1 )))))
                          +            
                        Date.EndOfYear(CurrentDate) - (CurrentDate) )
                  else  // date is before October 1st of the current year
                  Date.DayOfYear(Date.FromText(FYEndDay &
                      Text.From(Date.Year(DateTime.LocalNow()))))
                      -
                        Date.DayOfYear(CurrentDate),
        CurrentFYDay = 365 - CurrentFYDaysRemaining,
         
        CurrentFYWeeksRemaining = if Date.Month(CurrentDate) >=10 then // if it is October or higher
              Number.From(
              Date.WeekOfYear(Date.FromText(FYStartDay & Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))) )
                          - Date.WeekOfYear( Date.StartOfYear(Date.FromText(FYStartDay & Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))) )
                          + (52-Date.WeekOfYear(CurrentDate))  
              else
              Date.WeekOfYear((Date.FromText(FYEndDay &
                      Text.From(Date.Year( CurrentDate )))))
                      -
                        Date.WeekOfYear(CurrentDate),
         CurrentFYWeekNum = 52-CurrentFYWeeksRemaining,

         CurrentFYQuarter = if CurrentMonthNum >= 10 and CurrentMonthNum <=13 then 1
                else if CurrentMonthNum >= 1 and CurrentMonthNum<= 3 then 2
                else if CurrentMonthNum >= 4 and CurrentMonthNum <= 6 then 3
                else if CurrentMonthNum>= 7 and CurrentMonthNum <= 9 then 4
                else 0,

        CurrentFYQuarterStart = if MonthNum >= 10 and MonthNum <=12 then #date(Date.Year([DateField]),10,1)
              else if Date.Month(CurrentDate) >= 1 and Date.Month(CurrentDate) <= 3 then #date(Date.Year(CurrentDate),1,1)
              else if Date.Month(CurrentDate) >= 4 and Date.Month(CurrentDate) <= 6 then  #date(Date.Year(CurrentDate) ,4,1)
              else if Date.Month(CurrentDate) >= 7 and Date.Month(CurrentDate)<= 9 then #date(Date.Year(CurrentDate),7,1)
              else #date(2099,1,1),
        CurrentFYQuarterEnd = if MonthNum >= 10 and MonthNum <=12 then #date(Date.Year([DateField]),12,31)
              else if Date.Month(CurrentDate) >= 1 and Date.Month(CurrentDate) <= 3 then #date(Date.Year(CurrentDate),3,31)
              else if Date.Month(CurrentDate) >= 4 and Date.Month(CurrentDate) <= 6 then  #date(Date.Year(CurrentDate) ,6,30)
              else if Date.Month(CurrentDate) >= 7 and Date.Month(CurrentDate)<= 9 then #date(Date.Year(CurrentDate),9,30)
              else #date(2099,1,1),






        TimeField =Time.From([DateField]),
        TimeHour = Time.Hour([DateField]),
        TimeMinute = Time.Minute([DateField]),
        TimeAMPM = if Text.EndsWith(Text.From([DateField]),"AM") = true then "AM" else "PM",

        DayOfMonth=Date.Day([DateField])as number,
        TwoDigitDay = if DayOfMonth < 10 then Text.From("0" & Text.From(DayOfMonth)) else DayOfMonth,
       
        Last2Characters = Text.End( Number.ToText(DayOfMonth), 2 ),
        LastCharacter = Text.End( Number.ToText( DayOfMonth), 1 ),

       OrdinalSuffix =
          if List.Contains( { "11", "12", "13" }, Last2Characters ) then Number.ToText(DayOfMonth) & "th"
          else if LastCharacter = "1" then Number.ToText(DayOfMonth) & "st"
          else if LastCharacter = "2" then Number.ToText(DayOfMonth) & "nd"
          else if LastCharacter = "3" then Number.ToText(DayOfMonth) & "rd"
          else Number.ToText(DayOfMonth) & "th",

        DayOfWeekNum=((Date.DayOfWeek([DateField]))),
        UseDate = [DateField],

        NextWorkDay =
          if DayOfWeekNum = 5 then (Date.AddDays([DateField],3)) else
          if DayOfWeekNum = 6 then (Date.AddDays([DateField],2)) else
          Date.AddDays([DateField],1),

        PreviousWorkDay =  
          if DayOfWeekNum = 5 then (Date.AddDays([DateField],-3)) else
          if DayOfWeekNum = 6 then (Date.AddDays([DateField],-2)) else
          Date.AddDays([DateField],-1),  
        DayName=Date.DayOfWeekName(([DateField])),
        DayAbbr=Text.Start(Text.Upper(Date.DayOfWeekName(([DateField]))),3) as text,
        DaysInMonth=Date.DaysInMonth([DateField]),
        DaysRemainingInMonth = DaysInMonth - DayOfMonth,
        DayOfYear=Date.DayOfYear([DateField]),

        DaysRemainingInSelectedYear =  Date.DayOfYear(Date.EndOfYear([DateField]))-Date.DayOfYear([DateField]),
        DaysInSelectedYear = Date.DayOfYear(Date.EndOfYear([DateField])),

        DaysRemainingInCurrentYear =  Date.DayOfYear(Date.EndOfYear(DateTime.LocalNow()))-Date.DayOfYear([DateField]),
        DaysInCurrentYear = Date.DayOfYear(Date.EndOfYear(DateTime.LocalNow())),

        DaySorter=Text.From(Date.DayOfWeek([DateField])) & "-" & Date.DayOfWeekName(([DateField])) as text ,

        startOfWeek = 1 ,
        Start = List.Min({Date.StartOfMonth(Date.From(UseDate)), Date.EndOfMonth(Date.From(UseDate))}),
        End = List.Max({Date.StartOfMonth(Date.From(UseDate)), Date.EndOfMonth(Date.From(UseDate))}),
        ListOfDates = List.Dates(Start, Number.From(End - Date.From(UseDate)) + 1,#duration(1,0,0,0)),
        TotalBizDays = List.Dates(Start, Number.From(End - Start) + 1,#duration(1,0,0,0)),
        DeleteWeekends = List.Select(ListOfDates, each Date.DayOfWeek(_, startOfWeek) < 5 ),
        DeleteWeekends2 = List.Select(TotalBizDays, each Date.DayOfWeek(_, startOfWeek) < 5 ),
        BusinessDaysRemaining = (List.Count(DeleteWeekends) -1   ),
        TotalBizDaysExclHolidays  = (List.Count(DeleteWeekends2) ),
        CurrentBusinessDay = TotalBizDaysExclHolidays - BusinessDaysRemaining ,


         WeekStart=Date.StartOfWeek([DateField]) as datetime,
         WeekEnd=Date.EndOfWeek([DateField]) as datetime,
         WeekInYear=Date.WeekOfYear([DateField]),
         TwoDigitWeek = if WeekInYear < 10 then Text.From("0" & Text.From(WeekInYear)) else WeekInYear,
         WeeksRemainingInYear = 52-WeekInYear,
         WeekInMonth=Date.WeekOfMonth([DateField]),
         WeeksRemainingInMonth = Number.RoundUp(DaysInMonth/7) - WeekInMonth,

         MonthNum=Date.Month([DateField]),
         TwoDigitMonth = if MonthNum < 10 then Text.From("0" & Text.From(MonthNum)) else MonthNum,
         MonthName = Date.MonthName([DateField]),
         MonthAbbr=Text.Start(Text.Upper(Date.MonthName([DateField])),3) as text,
         MonthStart = Date.StartOfMonth([DateField]),
         MonthEnd = Date.EndOfMonth([DateField]),
      MonthSorter=Text.From(Date.Month([DateField])) & "-" & Text.Start(Text.Upper(Date.MonthName([DateField])),3) as text,

         Quarter=Date.QuarterOfYear([DateField]),
         TwoDigitQuarter = if Quarter < 10 then Text.From("0" & Text.From(Quarter)) else Quarter,
         QuarterStart = Date.StartOfQuarter([DateField]),
         QuarterEnd = Date.EndOfQuarter([DateField]),



        /////////////  Fiscal Year Section - If your FY is different than 1 October - 30 September, you can change

        FYDaysRemaining = if Date.Month([DateField]) >=10 then // if it is October or higher
              Number.From(
              Date.FromText(FYStartDay &
                        Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))  
                          - Date.StartOfYear(Date.FromText(FYStartDay &  Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1 )))))
                          +            
                        Date.EndOfYear([DateField]) - ([DateField]) )
                  else  // date is before October 1st of the current year
                  Date.DayOfYear(Date.FromText(FYEndDay &
                      Text.From(Date.Year(DateTime.LocalNow()))))
                      -
                        Date.DayOfYear([DateField]), FYDay = 365 - FYDaysRemaining,
         
        FYWeeksRemaining = if Date.Month([DateField]) >=10 then // if it is October or higher
              Number.From(
              Date.WeekOfYear(Date.FromText(FYStartDay & Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))) )
                          - Date.WeekOfYear( Date.StartOfYear(Date.FromText(FYStartDay & Text.From(Date.Year(Date.AddYears(DateTime.LocalNow(),1))))) )
                          + (52-Date.WeekOfYear([DateField]))  
              else
              Date.WeekOfYear((Date.FromText(FYEndDay &
                      Text.From(Date.Year( [DateField] )))))
                      -
                        Date.WeekOfYear([DateField]),
         FYWeekNum = 52-FYWeeksRemaining,

         FYQuarter = if MonthNum >= 10 and MonthNum <=13 then 1
                else if MonthNum >= 1 and MonthNum<= 3 then 2
                else if MonthNum >= 4 and MonthNum <= 6 then 3
                else if MonthNum>= 7 and MonthNum <= 9 then 4
                else 0,

        FYQuarterStart = if MonthNum >= 10 and MonthNum <=12 then #date(Date.Year([DateField]),10,1)
              else if Date.Month([DateField]) >= 1 and Date.Month([DateField]) <= 3 then #date(Date.Year([DateField]),1,1)
              else if Date.Month([DateField]) >= 4 and Date.Month([DateField]) <= 6 then  #date(Date.Year([DateField]) ,4,1)
              else if Date.Month([DateField]) >= 7 and Date.Month([DateField])<= 9 then #date(Date.Year([DateField]),7,1)
              else #date(2099,1,1),
        FYQuarterEnd = if MonthNum >= 10 and MonthNum <=12 then #date(Date.Year([DateField]),12,31)
              else if Date.Month([DateField]) >= 1 and Date.Month([DateField]) <= 3 then #date(Date.Year([DateField]),3,31)
              else if Date.Month([DateField]) >= 4 and Date.Month([DateField]) <= 6 then  #date(Date.Year([DateField]) ,6,30)
              else if Date.Month([DateField]) >= 7 and Date.Month([DateField])<= 9 then #date(Date.Year([DateField]),9,30)
              else #date(2099,1,1),
         
         FYYear = if Date.Month([DateField]) >9 then Date.Year([DateField])+1 else Date.Year([DateField]), // yes, FYYear is redundant, it is to stick with the column naming of the other FY columns
         FYCurrent = if Date.Month(DateTime.LocalNow()) >9 then Date.Year((DateTime.LocalNow()))+1 else Date.Year((DateTime.LocalNow())), // yes, FYYear is redundant, it is to stick with the column naming of the other FY columns
         FYSinceDate = FYCurrent - Number.From(Date.Year([DateField])),
         YearNum=Date.Year([DateField]),
         YearStart = Date.StartOfYear([DateField]),
         YearEnd = Date.EndOfYear([DateField]),
         FYStart =  Date.FromText(FYStartDay & Text.From(Date.Year([DateField]))),
         FYEnd =  Date.FromText(FYEndDay & Text.From(
           Date.Year(
             Date.AddYears([DateField],1)
             ))),
//////////// End of FYSection
        YYYYWW = Text.From(YearNum) & Text.From(TwoDigitWeek),
        YYYYMM = Text.From(YearNum) & Text.From(TwoDigitMonth),
        YYYYQQ = Text.From(YearNum) & Text.From(TwoDigitQuarter),
        YYYYMMDD = YYYYMM & Text.From(TwoDigitDay),

        DayMonthYear = Text.From(TwoDigitDay) & " " & MonthAbbr & " " & Text.End(Text.From(Date.Year([DateField])),2),
        LongDayMonthYear = Text.From(TwoDigitDay) & " " & MonthName &  " " & Text.From(YearNum),
        LongDate = DayName & ", "  & MonthName & " " & OrdinalSuffix & ", " & Text.From(Date.Year([DateField])),

        IsYTD = if Date.IsInYearToDate([DateField]) = true then "Y" else "N",
        IsQTD = if Date.Year([DateField]) = Date.Year(DateTime.LocalNow()) and Date.QuarterOfYear([DateField]) = Date.QuarterOfYear(DateTime.LocalNow()) and [DateField] <=DateTime.LocalNow() and [DateField] <= Date.EndOfQuarter(DateTime.LocalNow()) then "Y" else "N",
        IsMTD = if Date.Year([DateField]) = Date.Year(DateTime.LocalNow()) and Date.Month([DateField]) = Date.Month(DateTime.LocalNow()) and  [DateField] >= Date.StartOfMonth([DateField]) and [DateField] <= DateTime.LocalNow() then  "Y" else "N",

        1DayAgo = Date.AddDays(UseDate,-1),
        1WeekAgo = Date.AddWeeks(UseDate,-1),
        1MonthAgo = Date.AddMonths(UseDate,-1),
        1QuarterAgo = Date.AddQuarters(UseDate, -1),
        1YearAgo = Date.AddYears(UseDate,-1),


        FYQuartersSince = ((4-FYQuarter) + (4-CurrentFYQuarter) ) + ((Number.From(Date.Year(CurrentDate)))  - (Number.From(Date.Year([DateField]))+1)) * 4 ,

        PreviousDayNumber = if (DaysRemainingInCurrentYear +1) = 365 then 365 else if (DaysRemainingInCurrentYear +1)  = 366  then 365 else (DayOfYear -1),
        PreviousWeekNumber = if WeekInYear = 1 then 53 else WeekInYear -1 ,
        PreviousMonthNumber = Date.Month(Date.AddMonths(UseDate,-1)),        
        PreviousQuarterNumber =
              if Number.From(Quarter)-1 = 0  then 4
              else if Number.From(Quarter)-1 = 1 then 1
              else if Number.From(Quarter)-1 = 2  then 2
              else if Number.From(Quarter)-1 = 3  then 3
              else 0
     ]),

       ExpandFields = Table.ExpandRecordColumn(DateSplitter, ".",
       
       {"CurrentDate", "DaysSinceInputDate","CurrentTime","CurrentDay","CurrentDayNum","CurrentDayName","CurrentMonthNum","CurrentMonthName","CurrentQuarter","CurrentFYDaysRemaining","CurrentFYDay","CurrentFYWeeksRemaining","CurrentFYWeekNum","CurrentFYQuarter","CurrentFYQuarterStart","CurrentFYQuarterEnd", "TimeField", "TimeHour", "TimeMinute", "TimeAMPM", "DayOfMonth", "TwoDigitDay", "Last2Characters", "LastCharacter", "OrdinalSuffix", "DayOfWeekNum", "UseDate", "NextWorkDay", "PreviousWorkDay","DayName", "DayAbbr", "DaysInMonth", "DaysRemainingInMonth", "DayOfYear", "DaysRemainingInSelectedYear", "DaysInSelectedYear", "DaysRemainingInCurrentYear", "DaysInCurrentYear", "DaySorter", "startOfWeek", "Start", "End", "BusinessDaysRemaining", "TotalBizDaysExclHolidays", "CurrentBusinessDay", "WeekStart", "WeekEnd", "WeekInYear", "TwoDigitWeek", "WeeksRemainingInYear", "WeekInMonth", "WeeksRemainingInMonth", "MonthNum", "TwoDigitMonth", "MonthName", "MonthAbbr", "MonthStart", "MonthEnd", "MonthSorter", "Quarter", "TwoDigitQuarter", "QuarterStart", "QuarterEnd", "FYDaysRemaining", "FYDay", "FYWeeksRemaining", "FYWeekNum", "FYQuarter", "FYQuarterStart", "FYQuarterEnd", "FYYear", "FYCurrent" , "FYSinceDate","YearNum", "YearStart", "YearEnd", "FYStart", "FYEnd", "YYYYWW", "YYYYMM", "YYYYQQ", "YYYYMMDD", "DayMonthYear", "LongDayMonthYear", "LongDate", "IsYTD", "IsQTD", "IsMTD", "1DayAgo", "1WeekAgo", "1MonthAgo", "1QuarterAgo", "1YearAgo", "FYQuartersSince","PreviousDayNumber","PreviousWeekNumber","PreviousMonthNumber",  "PreviousQuarterNumber"},
       
       {"CurrentDate","DaysSinceInputDate","CurrentTime","CurrentDay","CurrentDayNum","CurrentDayName","CurrentMonthNum","CurrentMonthName","CurrentQuarter","CurrentFYDaysRemaining","CurrentFYDay","CurrentFYWeeksRemaining","CurrentFYWeekNum","CurrentFYQuarter","CurrentFYQuarterStart","CurrentFYQuarterEnd", "TimeField", "TimeHour", "TimeMinute", "TimeAMPM", "DayOfMonth", "TwoDigitDay", "Last2Characters", "LastCharacter", "OrdinalSuffix", "DayOfWeekNum", "UseDate", "NextWorkDay", "PreviousWorkDay", "DayName", "DayAbbr", "DaysInMonth", "DaysRemainingInMonth", "DayOfYear", "DaysRemainingInSelectedYear", "DaysInSelectedYear","DaysRemainingInCurrentYear", "DaysInCurrentYear", "DaySorter", "startOfWeek", "Start", "End", "BusinessDaysRemaining", "TotalBizDaysExclHolidays", "CurrentBusinessDay", "WeekStart", "WeekEnd", "WeekInYear", "TwoDigitWeek", "WeeksRemainingInYear", "WeekInMonth", "WeeksRemainingInMonth", "MonthNum", "TwoDigitMonth", "MonthName", "MonthAbbr", "MonthStart", "MonthEnd", "MonthSorter", "Quarter", "TwoDigitQuarter", "QuarterStart", "QuarterEnd", "FYDaysRemaining", "FYDay", "FYWeeksRemaining", "FYWeekNum", "FYQuarter", "FYQuarterStart", "FYQuarterEnd", "FYYear", "FYCurrent","FYSinceDate","YearNum", "YearStart", "YearEnd", "FYStart", "FYEnd", "YYYYWW", "YYYYMM", "YYYYQQ", "YYYYMMDD", "DayMonthYear", "LongDayMonthYear", "LongDate", "IsYTD", "IsQTD", "IsMTD", "1DayAgo", "1WeekAgo", "1MonthAgo", "1QuarterAgo", "1YearAgo", "FYQuartersSince","PreviousDayNumber", "PreviousWeekNumber","PreviousMonthNumber", "PreviousQuarterNumber"}),


    ChangeTypes = Table.TransformColumnTypes(ExpandFields,{{"PreviousQuarterNumber", Int64.Type}, {"PreviousMonthNumber", Int64.Type}, {"YearNum", Int64.Type}, {"FYYear", Int64.Type}, {"FYCurrent", Int64.Type}, {"FYQuarter", Int64.Type}, {"FYWeekNum", Int64.Type}, {"FYWeeksRemaining", Int64.Type}, {"FYDay", Int64.Type}, {"FYDaysRemaining", Int64.Type}, {"Quarter", Int64.Type}, {"MonthNum", Int64.Type}, {"WeeksRemainingInMonth", Int64.Type}, {"WeekInMonth", Int64.Type}, {"WeeksRemainingInYear", Int64.Type}, {"PreviousDayNumber", Int64.Type}, {"PreviousWeekNumber", Int64.Type}, {"WeekInYear", Int64.Type}, {"CurrentBusinessDay", Int64.Type}, {"TotalBizDaysExclHolidays", Int64.Type}, {"BusinessDaysRemaining", Int64.Type}, {"startOfWeek", Int64.Type}, {"DaysInCurrentYear", Int64.Type}, {"DaysRemainingInCurrentYear", Int64.Type}, {"DaysRemainingInSelectedYear", Int64.Type},{"DaysInSelectedYear", Int64.Type},{"DayOfYear", Int64.Type}, {"DaysRemainingInMonth", Int64.Type}, {"DaysInMonth", Int64.Type}, {"DayOfWeekNum", Int64.Type}, {"DayOfMonth", Int64.Type}, {"TimeMinute", Int64.Type}, {"TimeHour", Int64.Type}, {"FYSinceDate", Int64.Type}, {"CurrentDate", type datetime}, {"UseDate", type datetime}, {"NextWorkDay", type datetime}, {"PreviousWorkDay", type datetime},{"WeekStart", type datetime}, {"WeekEnd", type datetime}, {"MonthStart", type datetime}, {"MonthEnd", type datetime}, {"QuarterStart", type datetime}, {"QuarterEnd", type datetime}, {"YearStart", type datetime}, {"YearEnd", type datetime}, {"1DayAgo", type datetime}, {"1WeekAgo", type datetime}, {"1MonthAgo", type datetime}, {"1QuarterAgo", type datetime}, {"1YearAgo", type datetime}, {"IsMTD", type text}, {"IsQTD", type text}, {"IsYTD", type text}, {"LongDate", type text}, {"LongDayMonthYear", type text}, {"DayMonthYear", type text}, {"YYYYMMDD", type text}, {"YYYYQQ", type text}, {"YYYYMM", type text}, {"YYYYWW", type text}, {"MonthSorter", type text}, {"TwoDigitQuarter", type text}, {"MonthAbbr", type text}, {"MonthName", type text}, {"TwoDigitMonth", type text}, {"TwoDigitWeek", type text}, {"DaySorter", type text}, {"DayAbbr", type text}, {"DayName", type text}, {"OrdinalSuffix", type text}, {"Last2Characters", type text}, {"TwoDigitDay", type text}, {"TimeAMPM", type text}, {"TimeField", type time}, {"LastCharacter", Int64.Type}, {"Start", type date}, {"End", type date}, {"FYQuarterStart", type date}, {"FYQuarterEnd", type date}, {"FYEnd", type date}, {"FYStart", type date},{"DaysSinceInputDate", Int64.Type}, {"CurrentDay", Int64.Type}, {"CurrentDayNum", Int64.Type}, {"CurrentMonthNum", Int64.Type}, {"CurrentQuarter", Int64.Type}, {"CurrentFYDaysRemaining", Int64.Type}, {"CurrentFYDay", Int64.Type}, {"CurrentFYWeeksRemaining", Int64.Type}, {"FYQuartersSince", Int64.Type}, {"CurrentFYWeekNum", Int64.Type}, {"CurrentFYQuarter", Int64.Type}, {"CurrentFYQuarterEnd", type date}, {"CurrentFYQuarterStart", type date}, {"CurrentMonthName", type text}, {"CurrentDayName", type text}, {"CurrentTime", type time}}),

    TableSorter = Table.Sort(ChangeTypes, {"DateField"})
   
   
in
    TableSorter,
// ----------------------- New Function Type -----------------------
type_ = type function (
      From as (type date),
      To as (type date),
      optional Selection as (type text meta [
                            Documentation.FieldCaption = "Select Date Interval",
                            Documentation.FieldDescription = "Select Date Interval, if nothing selected, the default value will be ""Day""",
                            Documentation.AllowedValues = {"Day", "Week", "Month", "Quarter", "Year"}
                            ])
        )
    as table meta documentation_,

// Replace the extisting type of the function with the individually defined
Result =  Value.ReplaceType(function_, type_)
 in
Result

// d_DateTable
// d_DateTable
let
    Source = fnSuperDateTableCreator(#date(pBeginDate[BeginYear]{0}, pBeginDate[BeginMonth]{0}, pBeginDate[BeginDay]{0}), #date(pEndDate[BeginYear]{0}, pEndDate[BeginMonth]{0}, pEndDate[BeginDay]{0}), null)
in
    Source