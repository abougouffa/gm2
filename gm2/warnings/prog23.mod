(* Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE prog23;

FROM StrIO IMPORT WriteString,WriteLn;
FROM NumberIO IMPORT ReadInt,WriteInt;

CONST
    DaysInAWeek = 7 ;
    Day1Jan1800 = 3 ; (*The 1st jan 1800 was a wednesday*)

VAR
    year     : INTEGER ; (*Year input by user (- 1800)*)
    month    : INTEGER ; (*month input by user *)
    day      : INTEGER ; (*day of month input by user *)
    answer   : INTEGER ; (*day of the week*)


(*----------------------------------------------------------------------
 This routine reads in a year from the user until the year is valida
 It then subtracts 1800 and returns this numbera*)

    PROCEDURE GetYear(): INTEGER ;
    VAR
        YearInput : INTEGER ;
        year      : INTEGER ;
    BEGIN
        WriteString('What year is it ? (0 to exit)') ;
        ReadInt(YearInput) ;
        WriteLn;
        WHILE ((YearInput < 1800) OR (YearInput > 2100))
              AND (YearInput <> 0) DO
            WriteString('Bad year please try again (1800-2100) ') ;
            ReadInt(YearInput) ;
            WriteLn;
        END ;
        RETURN YearInput - 1800 ;
    END GetYear;

(*----------------------------------------------------------------------
 This routine reads in a month from the user until the month is valida*)

    PROCEDURE GetMonth(): INTEGER ;
    VAR
        MonthInput : INTEGER;

    BEGIN
       WriteString('What month is it ?');
       ReadInt(MonthInput);
       WriteLn;
       WHILE (( MonthInput < 1) OR (MonthInput > 12)) DO
             WriteString('Bad month please try again (1-12)');
             ReadInt(MonthInput)
       END;
        RETURN MonthInput ;
    END GetMonth;

(*----------------------------------------------------------------------
 This routine reads in the day of the month until it is valida*)

    PROCEDURE GetDom(NumDays : INTEGER) : INTEGER ;
    VAR
        date : INTEGER ;
    BEGIN
        WriteString('What date is it (1-Numdays)?');
        ReadInt(date);
          WHILE (date < 1) OR (date>NumDays) DO
                WriteString('Bad month re-enter');
                ReadInt(date);
          END;(*WHILE*)
        (*code needed here*)
        RETURN date ;
    END GetDom;

(*----------------------------------------------------------------------
 This PROCEDURE returns TRUE if the year is a leap year *)

    PROCEDURE IsALeap(year : INTEGER) : BOOLEAN ;
    VAR
    temp : BOOLEAN;

    BEGIN
       IF(year MOD 4 = 0) AND (year MOD 100 <> 0) OR (year = 200) THEN
          temp := TRUE
         ELSE
          temp := FALSE
       END;(*IF*)
        RETURN temp;
    END IsALeap;

(*----------------------------------------------------------------------
 This PROCEDURE returns the number of days in a given month *)

    PROCEDURE DaysInMonth(month : INTEGER; year : INTEGER) : INTEGER ;

    CONST
        SHORTMONTH = 30 ; (*Most months are 30 or 31 days*)
        LONGMONTH = 31 ;
        ODDMONTH = 28 ; (*We will deal with leap years latera*)

        JAN = 1 ;
        FEB = 2 ;
        MAR = 3 ;
        APR = 4 ;
        MAY = 5 ;
        JUN = 6 ;
        JUL = 7 ;
        AUG = 8 ;
        SEP = 9 ;
        OCT = 10 ;
        NOV = 11 ;
        DEC = 12 ;
    VAR
       NumDays : INTEGER;

    BEGIN
       IF (month =9) OR (month = 4) OR (month = 6) OR (month = 11)
       THEN
          NumDays  := SHORTMONTH
       END; (*IF*)

       IF month = 2
       THEN
          NumDays := ODDMONTH
       ELSE
          NumDays := LONGMONTH
       END;(*IF*)
       RETURN NumDays
    END DaysInMonth;


    (*This PROCEDURE calculates the number of extra days due to leap*)
    (*years up to the given yeara*)

    PROCEDURE LeapYearCount(ThisYear : INTEGER) : INTEGER ;
    VAR LeapCount : INTEGER ;

    BEGIN
       LeapCount := ((ThisYear + 3) / 4);
       LeapCount := LeapCount + ((ThisYear+99)/100);
       IF ThisYear > 200
       THEN
          LeapCount := LeapCount + 1;
       END;(*IF*)
        (*code needed here*)
       RETURN LeapCount ;
    END LeapYearCount;

   (*This PROCEDURE calculates the number of days since Jan 1st 1800 to the*)
   (*first day of the given month in the given yeara*)

   PROCEDURE YearMonthDay(year:INTEGER;
                          month:INTEGER;
                          day : INTEGER) : LONGINT ;
   CONST
      DaysInYear = 365 ; (*We will deal with leap years latera*)
   VAR
      Daycount : LONGINT; (*The number of days counted so far*)
      MonthCount : INTEGER ; (*A counter to count up to this month*)

   BEGIN
      (*First count the days in previous years*)
      Daycount := VAL(LONGINT,year) * DaysInYear ;
      Daycount := Daycount + VAL(LONGINT,LeapYearCount(year)) ;
      MonthCount := 1;
      WHILE MonthCount < month DO
         Daycount := Daycount + (DaysInMonth(MonthCount, year));
      END;(*WHILE*)

(* was and compiler finds this error *)
(*    Daycount := Daycount + DaysInMonth - 1; *)
(*                           ^^^^^^^^^^^ *)

      (*Now count the days in previous months*)
      (*code needed here*)

      (*Add in number of days this month before day*)
      (*code needed here*)

      RETURN Daycount ;
   END YearMonthDay;


   PROCEDURE GetInput;
   BEGIN
(*Set the year to a valid input year since 1800*)
      year := GetYear() ;
      IF year > 0
      THEN
         (*Next set the month to a valid month*)
         month := GetMonth() ;
         (*Set the day of month to a valid day*)
         day := GetDom(DaysInMonth(month, year)) ;
      END;(*if*)
   END GetInput;

(*----------------------------------------------------------------------
This is the main program*)
BEGIN
   GetInput;
   WHILE year > 0 DO

      (*Now calculate the day as the remaninder when divided by DaysInAWeek
       of the total number of days + Day1Jan1800 *)
      WriteLn;
      WriteString('The day of the week of');
      answer := (VAL(INTEGER,(YearMonthDay(year, month, day)) + Day1Jan1800))
                MOD DaysInAWeek ;
      CASE answer OF
      1: WriteString('The day is Sunday')|
      2: WriteString('The day is Monday')|
      3: WriteString('The day is Tuesday')|
      4: WriteString('The day is Wednesday')|
      5: WriteString('The day is Thursday')|
      6: WriteString('The day is Friday')|
      7: WriteString('The day is Saturday');
      END;

      GetInput;
   END;(*WHILE*)
   (*code needed here*)
END prog23. (*Main prog*)
