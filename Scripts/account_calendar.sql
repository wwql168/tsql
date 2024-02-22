

IF OBJECT_ID('DM.AccountingCalendar') IS NOT NULL
BEGIN
	
	IF NOT EXISTS (SELECT TOP 1 1 FROM DM.AccountingCalendar)
	BEGIN

		PRINT 'ADDING INITIAL RECORD SET to DM.AccountingCalendar'	

		TRUNCATE TABLE DM.AccountingCalendar	
 
		DECLARE 
				  @JdeJulian				VARCHAR(80),
				  @FiscalCalENDarStart		DATE			= '2020/02/01',
				  @ENDOfCalENDar			DATE			= '2050/12/31',
				  @CurrentDate				DATE,
				  @RunningDaySeed			INT				= 1,
				  @RunningPeriodSeed		INT				= 1,
				  @WorkWeekSeed				INT,
				  @RunningWeekSeed			INT				= 1,
				  @FiscalYearSeed			INT				= 2020,
				  @WorkQuarterSeed			INT,
				  @WeekOfMonth				INT,
				  @WorkPeriodSeed			INT,
				  @IsLeapYear				BIT,
				  @RetailCountWeek			INT      
 
		/*  These are iteration variables, do not mess with these  */
		SELECT  @WorkPeriodSeed			= 1,
				@WorkWeekSeed			= 1,
				@WeekOfMonth			= 1,
				@IsLeapYear				= 0,
				@WorkQuarterSeed		= 1

		IF OBJECT_ID('tempdb.dbo.#FiscalTimePeriods', 'U') IS NOT NULL DROP TABLE #FiscalTimePeriods; 

		CREATE TABLE #FiscalTimePeriods 
		(
				  DateKey					INT,
				  CalendarDate				DATE,
				  FiscalWeekNo				INT,
				  FiscalYear 				INT,
				  FiscalYearLabel 			VARCHAR(80),
				  FiscalQuarter 			INT,
				  FiscalYearQuarter 		INT,
				  FiscalQuarterLabel		VARCHAR(80),
				  FiscalMonth 				INT,
				  FiscalMonthLabel 			VARCHAR(80),
				  FiscalPeriod 				INT,
				  FiscalPeriodLabel 		VARCHAR(80),
				  RetailCountWeek			INT,
				  FiscalWeekNoOfMonth		INT,
				  FiscalWeekLabel 			VARCHAR(80),
				  DayofWeekStartsSun		INT, 
				  WeekDayName				VARCHAR(80),
				  FiscalCalendarWeekName	VARCHAR(80),
				  CalendarDateName			VARCHAR(80),
				  CalendarYear 				INT,
				  CalendarQuarter 			INT,
				  CalendarQuarterLabel 		VARCHAR(80),
				  CalendarMonth 			INT,
				  CalendarMonthLabel		VARCHAR(80),
				  WeekStartingDate			DATE,
				  WeekEndingDate			DATE
		);   
 
 
		/*  The loop is iterated once for each day  */
		SET	   @CurrentDate = @FiscalCalENDarStart
 
		WHILE  @CurrentDate <= @ENDOfCalENDar
		BEGIN
 
			   /*  Each day we need to calculate that day's JDE date AND SET the fiscal week ENDing  */
			   SELECT @JdeJulian = CONVERT(VARCHAR(3), year (@CurrentDate)- 1900) + RIGHT('000' + CONVERT(VARCHAR(3), DATEPART(DY, @CurrentDate)), 3)
            
			   INSERT INTO #FiscalTimePeriods
			   SELECT  
					DateKey					= CAST(CONVERT(VARCHAR(8), @CurrentDate, 112) AS INT)
				   ,CalENDarDate			= @CurrentDate
				   ,FiscalWeekNo			= @RunningWeekSeed
				   ,FiscalYear				= @FiscalYearSeed
				   ,FiscalYearLabel			= 'FY ' + CONVERT(VARCHAR(4), @FiscalYearSeed) 
				   ,WorkQuarter				= @WorkQuarterSeed
				   ,FiscalQuarter			= CONVERT(INT, CONVERT(VARCHAR(4), @FiscalYearSeed) + CONVERT(VARCHAR(2), @WorkQuarterSeed))
				   ,FiscalQuarterLabel		= 'FY ' + CONVERT(VARCHAR(4), @FiscalYearSeed) + ' Q' + CONVERT(VARCHAR(2), @WorkQuarterSeed)
				   ,FiscalMonth				= @WorkPeriodSeed
				   ,FiscalMonthLabel		= DATENAME(MM, CONVERT(VARCHAR(4), @FiscalYearSeed) + '/' + CONVERT(VARCHAR(2), CASE @WorkPeriodSeed WHEN 11 THEN 12 ELSE (@WorkPeriodSeed+1)%12 END) + '/1') 
				   ,FiscalPeriod			= CONVERT(INT, CONVERT(VARCHAR(4), @FiscalYearSeed) + CONVERT(VARCHAR(2), @WorkQuarterSeed) + RIGHT('0' + CONVERT(VARCHAR(2), @WorkPeriodSeed), 2))
				   ,FiscalPeriodLabel		= 'FY ' + CONVERT(VARCHAR(4), @FiscalYearSeed) + ' M ' + RIGHT('0' + CONVERT(VARCHAR(2), @WorkPeriodSeed), 2) 
				   ,RetailCountWeek			= CASE WHEN (@CurrentDate BETWEEN DATEADD(D, 1- DATEPART(DW, DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0)), DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0)) 
																	 AND  DATEADD(D, 7 - DATEPART(DW, DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0)), DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0))) 
															AND Year(@CurrentDate) = YEAR(DATEADD(D, 1- DATEPART(DW, DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0)), DATEADD(yy, DATEDIFF(yy, 0, DATEADD(yy,1,@CurrentDate)), 0))) THEN 1
												   ELSE ((datediff(dd, DATEADD(yy, DATEDIFF(yy, 0, @CurrentDate), 0),@CurrentDate) -1 + DATEPART(DW,DATEADD(yy, DATEDIFF(yy, 0, @CurrentDate), 0)))/7)+1  END
				   ,WeekOfMonth				= @WeekOfMonth
				   ,FiscalWeekLabel			= 'FY ' + CONVERT(VARCHAR(4), @FiscalYearSeed) + ' M ' + RIGHT('0' + CONVERT(VARCHAR(2), @WorkPeriodSeed), 2) + ' WK ' + CONVERT(CHAR(1), @WeekOfMonth)
				   ,DayofWeek_StartsSun		= DATEDIFF(DAY,'17530107',@CurrentDate)%7 + 1
				   ,WeekDayName				= DATENAME(weekday,@CurrentDate)
				   ,FiscalCalENDarWeekName	= DATENAME(MM, CONVERT(VARCHAR(4), @FiscalYearSeed) + '/' + CONVERT(VARCHAR(2), CASE @WorkPeriodSeed WHEN 11 THEN 12 ELSE (@WorkPeriodSeed+1)%12 END) + '/1') + ' Week ' + CAST(@WeekOfMonth AS VARCHAR(80)) 
				   ,CalENDarDateName		= DATENAME(MM, @CurrentDate) +' ' + CAST(DAY(@CurrentDate) AS VARCHAR(80)) 
				   ,CalENDarYear			= YEAR(@CurrentDate) 
				   ,CalENDarQuarter			= DATEPART(QUARTER, @CurrentDate) 
				   ,CalENDarQuarterLabel	= CASE DATEPART(QUARTER, @CurrentDate) WHEN 1 THEN '1st' WHEN 2 THEN '2nd' WHEN 3 THEN '3rd' WHEN 4 THEN '4th' END + ' Quarter ' + CONVERT(VARCHAR(4), year (@CurrentDate))
				   ,CalENDarMonth			= MONTH(@CurrentDate) 
				   ,CalENDarMonthLabel		= DATENAME(MM, @CurrentDate)
				   ,WeekStarting			= DATEADD(D, 1- DATEPART(DW, @CurrentDate), @CurrentDate)
				   ,WeekENDing				= DATEADD(D, 7 - DATEPART(DW, @CurrentDate), @CurrentDate) 


			   /*  Iterate the date AND increment the RunningDay  */
			   SET @CurrentDate = DATEADD (D, 1, @CurrentDate)
			   SELECT @RunningDaySeed = @RunningDaySeed + 1
 
			   /*  Checks leap year: if the previous year is a leap year, THEN there will be 53 weeks on that year */      
			   IF  ((@FiscalYearSeed-1)%4 = 0 AND (@FiscalYearSeed-1)%100 <> 0) OR (@FiscalYearSeed-1)%400 = 0 
					 SET @IsLeapYear = 1

			   /*  Every Sunday (start of new fiscal week), increment fiscal counters  */
			   if DATEPART(DW, @CurrentDate) = 1
					 BEGIN
					 /*  These months have 5 weeks in the 4-5-4 calENDar  */
					 IF @WorkPeriodSeed in (2, 5, 8, 11)
						   BEGIN
								 /*  Iterate the RunningWeek AND WeekOfMonth (roll WeekOfMonth if necessary)  */
								 SELECT @RunningWeekSeed	= @RunningWeekSeed + 1,
									  @WeekOfMonth			= CASE @WeekOfMonth WHEN 5 THEN 1 ELSE @WeekOfMonth + 1 END,
									  @WorkWeekSeed			= @WorkWeekSeed + 1
                        
								 /*  First week of the month we need to update the WorkPeriod AND RunninfPeriod  */
								 IF @WeekOfMonth = 1
									   SELECT @WorkPeriodSeed		= @WorkPeriodSeed + 1
											, @RunningPeriodSeed	= @RunningPeriodSeed + 1
						   END
					 ELSE
						   BEGIN                
						   /*
								December in the year after a leap years get 5 weeks also, so 3rd quarter is 4-5-5
								Change @WorkPeriodSeed to the month you want to add the extra week into
						  */
						   IF @IsLeapYear = 1 AND @WorkPeriodSeed = 12
								 BEGIN
								 /*  Iterate the RunningWeek AND WeekOfMonth (roll WeekOfMonth if necessary)  */
								 SELECT @RunningWeekSeed	= @RunningWeekSeed + 1,
											@WeekOfMonth	= CASE @WeekOfMonth WHEN 5 THEN 1 ELSE @WeekOfMonth + 1 END,
											@WorkWeekSeed	= @WorkWeekSeed + 1
                                
									   IF @WeekOfMonth = 1
											 SELECT @WorkPeriodSeed		= @WorkPeriodSeed + 1
												  , @RunningPeriodSeed	= @RunningPeriodSeed + 1                              
								 END
						   ELSE
								 BEGIN
								 /*  Iterate the RunningWeek AND WeekOfMonth (roll WeekOfMonth if necessary)  */
								 SELECT @RunningWeekSeed	= @RunningWeekSeed + 1,
											@WeekOfMonth	= CASE @WeekOfMonth WHEN 4 THEN 1 ELSE @WeekOfMonth + 1 END,
											@WorkWeekSeed	= @WorkWeekSeed + 1
                              
									   /*  First week of the month we need to update the WorkPeriod AND RunninfPeriod  */
									   IF @WeekOfMonth = 1
											 SELECT @WorkPeriodSeed		= @WorkPeriodSeed + 1
												  , @RunningPeriodSeed	= @RunningPeriodSeed + 1
								 END                                  
						   END
                  
					 /*  These months are the first of each quarter (Jan handled below), so we need to roll the WorkQuarter  */
					 IF @WeekOfMonth = 1 AND @WorkPeriodSeed in (4, 7, 10)
								 SET @WorkQuarterSeed = @WorkQuarterSeed + 1
 
					 /*
						  Check to see if the current week is the start of a new fiscal year.
						  If we've started a new year, we need to reset some iteration variables.
					*/
					 IF @IsLeapYear = 1
						   BEGIN
						   /*The fiscal year following a leap year actually starts in the second week of the year.*/
						   IF @RunningWeekSeed = 54 AND @RunningDaySeed%7 = 1
								 SELECT @FiscalYearSeed  = @FiscalYearSeed + 1
									  , @WorkPeriodSeed  = 1
									  , @WorkWeekSeed	 = 1
									  , @IsLeapYear		 = 0
									  , @WorkQuarterSeed = 1
									  , @RunningWeekSeed = 1
						   END
					 ELSE
						   BEGIN
						   IF @RunningWeekSeed = 53 AND @RunningDaySeed%7 = 1
								 SELECT @FiscalYearSeed  = @FiscalYearSeed + 1
									  , @WorkPeriodSeed  = 1
									  , @WorkWeekSeed	 = 1
									  , @IsLeapYear		 = 0
									  , @WorkQuarterSeed = 1
									  , @RunningWeekSeed = 1
						   END
					 END
		END

		INSERT INTO	DM.AccountingCalendar(DateKey
										,DateValue
										,FiscalCountWeek
										,FiscalCountDay
										,FiscalYear 
										,FiscalYearLabel 
										,FiscalQuarter 
										,FiscalYearQuarter 
										,FiscalQuarterLabel
										,FiscalMonth 
										,FiscalMonthCountDay
										,FiscalMonthLabel 
										,FiscalPeriod 
										,FiscalPeriodLabel 
										,RetailCountWeek
										,FiscalWeekNoOfMonth
										,FiscalWeekLabel 
										,DayofWeekStartsSun
										,WeekDayName
										,FiscalCalendarWeekName
										,CalendarDateName
										,CalendarYear 
										,CalendarQuarter 
										,CalendarQuarterLabel 
										,CalendarMonth 
										,CalendarMonthLabel
										,WeekStartingDate
										,WeekEndingDate)
		SELECT DateKey
				,CalendarDate
				,FiscalWeekNo
				,ROW_NUMBER() OVER (PARTITION BY FiscalYear ORDER BY DateKey) 
				,FiscalYear 
				,FiscalYearLabel 
				,FiscalQuarter 
				,FiscalYearQuarter 
				,FiscalQuarterLabel
				,FiscalMonth 
				,ROW_NUMBER() OVER (PARTITION BY FiscalYear, FiscalMonth ORDER BY DateKey) 
				,FiscalMonthLabel 
				,FiscalPeriod 
				,FiscalPeriodLabel 
				,RetailCountWeek
				,FiscalWeekNoOfMonth
				,FiscalWeekLabel 
				,DayofWeekStartsSun
				,WeekDayName
				,FiscalCalendarWeekName
				,CalendarDateName
				,CalendarYear 
				,CalendarQuarter 
				,CalendarQuarterLabel 
				,CalendarMonth 
				,CalendarMonthLabel
				,WeekStartingDate
				,WeekEndingDate
		FROM #FiscalTimePeriods 

	END
END
