# Libraries and prep parameters------------------
#Sample language to install packages
#install.packages("devtools")

#Packages you may need
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(RODBC)
library(odbc)
library(xts)

library(reshape2)
#Setting working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#Database Connection
con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "BALT-SQL-FC", 
                 Database = "CitiStat", 
                 Trusted_Connection = "True")

#Sample Import
######Crime Data data import------
####pulling crime data from SQL

Sr_created<- dbGetQuery(con,'SELECT \'\' as \'SALESFORCE_CREATED\',
  		q1.[SR Type],
  		q1.[Created Week], 
  		Count(distinct [Service Request Number]) as [Created Count]
  FROM ( 
		SELECT	[Service Request Number]
				,[SR Type]
				,[Created Date]
				, CASE 
  					WHEN [Created Date] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
  					WHEN [Created Date] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
  					WHEN [Created Date] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
  					WHEN [Created Date] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\'
  					END [Created Week]
		FROM [CitiStat].[dbo].[CSR]
		WHERE	[SR Type] in(\'TRM-Grass Mowing\',\'SW-SIU Clean Up\',\'SW-Dirty Alley\',\'SW-Dirty Street\',\'SW-Dirty Alley Proactive\',\'SW-Dirty Street Proactive\',\'RP-Park Maintenance\',\'RP-Grass Cutting\')  
				AND 
				([SR Status] NOT LIKE \'%DUP%\' AND [SR Status] NOT LIKE \'%TRANS%\')
		) q1
  WHERE q1.[Created Week] is not null 
  GROUP BY q1.[SR Type], q1.[Created Week]
  ORDER BY q1.[SR Type], q1.[Created Week]
  ;
 
')

Sr_closed<- dbGetQuery(con,'
with bqClosed AS(
  SELECT 
        [Service Request Number]
        ,[SR Type]
        ,CASE 
            WHEN [Close Date] > [Due Date] THEN \'Missed\'
            WHEN  [Close Date] <= [Due date] THEN \'Met\'
            END [Closure Status_calc]
        ,CASE 
            WHEN [Close Date] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
            WHEN [Close Date] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
            WHEN [Close Date] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
            WHEN [Close Date] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\'
            END [Closed Week]
  FROM [CitiStat].[dbo].[CSR]
  WHERE	[SR Type] in(\'TRM-Grass Mowing\',\'SW-SIU Clean Up\',\'SW-Dirty Alley\',\'SW-Dirty Street\',\'SW-Dirty Alley Proactive\',\'SW-Dirty Street Proactive\',\'RP-Park Maintenance\',\'RP-Grass Cutting\')  
    AND 
    [Close Date] >= \'2022-05-29\'
    AND
    ([SR Status] NOT LIKE \'%DUP%\' AND [SR Status] NOT LIKE \'%TRANS%\'))

,

count_closed AS(
  SELECT 
      [SR Type]
      ,[Closed Week]
      ,Count(distinct [Service Request Number]) as [Total Closed Count]
  FROM bqClosed
  WHERE [Closed Week] is not null
  GROUP BY [SR Type], [Closed Week])

,

Closed_by_status AS(
  SELECT
      [SR Type]
      ,[Closed Week]
      ,[Closure Status_Calc]
      ,Count([Closure Status_calc]) as closure_count
  FROM bqClosed
  WHERE [Closed Week] is not null
  GROUP BY [SR Type],[Closed week],[Closure status_calc]) 

select * from count_closed order by [sr type],[closed week]
;
')
Sr_daclosed<- dbGetQuery(con,'
  with bqClosed AS(
      SELECT 
          [Service Request Number]
          ,[SR Type]
          ,datediff(day,[created date],[close date]) as days_to_close
          ,[created date]
          ,[close date]
          ,CASE 
              WHEN [Close Date] > [Due Date] THEN \'Missed\'
              WHEN  [Close Date] <= [Due date] THEN \'Met\'
              END [Closure Status_calc]
          ,CASE 
              WHEN [Close Date] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
              WHEN [Close Date] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
              WHEN [Close Date] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
              WHEN [Close Date] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\'
              END [Closed Week]
      FROM [CitiStat].[dbo].[CSR]
  #/*Proactive work is not included because it is created/entered by DPW as it is completed*/
      WHERE	[SR Type] in(\'TRM-Grass Mowing\', \'SW-SIU Clean Up\',\'SW-Dirty Alley\',\'SW-Dirty Street\',\'RP-Park Maintenance\',\'RP-Grass Cutting\')  
          AND 
          [Close Date] >= \'2022-05-29\'
          AND
          ([SR Status] NOT LIKE \'%DUP%\' AND [SR Status] NOT LIKE \'%TRANS%\'))
    
,
avg_closed AS(
  SELECT	[SR Type]			
      ,[Closed Week]
      ,avg(cast([Days_to_Close] as decimal(10,2))) as avg_days_to_close
  FROM bqClosed 
  WHERE [Closed week] is not null
  GROUP BY [SR Type],[Closed Week])

,

count_closed AS(
  SELECT 
    [SR Type]
    ,[Closed Week]
    ,Count(distinct [Service Request Number]) as [Total Closed Count]
  FROM bqClosed
  WHERE [Closed Week] is not null
  GROUP BY [SR Type], [Closed Week])

select \'\' AS \'SR TIME TO CLOSE\'
    ,bq.[SR Type]
    ,bq.[Closed Week]
    ,min(ac.[avg_days_to_close]) as avg_days_to_close
    ,min(cc.[total closed count]) as total_count_closed
    ,sum(bq.days_to_close) as days_to_close
from bqclosed bq
left join count_closed cc
  on bq.[sr type] = cc.[sr type]
    and bq.[closed week]=cc.[closed week]
left join avg_closed ac
  on bq.[sr type] = ac.[SR Type]
    and bq.[Closed Week]=ac.[Closed Week]
where bq.[Closed Week] is not null
group by bq.[sr type], bq.[closed week]
order by [sr type],[closed week]

;
')

Sr_ontimep<- dbGetQuery(con,'
  with bqClosed AS(
		SELECT 
				[Service Request Number]
				,[SR Type]
				,datediff(day,[created date],[close date]) as days_to_close
				,[created date]
				,[close date]
				,CASE 
						WHEN [Close Date] > [Due Date] THEN \'Missed\'
						WHEN  [Close Date] <= [Due date] THEN \'Met\'
						END [Closure Status_calc]
				,CASE 
						WHEN [Close Date] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
						WHEN [Close Date] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
						WHEN [Close Date] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
						WHEN [Close Date] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\'
						END [Closed Week]
		FROM [CitiStat].[dbo].[CSR]
#/*Proactive work is not included because it is created/entered by DPW as it is completed*/
		WHERE	[SR Type] in(\'TRM-Grass Mowing\',\'SW-SIU Clean Up\',\'SW-Dirty Alley\',\'SW-Dirty Street\',\'RP-Park Maintenance\',\'RP-Grass Cutting\')  
				AND 
				[Close Date] >= \'2022-05-29\'
				AND
				([SR Status] NOT LIKE \'%DUP%\' AND [SR Status] NOT LIKE \'%TRANS%\'))

  ,
  count_closed AS(
		SELECT 
				[SR Type]
				,[Closed Week]
				,Count(distinct [Service Request Number]) as [Total Closed Count]
		FROM bqClosed
		WHERE [Closed Week] is not null
		GROUP BY [SR Type], [Closed Week])




  select \'\' AS \'SR ON TIME PERCENTAGE\'
  		,bq.[SR Type]
  		,bq.[Closed Week]
  		,bq.[Closure Status_calc]
  		,count(bq.[Closure Status_calc]) as count_by_closurestatus
  		,min(cc.[total closed count]) as total_count_closed
  from bqclosed bq
  left join count_closed cc
  	on bq.[sr type] = cc.[sr type]
  		and bq.[closed week]=cc.[closed week]
  where bq.[Closed Week] is not null
  group by bq.[sr type], bq.[closed week],bq.[Closure Status_calc]
  order by bq.[closure status_calc],[sr type],[closed week]
   ;
')

Sr_opensr<- dbGetQuery(con,'
   SELECT \'\' as \'SALESFORCE_OPEN_SRs\', 
  		q1.[WeekEnding], 
  		q1.[SR Type], q1.[calc_Status], 
  		count(distinct [Service Request Number]) as [SR_Status_Count] 
  		FROM (
  				SELECT 
  						[Service Request Number]
  						,[SR Type]
  						,[SR Status]
  						,[Created Date]
  						,[Close Date]
  						,[Status Date]
  						,[Due Date]
  /*Create Ontime/Overdue Classification for all SRs open as of end of reporting week. Manually replace dates with last day of reporting week   */		
  					, CASE 
  						WHEN [Due Date] <= \'2022-06-05\' and [Close Date] > \'2022-06-05\' THEN \'Overdue\'
  						WHEN [Due Date] <= \'2022-06-05\' and [Close Date] is null THEN \'Overdue\'
  						WHEN [Created Date] <= \'2022-06-05\' and [Close Date] > \'2022-06-05\' THEN \'OnTime\'
  						WHEN [Created Date] <= \'2022-06-05\' and [Close Date] is null THEN \'OnTime\'		
  						END [calc_Status]
  /*Create a Keep/delete variable to use to select/de-select records in the outer query, as needed. Variable flags records to keep based on 
    whether the SR Status includes \'Duplicate\' or \'transferred\'. Usage depends on choice of methodology and analysis need.  Manually replace dates with 
    last day of the reporting week. The date makes sure the SR status date happened after the end of reporting week to attempt to re-create what the
    record looked like at the end of the reporting week and make selections from that. */		
  					,CASE
  						WHEN UPPER([SR Status]) LIKE \'%TRANS%\' and [Status Date] >= \'2022-06-05\' THEN \'keep\'
  						WHEN UPPER([SR STATUS]) LIKE \'%DUP%\' and [Status Date] >= \'2022-06-05\' THEN \'keep\'
  						WHEN UPPER([SR Status]) NOT LIKE \'%TRANS%\' AND UPPER([SR Status]) NOT LIKE \'%DUP%\' THEN \'keep\'
  						END [calc_keep]
  					, \'2022-06-04\' as [WeekEnding]
  			FROM [CitiStat].[dbo].[CSR]
  /*Manually replace SR Type to get the needed SR*/
  			WHERE	[SR Type] in(\'TRM-Grass Mowing\',\'SW-SIU Clean Up\',\'SW-Dirty Alley\',\'SW-Dirty Street\',\'SW-Dirty Alley Proactive\',\'SW-Dirty Street Proactive\',\'RP-Park Maintenance\',\'RP-Grass Cutting\')
  /*Based on prior code, it looks like only records created after 1/1/2021 were used. Running for after 1/1/2020 results in a much larger output of records.
    Keep or run without as methodology/analysis requires  */
  					AND 
  					[Created Date] >= \'2021-01-01\'  ) q1
  WHERE  
  /* Select records where a calc_status exists per the above criteria. Any record not meeting the stated criteria will be null and not selected */
  		q1.[calc_Status] is not null
  		AND
  /* Select records where a calc_keep exists per the above criteria. Any record not meeting the stated criteria will be null and not selected */
  		q1.[calc_keep] is not null
  GROUP BY q1.[WeekEnding], q1.[SR Type], q1.[calc_Status]
  ORDER BY q1.[WeekEnding], q1.[SR Type], q1.[calc_Status]
  ;
')
chip_created<- dbGetQuery(con,'
  SELECT	\'\' as \'CHIP_CREATED\', 
  		q1.[work order type],
  		q1.[Cleaning Type], 
  		q1.[Created week],
  		count(distinct [Record ID]) as [CHIP Created_count] 
  FROM (
  		SELECT	[Record ID]
  				,[Work Order Type]
  				,[Status]
  				,CASE 
  					WHEN UPPER([Clean Type]) like \'HIGH GRASS & WEEDS\' THEN \'HGW\'
  					ELSE [Work Order type]
  					END AS [Cleaning Type]
  				,[Date Create]
  				,CASE 
  					WHEN [Date Create] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
  					WHEN [Date Create] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
  					WHEN [Date Create] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
  					WHEN [Date Create] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\'
  					END [Created Week]
  		FROM [CitiStat].[dbo].[CHIP_WorkOrders]
  		WHERE [Work Order Type] in(\'Boarding\',\'Cleaning\')
  		and [Status] <> \'PENDING\'
  		) q1
  WHERE q1.[Created Week] is not null
  GROUP BY q1.[Work order type], q1.[Cleaning Type], q1.[Created Week]
  order by q1.[Work order type],q1.[Cleaning Type], q1.[created week]
  ;
')  
chip_close<- dbGetQuery(con,'  
  with bqClosed AS(
  				SELECT	[Record ID]
  						,[Work Order Type]
  						,CASE 
  							WHEN UPPER([Clean Type]) like \'HIGH GRASS & WEEDS\' THEN \'HGW\'
  							ELSE [Work Order type]
  							END AS [Cleaning Type]
  						,[Status]
  						,[date create]
  						,[Date Finish]
  						,CASE 
  							WHEN [Date Finish] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29-06/04\'
  							WHEN [Date Finish] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05-06/11\'
  							WHEN [Date Finish] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12-06/18\'
  							WHEN [Date Finish] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19-06/25\' 
  							END [Closed Week]
  				FROM [CitiStat].[dbo].[CHIP_WorkOrders]
  				WHERE	[work order Type] in(\'Boarding\',\'Cleaning\')  
  						AND 
  						[Date Finish] >= \'2022-05-29\'
  						AND
  						([Status] like \'%CLOSE%\' OR [Status] like \'%CANCEL%\'  )   )
  
  ,
  
  count_closed AS(
  				SELECT 
  						[Work Order Type]
  						,[Cleaning Type]
  						,[Closed Week]
  						,Count(distinct [Record ID]) as [Total Closed Count]
  		FROM bqClosed
  		WHERE [Closed Week] is not null
  		GROUP BY [Work Order Type], [Cleaning Type], [Closed Week])
  
  select * from count_closed order by [cleaning type],[closed week]
  ;
')  
chip_ttc <- dbGetQuery(con,'
  
  				SELECT	[Record ID]
  						,[Work Order Type]
  						,CASE 
  							WHEN UPPER([Clean Type]) like \'HIGH GRASS & WEEDS\' THEN \'HGW\'
  							ELSE [Work Order type]
  							END AS [Cleaning Type]
  						,[Status]
  						,[date create]
  						,[Date Finish]
  						,CASE 
  							WHEN [Date Finish] BETWEEN \'2022-05-29\' AND \'2022-06-05\' THEN \'05/29 - 06/04\'
  							WHEN [Date Finish] BETWEEN \'2022-06-05\' AND \'2022-06-12\' THEN \'06/05 - 06/11\'
  							WHEN [Date Finish] BETWEEN \'2022-06-12\' AND \'2022-06-19\' THEN \'06/12 - 06/18\'
  							WHEN [Date Finish] BETWEEN \'2022-06-19\' AND \'2022-06-26\' THEN \'06/19 - 06/25\' 
  							END [Closed Week]
  						,datediff(day,[date create],[date finish]) as [DaysToClose]
  				FROM [CitiStat].[dbo].[CHIP_WorkOrders]
  				WHERE	[work order Type] in(\'Boarding\',\'Cleaning\')  
  						AND 
  						[Date Finish] >= \'2022-05-29\'
  						AND
  						[Status] like \'%CLOSE%\';
  						')

chip_opensrs <- dbGetQuery(con,'SELECT \'\' as \'CHIP_OPEN_SRs\' 
  		,q1.[work order type]
  		,q1.[cleaning type]
  		,q1.[week ending]
  		,q1.[calc_status]
  		,count(distinct [Record id]) as [Open_Status_Count]
  FROM (
  		SELECT
  				[Record ID]
  				,[Work Order Type]
  				,[Status]
   				,CASE 
  					WHEN UPPER([Clean Type]) like \'HIGH GRASS & WEEDS\' THEN \'HGW\'
  					ELSE [Work Order type]
  					END AS [Cleaning Type]
  				,[Date Create]
  				,[Date Finish]	
 
  				, CASE 
  					WHEN [Work Order Type] = \'Boarding\' and datediff(day,[Date Create],\'2022-06-05\') > 10 then \'Overdue\'
  					WHEN [Work Order Type] = \'Boarding\' AND datediff(day,[Date Create],\'2022-06-05\') <= 10 then \'OnTime\'
  					WHEN [Work Order Type] = \'Cleaning\' AND datediff(day,[date create],\'2022-06-05\') > 43 then \'Overdue\'
  					WHEN [Work order type] = \'Cleaning\' AND datediff(day,[Date Create],\'2022-06-05\') <= 43 then \'Ontime\'
  					END [calc_Status]
  				,\'2022-06-04\' as [Week Ending]
  		FROM [CitiStat].[dbo].[CHIP_WorkOrders]
  		WHERE	
  				[Work Order Type] in(\'Boarding\',\'Cleaning\')  
  				AND
  				[Status] <> \'PENDING\'
  				AND
  				(([Date Finish] is null and [Date Create] <= \'2022-06-05\')
  				OR
  				([Date Finish] >= \'2022-06-05\' AND [Date Create] <= \'2022-06-05\'))
  				) q1
  GROUP BY q1.[work order type], q1.[cleaning type], q1.[week ending], q1.[calc_status]
  ORDER BY q1.[work order type], q1.[cleaning type], q1.[week ending], q1.[calc_status]
  ;  
')

#slide 5a
colnames(Sr_opensr)[colnames(Sr_opensr) == "SR Type"] <- "SR_Type"
colnames(Sr_created)[colnames(Sr_created) == "Created Week"] <- "week"
colnames(Sr_closed)[colnames(Sr_closed) == "Closed Week"] <- "week"

merge_sr_cc <- merge(Sr_created, Sr_closed, by=c("SR Type","week"), all.y = TRUE )
colnames(merge_sr_cc)[colnames(merge_sr_cc) == "Created Count"] <- "Created_Count"
colnames(merge_sr_cc)[colnames(merge_sr_cc) == "Total Closed Count"] <- "Closed_Count"
colnames(merge_sr_cc)[colnames(merge_sr_cc) == "SR Type"] <- "SR_Type"

dfm<-merge_sr_cc%>% gather(key = SALESFORCE_CREATED, value = Count, Created_Count :Closed_Count)
dfm%>%  
  filter(SR_Type == "TRM-Grass Mowing")%>%
  ggplot(aes(x = week, y = Count))+
  geom_bar(aes(fill = SALESFORCE_CREATED ),position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("Closed_Count"="black"
                               ,"Created_Count"="orange2"))+
  geom_text(aes(label = Count), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "TRM - Grass Mowing Service Request
         count Per Week May 29 - June 25",
       y = "",
       x = "TRM - Grass Mowing")


#slide 5b
colnames(Sr_opensr)[colnames(Sr_opensr) == "SR Type"] <- "SR_Type"

Sr_opensr %>%
  filter(SR_Type == "TRM-Grass Mowing")%>%
  drop_na(calc_Status)%>%
  ggplot(aes(x = WeekEnding, y = SR_Status_Count , fill = calc_Status))+
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("OnTime"="deepskyblue3"
                               ,"Overdue"="firebrick1"))+
  geom_text(aes(label = SR_Status_Count), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "TRM - Grass Mowing Service Request
         Status Per Week May 29 - June 25",
       y = "",
       x = "TRM - Grass Mowing")
#slide 5c
colnames(Sr_daclosed)[colnames(Sr_daclosed) == "SR Type"] <- "SR_Type"
colnames(Sr_daclosed)[colnames(Sr_daclosed) == "Closed Week"] <- "Closed_Week"

Sr_daclosed %>%
  filter(SR_Type == "TRM-Grass Mowing")%>%
  drop_na(Closed_Week)%>%
  ggplot(aes(x = Closed_Week, y = avg_days_to_close))+
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  geom_text(aes(label = avg_days_to_close), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "TRM - Grass Mowing Service Request
         Average Age Per Week May 29 - June 25",
       y = "",
       x = "TRM - Grass Mowing") 

#slide 8a
colnames(chip_close) <- c("work_order_type","cleaning_type","week","total_closed_count")
colnames(chip_created) <- c("chip_created","work_order_type","cleaning_type","week","chip_created_count")
merge_chip_cc <- merge(chip_created, chip_close, by=c("cleaning_type","week","work_order_type"), all.y = TRUE )

df_chip<-merge_chip_cc%>% gather(key = chip_created, value = Counts, chip_created_count :total_closed_count)
df_chip%>%
  filter(cleaning_type == "Boarding")%>%
  ggplot(aes(x = week, y = Counts ))+
  geom_bar(aes(fill = chip_created),position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("total_closed_count"="orange2"
                               ,"chip_created_count"="black"))+
  geom_text(aes(label = Counts), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Boarding SLA Status Count Per Week
         May 29 - June 25",
       y = "",
       x = "SW-Boarding")
#slide 8b
colnames(chip_opensrs)[colnames(chip_opensrs) == "cleaning type"] <- "cleaning_type"
colnames(chip_opensrs)[colnames(chip_opensrs) == "week ending"] <- "week_ending"

chip_opensrs %>%
  filter(cleaning_type == "Boarding")%>%
  drop_na(calc_status)%>%
  ggplot(aes(x = week_ending, y = Open_Status_Count , fill = calc_status))+
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("OnTime"="deepskyblue3"
                               ,"Overdue"="firebrick1"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Boarding SLA Status per Week
       May 29 - June 25",
       y = "",
       x = "SW-Boarding") 


#slide 8c 
colnames(chip_ttc) <- c("record_id","work_order_type","cleaning_type"
                        ,"status","date_create","date_finish",
                        "closed_week","days_to_close")

chip_ttc %>%
  #apply.weekly(chip_ttc$days_to_close , mean)%>%
  filter(days_to_close > 0)%>%
  filter(cleaning_type == "Boarding")%>%
  drop_na(closed_week)%>%
  ggplot(aes(x = closed_week, y = days_to_close))+
  
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  geom_text(aes(label = days_to_close), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Boarding Average Age Per Week May 29 - June 25",
       y = "",
       x = "TRM - Grass Mowing")

#9a slide
df_chip<-merge_chip_cc%>% gather(key = chip_created, value = Counts, chip_created_count :total_closed_count)
df_chip%>%
  filter(cleaning_type == "Cleaning")%>%
  ggplot(aes(x = week, y = Counts ))+
  geom_bar(aes(fill = chip_created),position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("total_closed_count"="orange2"
                               ,"chip_created_count"="black"))+
  geom_text(aes(label = Counts), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Cleaning SLA Status Count Per Week
         May 29 - June 25",
       y = "",
       x = "SW-Cleaning")

#9b slide
chip_opensrs %>%
  filter(cleaning_type == "Cleaning")%>%
  drop_na(calc_status)%>%
  ggplot(aes(x = week_ending, y = Open_Status_Count , fill = calc_status))+
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("OnTime"="deepskyblue3"
                               ,"Overdue"="firebrick1"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Cleaning SLA Status per Week
       May 29 - June 25",
       y = "",
       x = "SW-Cleaning")
#9c slide
chip_ttc%>%
  #apply.weekly(chip_ttc$days_to_close , mean)%>%
  filter(days_to_close > 0)%>%
  filter(cleaning_type == "Cleaning")%>%
  drop_na(closed_week)%>%
  ggplot(aes(x = closed_week, y = days_to_close))+
  
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  geom_text(aes(label = days_to_close), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-Cleaning Average Days To Close
       Per Week May 29 - June 25",
       y = "",
       x = "SW-Cleaning")
#10a slide
df_chip<-merge_chip_cc%>% gather(key = chip_created, value = Counts, chip_created_count :total_closed_count)
df_chip%>%
  filter(cleaning_type == "HGW")%>%
  ggplot(aes(x = week, y = Counts ))+
  geom_bar(aes(fill = chip_created),position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("total_closed_count"="orange2"
                               ,"chip_created_count"="black"))+
  geom_text(aes(label = Counts), position=position_stack(vjust = 0.5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-HGW SLA Status Count Per Week
         May 29 - June 25",
       y = "",
       x = "SW-HGW")
#10b slide
chip_opensrs %>%
  filter(cleaning_type == "HGW")%>%
  drop_na(calc_status)%>%
  ggplot(aes(x = week_ending, y = Open_Status_Count , fill = calc_status))+
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  scale_fill_manual(values = c("OnTime"="deepskyblue3"
                               ,"Overdue"="firebrick1"))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-HGW SLA Status per Week
       May 29 - June 25",
       y = "",
       x = "SW-HGW")

#10c slide
chip_ttc %>%
  filter(days_to_close > 0)%>%
  filter(cleaning_type == "HGW" )%>%
  drop_na(closed_week)%>%
  ggplot(aes(x = closed_week, y = days_to_close))+
  
  geom_bar(position = "dodge", stat = 'identity', alpha = .5)+
  geom_text(aes(label = days_to_close), vjust = 0)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  labs(title = "SW-HGW Average Days To Close Per Week 
       May 29 - June 25",
       y = "",
       x = "SW-HGW")
