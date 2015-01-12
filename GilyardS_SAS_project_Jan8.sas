/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Monday, January 12, 2015     TIME: 6:40:28 PM
PROJECT: GilyardS_SAS_project_Jan13
PROJECT PATH: P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\sgilyard' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\sgilyard' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\sgilyard" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter and Sort   */
%LET _CLIENTTASKLABEL='Filter and Sort';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.Filter_Sort_1_shelby);

PROC SQL;
   CREATE TABLE MYDATA.Filter_Sort_1_shelby(label="Filter & Sort 1 shelby") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X
      FROM EC100002.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS program code   */
%LET SYSLAST=MYDATA.FILTER_SORT_1_SHELBY;
%LET _CLIENTTASKLABEL='SAS program code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\sgilyard\SAS program code\SAS program code.sas';

GOPTIONS ACCESSIBLE;
/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 08, 2015     TIME: 11:22:08 AM
PROJECT: GilyardS_SAS_project_Jan8
PROJECT PATH: P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\sgilyard' ;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGPDFX TEMP;
ODS PDF(ID=EGPDFX) FILE=EGPDFX STYLE=printer SAS;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: SAS program code   */
%LET SYSLAST=MYDATA.DATASUBSET1;
%LET _CLIENTTASKLABEL='SAS program code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan8.egp';
%LET _SASPROGRAMFILE='P:\QAC\qac200\students\sgilyard\SAS program code\SAS program code.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:50:25 AM
   By task: Data Set Attributes1

   Input Data: Local:MYDATA.FILTER_SORT_1_SHELBY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK."%STR(CONTContentsForFILTER_SORT_1_S)"n);
TITLE "Data set attributes for subset data set";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.FILTER_SORT_1_SHELBY ;

RUN;







GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies for 2012 MEPS Codebook subset   */
%LET _CLIENTTASKLABEL='One-Way Frequencies for 2012 MEPS Codebook subset';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:33:57 PM
   By task: One-Way Frequencies for 2012 MEPS Codebook subset

   Input Data: Local:MYDATA.FILTER_SORT_1_SHELBY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FILTER_SORT_1_SHELBY
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.AGE12X, T.SEX, T.RACETHX, T.MARRY12X, T.REGION12, T.CANCERDX, T.CALEUKEM, T.CACERVIX, T.CABREAST, T.CABRAIN, T.CABLADDR, T.MIDX, T.STRKDX, T.EMPHDX, T.CACOLON, T.CALUNG, T.CALYMPH, T.CAMELANO, T.CAOTHER, T.CAPROSTA, T.CASKINNM
		     , T.CASKINDK, T.CATHYROD, T.DIABDX, T.ARTHDX, T.ARTHTYPE, T.ASTHDX, T.WRGLAS42, T.VISION42, T.BMINDX53, T.BRSTEX53, T.SEATBE53, T.SAQELIG, T.ADSMOK42, T.ADGENH42, T.EMPST31, T.EMPST42, T.EMPST53, T.JOBORG31, T.JOBORG42
		     , T.JOBORG53, T.HELD31X, T.HELD42X, T.HELD53X, T.OFFER31X, T.OFFER42X, T.OFFER53X, T.OFREMP31, T.OFREMP42, T.OFREMP53, T.YNOINS31, T.YNOINS42, T.YNOINS53, T.FAMINC12, T.TTLP12X, T.PRVEV12, T.TRIEV12, T.MCDEV12, T.OPAEV12, T.OPBEV12
		     , T.TRIST12X, T.MCRPD12, T.PRVHMO12, T.PHMONP12, T.AMCHIR12, T.AMCTCH12, T.ERDTCH12, T.IPZERO12, T.IPDIS12, T.DVTOT12, T.IPNGTD12, T.DVGEN12, T.VISEXP12, T.RXTOT12, T.RXEXP12, T.FAMWT12F, T.PERWT12F, T.DIABW12F, T.SAQWT12F
		     , T.EDUCYR, T.EDUYRDEG, T.HIDEG, T.EDRECODE, T.OHRTDX, T.ANGIDX, T.PREGNT31, T.PREGNT42, T.PREGNT53, T.ADPRX42, T.ADILCR42, T.ADILWW42, T.ADRTCR42, T.ADRTWW42, T.ADAPPT42, T.ADNDCR42, T.ADEGMC42, T.ADLIST42, T.ADEXPL42
		     , T.ADRESP42, T.ADPRTM42, T.ADINST42, T.ADEZUN42, T.ADTLHW42, T.ADFFRM42, T.ADFHLP42, T.ADHECR42, T.ADNSMK42, T.ADDRBP42, T.ADSPEC42, T.ADSPRF42, T.ADDAYA42, T.ADCLIM42, T.ADPALS42, T.ADPWLM42, T.ADMALS42, T.ADMWLM42, T.ADPAIN42
		     , T.ADCAPE42, T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T.PCS42, T.MCS42, T.SFFLAG42, T.ADNERV42, T.ADHOPE42, T.ADREST42, T.ADSAD42, T.ADEFRT42, T.ADWRTH42, T.K6SUM42, T.ADINTR42, T.ADDPRS42, T.PHQ242, T.ADINSA42, T.ADINSB42
		     , T.ADRISK42, T.ADOVER42, T.ADCMPM42, T.ADCMPD42, T.ADCMPY42, T.ADLANG42, T.POVCAT12, T.POVLEV12, T.WAGEP12X, T.PENSP12X
	FROM MYDATA.FILTER_SORT_1_SHELBY(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results 2012 MEPS Codebook";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE2 "Shelby Gilyard";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES AGE12X / MISSPRINT  SCORES=TABLE;
	TABLES SEX / MISSPRINT  SCORES=TABLE;
	TABLES RACETHX / MISSPRINT  SCORES=TABLE;
	TABLES MARRY12X / MISSPRINT  SCORES=TABLE;
	TABLES REGION12 / MISSPRINT  SCORES=TABLE;
	TABLES CANCERDX / MISSPRINT  SCORES=TABLE;
	TABLES CALEUKEM / MISSPRINT  SCORES=TABLE;
	TABLES CACERVIX / MISSPRINT  SCORES=TABLE;
	TABLES CABREAST / MISSPRINT  SCORES=TABLE;
	TABLES CABRAIN / MISSPRINT  SCORES=TABLE;
	TABLES CABLADDR / MISSPRINT  SCORES=TABLE;
	TABLES MIDX / MISSPRINT  SCORES=TABLE;
	TABLES STRKDX / MISSPRINT  SCORES=TABLE;
	TABLES EMPHDX / MISSPRINT  SCORES=TABLE;
	TABLES CACOLON / MISSPRINT  SCORES=TABLE;
	TABLES CALUNG / MISSPRINT  SCORES=TABLE;
	TABLES CALYMPH / MISSPRINT  SCORES=TABLE;
	TABLES CAMELANO / MISSPRINT  SCORES=TABLE;
	TABLES CAOTHER / MISSPRINT  SCORES=TABLE;
	TABLES CAPROSTA / MISSPRINT  SCORES=TABLE;
	TABLES CASKINNM / MISSPRINT  SCORES=TABLE;
	TABLES CASKINDK / MISSPRINT  SCORES=TABLE;
	TABLES CATHYROD / MISSPRINT  SCORES=TABLE;
	TABLES DIABDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHDX / MISSPRINT  SCORES=TABLE;
	TABLES ARTHTYPE / MISSPRINT  SCORES=TABLE;
	TABLES ASTHDX / MISSPRINT  SCORES=TABLE;
	TABLES WRGLAS42 / MISSPRINT  SCORES=TABLE;
	TABLES VISION42 / MISSPRINT  SCORES=TABLE;
	TABLES BMINDX53 / MISSPRINT  SCORES=TABLE;
	TABLES BRSTEX53 / MISSPRINT  SCORES=TABLE;
	TABLES SEATBE53 / MISSPRINT  SCORES=TABLE;
	TABLES SAQELIG / MISSPRINT  SCORES=TABLE;
	TABLES ADSMOK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADGENH42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST31 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST42 / MISSPRINT  SCORES=TABLE;
	TABLES EMPST53 / MISSPRINT  SCORES=TABLE;
	TABLES JOBORG31 / MISSPRINT  SCORES=TABLE;
	TABLES JOBORG42 / MISSPRINT  SCORES=TABLE;
	TABLES JOBORG53 / MISSPRINT  SCORES=TABLE;
	TABLES HELD31X / MISSPRINT  SCORES=TABLE;
	TABLES HELD42X / MISSPRINT  SCORES=TABLE;
	TABLES HELD53X / MISSPRINT  SCORES=TABLE;
	TABLES OFFER31X / MISSPRINT  SCORES=TABLE;
	TABLES OFFER42X / MISSPRINT  SCORES=TABLE;
	TABLES OFFER53X / MISSPRINT  SCORES=TABLE;
	TABLES OFREMP31 / MISSPRINT  SCORES=TABLE;
	TABLES OFREMP42 / MISSPRINT  SCORES=TABLE;
	TABLES OFREMP53 / MISSPRINT  SCORES=TABLE;
	TABLES YNOINS31 / MISSPRINT  SCORES=TABLE;
	TABLES YNOINS42 / MISSPRINT  SCORES=TABLE;
	TABLES YNOINS53 / MISSPRINT  SCORES=TABLE;
	TABLES FAMINC12 / MISSPRINT  SCORES=TABLE;
	TABLES TTLP12X / MISSPRINT  SCORES=TABLE;
	TABLES PRVEV12 / MISSPRINT  SCORES=TABLE;
	TABLES TRIEV12 / MISSPRINT  SCORES=TABLE;
	TABLES MCDEV12 / MISSPRINT  SCORES=TABLE;
	TABLES OPAEV12 / MISSPRINT  SCORES=TABLE;
	TABLES OPBEV12 / MISSPRINT  SCORES=TABLE;
	TABLES TRIST12X / MISSPRINT  SCORES=TABLE;
	TABLES MCRPD12 / MISSPRINT  SCORES=TABLE;
	TABLES PRVHMO12 / MISSPRINT  SCORES=TABLE;
	TABLES PHMONP12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCHIR12 / MISSPRINT  SCORES=TABLE;
	TABLES AMCTCH12 / MISSPRINT  SCORES=TABLE;
	TABLES ERDTCH12 / MISSPRINT  SCORES=TABLE;
	TABLES IPZERO12 / MISSPRINT  SCORES=TABLE;
	TABLES IPDIS12 / MISSPRINT  SCORES=TABLE;
	TABLES DVTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES IPNGTD12 / MISSPRINT  SCORES=TABLE;
	TABLES DVGEN12 / MISSPRINT  SCORES=TABLE;
	TABLES VISEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES RXTOT12 / MISSPRINT  SCORES=TABLE;
	TABLES RXEXP12 / MISSPRINT  SCORES=TABLE;
	TABLES FAMWT12F / MISSPRINT  SCORES=TABLE;
	TABLES PERWT12F / MISSPRINT  SCORES=TABLE;
	TABLES DIABW12F / MISSPRINT  SCORES=TABLE;
	TABLES SAQWT12F / MISSPRINT  SCORES=TABLE;
	TABLES EDUCYR / MISSPRINT  SCORES=TABLE;
	TABLES EDUYRDEG / MISSPRINT  SCORES=TABLE;
	TABLES HIDEG / MISSPRINT  SCORES=TABLE;
	TABLES EDRECODE / MISSPRINT  SCORES=TABLE;
	TABLES OHRTDX / MISSPRINT  SCORES=TABLE;
	TABLES ANGIDX / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT31 / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT42 / MISSPRINT  SCORES=TABLE;
	TABLES PREGNT53 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRX42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADILWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRTWW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADAPPT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNDCR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEGMC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLIST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEXPL42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRESP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPRTM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEZUN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADTLHW42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFFRM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADFHLP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHECR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNSMK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDRBP42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPEC42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSPRF42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDAYA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCLIM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMALS42 / MISSPRINT  SCORES=TABLE;
	TABLES ADMWLM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADPAIN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCAPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNRGY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDOWN42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSOCA42 / MISSPRINT  SCORES=TABLE;
	TABLES PCS42 / MISSPRINT  SCORES=TABLE;
	TABLES MCS42 / MISSPRINT  SCORES=TABLE;
	TABLES SFFLAG42 / MISSPRINT  SCORES=TABLE;
	TABLES ADNERV42 / MISSPRINT  SCORES=TABLE;
	TABLES ADHOPE42 / MISSPRINT  SCORES=TABLE;
	TABLES ADREST42 / MISSPRINT  SCORES=TABLE;
	TABLES ADSAD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADEFRT42 / MISSPRINT  SCORES=TABLE;
	TABLES ADWRTH42 / MISSPRINT  SCORES=TABLE;
	TABLES K6SUM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINTR42 / MISSPRINT  SCORES=TABLE;
	TABLES ADDPRS42 / MISSPRINT  SCORES=TABLE;
	TABLES PHQ242 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSA42 / MISSPRINT  SCORES=TABLE;
	TABLES ADINSB42 / MISSPRINT  SCORES=TABLE;
	TABLES ADRISK42 / MISSPRINT  SCORES=TABLE;
	TABLES ADOVER42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPM42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPD42 / MISSPRINT  SCORES=TABLE;
	TABLES ADCMPY42 / MISSPRINT  SCORES=TABLE;
	TABLES ADLANG42 / MISSPRINT  SCORES=TABLE;
	TABLES POVCAT12 / MISSPRINT  SCORES=TABLE;
	TABLES POVLEV12 / MISSPRINT  SCORES=TABLE;
	TABLES WAGEP12X / MISSPRINT  SCORES=TABLE;
	TABLES PENSP12X / MISSPRINT  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recode variables   */
%LET _CLIENTTASKLABEL='Recode variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.Subset_codebook_2012_managed);

PROC SQL;
   CREATE TABLE WORK."Subset_codebook_2012_managed"n AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          /* Health in general */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health in general during year" AS 'Health in general'n, 
          /* Nervous */
            (CASE 
               WHEN -7 = t1.ADNERV42 THEN .
               WHEN -8 = t1.ADNERV42 THEN .
               WHEN -9 = t1.ADNERV42 THEN .
               ELSE t1.ADNERV42
            END) LABEL="How often felt nervous recoded" AS Nervous, 
          /* health limits */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN 0
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="health limits mod activities recoded" AS 'health limits'n, 
          /* Health limits climbing stairs */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN 0
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="climbing stairs recoded" AS 'Health limits climbing stairs'n, 
          /* physical problems */
            (CASE 
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="accomp less b/c phy probs" AS 'physical problems'n, 
          /* work limit b/c phys probs */
            (CASE 
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="work limit b/c phys probs recoded" AS 'work limit b/c phys probs'n, 
          /* accomp less b/c ment probs */
            (CASE 
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="accomp less b/c ment probs recoded" AS 'accomp less b/c ment probs'n, 
          /* work limit b/c ment probs */
            (CASE 
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="work limit b/c ment probs recoded" AS 'work limit b/c ment probs'n, 
          /* pain limits normal work */
            (CASE 
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="pain limits normal work recoded" AS 'pain limits normal work'n, 
          /* felt calm/peaceful */
            (CASE 
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="felt calm/peaceful recoded" AS 'felt calm/peaceful'n, 
          /* had a lot of energy */
            (CASE 
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="had a lot of energy recoded" AS 'had a lot of energy'n, 
          /* felt downhearted/depr */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN 0
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="felt downhearted/depr recoded" AS 'felt downhearted/depr'n, 
          /* health stopped soc activity */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN 0
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="health stopped soc activity recoded" AS 'health stopped soc activity'n, 
          /* how often felt sad */
            (CASE 
               WHEN -7 = t1.ADSAD42 THEN .
               WHEN -8 = t1.ADSAD42 THEN .
               WHEN -9 = t1.ADSAD42 THEN .
               ELSE t1.ADSAD42
            END) LABEL="how often felt sad recoded" AS 'how often felt sad'n, 
          /* more likely to take risks */
            (CASE 
               WHEN -7 = t1.ADRISK42 THEN .
               WHEN -8 = t1.ADRISK42 THEN .
               WHEN -9 = t1.ADRISK42 THEN .
               ELSE t1.ADRISK42
            END) LABEL="risks recoded" AS 'more likely to take risks'n, 
          /* felt down/depressed/hopeless */
            (CASE 
               WHEN -1 = t1.ADDPRS42 THEN 0
               WHEN -7 = t1.ADDPRS42 THEN .
               WHEN -8 = t1.ADDPRS42 THEN .
               WHEN -9 = t1.ADDPRS42 THEN .
               ELSE t1.ADDPRS42
            END) LABEL="down/depressed/hopeless recoded" AS 'felt down/depressed/hopeless'n, 
          /* dr advised to quit smoking */
            (CASE 
               WHEN -9 = t1.ADNSMK42 THEN .
               ELSE t1.ADNSMK42
            END) LABEL="quit smoking recoded" AS 'dr advised to quit smoking'n, 
          /* how often felt worthless */
            (CASE 
               WHEN -7 = t1.ADWRTH42 THEN .
               WHEN -8 = t1.ADWRTH42 THEN .
               WHEN -9 = t1.ADWRTH42 THEN .
               ELSE t1.ADWRTH42
            END) LABEL="how often felt worthless recoded" AS 'how often felt worthless'n, 
          /* how often felt nervous */
            (CASE 
               WHEN -7 = t1.ADNERV42 THEN .
               WHEN -8 = t1.ADNERV42 THEN .
               WHEN -9 = t1.ADNERV42 THEN .
               ELSE t1.ADNERV42
            END) LABEL="how often felt nervous recoded" AS 'how often felt nervous'n, 
          /* How often felt hopeless */
            (CASE 
               WHEN -7 = t1.ADHOPE42 THEN .
               WHEN -8 = t1.ADHOPE42 THEN .
               WHEN -9 = t1.ADHOPE42 THEN .
               ELSE t1.ADHOPE42
            END) LABEL="how often felt hopeless recoded" AS 'How often felt hopeless'n, 
          /* how often felt restless */
            (CASE 
               WHEN -7 = t1.ADREST42 THEN .
               WHEN -8 = t1.ADREST42 THEN .
               WHEN -9 = t1.ADREST42 THEN .
               ELSE t1.ADREST42
            END) LABEL="how often felt restless recoded" AS 'how often felt restless'n, 
          /* how often everything an effort */
            (CASE 
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="how often everything an effort recoded" AS 'how often everything an effort'n, 
          /* little interest in things */
            (CASE 
               WHEN -7 = t1.ADINTR42 THEN .
               WHEN -8 = t1.ADINTR42 THEN .
               WHEN -9 = t1.ADINTR42 THEN .
               ELSE t1.ADINTR42
            END) LABEL="little interest in things recoded" AS 'little interest in things'n, 
          /* family total income */
            (CASE 
               WHEN -10794 = t1.FAMINC12 THEN .
               WHEN -15612 = t1.FAMINC12 THEN .
               WHEN -19924 = t1.FAMINC12 THEN .
               WHEN -3772 = t1.FAMINC12 THEN .
               WHEN -4246 = t1.FAMINC12 THEN .
               WHEN -9063 = t1.FAMINC12 THEN .
               ELSE t1.FAMINC12
            END) LABEL="family total income recoded" AS 'family total income'n
      FROM MYDATA.FILTER_SORT_1_SHELBY t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: reverse recode_SF12_variables   */
%LET _CLIENTTASKLABEL='reverse recode_SF12_variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          /* ADGEN42_R */
            (6-t1.'Health in general'n) LABEL="SF12 health in general recoded" AS ADGEN42_R, 
          /* ADMALS42_R */
            (6-t1.'accomp less b/c ment probs'n) LABEL="accom less b/c ment probs rev recode" AS ADMALS42_R, 
          /* ADMWLM42_R */
            (5-t1.'work limit b/c ment probs'n) LABEL="work lim bc ment probs rev recode" AS ADMWLM42_R, 
          /* ADCAPE42_R */
            (6-t1.'felt calm/peaceful'n) LABEL="felt calm/peaceful rev recode" AS ADCAPE42_R, 
          /* ADNRGY42_R */
            (5-t1.'had a lot of energy'n) LABEL="had energy rev recode" AS ADNRGY42_R, 
          /* ADPAIN42_R */
            (5-t1.'pain limits normal work'n) LABEL="pain limits normal work rev recode" AS ADPAIN42_R, 
          /* accom less b/c physical probs */
            (CASE 
               WHEN -1 = t1.'physical problems'n THEN 0
               ELSE t1.'physical problems'n
            END) LABEL="recode" AS 'accom less b/c physical probs'n, 
          /* work limit phys probs */
            (CASE 
               WHEN -1 = t1.'work limit b/c phys probs'n THEN 0
               ELSE t1.'work limit b/c phys probs'n
            END) LABEL="recode" AS 'work limit phys probs'n
      FROM WORK.SUBSET_CODEBOOK_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:17 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGEN42_R, T.ADGENH42, T.ADDAYA42, T.ADPALS42, T.ADPWLM42, T.ADMALS42_R, T.ADMALS42, T.ADMWLM42_R, T.ADMWLM42, T.ADCAPE42_R, T.ADCAPE42, T.ADNRGY42_R, T.ADNRGY42, T.ADPAIN42_R, T.ADPAIN42
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGEN42_R * ADGENH42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPALS42_R * ADPALS42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPWLM42_R * ADPWLM42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMALS42_R * ADMALS42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADMWLM42_R * ADMWLM42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42_R * ADCAPE42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42_R * ADNRGY42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN42_R * ADPAIN42 /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:18 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ADGEN42_R, T.ADMALS42_R, T.ADMWLM42_R, T.ADCAPE42_R, T.ADNRGY42_R, T.ADPAIN42_R, T."health limits"n, T."Health limits climbing stairs"n, T."felt downhearted/depr"n, T."health stopped soc activity"n, T.ADGENH42, T.ADDAYA42
		     , T.ADCLIM42, T.ADPALS42, T.ADPWLM42, T.ADMALS42, T.ADMWLM42, T.ADPAIN42, T.ADCAPE42, T.ADNRGY42, T.ADDOWN42, T.ADSOCA42, T."accom less b/c physical probs"n, T."work limit phys probs"n
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ADGEN42_R /  SCORES=TABLE;
	TABLES ADMALS42_R /  SCORES=TABLE;
	TABLES ADMWLM42_R /  SCORES=TABLE;
	TABLES ADCAPE42_R /  SCORES=TABLE;
	TABLES ADNRGY42_R /  SCORES=TABLE;
	TABLES ADPAIN42_R /  SCORES=TABLE;
	TABLES "health limits"n /  SCORES=TABLE;
	TABLES "Health limits climbing stairs"n /  SCORES=TABLE;
	TABLES "felt downhearted/depr"n /  SCORES=TABLE;
	TABLES "health stopped soc activity"n /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES "accom less b/c physical probs"n /  SCORES=TABLE;
	TABLES "work limit phys probs"n /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.SUBSET_MEPSCODEBOOK_2_0001);

PROC SQL;
   CREATE TABLE WORK."SUBSET_MEPSCODEBOOK_2_0001"n AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          t1.ADGEN42_R, 
          t1.ADMALS42_R, 
          t1.ADMWLM42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.ADPAIN42_R, 
          t1.'accom less b/c physical probs'n, 
          t1.'work limit phys probs'n, 
          /* Sum_aggregates */
            (SUM(t1.ADGEN42_R,t1.ADMALS42_R,t1.ADMWLM42_R,t1.ADCAPE42_R,t1.ADNRGY42_R,t1.ADPAIN42_R,t1.
            'accom less b/c physical probs'n,t1.'work limit phys probs'n,t1.'health stopped soc activity'n,t1.
            'Health limits climbing stairs'n,t1.'felt downhearted/depr'n,t1.'health limits'n)) LABEL=
            "sum of aggregates" AS Sum_aggregates
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0000 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:18 PM
   By task: List Data

   Input Data: Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGEN42_R, T.ADMALS42_R, T.ADMWLM42_R, T.ADCAPE42_R, T.ADNRGY42_R, T.ADPAIN42_R, T."accom less b/c physical probs"n, T."work limit phys probs"n, T."health stopped soc activity"n, T."felt downhearted/depr"n
		     , T."Health limits climbing stairs"n, T."health limits"n, T.Sum_aggregates
	FROM WORK.SUBSET_MEPSCODEBOOK_2_0001 as T
;
QUIT;
TITLE;
TITLE1 "Check aggregate variables coding";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ADGEN42_R ADMALS42_R ADMWLM42_R ADCAPE42_R ADNRGY42_R ADPAIN42_R "accom less b/c physical probs"n "work limit phys probs"n "health stopped soc activity"n "felt downhearted/depr"n "Health limits climbing stairs"n "health limits"n Sum_aggregates;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:18 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_aggregates
	FROM WORK.SUBSET_MEPSCODEBOOK_2_0001 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_aggregates /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics   */
%LET _CLIENTTASKLABEL='Summary Statistics';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:19 PM
   By task: Summary Statistics

   Input Data: Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_aggregates
	FROM WORK.SUBSET_MEPSCODEBOOK_2_0001 as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results summary stats";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_aggregates;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for sum aggregates   */
%LET _CLIENTTASKLABEL='Distribution Analysis for sum aggregates';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:19 PM
   By task: Distribution Analysis for sum aggregates

   Input Data: Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.SUBSET_MEPSCODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_aggregates
	FROM WORK.SUBSET_MEPSCODEBOOK_2_0001(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Sum_aggregates distribution analysis";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))by Shelby Gilyard";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_aggregates;
	HISTOGRAM   Sum_aggregates / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_SUBSET_MEPSCODEBOOK_2);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_SUBSET_MEPSCODEBOOK_2(label="QUERY_FOR_SUBSET_MEPSCODEBOOK_2") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          t1.ADGEN42_R, 
          t1.ADMALS42_R, 
          t1.ADMWLM42_R, 
          t1.ADCAPE42_R, 
          t1.ADNRGY42_R, 
          t1.ADPAIN42_R, 
          t1.'accom less b/c physical probs'n, 
          t1.'work limit phys probs'n, 
          t1.Sum_aggregates, 
          /* Sum aggregate categories */
            (CASE  
               WHEN t1.Sum_aggregates >=2 and t1.Sum_aggregates <32
               THEN 1
               WHEN t1.Sum_aggregates >=32 and t1.Sum_aggregates <=41
               THEN 2
               WHEN t1.Sum_aggregates >=41 and t1.Sum_aggregates <=52
               THEN 3
            END) LABEL="sum aggregate categories typed" AS 'Sum aggregate categories'n
      FROM WORK.SUBSET_MEPSCODEBOOK_2_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies2   */
%LET _CLIENTTASKLABEL='One-Way Frequencies2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:19 PM
   By task: One-Way Frequencies2

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_aggregates, T."Sum aggregate categories"n
	FROM WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_aggregates /  SCORES=TABLE;
	TABLES "Sum aggregate categories"n /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis1   */
%LET _CLIENTTASKLABEL='Table Analysis1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:42 PM
   By task: Table Analysis1

   Input Data: Local:WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_aggregates, T."Sum aggregate categories"n
	FROM WORK.QUERY_FOR_SUBSET_MEPSCODEBOOK_2 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES "Sum aggregate categories"n * Sum_aggregates /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: query_part2_assign5   */
%LET _CLIENTTASKLABEL='query_part2_assign5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_SUBSET_CODEBOOK_2012_M);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_SUBSET_CODEBOOK_2012_M(label="QUERY_FOR_SUBSET_CODEBOOK_2012_M") AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          /* FAMINC_re */
            (CASE 
               WHEN 0 = t1.FAMINC12 THEN .
               WHEN -10794 = t1.FAMINC12 THEN .
               WHEN -15612 = t1.FAMINC12 THEN .
               WHEN -19924 = t1.FAMINC12 THEN .
               WHEN -3772 = t1.FAMINC12 THEN .
               WHEN -4246 = t1.FAMINC12 THEN .
               WHEN -9063 = t1.FAMINC12 THEN .
               ELSE t1.FAMINC12
            END) LABEL="family income recoded" AS FAMINC_re, 
          /* TTLP12_re */
            (CASE 
               WHEN 0 = t1.TTLP12X THEN .
               WHEN -1000 = t1.TTLP12X THEN .
               WHEN -11459 = t1.TTLP12X THEN .
               WHEN -1200 = t1.TTLP12X THEN .
               WHEN -12002 = t1.TTLP12X THEN .
               WHEN -1289 = t1.TTLP12X THEN .
               WHEN -1500 = t1.TTLP12X THEN .
               WHEN -15982 = t1.TTLP12X THEN .
               WHEN -18132 = t1.TTLP12X THEN .
               WHEN -18938 = t1.TTLP12X THEN .
               WHEN -22919 = t1.TTLP12X THEN .
               WHEN -26861 = t1.TTLP12X THEN .
               WHEN -2890 = t1.TTLP12X THEN .
               WHEN -3772 = t1.TTLP12X THEN .
               WHEN -38074 = t1.TTLP12X THEN .
               WHEN -4246 = t1.TTLP12X THEN .
               WHEN -600 = t1.TTLP12X THEN .
               WHEN -6981 = t1.TTLP12X THEN .
               WHEN -7283 = t1.TTLP12X THEN .
               WHEN -8051 = t1.TTLP12X THEN .
               WHEN -9063 = t1.TTLP12X THEN .
               WHEN -9903 = t1.TTLP12X THEN .
               ELSE t1.TTLP12X
            END) LABEL="persons income recoded" AS TTLP12_re, 
          /* BMINDX53_re */
            (CASE 
               WHEN -1 = t1.BMINDX53 THEN .
               WHEN -9 = t1.BMINDX53 THEN .
               ELSE t1.BMINDX53
            END) LABEL="bmi recoded" AS BMINDX53_re, 
          /* DIABDX_re */
            (CASE 
               WHEN -7 = t1.MIDX THEN .
               WHEN -8 = t1.MIDX THEN .
               WHEN -9 = t1.MIDX THEN .
               ELSE t1.MIDX
            END) LABEL="Diabetes diagnosis recoded" AS DIABDX_re, 
          /* ARTHDX_re */
            (CASE 
               WHEN -7 = t1.ARTHDX THEN .
               WHEN -8 = t1.ARTHDX THEN .
               WHEN -9 = t1.ARTHDX THEN .
               ELSE t1.ARTHDX
            END) LABEL="arthritis diagnosis recoded" AS ARTHDX_re, 
          /* MARRY12X_re */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="marital status recoded" AS MARRY12X_re, 
          /* EDRECODE_re */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="education recode" AS EDRECODE_re
      FROM WORK.SUBSET_CODEBOOK_2012_MANAGED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies3   */
%LET _CLIENTTASKLABEL='One-Way Frequencies3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:43 PM
   By task: One-Way Frequencies3

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2012_M
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2012_M
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.DIABDX, T.ARTHDX, T.BMINDX53, T.FAMINC12, T.TTLP12X, T.FAMINC_re, T.TTLP12_re, T.BMINDX53_re, T.DIABDX_re, T.ARTHDX_re
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2012_M as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES DIABDX /  SCORES=TABLE;
	TABLES ARTHDX /  SCORES=TABLE;
	TABLES BMINDX53 /  SCORES=TABLE;
	TABLES FAMINC12 /  SCORES=TABLE;
	TABLES TTLP12X /  SCORES=TABLE;
	TABLES FAMINC_re /  SCORES=TABLE;
	TABLES TTLP12_re /  SCORES=TABLE;
	TABLES BMINDX53_re /  SCORES=TABLE;
	TABLES DIABDX_re /  SCORES=TABLE;
	TABLES ARTHDX_re /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          t1.FAMINC_re, 
          t1.TTLP12_re, 
          t1.BMINDX53_re, 
          t1.DIABDX_re, 
          t1.ARTHDX_re, 
          /* Sum_barriers_pttwo */
            (SUM(t1.FAMINC_re,t1.TTLP12_re,t1.BMINDX53_re,t1.DIABDX_re,t1.ARTHDX_re)) LABEL="new variables part 2" AS 
            Sum_barriers_pttwo
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2012_M t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data1   */
%LET _CLIENTTASKLABEL='List Data1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:43 PM
   By task: List Data1

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.FAMINC_re, T.TTLP12_re, T.BMINDX53_re, T.DIABDX_re, T.ARTHDX_re, T.Sum_barriers_pttwo
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001 as T
;
QUIT;
TITLE;
TITLE1 "CHECK sum barriers variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=10)
	OBS="Row number"
	LABEL
	;
	VAR FAMINC_re TTLP12_re BMINDX53_re DIABDX_re ARTHDX_re Sum_barriers_pttwo;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies4   */
%LET _CLIENTTASKLABEL='One-Way Frequencies4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:43 PM
   By task: One-Way Frequencies4

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.Sum_barriers_pttwo
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES Sum_barriers_pttwo /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: newvariable_pt2   */
%LET _CLIENTTASKLABEL='newvariable_pt2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0002 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.CACERVIX, 
          t1.CABREAST, 
          t1.CABRAIN, 
          t1.CABLADDR, 
          t1.MIDX, 
          t1.STRKDX, 
          t1.EMPHDX, 
          t1.CACOLON, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CASKINDK, 
          t1.CATHYROD, 
          t1.DIABDX, 
          t1.ARTHDX, 
          t1.ARTHTYPE, 
          t1.ASTHDX, 
          t1.WRGLAS42, 
          t1.VISION42, 
          t1.BMINDX53, 
          t1.BRSTEX53, 
          t1.SEATBE53, 
          t1.SAQELIG, 
          t1.ADSMOK42, 
          t1.ADGENH42, 
          t1.EMPST31, 
          t1.EMPST42, 
          t1.EMPST53, 
          t1.JOBORG31, 
          t1.JOBORG42, 
          t1.JOBORG53, 
          t1.HELD31X, 
          t1.HELD42X, 
          t1.HELD53X, 
          t1.OFFER31X, 
          t1.OFFER42X, 
          t1.OFFER53X, 
          t1.OFREMP31, 
          t1.OFREMP42, 
          t1.OFREMP53, 
          t1.YNOINS31, 
          t1.YNOINS42, 
          t1.YNOINS53, 
          t1.FAMINC12, 
          t1.TTLP12X, 
          t1.PRVEV12, 
          t1.TRIEV12, 
          t1.MCDEV12, 
          t1.OPAEV12, 
          t1.OPBEV12, 
          t1.TRIST12X, 
          t1.MCRPD12, 
          t1.PRVHMO12, 
          t1.PHMONP12, 
          t1.AMCHIR12, 
          t1.AMCTCH12, 
          t1.ERDTCH12, 
          t1.IPZERO12, 
          t1.IPDIS12, 
          t1.DVTOT12, 
          t1.IPNGTD12, 
          t1.DVGEN12, 
          t1.VISEXP12, 
          t1.RXTOT12, 
          t1.RXEXP12, 
          t1.FAMWT12F, 
          t1.PERWT12F, 
          t1.DIABW12F, 
          t1.SAQWT12F, 
          t1.EDUCYR, 
          t1.EDUYRDEG, 
          t1.HIDEG, 
          t1.EDRECODE, 
          t1.OHRTDX, 
          t1.ANGIDX, 
          t1.PREGNT31, 
          t1.PREGNT42, 
          t1.PREGNT53, 
          t1.ADPRX42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADAPPT42, 
          t1.ADNDCR42, 
          t1.ADEGMC42, 
          t1.ADLIST42, 
          t1.ADEXPL42, 
          t1.ADRESP42, 
          t1.ADPRTM42, 
          t1.ADINST42, 
          t1.ADEZUN42, 
          t1.ADTLHW42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADHECR42, 
          t1.ADNSMK42, 
          t1.ADDRBP42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADDAYA42, 
          t1.ADCLIM42, 
          t1.ADPALS42, 
          t1.ADPWLM42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADPAIN42, 
          t1.ADCAPE42, 
          t1.ADNRGY42, 
          t1.ADDOWN42, 
          t1.ADSOCA42, 
          t1.PCS42, 
          t1.MCS42, 
          t1.SFFLAG42, 
          t1.ADNERV42, 
          t1.ADHOPE42, 
          t1.ADREST42, 
          t1.ADSAD42, 
          t1.ADEFRT42, 
          t1.ADWRTH42, 
          t1.K6SUM42, 
          t1.ADINTR42, 
          t1.ADDPRS42, 
          t1.PHQ242, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADRISK42, 
          t1.ADOVER42, 
          t1.ADCMPM42, 
          t1.ADCMPD42, 
          t1.ADCMPY42, 
          t1.ADLANG42, 
          t1.POVCAT12, 
          t1.POVLEV12, 
          t1.WAGEP12X, 
          t1.PENSP12X, 
          t1.'Health in general'n, 
          t1.Nervous, 
          t1.'health limits'n, 
          t1.'Health limits climbing stairs'n, 
          t1.'physical problems'n, 
          t1.'work limit b/c phys probs'n, 
          t1.'accomp less b/c ment probs'n, 
          t1.'work limit b/c ment probs'n, 
          t1.'pain limits normal work'n, 
          t1.'felt calm/peaceful'n, 
          t1.'had a lot of energy'n, 
          t1.'felt downhearted/depr'n, 
          t1.'health stopped soc activity'n, 
          t1.'how often felt sad'n, 
          t1.'more likely to take risks'n, 
          t1.'felt down/depressed/hopeless'n, 
          t1.'dr advised to quit smoking'n, 
          t1.'how often felt worthless'n, 
          t1.'how often felt nervous'n, 
          t1.'How often felt hopeless'n, 
          t1.'how often felt restless'n, 
          t1.'how often everything an effort'n, 
          t1.'little interest in things'n, 
          t1.'family total income'n, 
          t1.FAMINC_re, 
          t1.TTLP12_re, 
          t1.BMINDX53_re, 
          t1.DIABDX_re, 
          t1.ARTHDX_re, 
          t1.Sum_barriers_pttwo, 
          /* sum_barriers_pttwo1 */
            (CASE  
               WHEN t1.Sum_barriers_pttwo >=2 and t1.Sum_barriers_pttwo <=34,077
               THEN 1
               WHEN t1.Sum_barriers_pttwo >=34,077 and t1.Sum_barriers_pttwo <=119,706.7
                THEN 2
               WHEN t1.Sum_barriers_pttwo >=119,706.7 and t1.Sum_barriers_pttwo <=713,954.4
                THEN 3
            END) LABEL="summ barriers for pt2" AS sum_barriers_pttwo1
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Summary Statistics part 2   */
%LET _CLIENTTASKLABEL='Summary Statistics part 2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:34:44 PM
   By task: Summary Statistics part 2

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_barriers_pttwo
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results: sum stats part 2";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE2 "by Shelby Gilyard";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MIN 
		MAX 
		MODE 
		N	
		Q1 
		MEDIAN 
		Q3	;
	VAR Sum_barriers_pttwo;

RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: marry_edu_query   */
%LET _CLIENTTASKLABEL='marry_edu_query';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_21);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_21 AS 
   SELECT t1.MARRY12X, 
          t1.EDRECODE, 
          /* MARRY_CATEGORIES */
            (CASE  
               WHEN t1.MARRY12X =1
               THEN 1
               WHEN t1.MARRY12X =2
               THEN 2
               WHEN t1.MARRY12X =5
               THEN 3
            END) LABEL="marry categories part 2" AS MARRY_CATEGORIES, 
          /* EDRECODE_categories */
            (CASE  
               WHEN t1.EDRECODE >=6 and t1.EDRECODE <=13
               THEN 1
               WHEN t1.EDRECODE >=13 and t1.EDRECODE <=16
               THEN 2
            END) LABEL="educ categories pt 2" AS EDRECODE_categories, 
          t1.FAMINC_re, 
          t1.TTLP12_re, 
          t1.BMINDX53_re, 
          t1.DIABDX_re, 
          t1.ARTHDX_re
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis2   */
%LET _CLIENTTASKLABEL='Table Analysis2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:35:05 PM
   By task: Table Analysis2

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_21
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_21
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.EDRECODE, T.MARRY_CATEGORIES, T.EDRECODE_categories
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_21 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE_categories * EDRECODE /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES MARRY_CATEGORIES * MARRY12X /
		NOROW
		NOPERCENT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies5   */
%LET _CLIENTTASKLABEL='One-Way Frequencies5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:35:06 PM
   By task: One-Way Frequencies5

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_21
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_21
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.MARRY12X, T.EDRECODE, T.MARRY_CATEGORIES, T.EDRECODE_categories
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_21 as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES EDRECODE /  SCORES=TABLE;
	TABLES MARRY_CATEGORIES /  SCORES=TABLE;
	TABLES EDRECODE_categories /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis for barriers part 2   */
%LET _CLIENTTASKLABEL='Distribution Analysis for barriers part 2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:35:06 PM
   By task: Distribution Analysis for barriers part 2

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Sum_barriers_pttwo
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: sum analysis or barriers marry and education part 2. Sum_barriers_pttwo, MARRY12X, EDRECODE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
FOOTNOTE2 "by Shelby Gilyard";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Sum_barriers_pttwo;
	HISTOGRAM / 	CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	 
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis of Marry and Education   */
%LET _CLIENTTASKLABEL='Distribution Analysis of Marry and Education';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:35:06 PM
   By task: Distribution Analysis of Marry and Education

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X, T.EDRECODE
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0001(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: MARRY12X, EDRECODE";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by Shelby Gilyard";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR MARRY12X EDRECODE;
	HISTOGRAM   MARRY12X EDRECODE / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;
ODS GRAPHICS OFF;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan13.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan13.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 6:35:10 PM
   By task: Data Set Attributes

   Input Data: P:\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.CONTContentsFormeps_fullyr_2012);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA WORK.CONTContentsFormeps_fullyr_2012(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM WORK.CONTContentsFormeps_fullyr_2012
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.CONTContentsFormeps_fullyr_2012 OUT=WORK.CONTContentsFormeps_fullyr_2012;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.CONTContentsFormeps_fullyr_2012
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
