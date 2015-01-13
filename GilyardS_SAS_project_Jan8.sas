/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 2:34:17 PM
PROJECT: GilyardS_SAS_project_Jan14
PROJECT PATH: P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\sgilyard' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA BASE 'P:\QAC\qac200\students\sgilyard' ;


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
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA BASE "P:\QAC\qac200\students\sgilyard" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder   */
%LET _CLIENTTASKLABEL='Query Builder';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2 AS 
   SELECT t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t2.DUID, 
          t2.PID, 
          t2.INER AS INER1, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F AS PERWT12F1, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.INER, 
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
          t1.MARRY12X_re, 
          t1.EDRECODE_re, 
          t1.INFULLYR
      FROM MYDATA.QUERY_FOR_SUBSET_CODEBOOK_2 t1
           FULL JOIN MYDATA.QUERY_FOR_MEPS_ER_2012_SAS7BDAT t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t1.INFULLYR = 1 AND t2.INER = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: List Data   */
%LET _CLIENTTASKLABEL='List Data';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:02 PM
   By task: List Data

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.DUPERSID, T.DUPERSID1, T.INER1
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2 as T
;
QUIT;
TITLE;
TITLE1 "Report Listing Check merging";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))by shelby gilyard";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR DUPERSID DUPERSID1 INER1;
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:02 PM
   By task: Data Set Attributes

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=WORK.QUERY_FOR_SUBSET_CODEBOOK_2 OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_(LABEL="Contents Details for QUERY_FOR_SUBSET_CODEBOOK_2");
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
			typemem LABEL="Data Set Type" FROM MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='QUERY_FOR_SUBSET_CODEBOOK_2';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_ OUT=MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.CONTCONTENTSFORQUERY_FOR_SUBSET_
		WHERE memname='QUERY_FOR_SUBSET_CODEBOOK_2';
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


/*   START OF NODE: Query Builder1   */
%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0002);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0002 AS 
   SELECT /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID, 
          t1.DUPERSID AS DUPERSID1
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2 t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder2   */
%LET _CLIENTTASKLABEL='Query Builder2';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003 AS 
   SELECT t2.COUNT_of_DUPERSID, 
          t2.DUPERSID1, 
          t1.DUPERSID, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.DUID, 
          t1.PID, 
          t1.INER1, 
          t1.DUPERSID1 AS DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
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
          t1.MARRY12X_re, 
          t1.EDRECODE_re, 
          t1.INFULLYR, 
          /* XRAYS_R */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 3
               ELSE t1.XRAYS
            END) LABEL="xrays recoded" AS XRAYS_R, 
          /* MRI_R */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 3
               ELSE t1.MRI
            END) LABEL="mri recoded" AS MRI_R
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2 t1
           INNER JOIN WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0002 t2 ON (t1.DUPERSID1 = t2.DUPERSID1);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:03 PM
   By task: One-Way Frequencies

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT,
		WORK.OneWayFreqOfXRAYS_RInQUERY_FOR_S,
		WORK.OneWayFreqOfMRI_RInQUERY_FOR_SUB);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAYS_R, T.MRI_R
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003 as T
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
	TABLES XRAYS_R / 	OUT=WORK.OneWayFreqOfXRAYS_RInQUERY_FOR_S(LABEL="Cell statistics for XRAYS_R analysis of WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003")
 SCORES=TABLE;
	TABLES MRI_R / 	OUT=WORK.OneWayFreqOfMRI_RInQUERY_FOR_SUB(LABEL="Cell statistics for MRI_R analysis of WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003")
 SCORES=TABLE;
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


/*   START OF NODE: Query Builder3   */
%LET _CLIENTTASKLABEL='Query Builder3';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0004);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0004 AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003 t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query Builder4   */
%LET _CLIENTTASKLABEL='Query Builder4';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005 AS 
   SELECT t2.DUPERSID, 
          t2.COUNT_of_DUPERSID, 
          t1.COUNT_of_DUPERSID AS COUNT_of_DUPERSID1, 
          t1.DUPERSID1, 
          t1.DUPERSID AS DUPERSID2, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.DUID, 
          t1.PID, 
          t1.INER1, 
          t1.DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
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
          t1.MARRY12X_re, 
          t1.EDRECODE_re, 
          t1.INFULLYR, 
          t1.XRAYS_R, 
          t1.MRI_R
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0003 t1
           INNER JOIN WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0004 t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Distribution Analysis   */
%LET _CLIENTTASKLABEL='Distribution Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:04 PM
   By task: Distribution Analysis

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
ODS GRAPHICS ON;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005 as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID frequency table and histogram";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))by shelby gilyard";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
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


/*   START OF NODE: Query Builder5   */
%LET _CLIENTTASKLABEL='Query Builder5';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006);

PROC SQL;
   CREATE TABLE WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006 AS 
   SELECT /* ERVISITS_catvar */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID =1
               THEN 1
              WHEN t1.COUNT_of_DUPERSID >=2 and t1.COUNT_of_DUPERSID <=4
               THEN 2
              WHEN t1.COUNT_of_DUPERSID >=5 and t1.COUNT_of_DUPERSID <=15
               THEN 3
            END) LABEL="categorical variables for ER" AS ERVISITS_catvar, 
          t1.DUPERSID, 
          t1.COUNT_of_DUPERSID, 
          t1.COUNT_of_DUPERSID1, 
          t1.DUPERSID1, 
          t1.DUPERSID2, 
          t1.AGE12X, 
          t1.SEX, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.REGION12, 
          t1.CANCERDX, 
          t1.CALEUKEM, 
          t1.DUID, 
          t1.PID, 
          t1.INER1, 
          t1.DUPERSID11, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F1, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.INER, 
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
          t1.MARRY12X_re, 
          t1.EDRECODE_re, 
          t1.INFULLYR, 
          t1.XRAYS_R, 
          t1.MRI_R
      FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0005 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Table Analysis   */
%LET _CLIENTTASKLABEL='Table Analysis';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:05 PM
   By task: Table Analysis

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.ERVISITS_catvar
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006 as T
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
	TABLES ERVISITS_catvar * COUNT_of_DUPERSID /
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


/*   START OF NODE: One-Way Frequencies1   */
%LET _CLIENTTASKLABEL='One-Way Frequencies1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan14.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan14.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 2:31:06 PM
   By task: One-Way Frequencies1

   Input Data: Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID, T.ERVISITS_catvar
	FROM WORK.QUERY_FOR_SUBSET_CODEBOOK_2_0006 as T
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
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
	TABLES ERVISITS_catvar /  SCORES=TABLE;
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
