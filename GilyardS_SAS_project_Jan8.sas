/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Thursday, January 08, 2015     TIME: 9:26:39 AM
PROJECT: GilyardS_SAS_project_Jan8
PROJECT PATH: P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp
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

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan8.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\sgilyard" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Filter and Sort   */
%LET _CLIENTTASKLABEL='Filter and Sort';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan8.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.'Filter & Sort 1 shelby'n);

PROC SQL;
   CREATE TABLE MYDATA.'Filter & Sort 1 shelby'n(label="Filter & Sort 1 shelby") AS 
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
      FROM EC100030.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes1   */
%LET _CLIENTTASKLABEL='Data Set Attributes1';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan8.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:18:01 AM
   By task: Data Set Attributes1

   Input Data: Local:MYDATA.FILTER & SORT 1 SHELBY
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK."%STR(CONTContentsForFILTER _ SORT 1 S)"n);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.'FILTER & SORT 1 SHELBY'n OUT=WORK.SUCOUT1;

RUN;

DATA WORK.'CONTContentsForFILTER _ SORT 1 S'n(LABEL="Contents Details for FILTER %nrbquote(%)nrbquote(&) SORT 1 SHELBY");
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
			typemem LABEL="Data Set Type" FROM WORK.'CONTContentsForFILTER _ SORT 1 S'n
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='FILTER & SORT 1 SHELBY';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=WORK.'CONTContentsForFILTER _ SORT 1 S'n OUT=WORK.'CONTContentsForFILTER _ SORT 1 S'n;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.'CONTContentsForFILTER _ SORT 1 S'n
		WHERE memname='FILTER & SORT 1 SHELBY';
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\sgilyard\Assignments\GilyardS_SAS_project_Jan8.egp';
%LET _CLIENTPROJECTNAME='GilyardS_SAS_project_Jan8.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "P:\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 9:18:04 AM
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
