 /********************************************************************/
 /*                         A L L S C A N                            */
 /*                           Susan Chen                             */
 /*                                                                  */
 /*                      Date: July 19,  2011                        */
 /*                                                                  */
 /*  Purpose                                                         */
 /*  -------                                                         */
 /*  Macro ALLSCAN fully automates the variable scan of a data set.  */
 /*  It scans all variables of the input data set. The description   */
 /*  and method of a simple variable scan can be found in the        */
 /*  documentation of macro VARSCAN.                                 */
 /*                                                                  */
 /*  Method                                                          */
 /*  ------                                                          */
 /*  Macro ALLSCAN takes all variables of the input data set and     */
 /*  invokes macro VARSCAN for each of them.                         */
 /*                                                                  */
 /*  Parameters                                                      */
 /*  ----------                                                      */
 /*  DATA    = SAS data set name of the input data set. Default is   */
 /*            _LAST_.                                               */
 /*  FROMINT = number. The countinuous scan is executed for the      */
 /*            number of intervals defined by the user. The user     */
 /*            can define a range of intervals by setting the values */
 /*            of parameters FROMINT and TOINT. One scan is executed */
 /*            for each number of intervals between FROMINT and      */
 /*            TOINT. Default value is 10. If we want to request one */
 /*            scan only, FROMINT and TOINT must have the same       */
 /*            value. This parameter applies to all continuous scans */
 /*            macro ALLSCAN executes.                               */
 /*  TOINT   = number. See parameter FROMINT. Default value is 10.   */
 /*  CLASS   = the name of the binary variable with two values of    */
 /*            success and failure. The variable can either numeric  */
 /*            or character.                                         */
 /*  SUCCESS = value. The value of success for variable CLASS. If    */
 /*            the value is a character string, it should be         */
 /*            enclosed in quotation marks. The default value is     */
 /*            '1';                                                  */
 /*  MISSING = EXCLUDE or INCLUDE. It determines whether the missing */
 /*            value of VAR is excluded from or included in the      */
 /*            scan. Default value is EXCLUDE.                       */
 /*  TYPEOFN = CONT (continuous) or DISC (discrete). Determines the  */
 /*            type of scan for a numeric variables. If the type is  */
 /*            DISC and a numeric variable would yield too many      */
 /*            intervals, the macro repeats the scan with type of    */
 /*            CONT. All character variables are scanned with type   */
 /*            of DISC. Default value is TYPE=CONT.                  */
 /*  MAXINT  = number. It gives the maximum number of intervals      */
 /*            allowed for a continuous scan. Default is 100.        */
 /*  MAXDIFF = number. It determines the maximum difference allowed  */
 /*            between parameters TOINT and FROMINT. Default value   */
 /*            is 30.                                                */
 /*  FORMAT  = SAS format. Specifies the format of printing the      */
 /*            intervals of a continuous scan. The default value is  */
 /*            BEST11.                                               */
 /*  DISCSEQ = ORIG or INDEX. Determines the printing sequence of    */
 /*            intervals of a discrete scan. If the parameter is     */
 /*            ORIG, the intervals are printed in the standard       */
 /*            order. If it is INDEX, the intervals are sorted and   */
 /*            printed by ascending indeces. This parameter applies  */
 /*            to all discrete scans.                                */
 /*                                                                  */
 /*  SPECIALF = Y or N.  Determines if user format, SPEC., is used   */
 /*             on grouping.  The default value is N.                */
 /*             If Y, then SAS program in need in the folder         */
 /*                fmtspec.sas - to create format, SPEC.             */
 /*                                                                  */
 /*  REPORTDS = Y or N.  Specifies if report data set to be created. */
 /*             The default value is N.                              */
 /*                                                                  */
 /*  RPTOUT   = Report data library                                  */
 /*             RPTOUT.NUMVARS for numeric variables                 */
 /*             RPTOUT.CHARVARS for character variables              */
 /*                                                                  */
 /*  CHARTTYP = Type of chart                                        */
 /*             I for Index                                          */
 /*             W for Weight Of Evidence                             */
 /*                                                                  */
 /*  USERFMTS = Y/N - specify whether user-defined format is used    */
 /*             If Y, one SAS dataset is needed in WORK directory:   */
 /*               varlist - contains the variable name (VARNAME)     */
 /*                          and corresponding format (FMTNAME2)     */
 /*                                                                  */
 /*  WGT      = Variable name of weight in the dataset               */
 /*             The default value is 1 (no weight is used)           */
 /*                                                                  */
 /*  Location                                                        */
 /*  --------                                                        */
 /*  Macro ALLSCAN is a member of data set 'VSG27.SAS.MACRO'.        */
 /*  Member name is ALLSCAN.                                         */
 /*                                                                  */
 /*  Example                                                         */
 /*  -------                                                         */
 /*                                                                  */
 /*    //VSG27SCN JOB...                                             */
 /*    //MACRO    DD DSN=VSG27.SAS.MACRO,DISP=SHR,UNIT=SYSDA,        */
 /*    //         VOL=SER=VEN005                                     */
 /*    //STEP     EXEC SAS,OPTIONS='MACRO'                           */
 /*    //SAS.SYSIN DD *                                              */
 /*     %INCLUDE MACRO(ALLSCAN) / NOSOURCE2;                         */
 /*     DATA A;                                                      */
 /*          ARRAY NN(I) N1-N10;                                     */
 /*          DO I=1 TO 1000;                                         */
 /*             DO J=1 TO 10;                                        */
 /*                NN(J)=RANUNI(12345);                              */
 /*             END;                                                 */
 /*             IF RANUNI(12345)<0.5 THEN RESP='1'; ELSE RESP='0';   */
 /*             OUTPUT; DROP I J;                                    */
 /*          END;                                                    */
 /*     %ALLSCAN(CLASS=RESP)                                         */
 /*    //                                                            */
 /*                                                                  */
 /*  This example scans all variables (N1-N10) of data set A with    */
 /*  variable RESP as CLASS variable.                                */
 /*                                                                  */
 /*  Usage notes                                                     */
 /*  -----------                                                     */
 /*  1. The macro works only if the WORK files are random access     */
 /*     (disk) files. Unless the user defines the WORK files         */
 /*     differently, they are random access files.                   */
 /*  2. Always specify DISP=SHR in your DD statement defining data   */
 /*     set VSG27.SAS.MACRO !                                        */
 /*  3. The %INCLUDE statement must preceed the macro call!          */
 /*                                                                  */
 /* ### 10/13/2013 - added pprint and gencode options.               */ 
 /*------------------------------------------------------------------*/
 %MACRO ALLSCAN(DATA=_LAST_,FROMINT=10,TOINT=10,CLASS=,GOOD='0',BAD='1',
                MISSING=EXCLUDE,MAXINT=100,MAXDIFF=30,FORMAT=BEST11.,
                TYPEOFN=CONT,DISCSEQ=ORIG,RPTOUT=OUT,SPECIALF=N,SPECFMTLOC=,
                REPORTDS=N,CHARTTYP=I,USERFMTS=N,WGT=1,PPRINT=,GENCODE=)  ;
  
 
                
  %if %upcase(&specialf) = Y %then %do;
  	%if %length(&specfmtloc) = 0 %then %do;
  		   %put ERROR: MISSING CLASS SPECFMTLOC;
  		   %goto finish;
    %end;
    %include "&specfmtloc";
  %end;
        
  data varnames;   * <-- name changed 8/28/2014 to accomodate larger variable lists (temp datanames >9999);
   length variable $32
          desc     $32
          sinfoval   8
          pagenum    8
	      vtype      8
          rc       $18;
   if _n_>1 then output;
  run;
  %let varsindex=&syslast; 

  *OPTION DQUOTE;
  %global inputds;
  %if %length(&class)=0
  %then %do;
           %put ERROR: MISSING CLASS ARGUMENT.;
           %put ERROR: MACRO ALLSCAN STOPS.;
           %goto finish;
        %end;
        
        
  %if %upcase(&REPORTDS) = Y and %length(&rptout) = 0 %then %do;
           %put ERROR: ARGUMENT 'RPTOUT' IS INCORRECT.;
           %put ERROR: IT SHOULD HAVE A VALUE WHEN 'REPORTDS' IS 'Y'.;
           %put ERROR: MACRO ALLSCAN STOPS.;
           %goto finish;  	 	
  %end;          
  %if %upcase(&gencode) = Y and %upcase(&REPORTDS) ne Y %then %do;
           %put ERROR: ARGUMENT 'REPORTDS' IS INCORRECT.;
           %put ERROR: IT SHOULD BE 'Y' WHEN 'GENCODE' IS 'Y'.;
           %put ERROR: MACRO ALLSCAN STOPS.;
           %goto finish;  	 	
  %end;      
  %if &missing ne EXCLUDE and &missing ne INCLUDE
  %then %do;
           %put ERROR: ARGUMENT 'MISSING' IS INCORRECT.;
           %put ERROR: IT SHOULD BE 'EXCLUDE' OR 'INCLUDE'.;
           %put ERROR: MACRO ALLSCAN STOPS.;
           %goto finish;
        %end;
   %if &fromint>&toint
   %then %do;
            %let HELP=&FROMINT;
            %let FROMINT=&TOINT;
            %let TOINT=&HELP;
         %end;
   %if &toint>&maxint 
   %then %do;
            %put ERROR: THE SPECIFIED NUMBER OF INTERVALS EXCEEDS THE MAXIMUM.;
            %put ERROR: THE MAXIMUM IS &MAXINT, SPECIFIED BY PARAMETER MAXINT.;
            %put ERROR: MACRO ALLSCAN STOPS.;
            %goto finish; 
         %end;
   %let k=%eval(&toint-&fromint+1);
   %if &k>&maxdiff
   %then %do;
            %put ERROR: THE DIFFERENCE BETWEEN THE 'FROM' NUMBER OF INTERVALS;
            %put ERROR: AND THE 'TO' NUMBER OF INTERVALS EXCEEDS THE MAXIMUM.;
            %put ERROR: THE MAXIMUM IS &MAXDIFF, SPECIFIED BY PARAMETER MAXDIFF.;
            %put ERROR: MACRO ALLSCAN STOPS.;
            %goto finish; 
         %end;
   %if &discseq ne ORIG and &discseq ne INDEX
   %then %do;
            %put ERROR: ARGUMENT 'DISCSEQ' IS INCORRECT.;
            %put ERROR: IT SHOULD BE 'ORIG' OR 'INDEX'.;
            %put ERROR: MACRO ALLSCAN STOPS.;
            %goto finish;
         %end;
    %if &userfmts=Y and %sysfunc(exist(work.varlist))=0 
   %then %do;
   			%put ERROR: OPTION 'Y' SPECIFIED FOR ARGUMENT 'USERFMTS'.;
			%put ERROR: HOWEVER VARLIST DATASET IS NOT FOUND.;
			%put ERROR: VARLIST DATASET IS REQUIRED WHEN USERFMTS=Y.;
			%put ERROR: MACRO ALLSCAN STOPS.;
			%goto finish;
   %end;
   %let classnm=%str(%'&class%');
   /*---Obtain the name of the input data set: variable &DATA.-------*/
   %if &data eq _last_
   %then %do;
            %let dsn1=%substr(&sysdsn,1,8);
            %let dsn2=%substr(&sysdsn,9);
            %let data=&dsn1 . &dsn2;
         %end;

   proc contents data=&data noprint out=varcont; /* work.varlist */  * <-- name changed 8/28/2014 to accomodate larger variable lists;
   run;
   %let ds7=%substr(&sysdsn,9);

   %if %upcase(&reportds)=Y
   %then %do;
            data _null_;
             retain maxchlen 0;
             set &ds7 end=bye;
             if type=2
             then do;
                    if maxchlen<length then maxchlen=length;
                                        /**if maxchlen>8 then maxchlen=8;
                                        if maxchlen<3 then maxchlen=3;**/
                  end;
             if bye
             then do;
                    file log;
                     put @5 'Maximum length for character variable ===> ' maxchlen 3.;
                    call symput('maxchlen',left(put(maxchlen,8.)));
                  end;
         %end;

   %if %upcase(&userfmts)=Y
   %then %do;
			  data varlist2; set varlist ;
   						length fmtname2 $32 ;
						fmtname=upcase(fmtname) ;
   				if type='C' then do;
    					fmtname2=compress('$'||upcase(fmtname)||'.'); end;
   				else do;
						fmtname2=compress(upcase(fmtname)||'.') ; end;
	

			proc sort data=varlist2 nodupkey ; by varname; 
      proc sort  data=&ds7 ;
			 by name;
			run;
 
            data &ds7;
             merge &ds7(in=in1)
                   varlist2(keep = varname fmtname2 rename=(varname=name) in=in2) end=bye;
             by name; if in1=1 ;
             retain read1 read2 match 0;
             drop read1 read2 match;
             if in1 then read1=read1+1;
             if in2 then read2=read2+1;
             if in1
             then do;
                    if in2 then match=match+1;
                           else fmtname2=' ';
                    output;
                  end;
             if bye
             then do;
                    file log;
                     put @5 '# of In1 records read ' read1 comma10.;
                     put @5 '# of in2 records read ' read2 comma10.;
                     put @5 '# of matches          ' match comma10.;
                  end;
         %end;
   %else %do;
            data &ds7;
             set &ds7;
             fmtname2=' ';
         %end;
         
         
         
   %let wgtnm=%str(%'&wgt%');
   %let data1=%substr(&sysdsn,9);
   %let nv=0;
   %let existcl=NO;
   %let existwgt=NO;
   /*---Check if the specified CLASS variable is part of the input---*/
   /*---data set. Place the names of all variables (except CLASS)----*/
   /*---into macro variables &Vi. Put their types into macro---------*/
   /*---variables &Ti. &NV holds the number of variables to be-------*/
   /*---scanned.-----------------------------------------------------*/

   *roc print data=work. &data1(obs=100);
    *itle 'work.data1';

   data _null_;
    set work. &data1 end=bye;
    retain n 0;
    if name=%unquote(&classnm)
    then call symput('existcl','YES');
	else if name=%unquote(&wgtnm)
	     then call symput('existwgt','YES');
	     else do;
                userfmt=fmtname2;
                n=n+1;
                var='V'||put(n,6.0);
                var=compress(var);
                call symput(var,name);
                var='T'||put(n,6.0);
                var=compress(var);
                call symput(var,put(TYPE,2.0));
                var2='UF' || put(n,6.0);
                var2=compress(var2);
                call symput(var2,userfmt);
                call symput('NV',put(n,8.0));
              end;
   run;

   %if %length(&wgt)>0 
   %then %do;
            %if &existwgt=NO
			%then %do;
                     %put **************************************;
		             %put *** weight variable &wgt not found ***;
		             %put **************************************;
				  %end;
            %else %do;
                     %put ************************************;
	                 %put *** weight variable &wgt is used ***;
	                 %put ************************************; 
                  %end;
         %end; 

   /*---Stop if the CLASS variable is not found or there is no-------*/
   /*---variable to be scanned.--------------------------------------*/

   %if &existcl=NO
   %then %do;
            %put ERROR: THE SPECIFIED CLASS VARIABLE IS NOT IN THE DATA SET.;
            %put ERROR: MACRO ALLSCAN STOPS.;
            %goto finish;
         %end;
   %if &nv=0
   %then %do;
            %put ERROR: NO VARIABLE IN THE INPUT DATA SET TO SCAN.;
            %put ERROR: MACRO ALLSCAN STOPS.;
            %goto finish;
         %end;

   /*---Invoke macro VARSCAN for each variable of the input data-----*/
   /*---set. Set the type of scan for each variable.-----------------*/
   options pageno=1;
   %do i=1 %to &nv;
       %let varnum=&i;
       %put NOTE: VARIABLE SCAN #&i FOR VARIABLE &&v&i.;
       %if &&t&i=1 %then %do; %let type=&typeofn; %end;
                   %else %do; %let type=DISC; %end;
       %varscan(data=&data,
                fromint=&fromint,
                toint=&toint,
                class=&class,
                good=&good,
                bad=&bad,
                missing=&missing,
                maxint=&maxint,
                maxdiff=&maxdiff,
                format=&format,
                type=&type,
                var=&&v&i,
                discseq=&discseq,
                calltype=A,
                userfmt=&&UF&i,
                pprint=&pprint);
   %END;
   %if %upcase(&reportds)=Y
   %then %do;
   	
   	    %if %sysfunc(exist(work.&inputds.c)) ne 0 %then %do;  * Sometimes there are no char variables;
            data &rptout..charvars;
             set work.&inputds.c(rename=(interv=col1
                                         prtintrv=col2
                                         n=col3
                                         distper=col4
                                      /*   u=col5*/
                                         distper2=col6
                                         rate=col7
                                         woe=col8
                                         chisq=col9
                                         p=col10
                                         infoval=col11
                                         ks=col12
                                         prtindx=col13));
             label col1='Interval #'
                   col2='Interval'
                   col3='# of Obs'
                   col4='Distribution % of base'
                   col5='# of Resps'
                   col6='Distribution % of Resp'
                   col7='Resp Rate'
                   col8='Weight of Evidence'
                   col9='Chi Square'
                   col10='Probability'
                   col11='Information Value'
                   col12='K-S'
                   col13='Index'
                   ;
            %end;
            data &rptout..numvars;
             set work.&inputds.n(rename=(interv=col1
                                         prtintrv=col2
                                         n=col3
                                         distper=col4
                                         good=col5
                                         distper2=col6
                                         rate=col7
                                         woe=col8
                                         chisq=col9
                                         p=col10
                                         infoval=col11
                                         ks=col12
                                         prtindx=col13));
             label col1='Interval #'
                   col2='Interval'
                   col3='# of Obs'
                   col4='Distribution % of base'
                   col5='# of Resps'
                   col6='Distribution % of Resp'
                   col7='Resp Rate'
                   col8='Weight of Evidence'
                   col9='Chi Square'
                   col10='Probability'
                   col11='Information Value'
                   col12='K-S'
                   col13='Index'
                   ;
         %end;
   *proc print data=&varsindex;
   data _data_;
    set &varsindex;
	drop pagenum lastpage;
	retain lastpage 0 pstart pend 0;
    if _n_=1 
    then do;
           if pagenum>.
           then do;
		          pstart=1;
                  pend=pagenum;
		          lastpage=pagenum;
                end;
		 end;
    else do;
	       if pagenum>.
           then do; 
	              pstart=lastpage+1;
		          pend=pstart+(pagenum-1);
		          lastpage=pend;
				end;
		   else do;
		          pstart=.;
				  pend=.;
		        end; 
	     end;  
   run;
   %let countpage=&syslast;
   proc print data=&countpage noobs label;
    %if %length(&wgt)>0
	%then %do;
	         %if &existwgt=YES
			 %then %do;
			          %let wgtcomment=/ Weight variable &wgt is used;
			       %end; 
			 %else %do;
			          %let wgtcomment=/ Weight variable &wgt is not found;
			       %end;
	      %end;
	%else %do;
	         %let wgtcomment=;
	      %end;
    title "Variables Index Page &wgtcomment";
    var variable desc sinfoval vtype pstart pend rc;
	label variable='Variable'
	      desc='Description'
          sinfoval='Info Value'
		  vtype='Interval'
          pstart='From Page'
          pend='To Page'
          rc='Status';

   proc sort data=&countpage; by descending sinfoval; run;
   proc print data=&countpage noobs label;
    %if %length(&wgt)>0
	%then %do;
	         %if &existwgt=YES
			 %then %do;
			          %let wgtcomment=/ Weight variable &wgt is used;
			       %end; 
			 %else %do;
			          %let wgtcomment=/ Weight variable &wgt is not found;
			       %end;
	      %end;
	%else %do;
	         %let wgtcomment=;
	      %end;
    title "Variables Index Page by Info Value &wgtcomment";
    var variable desc sinfoval vtype pstart pend rc;
	label variable='Variable'
	      desc='Description'
          sinfoval='Info Value'
		  vtype='Interval'
          pstart='From Page'
          pend='To Page'
          rc='Status';
 %finish:;
 
  
 
   %if %upcase(&gencode) = Y %then %do;
   	 * Create woe waterfall if requested - added 10/10/2014;
   	 
   	 
   	
    proc sort data=&rptout..numvars out=numvars_woe nodupkey;
       by variable col1 col8;
    run;

    options pagesize=10000;
   	 
    data _null_;
      set numvars_woe;
      by variable;
      ln=-1;
      file print;
	
	    if _n_= 1 then do; put;put;put;end;
          
      lag_r2 = lag(range2);

      if first.variable then do;
         put ' ';
         if range1 = range2 then put '       if ' variable  @44 ' = .' @66 ' then ' @73 variable  +ln '_w = ' @107 col8 ';';
         else
         put '       if ' variable  @44 ' <= ' range2 @66 ' then ' @73  variable   +ln '_w = ' @107 col8 ';';
      end;
      else
       if last.variable then put ' else  if ' variable @44 ' >  ' lag_r2  @66 ' then '  @73 variable  +ln '_w = ' @107 col8 ';';
      else
       put  ' else  if ' variable @44 ' <= ' range2 @66 ' then ' @73 variable  +ln  '_w = ' @107 col8 ';';

    run;   
   
    options pagesize=60;
   	
   %end;
   
   
 
 
 
 
 
 %mend;
