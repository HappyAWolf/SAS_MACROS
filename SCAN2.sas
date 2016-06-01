 /********************************************************************/
 /*                         V A R S C A N                            */
 /*                      Susan Chen(revised)                         */
 /*                                                                  */
 /*                    Date: July 19, 2011                           */
 /*                                                                  */
 /*  Purpose                                                         */
 /*  -------                                                         */
 /*  Macro VARSCAN executes variable scan. The scan serves two       */
 /*  purposes:                                                       */
 /*     - To examines the discriminating power of a variable         */
 /*       on a second, binary variable.                              */
 /*     - To transform a countinuous variable into a discrete one.   */
 /*                                                                  */
 /*  Method                                                          */
 /*  ------                                                          */
 /*  Macro VARSCAN examines two variables given by parameters VAR    */
 /*  and CLASS. Variable VAR is the variable to be scanned and       */
 /*  variable CLASS is a binary variable with two possible values    */
 /*  (succes and failure). The scan has two types.                   */
 /*  - Continuous scan: The macro groups the values of VAR into a    */
 /*                     given number of intervals by attempting to   */
 /*                     assign the same number of observations to    */
 /*                     each interval. If it is not possible, the    */
 /*                     number of observations in each interval is   */
 /*                     as similar as possible.                      */
 /*  - Discrete scan: The macro places the observations into as many */
 /*                   groups as the number of different values.      */
 /*  After the intervals are set up, the macro counts the number of  */
 /*  successes in each interval determined by variable CLASS. Then   */
 /*  the success rate and the success index (individual success rate */
 /*  divided by overall success rate) are calculated along with the  */
 /*  chi-square value to test if the success rate of an interval     */
 /*  differs from the overall success rate. Finally, the success     */
 /*  indeces are represented in a form of a histogram.               */
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
 /*            value.                                                */
 /*  TOINT   = number. See parameter FROMINT. Default value is 10.   */
 /*  VAR     = the name of the variable to be scanned. The variable  */
 /*            can be either numeric or character.                   */
 /*  CLASS   = the name of the binary variable with two values of    */
 /*            success and failure. The variable can either numeric  */
 /*            or character.                                         */
 /*  SUCCESS = value. The value of success for variable CLASS. If    */
 /*            the value is a character string, it should be         */
 /*            enclosed in quotation marks. The default value is     */
 /*            '1'.                                                  */
 /*  MISSING = EXCLUDE or INCLUDE. It determines whether the missing */
 /*            value of VAR is excluded from or included in the      */
 /*            scan. Default value is EXCLUDE.                       */
 /*  TYPE    = CONT (continuous) or DISC (discrete). Determines the  */
 /*            type of scan. A numeric variable can be scanned with  */
 /*            both types. If a discrete scan of a numeric variable  */
 /*            would yield too many intervals, the macro changes the */
 /*            type to countinuous and scans the variable with the   */
 /*            default number of intervals (10 or MAXINT, whichever  */
 /*            is smaller). A character variable can be scanned with */
 /*            discrete type only. If TYPE=CONT is specified, the    */
 /*            macro changes it to TYPE=DISC. Default value is       */
 /*            TYPE=CONT.                                            */
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
 /*            printed by ascending indeces.                         */
 /*  CALLTYPE= V/A - V means only calling VARSCAN/A means calling    */
 /*            ALLSCAN (VARSCAN imbedded).                           */
 /*  USERFMT = blank/Y - blank means no user-defined format.         */
 /*                                                                  */
 /*  Location                                                        */
 /*  --------                                                        */
 /*  Macro VARSCAN is a member of data set 'VSG27.SAS.MACRO'.        */
 /*  Member name is VARSCAN.                                         */
 /*                                                                  */
 /*  Example                                                         */
 /*  -------                                                         */
 /*                                                                  */
 /*    //VSG27SCN JOB...                                             */
 /*    //MACRO    DD DSN=VSG27.SAS.MACRO,DISP=SHR,UNIT=SYSDA,        */
 /*    //         VOL=SER=VEN005                                     */
 /*    //STEP     EXEC SAS,OPTIONS='MACRO'                           */
 /*    //SAS.SYSIN DD *                                              */
 /*     %INCLUDE MACRO(VARSCAN) / NOSOURCE2;                         */
 /*     DATA A;                                                      */
 /*          DO I=1 TO 1000;                                         */
 /*             N=RANUNI(12345);                                     */
 /*             IF RANUNI(12345)<N THEN RESP='1'; ELSE RESP='0';     */
 /*             OUTPUT;                                              */
 /*          END;                                                    */
 /*     %VARSCAN(VAR=N,CLASS=RESP)                                   */
 /*    //                                                            */
 /*                                                                  */
 /*  This example scans variable N with variable RESP as CLASS       */
 /*  variable. The scan will be continuous because N is a numeric    */
 /*  variable. The number of intervals will be 10 which is the       */
 /*  default.                                                        */
 /*                                                                  */
 /*  Usage notes                                                     */
 /*  -----------                                                     */
 /*  1. The macro works only if the WORK files are random access     */
 /*     (disk) files. Unless the user defines the WORK files         */
 /*     differently, they are random access files.                   */
 /*  2. Always specify DISP=SHR in your DD statement defining data   */
 /*     set VSG27.SAS.MACRO !                                        */
 /*  3. The %INCLUDE statement must preceed the macro call!          */
 /*  4. If you want to print the result with the HP LaserJet 2000    */
 /*     printer (designated as VSGRP4), direct the output to an      */
 /*     external file using PROC PRINTTO and set OPTION PS=46.       */
 /*     Before printing the file, you have to add the necessary      */
 /*     print control characters to the file: copy file              */
 /*     'VSG27.SAS.LIST(VSGRP4)' at the beginning of the file.       */
 /*                                                                  */ 
 /*                                                                  */
 /* Updates:                                                         */
 /* ### 07/01/2013 - changed report format                           */  
 /* ### 07/23/2013 - changed report output to include:               */
 /* ###  Interval                           # of      # of       # of                                     Resp    Resp           Info                         */
 /* ###      #          Interval            Good       Bad      Indeter   Total  % Good   % Bad  % Total  Rate    Rate2    WOE   Value    K-S     WOE Chart   */
 /* ### 10/30/2013 - added linesize=160 and j=144                    */
 /* ### 10/13/2014 - added proc print option. (pprint)               */
 /*------------------------------------------------------------------*/
 %macro varscan(data=_last_,
                fromint=10,
                toint=10,
                var=,
                class=,
                good='0',
					      bad='1',
                missing=EXCLUDE,
                type=CONT,
                maxint=100,
                maxdiff=30,
                format=BEST11.,
                discseq=ORIG,
                calltype=V,
                userfmt=,
                pprint=) ;
 *options linesize=160;
 
 
          %put  fromint=&fromint;
          %put  toint=&toint;
          %put  class=&class;
          %put  good=&good;
          %put  bad=&bad;
          %put  missing=&missing;
          %put  maxint=&maxint;
          %put  maxdiff=&maxdiff;
          %put  format=&format;
          %put  type=&type;
          %put  var=&&v&i;
          %put  discseq=&discseq;
          %put  calltype=A;
          %put  userfmts=&userfmts;
          %put  userfmt=&&UF&i); 
 

 
 
 /*---Check the macro parameters.----------------------------------*/
  %if %length(&var)=0 or %length(&class)=0
  %then %do;
           %put ERROR: MISSING VAR OR CLASS ARGUMENT.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %end;
  %if &missing ne EXCLUDE and &missing ne INCLUDE
  %then %do;
           %put ERROR: ARGUMENT 'MISSING' IS INCORRECT.;
           %put ERROR: IT SHOULD BE 'EXCLUDE' OR 'INCLUDE'.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %end;
  %if &type ne CONT and &type ne DISC
  %then %do;
           %put ERROR: ARGUMENT 'TYPE' IS INCORRECT.;
           %put ERROR: IT SHOULD BE 'DISC' OR 'CONT'.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %END;
  %if &fromint>&toint
  %then %do;
           %let help=&fromint;
           %let fromint=&toint;
           %let toint=&help;
        %end;
  %if &toint>&maxint
  %then %do;
           %put ERROR: THE SPECIFIED # OF INTERVALS EXCEEDS THE MAXIMUM.;
           %put ERROR: THE MAXIMUM IS &MAXINT, SPECIFIED BY PARAMETER MAXINT.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %end;
  %if &fromint<1 AND  &fromint NE -1
  %then %do;
           %put ERROR: THE SPECIFIED NUMBER OF INTERVALS IS INVALID.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %end;
  %let k=%eval(&toint-&fromint+1);
  %if &K>&MAXDIFF
  %then %do;
           %put ERROR: THE DIFFERENCE BETWEEN THE 'FROM' # OF INTERVALS;
           %put ERROR: AND THE 'TO' # OF INTERVALS EXCEEDS THE MAXIMUM.;
           %put ERROR: THE MAXIMUM IS &MAXDIFF, SPECIFIED BY PARAMETER MAXDIFF;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %end;
  %if &discseq ne ORIG and &discseq ne INDEX
  %then %do;
           %put ERROR: ARGUMENT 'DISCSEQ' IS INCORRECT.;
           %put ERROR: IT SHOULD BE 'ORIG' OR 'INDEX'.;
           %put ERROR: MACRO VARSCAN STOPS.;
           %goto FINISH;
        %END;
  %let varname=%str(%'&var%');




  /*---Obtain the name of the input data set: variable &DATA.-------*/
  %if &data eq _last_
  %then %do;
           %let dsn1=%substr(&sysdsn,1,8);
           %let dsn2=%substr(&sysdsn,9);
           %let data=&dsn1 . &dsn2;
        %end;
  /*---Take the type, length and label of the variable to be--------*/
  /*---scanned.-----------------------------------------------------*/
  /*------&VTYPE holds the type of variable VAR.--------------------*/
  /*------&KNGTH holds the length of variable VAR.------------------*/
  /*------&LABEL holds the label of variable VAR. If the variable---*/
  /*-------------has no label, &LABEL holds the variable name.------*/
  %if %upcase(&calltype)=V
  %then %do;
           proc contents data=&data noprint out=&ds7;
           run;
        %end;

  %let vtype=0; 
  %let label=&var;
  data _null_;
   *SET WORK.VARLIST;
   set &ds7;
   if _n_=1
   then do;
          call symput('inputds',trim(substr(memname,1,7)));
        end;
   if name=%unquote(&varname)
   then do;
          call symput('vtype',put(TYPE,4.0));
          call symput('lngth',put(LENGTH,4.0));
          if LABEL ne ' '
          then do;
                 call symput('label',LABEL);
                 call symput('xlabel','YES');
               end;
          else call symput('xlabel','NO');
        end;
  run;
  /*---Set up a data set (WORK.&DATA2) of two variables only: VAR---*/
  /*---and CLASS. Variable VAR is the value of the variable to be---*/
  /*---scanned and the value of CLASS is 1 for success and 0 for----*/
  /*---failure.-----------------------------------------------------*/
  data _data_;
   %if &existwgt=YES
   %then %do;
            set &data(keep=&var &class &wgt rename=(&var=var &class=oldclass)) end=eof;
			keep var class &wgt;
         %end;
   %else %do;
            set &data(keep=&var &class rename=(&var=var &class=oldclass)) end=eof;
			keep var class;
		 %end;
   length class 3;
   retain count 0;
   select;
    when(oldclass in (&good)) class=0;                                    
	when(oldclass in (&bad)) class=1;
	otherwise class=3;
   end;
   %if %upcase(&missing)=EXCLUDE
   %then %do;
            if var ne %if &vtype=1 %then %do;  .  %end;
                                   %else %do; ' ' %end;
            then do;
                   count=count+1;
                   output;
                 end;
         %end;
   %else %do;
            count=count+1;
            output;
         %end;
   if eof
   then do;
          if count=0 then call symput('emptyds','YES');
                     else call symput('emptyds','NO');
        end;
  RUN;
  

  %let data1=%substr(&sysdsn,9);
  %let delfrom=&data1;

  %let data2=%substr(&sysdsn,9);

  %if "&emptyds"="YES"
  %then %do;
           %let data3=%substr(&sysdsn,9);
           %let ok=EMPTY;
           %goto FINISH1;
        %END;

  /*---Sort the values of VAR to prepare for grouping. Place the----*/
  /*---number of observations into &NOBS.---------------------------*/

  %if &vtype=1
  %then %do;
           proc univariate data=work. &data2 noprint;
		     var var;
			%if &existwgt=YES
			%then %do;
			         weight &wgt;
			      %end;
            output out=_data_ mean=vmean median=vmedian min=vmin max=vmax
                   std=vstd;
           run;
           %let data4=%substr(&sysdsn,9);
		   *proc print data=&data4(obs=100);
		    *title "DS -- &data4";
           data _null_;
            set &data4;
            call symput('vmean',put(vmean,best10.));
            call symput('vmin',put(vmin,best10.));
            call symput('vmax',put(vmax,best10.));
            call symput('vmedian',left(put(vmedian,best10.)));
            call symput('vstd',put(vstd,best10.));

           proc corr data=work. &data2 noprint outp=_data_ outs=_data_;
            var var;
            with class;
		   run;
           %let data5b=%substr(&sysdsn,9);
           %let t5a1=%substr(&data5b,1,4);
           %let t5a2=%substr(&data5b,5);
           %let t5a3=%eval(&t5a2-1);
           %let data5a=&t5a1&t5a3;
           data _null_;
            set &data5a;
            if _type_='CORR'
            then do;
                   call symput('corrp',left(put(var,10.6)));
                 end;
           data _null_;
            set &data5b;
            if _type_='CORR'
            then do;
                   call symput('corrs',left(put(var,10.6)));
                 end;
        %end;
  %else %do;
           data _null_;
            call symput('vmean','n/a');
            call symput('vmin','n/a');
            call symput('vmax',' ');
            call symput('vstd','n/a');
            call symput('corrp','n/a');
            call symput('corrs','n/a');
            call symput('vmedian','n/a');
        %end;

  proc sort data=WORK. &data2;
   title ' ';
   by var;
  run;

  data _null_;
   retain nobs 0; 
   set work. &DATA2 end=bye;
   %if &existwgt=YES
   %then %do;
            nobs=nobs+&wgt; 
         %end;
   %else %do;
            nobs=nobs+1;
         %end;
   if bye
   then do;
          call symput('nobs',put(nobs,8.0));
        end;
  run;
  /*---Change the scan type of DISC to CONT if it is needed. Set----*/
  /*---the value of &K. &K is the number of scans. It is 1 for------*/
  /*---a discrete scan and it is the difference of TOINT and--------*/
  /*---FROMINT for a countinuous scan.------------------------------*/
  %if %upcase(&type)=DISC or &fromint=-1 or &toint=-1 or &vtype=2
  %then %do;
           %let fromint=&nobs;
           %let toint=&nobs;
           %let k=1;
           %let type=DISC;
        %end;
  %else %let type=CONT;
  %let ok=YES;
  %repeat:;
  /*---Execute the grouping and count the number of successes. This-*/
  /*---data step creates a file called WORK.&DATA3. The variables---*/
  /*---of the file are----------------------------------------------*/
  /*------VTYPE   number of intervals of the scan,------------------*/
  /*------INTERV  interval number of the interval represented by te-*/
  /*------        the observation,----------------------------------*/
  /*------N       number of observations in the interval,-----------*/
  /*------SUCCESS number of successes in the interval,--------------*/
  /*------RANGE1  the low end of the interval,----------------------*/
  /*------RANGE2  the high end of the interval.---------------------*/     
  
 
  
  
  
  
  
  %if %length(&userfmt)=0
  %then %do; 
     %put USERFMT=&userfmt;
           data _data_;
            set work. &data2 end=eof;
            by var;
            array nint{*} nint1-nint&k;
            array nobs{*} nobs1-nobs&k;
            array succ{*} succ1-succ&k;
			      array fail{*} fail1-fail&k;                                                                               
			      array inde{*} inde1-inde&k;                                                                               
            array cutoff{*} cut1-cut&k;
            array minv{*} %if &vtype=2 %then %do; $ %end; &lngth
                             min1-min&k;
			%if &vtype=2
			%then %do;
			         length lastvar llastvar $ &lngth;
			      %end;
            %if %upcase(&reportds)=Y
            %then %do;
                     %if &vtype=2
                     %then %do;
                              length range1 range2 $ &maxchlen;
                           %end;
                  %end;
            retain;
            if _n_=1
            then do;
                   lastvar=var;
                   do i=1 to &k;
                      nint(i)=0;
                      nobs(i)=0;
                      succ(i)=0;                     
					            fail(i)=0;                                                                                             
					            inde(i)=0;                                                                                             
                      cutoff(i)=int(&nobs/(&fromint+i-1));
                   end;
                 end;
            do i=1 to &k;
               %if &vtype=1 and &specialf=Y
               %then %do;
                        special=put(var,spec.);
                        llastvar=lastvar;
                        if last.var then lastvar=var;
                     %end;
               %else %do;
                        special='0';
                     %end;
            if special='1' and first.var and nobs{i}>0
            then do;
                   specialb='1';
                   goto specbrch;
                 end;
            recount:
			%if &existwgt=YES
			%then %do;
			         nobs(i)=nobs(i)+&wgt;
					* succ(i)=succ(i)+(class*&wgt);
					 if nobs{i}=&wgt then minv{i}=var;
                     if class=&good then succ(i)=succ(i)+(1*&wgt);     
					 else
                     if class=&bad then fail(i)=fail(i)+(1*&wgt);
					 else 
                     inde(i)=inde(i)+(1*&wgt);
			      %end;
			%else %do;
                     nobs(i)=nobs(i)+1;
					 if class=&good then succ(i)=succ(i)+1;     
					 else
                     if class=&bad then fail(i)=fail(i)+1;
					 else 
                     inde(i)=inde(i)+1;
					 if nobs(i)=1 then minv(i)=var;
			      %end; 
            if (nobs(i) ge cutoff(i) and last.var) or
               (special='1' and last.var) or
               (eof)
            then do;
                   specbrch:
                   vtype=&fromint+i-1;
                   nint(i)=nint(i)+1;
                   interv=nint(i);
                   if interv>&maxint
                   then do;
                          call symput('ok','NO');
                          stop;
                        end;
                   n=nobs(i);
                   good=succ(i);
				  	       bad=fail(i);                                                                                        
				           indetr=inde(i);                                                                           
                   range1=minv(i);
                   if specialb='1'
                   then range2=llastvar;
                   else range2=var;
                   output;
                   keep vtype interv n good bad indetr range1 range2;
                   if specialb='1'
                   then do;
                          specialb='0';
                          nobs(i)=0;
                          succ(i)=0;        
                          fail(i)=0;                                                                         
                          inde(i)=0;                                                                         
                          goto recount;
                        end;
                   nobs(i)=0;
                   succ(i)=0;     
                   fail(i)=0;                                                                         
                   inde(i)=0;                                                                         
                   remndr=mod(&nobs,&fromint+i-1);
                   if (nint(i)+remndr)=(&fromint+i-1)
                   then cutoff(i)=cutoff(i)+1;
                 end;
            end;
            run;
        %end;
  %else %do;


 


     %put USERFMT=&userfmt;
           data _data_;
            set work. &data2 end=eof;
            length varlabel $40;
            varlabel=put(var,&userfmt);
           run;
           %let data5=%substr(&sysdsn,9);
           proc sort data=&data5;
            by varlabel;

           data _data_;
            keep interv n good bad varlabel indetr;
            retain interv 0 n 0 good 0 bad 0 indetr 0;
            set &data5 end=bye;
            by varlabel;
            if first.varlabel then interv=interv+1;
			%if &existwgt=YES
			%then %do;
			     n=n+&wgt;
					 *good=good+(1*&wgt);                     *????????????????????????????????????????????;
           if class=&good then good=good+(1*&wgt);     
					 else
           if class=&bad then class=&bad then bad=bad+(1*&wgt);
					 else 
            indetr=indetr+(1*&wgt);
			%end;
			%else %do;
			     n=n+1;
					 *good=good+1;
			     if class=&good then good=good+1;     
					 else
           if class=&bad then bad=bad+1;
					 else 
           indetr=indetr+1;
	        %end;
            if last.varlabel
            then do;
                   output;
                   n=0;
                   good=0;
                   bad=0;
                   indetr=0;
                 end;
            if bye
            then do;
                   call symput('totlvl',left(put(interv,8.)));
                 end;
           run;
           
           

           
           %let data6=%substr(&sysdsn,9);                
		   *proc print data=&data6;
		    *title "DS - &data6";
           data _data_;
            set &data6;
            vtype=input("&totlvl",8.);

           *roc print;
           run;
        %end;
  %let DATA7=%substr(&sysdsn,9);
  
  
  
  
  
  /*---Check if the number of interval produced by a discrete scan--*/
  /*---exceeds MAXINT. If it does and the type of variable VAR is---*/
  /*---numeric, change the scan type to CONT and repeat it.---------*/
  /*---Otherwise end the macro with an error message.---------------*/
  %if &ok=NO
  %then %do;
           %if &vtype=2
           %then %do;
                    %put ERROR: MORE THAN &MAXINT INTERVALS HAVE BEEN DEFINED FOR;  ;
                    %put ERROR: A CHARACTER VARIABLE (&VAR).;
                    %put ERROR: MACRO VARSCAN STOPS.;
					%let data3=%substr(&sysdsn,9);
					%goto finish1;
                 %end;
           %if &vtype=1
           %then %do;
                    %put WARNING: MORE THAN &MAXINT INTERVALS HAS DEFINED FOR;
                    %put WARNING: A DISCRETE TYPE OF SCAN.;
                    %put WARNING: THE SCAN IS CHANGED TO CONTINUOUS.;
                    %let type=CONT;
                    %let ok=REPEAT;
                    %let fromint=10;
                    %if &fromint>&maxint %then %let fromint=&maxint;
                    %let toint=&fromint;
                    %let k=1;
                    %goto repeat;
                 %end;
        %end;
  /*---Calculate the total number of successes (totalbad) and the---*/
  /*---index (INDEX) of each interval.------------------------------*/

  *roc print;

  proc sort data=work. &data7;
   by vtype interv;
  run;     
 
  /* 
  proc means data=work. &data7 n sum min max noprint;
   var bad;
   by vtype;
   output out=_data_ n=_vtype_ sum=totalbad;
  run;
  */   
   
  proc means data=work. &data7 nway noprint;
  	by vtype;
  	output out=_data_ 
  	sum
  	 (bad
  	 good)
  	 =
  	 totalbad
  	 totalgood
  	  
  	 n = _vtype_;
  run;
   
  

  %let data8=%substr(&sysdsn,9);
  data _data_;
   merge work. &data7 work. &data8;
   by vtype;
   vtype=_vtype_;
   if totalbad ne 0 then index=(((bad+indetr)/n)/(totalbad/&nobs))*100;
                    else index=0;
  run;
  %let data9=%substr(&sysdsn,9);
 
  /*---Determine the minimum and the maximum index for each scan.---*/
  /*---They are used for producing the histogram.-------------------*/
  proc means data=work. &data9 min max noprint;
   var index;
   by vtype;
   output out=_data_ min=minindex max=maxindex;
  run;
  %let data10=%substr(&sysdsn,9);
  /*---Sort the intervals of a discrete scan if it is requested by--*/
  /*---DISCSEQ.-----------------------------------------------------*/
  %if &type=DISC and &discseq=INDEX
  %then %do;
           proc sort data=&data9;
            by vtype index;
           data &data9;
            set &data9;
            by vtype;
            retain newn;
            if first.vtype then newn=0;
            newn=newn+1;
            interv=newn;
           run;
        %end;
  /*---Print the result of the scan. Each scan starts on a new------*/
  /*---page. INTERV=1 indicates the start of a scan and-------------*/
  /*---INTERV=VTYPE the end of a scan. Important variables:---------*/
  /*------RATE:  success rate of the interval,----------------------*/
  /*------ORATE: overall success rate,------------------------------*/
  /*------CHISQ: chi-square value of the interval,------------------*/
  /*------CH:    overall chi-square value,--------------------------*/
  /*------NC-1:  degree of freedom of CH,---------------------------*/
  /*------UNIT:  unit value (scale) of the histogram,---------------*/
  /*------CHART: one line of the histogram,-------------------------*/
  /*------RL:    the location of the reference line (index=100) in--*/
  /*-------------the histogram.-------------------------------------*/


 

 




  /* %let data11=%substr(&sysdsn,9); */
  data _data_;
  run;
  %let vars_with_normal_rc=&syslast;
  %if %upcase(&reportds)=Y
  %then %do;
           data charvars(keep=variable interv range1 range2 prtintrv
                              n distper good distper2 rate
                              woe chisq p infoval ks prtindx nobs varnum infoval ch cramer)
                numvars(keep=variable interv range1 range2 prtintrv
                             n distper good distper2 rate
                             woe chisq p infoval ks prtindx nobs varnum infoval ch cramer)
				&vars_with_normal_rc(keep=variable sinfoval vtype pagenum rc desc)
				&vars_with_normal_rc.2
                ;
        %end;
  %else %do;
           data &vars_with_normal_rc(keep=variable sinfoval vtype pagenum rc desc)
           	&vars_with_normal_rc.2;
        %end;

   merge work. &data9 work. &data10 end=eof;
   by vtype;
   file print n=ps linesleft=lleft;
   length chart $19 prtintrv $25 text $58 underl $156 variable $32 star cline $21 rc $18 desc $32;
   length pchartyp $11;
   %if &type=CONT
   %then %do;
            length tl tr $11;
         %end;
   retain orate unit ch nc nobs underl rl;
   retain cgood cbad cind sinfoval 0 pagenum 0;                             
   array nn{2,2} n11 n12 n21 n22;
   retain n11 n12 n21 n22 n1d n2d nd1 nd2 0;
   /*---Start a new scan. Set the intial values and print the---*/
   /*---heading by calling subroutine HEADER.-------------------*/
   if interv=1
   then do;
          ch=0;
          nc=0;
          sinfoval=0;
          nobs=&nobs;
          orate=totalbad/nobs;
          %if %upcase(&charttyp)=I
          %then %do;
                   unit=(maxindex-minindex)/17;
                   rl=0;
                   if unit>0 then rl=int((100-minindex)/unit)+1;
                   if unit=0 and index>0 then rl=8;
                %end;
          link header;
        end;
   expect=n*orate;
   if expect>0 then chisq=((bad-expect)**2)/expect;
   p=1-probchi(chisq,1);
   if expect>0
   then do
          ch=ch+chisq;
          nc=nc+1;
        end;
   rate=(bad/n)*100;
   rate2=(bad/(bad+good))*100;                                     
   %if %upcase(&charttyp)=I
   %then %do;
            chart='|';
            l=0;
            if unit>0 and index>0 then l=int((index-minindex)/unit)+1;
            if unit=0 and index>0 then l=8;
            if l>0 then substr(chart,2,l)=repeat('*',l);
            if rl>0 and substr(chart,rl+1,1) ne '*' then substr(chart,rl+1)='|';
         %end;
   if totalbad ne 0 then prtindx=index;
                    else prtindx=.;
   /*---Start a new page if there is no sufficient number of----*/
   /*---lines left on the current page to print the rest of the-*/
   /*---result.-------------------------------------------------*/
   if (interv=vtype and lleft<10)
   then do;
          put @1 underl;
          put _page_;
          link header;
        end;
   if lleft=1
   then do;
          put @1 underl;
          link header;
        end;
   /*---Place the interval into variable PRTINTRV. If the scan--*/
   /*---is discrete, the interval is centered, if it is---------*/
   /*---continuous, the low end of the interval is right- ------*/
   /*---aligned and the high end is left-aligned to the center.-*/
   %if %length(&userfmt) > 0
   %then %do;
            prtintrv=varlabel;
         %end;
   %else %if &type=CONT
         %then %do;
                  tl=put(range1,&format);
                  tl=right(tl);
                  tr=put(range2,&format);
                  tr=left(tr);
                  prtintrv=tl||' - '||tr;
               %end;
   %if &type=DISC and %length(&userfmt)=0
   %then %do;
            text=range1;
			text=left(text); /*TEXT=COMPRESS(TEXT);*/
            l=1+(25-length(text))/2;
            prtintrv=' ';                                               ;
            substr(prtintrv,l)=text;                                    ;
         %end;                                                          ;
   /*---Print one interval.-------------------------------------*/
   distper=(n/nobs)*100;
   if totalbad ne 0
   then distper2=(bad/totalbad)*100;
   else distper2=.;
   numgood=(n-bad-indetr);                                                                                                  
   numbad=(n-good-indetr);    
   totind=(nobs-totalgood-totalbad);                                                                                              
   totgood=(nobs-totalbad-totind);                                                                                                
   totbad=(nobs-totalgood-totind);     
                                                                                             
   numind=(n-good-bad);                                                                                                    
   if totgood ne 0                                                                                                         
   then pergood=(numgood/totgood)*100;                                                                                     
   else pergood=.;                                                                                                         
   if totbad ne 0                                                                                                          
   then perbad=(numbad/totbad)*100;                                                                                        
   else perbad=.;    
   pertot=n/nobs*100;                                                                                                      
                                                                                                                           
   if distper2 ne 0                                                                                                        
   then woei=(pergood/distper2);                                                                                           
   else woei=.;                                                                                                            
   if woei=. or woei=0 then woe=.; else woe=log(woei);                                                                     
   cgood=cgood+numgood;                                                                                                    
   cind=cind+numind;                                                                                                       
   cbad=cbad+bad;                                                                                                          
   if totgood ne 0 and totalbad ne 0                                                                                       
   then ks=abs((cgood/totgood*100)-(cbad/totalbad*100));
   else ks=.;
   infoval=woe*(pergood-distper2);
   if infoval ne . then sinfoval=sinfoval+infoval;
   cramer=sqrt(ch/nobs);
   if interv in (1,2)
   then do;
          nn{interv,1}=n-bad;
          nn{interv,2}=bad;
        end;
    
    kkname="&var";  
        
        
        
   %if %upcase(&charttyp) ne I
   %then %do;
            unitwoe=int(woe/.1);
            if unitwoe>11 then unitwoe=11;
            if unitwoe<-11 then unitwoe=-11;
            if woe=. then unitwoe=.;
            abunit=abs(unitwoe)-1;
            if abunit>0 then star=repeat('*',abunit);  
            else star='';                               * if abunit is zero, set star to nothing (repeat would give 1 byte);
            cline='|';
            if unitwoe>0
            then do;
                   cline=left(trim(cline))||left(trim(star));
                   j=144;
                 end;
            else if unitwoe=0 or unitwoe=.
                 then do;
                        cline=left(trim(cline));
                        j=144;
                      end;
                 else do;
                        cline=left(trim(star))||left(trim(cline));
                        j=144+unitwoe;
                      * j=144;
                      end;
                 %end;
         %if %upcase(&pprint) ^= Y %then %do;
             put @1   interv         5.
                 @8   prtintrv $char25.
                 @32  numgood       8.
                 @42  numbad        8.
                 @53  numind        8.
                 @63  n             8.
                 @73  pergood       6.2
                 @81  perbad        6.2
                 @89  pertot        6.2
                 @97  rate          6.2
	               @105 rate2         6.2
                 @113 woe           6.2
                 @121 infoval       6.3
                 @129 ks            4.1
                 %if %upcase(&charttyp)=I
                 %then %do;
                          @143 chart    $char19.
                       %end;
                 %else %do;
                          @j cline 12.
                       %end;
                 ;
	                  /*  Prior to 07/19/2013
               put @1   interv         5.
                 @8   prtintrv $char25.
                 @35  n              8.
                 @45  distper        6.2
                 @53  good        8.0
                 @63  distper2       6.2
                 @71  rate           6.2
                 @79  woe            6.2
                 @90 infoval        6.3
                 @99 ks             4.1
                 %if %upcase(&charttyp)=I
                 %then %do;
                          @114 chart    $char19.
                       %end;
                 %else %do;
                          @j cline 12.
                       %end;
                 ;
	                    */
	       %end;
   /* for SAS output data set */
   varnum=input("&varnum",8.);
   variable="&var";
   
   output &vars_with_normal_rc.2;
   
   %if %upcase(&reportds)=Y
   %then %do;
            if &vtype=1
            then do;
                   %if %length(&userfmt) ne 0
                   %then %do;
                            range1=.;
                            range2=.;
                         %end;
                   output numvars;
                 end;
            else do;
                   %if %length(&userfmt) ne 0
                   %then %do;
                            range1=repeat(' ',&maxchlen-1);
                            range2=repeat(' ',&maxchlen-1);
                         %end;
                   output charvars;
                 end;
         %end;

   /*---Last interval is encountered. Close the printout and----*/
   /*---print the overall figures.------------------------------*/
   if interv=vtype
   then do;
          if interv=2
          then do;
              /* file log;
                  put nn{1,1}= nn{1,2}=
                      nn{2,1}= nn{2,2}=; */
                 cramer22=(nn{1,1}*nn{2,2}-nn{1,2}*nn{2,1}) /
                          sqrt((nn{1,1}+nn{1,2})*(nn{2,1}+nn{2,2})*
                               (nn{1,1}+nn{2,1})*(nn{1,2}+nn{2,2}));
                 pc22="Cramer's V(2x2): " || put(cramer22,5.3);
               end;
          else pc22=' ';
          orate=orate*100;
          if nc>1 then p=1-probchi(ch,nc-1);
                  else p=.;
          %if %upcase(&charttyp)=I
          %then %do;
                   if not (unit=0 and index<=0)
                   then do;
                          substr(underl,127+rl,1)='|';
                          c=126+rl;
                          hundred='100';
                        end;
                   else do;
                          ch=.;
                          hundred=' ';
                        end;
                %end;
         %if %upcase(&pprint) ^= Y %then %do;       
             put @1   underl   /
                 @30  cgood    10.
                 @61  nobs     10.
                 @40  cbad     10.
                 @51  cind     10.
                 @91  orate    12.2
              /* @87  ch        9.2
                 @96  p         8.3*/
                 @120 sinfoval  7.3
             %if %upcase(&charttyp)=I
             %then %do;
                      @c hundred;
                   %end;
                 ;
          %end;
		  rc='OK';
		  desc="&label";
		  output &vars_with_normal_rc; pagenum=0;
	 
       /*   put;
          put @5  'Cramers V or Phi Coeff: '
              @29 cramer 5.3
              @45 pc22
              ;
          put @5  'Mean: '
              @11 "&vmean"
          %if %upcase(&charttyp)=W
          %then %do;
                   @115 'Each *: Diff of .1 in ln(odds)'
                %end;
              ;
          tttt=trim(left("&vmin")) || '/' || left("&vmax");
          put @5  'Min/Max: '
              @14 tttt
          %if %upcase(&charttyp)=W
          %then %do;
                   @115 'Max Cutoffs at 1.1=> 3 times odds'
                %end;
              ;
          put @5  'Median: '
              @13 "&vmedian";
          put @5  'Standard Deviation: '
              @25 "&vstd";
          put @5  'Pearson Correlation: '
              @26 "&corrp";
          put @5  'Spearman Correlation: '
              @27 "&corrs";
          put #49 @90 'Source: Global Strategic Analytic Unit(G-SAU)';*/
         *IF NOT EOF THEN PUT _PAGE_;
        end;
   return;
   /*---Print the header.---------------------------------------*/
   header:
    pagenum=pagenum+1;
    if "&xlabel"='YES'
    then text=trim("&label") || "(&var)";
    else text="&label";
    %if %upcase(&charttyp) eq I
    %then %do;
            pchartyp='Index Chart';
          %end;
    %else %do;
            pchartyp='WOE Chart';
          %end;
    %if %upcase(&pprint) ^= Y %then %do;      
        l=length(text);
        l=int((146-l)/2);
        underl=repeat('-',156);
        put / @l text;
        %if &ok=REPEAT
        %then %do;
                 put @46 '(DISCRETE SCAN WAS CHANGED TO CONTINUOUS)';
              %end;
        text=put(vtype,6.0);
        text=left(text);
        text=compress(text);
        text='NUMBER OF INTERVALS IS '||text;
        l=length(text);
        l=int((146-l)/2);
        put @l text;
        put // @1   'Interval'
               @36  '# of'
               @46  '# of'
               @57  '# of'                                                            
                                                                       
               @98  'Resp'                                                            
               @106 'Resp'                                                           
               @121 'Info'                                                            
                                                                             
             / @5   '#'                                                               
               @16  'Interval'                                                        
               @36  'Good'                                                            
               @47  'Bad'                                                             
               @56  'Indeter'                                                         
               @66  'Total'                                                           
               @73  '% Good'                                                       
               @82  '% Bad'                                                            
               @89  '% Total'    
               @98  'Rate'                                                                                                           
               @106 'Rate2' 
               @115 'WOE'                                                                                          
               @121 'Value'                                                                                           
               @130 'K-S'                                                                                             
                                                                                                                      
               @138 pchartyp $char11.                                                                                 
             / @1   underl;                                                          
        /*      
        put // @1   'Interval'
               @39  '# of'
               @47  'Dist'
               @57  '# of'
               @65  'Dist'
               @73  'Resp'
               @80  'Wt of'
               @108 'Info'
             / @5   '#'
               @16  'Interval'
               @40  'Obs'
               @50  '%'
               @56  'Resps'
               @68  '%'
               @73  'Rate'
               @81  'Evid'
               @91  'Chisq'
               @100 'Prob'
               @107 'Value'
               @115 'K-S'
               @120 'Index'
               @128 pchartyp $char11.
             / @1   underl;
         */      
   %end;
   return;
   %let vars_index_appendin=&vars_with_normal_rc;
   %let delto=&data10;
   %goto akos;
   %finish1:;
   %let delto=&data3;
   
   
   
   
   

   
   
   /*---Delete all WORK files that macro VARSCAN opened.-------------*/
   %akos: 
       
       %if %upcase(&pprint) = Y %then %do;
       proc print data=&vars_with_normal_rc.2 noobs l width=uniform;
       	  var kkname interv prtintrv good bad indetr n pergood perbad pertot rate rate2 woe infoval ks  ;
       	  
       	  sum good bad indetr n infoval;
       	  
       	  format pergood perbad pertot rate rate2 woe 5.2 infoval 5.3 ks 5.2;
       	       
       	  title "&var";        
          label interv     =    'Interval #'
                prtintrv   =    'Interval' 
                good       =    '# of Good'
                bad        =    '# of Bad'
                indetr     =    '# of Indeter'
                n          =    'Total'
                pergood    =    '% Good'
                perbad     =    '% Bad'
                pertot     =    '% Total'
                rate       =    'Resp rate'
                rate2      =    'Resp rate2'
                woe        =    'WOE'
                infoval    =    'Info Value'
                ks         =    'K-S';  
                   	  
       	run;
       %end;
       proc datasets nolist;
           delete &delfrom-&delto;           
   %if &ok=NO
   %then %do;
            data maxout;
             length varname $8;
             varname="&var";
             type="&type";
             vtype="&vtype";
            proc append base=amaxout data=maxout;
			data _data_;
			 length variable $32
		            sinfoval   8
                    pagenum    8
					vtype      8 
                    rc       $18;
			 variable="&var";
			 sinfoval=.;
			 pagenum=.;
			 rc="Intervals > &maxint";
			run;
			%let vars_index_appendin=&syslast;
	     %end;
   %else %if "&emptyds"="YES"
         %then %do;
                  data emptyds;
                   length varname $8;
                   varname="&var";
                   type="&type";
                   vtype="&vtype";
                  proc append base=aemptyds data=emptyds;
		          data _data_;
		 	       length variable $32
		                  sinfoval   8
                          pagenum    8
						  vtype      8
                          rc       $18;
			       variable="&var";
			       sinfoval=.;
			       pagenum=.;
			       rc='All missing value';
	              run;
			      %let vars_index_appendin=&syslast;
               %end;
   proc append base=&varsindex data=&vars_index_appendin force;
   /* for output data set */
   %if %upcase(&reportds)=Y
   %then %do;
            %if &vtype=1
            %then %do;
                     proc append base=work.&inputds.n data=numvars force;
                  %end;
            %else %do;
                     proc append base=work.&inputds.c data=charvars force;
                  %end;
         %end;
   %finish:;
 %mend varscan;

