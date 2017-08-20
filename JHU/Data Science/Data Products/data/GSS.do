#delimit ;

   infix
      year     1 - 11
      id_      12 - 22
      hrs2     23 - 33
      marital  34 - 44
      martype  45 - 55
      agewed   56 - 66
      divorce  67 - 77
      sphrs2   78 - 88
using GSS.dat;

label variable year     "Gss year for this respondent                       ";
label variable id_      "Respondent id number";
label variable hrs2     "Number of hours usually work a week";
label variable marital  "Marital status";
label variable martype  "Marital type";
label variable agewed   "Age when first married";
label variable divorce  "Ever been divorced or separated";
label variable sphrs2   "No. of hrs spouse usually works a week";


label define gsp001x
   99       "No answer"
   98       "Don't know"
   -1       "Not applicable"
;
label define gsp002x
   9        "No answer"
   5        "Never married"
   4        "Separated"
   3        "Divorced"
   2        "Widowed"
   1        "Married"
;
label define gsp003x
   2        "Marriage between people of the same gender"
   1        "Marriage between a man and a woman"
   0        "Not applicable"
;
label define gsp004x
   99       "No answer"
   98       "Don't know"
   0        "Not applicable"
;
label define gsp005x
   9        "No answer"
   8        "Don't know"
   2        "No"
   1        "Yes"
   0        "Not applicable"
;
label define gsp006x
   99       "No answer"
   98       "Don't know"
   -1       "Not applicable"
;


label values hrs2     gsp001x;
label values marital  gsp002x;
label values martype  gsp003x;
label values agewed   gsp004x;
label values divorce  gsp005x;
label values sphrs2   gsp006x;


