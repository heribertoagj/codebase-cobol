      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPARM_PROCM_AGNDA_CIPAG)                   *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB057))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB057)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPARM_PROCM_AGNDA_CIPAG TABLE
           ( CPARM_PROCM_AGNDA              DECIMAL(3, 0) NOT NULL,
             RPARM_PROCM_AGNDA              CHAR(60) NOT NULL,
             CDADO_PARM_AGNDA               DECIMAL(1, 0) NOT NULL,
             RCONTD_PARM_AGNDA              CHAR(60) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPARM_PROCM_AGNDA_CIPAG     *
      ******************************************************************
       01  BVVEB057.
      *    *************************************************************
           10 CPARM-PROCM-AGNDA    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RPARM-PROCM-AGNDA    PIC X(60).
      *    *************************************************************
           10 CDADO-PARM-AGNDA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RCONTD-PARM-AGNDA    PIC X(60).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
