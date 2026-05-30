***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PARM_DATA_PROCM)                           *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0A1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0A1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PARM_DATA_PROCM TABLE
           ( CSIST_PRINC                    CHAR(4) NOT NULL,
             DPROCM_ANTER                   DATE NOT NULL,
             DPROCM_ATUAL                   DATE NOT NULL,
             DPROCM_PROX                    DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PARM_DATA_PROCM             *
      ******************************************************************
       01  GFCTB0A1.
      *    *************************************************************
           10 CSIST-PRINC          PIC X(4).
      *    *************************************************************
           10 DPROCM-ANTER         PIC X(10).
      *    *************************************************************
           10 DPROCM-ATUAL         PIC X(10).
      *    *************************************************************
           10 DPROCM-PROX          PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
