***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPER_FCHDO_PCOTE)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0M1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0M1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPER_FCHDO_PCOTE TABLE
           ( DCOMPT_FCHTO_TARIF             DECIMAL(6, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             HMANUT_EXPUR_PER               TIMESTAMP NOT NULL,
             CIDTFD_EXPUR_PER               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPER_FCHDO_PCOTE            *
      ******************************************************************
       01  GFCTB0M1.
      *    *************************************************************
           10 DCOMPT-FCHTO-TARIF   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 HMANUT-EXPUR-PER     PIC X(26).
      *    *************************************************************
           10 CIDTFD-EXPUR-PER     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
