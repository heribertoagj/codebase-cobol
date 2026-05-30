      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_TEMPR_CATAO)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB064))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB064)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_TEMPR_CATAO TABLE
           ( CTRANS_PROG_APRES              CHAR(4) NOT NULL,
             CTERM_USUAR                    CHAR(15) NOT NULL,
             NCTRL_TEMPR                    DECIMAL(5, 0) NOT NULL,
             CINDCD_SELEC                   CHAR(1) NOT NULL,
             RCTRL_TEMPR                    CHAR(40) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_TEMPR_CATAO           *
      ******************************************************************
       01  BVVEB064.
      *    *************************************************************
           10 CTRANS-PROG-APRES    PIC X(4).
      *    *************************************************************
           10 CTERM-USUAR          PIC X(15).
      *    *************************************************************
           10 NCTRL-TEMPR          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-SELEC         PIC X(1).
      *    *************************************************************
           10 RCTRL-TEMPR          PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
