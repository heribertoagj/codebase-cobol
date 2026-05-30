      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMNTRC_PTCAO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB083))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB083)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMNTRC_PTCAO_CATAO TABLE
           ( DDIA_PTCAO                     DATE NOT NULL,
             CINDCD_ORIGE_PTCAO             DECIMAL(1, 0) NOT NULL,
             NDIA_MNTRC_MSGRA               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMNTRC_PTCAO_CATAO          *
      ******************************************************************
       01  BVVEB083.
      *    *************************************************************
           10 DDIA-PTCAO           PIC X(10).
      *    *************************************************************
           10 CINDCD-ORIGE-PTCAO   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NDIA-MNTRC-MSGRA     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 3       *
      ******************************************************************
