      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TMSGRA_COMPL_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB085))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB085)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TMSGRA_COMPL_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             NCONTD_MSGRA                   DECIMAL(2, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0),
             WCONTD_MSGEM_COMPL             CLOB(65536) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TMSGRA_COMPL_CATAO          *
      ******************************************************************
       01  BVVEB085.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 NCONTD-MSGRA         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 WCONTD-MSGEM-COMPL   USAGE SQL TYPE IS CLOB-LOCATOR.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
