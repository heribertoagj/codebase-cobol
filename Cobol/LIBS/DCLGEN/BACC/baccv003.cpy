      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PTDOR_CATAO)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV003))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV003)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PTDOR_CATAO TABLE
           ( CCPF_PTDOR                     DECIMAL(11, 0) NOT NULL,
             IPTDOR_CATAO_INSTC             CHAR(30) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PTDOR_CATAO              *
      ******************************************************************
       01  BACCV003.
      *    *************************************************************
           10 CCPF-PTDOR           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 IPTDOR-CATAO-INSTC   PIC X(30).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 2       *
      ******************************************************************
