      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_RURAL_CSCDO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09G))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09G)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_RURAL_CSCDO TABLE
           ( CCONTR_CREDT_RURAL             DECIMAL(9, 0) NOT NULL,
             NPRODT_RURAL_CSCDO             DECIMAL(5, 0) NOT NULL,
             CPRODT_RURAL                   DECIMAL(3, 0),
             DULT_ATULZ                     DATE,
             CUSUAR_RESP                    CHAR(9)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_RURAL_CSCDO          *
      ******************************************************************
       01  RUECB09G.
      *    *************************************************************
           10 CCONTR-CREDT-RURAL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NPRODT-RURAL-CSCDO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-ATULZ           PIC X(10).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
