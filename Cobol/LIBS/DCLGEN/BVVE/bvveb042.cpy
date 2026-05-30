      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TPRODT_AGNDA_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB042))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB042)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TPRODT_AGNDA_CATAO TABLE
           ( NPRODT_AGNDA_CATAO             DECIMAL(5, 0) NOT NULL,
             ROPER_PRODT_CATAO              CHAR(10) NOT NULL,
             RORD_PGTO_CATAO                CHAR(10) NOT NULL,
             RBANDE_CATAO_AGNDA             CHAR(15) NOT NULL,
             ICRDRA_CATAO                   CHAR(15) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TPRODT_AGNDA_CATAO          *
      ******************************************************************
       01  BVVEB042.
      *    *************************************************************
           10 NPRODT-AGNDA-CATAO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 ROPER-PRODT-CATAO    PIC X(10).
      *    *************************************************************
           10 RORD-PGTO-CATAO      PIC X(10).
      *    *************************************************************
           10 RBANDE-CATAO-AGNDA   PIC X(15).
      *    *************************************************************
           10 ICRDRA-CATAO         PIC X(15).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
