      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01CATAO_PGTO_FAT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV043))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV043)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01CATAO_PGTO_FAT TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CINSTC_ASSOC                   DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT_INSTC             CHAR(19) NOT NULL,
             DDIA_FCHTO_CATAO               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01CATAO_PGTO_FAT           *
      ******************************************************************
       01  BACCV043.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CINSTC-ASSOC         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT-INSTC   PIC X(19).
      *    *************************************************************
           10 DDIA-FCHTO-CATAO     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 5       *
      ******************************************************************
