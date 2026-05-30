      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01SENHA_TLCOD)                            *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV039))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV039)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01SENHA_TLCOD TABLE
           ( CPREFX_CATAO_CREDT             DECIMAL(6, 0) NOT NULL,
             CCATAO_CREDT                   DECIMAL(10, 0) NOT NULL,
             CSENHA_TLCOD_CATAO             CHAR(4) NOT NULL,
             RMSGEM_SENHA_TLCOD             CHAR(19) NOT NULL,
             CATULZ_SENHA_TLCOD             CHAR(1) NOT NULL,
             DATULZ_SENHA_TLCOD             DATE NOT NULL,
             DRETRD_SENHA_TLCOD             DECIMAL(6, 0) NOT NULL,
             CSIT_SENHA_TLCOD               DECIMAL(1, 0) NOT NULL,
             CTPO_ATULZ_SENHA               CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01SENHA_TLCOD              *
      ******************************************************************
       01  BACCV039.
      *    *************************************************************
           10 CPREFX-CATAO-CREDT   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCATAO-CREDT         PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CSENHA-TLCOD-CATAO   PIC X(4).
      *    *************************************************************
           10 RMSGEM-SENHA-TLCOD   PIC X(19).
      *    *************************************************************
           10 CATULZ-SENHA-TLCOD   PIC X(1).
      *    *************************************************************
           10 DATULZ-SENHA-TLCOD   PIC X(10).
      *    *************************************************************
           10 DRETRD-SENHA-TLCOD   PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-SENHA-TLCOD     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-ATULZ-SENHA     PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
