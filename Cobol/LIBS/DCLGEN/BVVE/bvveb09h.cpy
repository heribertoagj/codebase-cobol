      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TOPER_ATIVO_GARNT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09H))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09H)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TOPER_ATIVO_GARNT TABLE
           ( COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             CTPO_VISAO_BANDE               DECIMAL(3, 0) NOT NULL,
             CBANDE_VISAO                   DECIMAL(3, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_CLI                 DECIMAL(13, 0) NOT NULL,
             COPER_GARNT_REG                CHAR(20) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20),
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             HATULZ                         TIMESTAMP NOT NULL,
             CUSUAR_RESP                    CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TOPER_ATIVO_GARNT           *
      ******************************************************************
       01  BVVEB09H.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 CTPO-VISAO-BANDE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBANDE-VISAO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-GARNT-REG      PIC X(20).
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HATULZ               PIC X(26).
      *    *************************************************************
           10 CUSUAR-RESP          PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
