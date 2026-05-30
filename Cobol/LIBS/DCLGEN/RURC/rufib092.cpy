      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TINFO_COOP_CONTR)                          *
      *        LIBRARY(AD.DB2.DCLGEN(RUFIB092))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUFIB092)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TINFO_COOP_CONTR TABLE
           ( NID_COOP_RURAL                 DECIMAL(5, 0) NOT NULL,
             CTPO_PSSOA                     DECIMAL(1, 0) NOT NULL,
             CCNPJ_CPF_COOP                 DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_COOP               DECIMAL(5, 0) NOT NULL,
             CCTRL_CPF_COOP                 DECIMAL(2, 0) NOT NULL,
             VPCELA_COOP_RURAL              DECIMAL(15, 2) NOT NULL,
             CERRO_MSGEM_SIST               CHAR(8) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CPROG_RURAL                    CHAR(4),
             CSIT_REG_RURAL                 CHAR(1),
             RERRO_INCON_INTRN              CHAR(70),
             CPSSOA_JURID_CONTR             DECIMAL(10, 0),
             CTPO_CONTR_NEGOC               DECIMAL(3, 0),
             NSEQ_CONTR_NEGOC               DECIMAL(10, 0),
             CCONTR                         DECIMAL(9, 0),
             CJUNC_DEPDC                    DECIMAL(5, 0),
             CCDULA                         DECIMAL(9, 0),
             CCNPJ_CPF_COOP_ST              CHAR(9) NOT NULL,
             CFLIAL_CNPJ_COOP_ST            CHAR(5) NOT NULL,
             CCTRL_CPF_COOP_ST              DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TINFO_COOP_CONTR            *
      ******************************************************************
       01  RUFIB092.
      *    *************************************************************
           10 NID-COOP-RURAL       PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-PSSOA           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-COOP       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-COOP     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CPF-COOP       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-COOP-RURAL    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CERRO-MSGEM-SIST     PIC X(8).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CPROG-RURAL          PIC X(4).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 RERRO-INCON-INTRN    PIC X(70).
      *    *************************************************************
           10 CPSSOA-JURID-CONTR   PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-CONTR-NEGOC     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NSEQ-CONTR-NEGOC     PIC S9(10)V USAGE COMP-3.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CPF-COOP-ST    PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-COOP-ST  PIC X(5).
      *    *************************************************************
           10 CCTRL-CPF-COOP-ST    PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 23      *
      ******************************************************************
