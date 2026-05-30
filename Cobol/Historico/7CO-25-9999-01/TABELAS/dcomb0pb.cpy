      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONS_REJTD_DESC)                          *
      *        LIBRARY(AD.DB2.DCLGEN(DCOMB0PB))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(DCOMB0PB)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONS_REJTD_DESC TABLE
           ( NCONS_REJTD_DESC               DECIMAL(5, 0) NOT NULL,
             CCNPJ_ENTID_DESC               DECIMAL(9, 0) NOT NULL,
             CFLIAL_CNPJ_DESC               DECIMAL(4, 0) NOT NULL,
             CCTRL_CNPJ_DESC                DECIMAL(2, 0) NOT NULL,
             DANO_INIC_DESC                 DECIMAL(6, 0) NOT NULL,
             DANO_FIM_DESC                  DECIMAL(6, 0) NOT NULL,
             CREFT_OPER_RURAL               CHAR(11),
             QCDULA_CREDT_DESC              DECIMAL(7, 0),
             HINCL_REG                      TIMESTAMP NOT NULL,
             CCNPJ_ENTID_DESC_ST            CHAR(9) NOT NULL,
             CFLIAL_CNPJ_DESC_ST            CHAR(4) NOT NULL,
             CCTRL_CNPJ_DESC_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONS_REJTD_DESC            *
      ******************************************************************
       01  DCOMB0PB.
      *    *************************************************************
           10 NCONS-REJTD-DESC     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-DESC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CNPJ-DESC     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CNPJ-DESC      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-INIC-DESC       PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 DANO-FIM-DESC        PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CREFT-OPER-RURAL     PIC X(11).
      *    *************************************************************
           10 QCDULA-CREDT-DESC    PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CCNPJ-ENTID-DESC-ST  PIC X(9).
      *    *************************************************************
           10 CFLIAL-CNPJ-DESC-ST  PIC X(4).
      *    *************************************************************
           10 CCTRL-CNPJ-DESC-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
