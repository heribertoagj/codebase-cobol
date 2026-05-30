      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TSOLTC_CONS_OPER_CIPAG)                    *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09E))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09E)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TSOLTC_CONS_OPER_CIPAG TABLE
           ( NCONS_OPER_CIPAG               DECIMAL(9, 0) NOT NULL,
             COPER_ENVID_CIPAG              CHAR(20) NOT NULL,
             CTPO_CONS_CIPAG                CHAR(1) NOT NULL,
             CINDCD_ATULZ_AGNDA             CHAR(1) NOT NULL,
             CSIT_OPER_CIPAG                CHAR(1) NOT NULL,
             IMOTVO_REJEI_CIPAG             CHAR(60),
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HENVIO_MSGEM_CIPAG             TIMESTAMP NOT NULL,
             HRETOR_MSGEM_CIPAG             TIMESTAMP,
             CCNPJ_ENTID_REG                DECIMAL(9, 0) NOT NULL,
             CFLIAL_ENTID_REG               DECIMAL(4, 0) NOT NULL,
             CCTRL_ENTID_REG                DECIMAL(2, 0) NOT NULL,
             CIDTFD_NEGOC_CIPAG             CHAR(2) NOT NULL,
             DVCTO_OPER_CIPAG               DATE NOT NULL,
             VTOT_OPER_CIPAG                DECIMAL(17, 2) NOT NULL,
             VGARNT_OPER_CIPAG              DECIMAL(17, 2) NOT NULL,
             CINDCD_GSTAO_REG               CHAR(1) NOT NULL,
             CINDCD_REGRA_DIVIS             CHAR(1) NOT NULL,
             CINDCD_ALCAN_CRDRA             CHAR(1) NOT NULL,
             CINDCD_ACEIT_PCIAL             CHAR(1) NOT NULL,
             CINDCD_OPER_RENEG              CHAR(1) NOT NULL,
             CINDCD_ACEIT_RESER             CHAR(1) NOT NULL,
             CSIT_CONTR_CIPAG               CHAR(1) NOT NULL,
             CINDCD_AUTRZ_CSSAO             CHAR(1) NOT NULL,
             CCNPJ_CESS_AUTRZ               DECIMAL(9, 0) NOT NULL,
             CFLIAL_CESS_AUTRZ              DECIMAL(4, 0) NOT NULL,
             CCTRL_CESS_AUTRZ               DECIMAL(2, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL,
             CCNPJ_ENTID_REG_ST             CHAR(9) NOT NULL,
             CFLIAL_ENTID_REG_ST            CHAR(4) NOT NULL,
             CCTRL_ENTID_REG_ST             DECIMAL(2, 0) NOT NULL,
             CCNPJ_CESS_AUTRZ_ST            CHAR(9) NOT NULL,
             CFLIAL_CESS_AUTRZ_ST           CHAR(4) NOT NULL,
             CCTRL_CESS_AUTRZ_ST            DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TSOLTC_CONS_OPER_CIPAG      *
      ******************************************************************
       01  BVVEB09E.
      *    *************************************************************
           10 NCONS-OPER-CIPAG     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-ENVID-CIPAG    PIC X(20).
      *    *************************************************************
           10 CTPO-CONS-CIPAG      PIC X(1).
      *    *************************************************************
           10 CINDCD-ATULZ-AGNDA   PIC X(1).
      *    *************************************************************
           10 CSIT-OPER-CIPAG      PIC X(1).
      *    *************************************************************
           10 IMOTVO-REJEI-CIPAG   PIC X(60).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HENVIO-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 HRETOR-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 CCNPJ-ENTID-REG      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-ENTID-REG     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-ENTID-REG      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-NEGOC-CIPAG   PIC X(2).
      *    *************************************************************
           10 DVCTO-OPER-CIPAG     PIC X(10).
      *    *************************************************************
           10 VTOT-OPER-CIPAG      PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VGARNT-OPER-CIPAG    PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-GSTAO-REG     PIC X(1).
      *    *************************************************************
           10 CINDCD-REGRA-DIVIS   PIC X(1).
      *    *************************************************************
           10 CINDCD-ALCAN-CRDRA   PIC X(1).
      *    *************************************************************
           10 CINDCD-ACEIT-PCIAL   PIC X(1).
      *    *************************************************************
           10 CINDCD-OPER-RENEG    PIC X(1).
      *    *************************************************************
           10 CINDCD-ACEIT-RESER   PIC X(1).
      *    *************************************************************
           10 CSIT-CONTR-CIPAG     PIC X(1).
      *    *************************************************************
           10 CINDCD-AUTRZ-CSSAO   PIC X(1).
      *    *************************************************************
           10 CCNPJ-CESS-AUTRZ     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CESS-AUTRZ    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CESS-AUTRZ     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-REG-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ENTID-REG-ST  PIC X(4).
      *    *************************************************************
           10 CCTRL-ENTID-REG-ST   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CESS-AUTRZ-ST  PIC X(9).
      *    *************************************************************
           10 CFLIAL-CESS-AUTRZ-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CESS-AUTRZ-ST  PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 35      *
      ******************************************************************
