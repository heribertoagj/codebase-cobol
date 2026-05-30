      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCTRL_MSGRA_CIPAG)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB054))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB054)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCTRL_MSGRA_CIPAG TABLE
           ( CCTRO_CUSTO                    CHAR(4) NOT NULL,
             DINCL_CTRL_CIPAG               DATE NOT NULL,
             CSEQ_CTRL_CIPAG                DECIMAL(9, 0) NOT NULL,
             CSIT_PROCM_CIPAG               CHAR(1) NOT NULL,
             CMSGEM_ENVID_CIPAG             CHAR(8) NOT NULL,
             CPROT_SIST_ORIGE               CHAR(50) NOT NULL,
             HENVIO_MSGEM_CIPAG             TIMESTAMP,
             HRETOR_MSGEM_CIPAG             TIMESTAMP,
             CIDTFD_ENTID_REG               DECIMAL(1, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCTRL_MSGRA_CIPAG           *
      ******************************************************************
       01  BVVEB054.
      *    *************************************************************
           10 CCTRO-CUSTO          PIC X(4).
      *    *************************************************************
           10 DINCL-CTRL-CIPAG     PIC X(10).
      *    *************************************************************
           10 CSEQ-CTRL-CIPAG      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-CIPAG     PIC X(1).
      *    *************************************************************
           10 CMSGEM-ENVID-CIPAG   PIC X(8).
      *    *************************************************************
           10 CPROT-SIST-ORIGE     PIC X(50).
      *    *************************************************************
           10 HENVIO-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 HRETOR-MSGEM-CIPAG   PIC X(26).
      *    *************************************************************
           10 CIDTFD-ENTID-REG     PIC S9(1)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
