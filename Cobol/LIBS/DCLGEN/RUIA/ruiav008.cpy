      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PCELA_ORCAM)                            *
      *        LIBRARY(AD.DB2.DCLGEN(RUIAV008))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUIAV008)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PCELA_ORCAM TABLE
           ( CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             CPROJ_RURAL                    DECIMAL(3, 0) NOT NULL,
             QMIN_FAIXA_PROTV               DECIMAL(9, 0) NOT NULL,
             DINIC_VGCIA                    DATE NOT NULL,
             CTPO_OPER_ORCAM                DECIMAL(3, 0) NOT NULL,
             CPCELA_LIBRC                   CHAR(3) NOT NULL,
             DINIC_VGCIA_PCELA              DATE NOT NULL,
             DFIM_VGCIA                     DATE NOT NULL,
             QREFT_PCELA_ORCAM              DECIMAL(15, 2) NOT NULL,
             VPCELA_LIBRC                   DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PCELA_ORCAM              *
      ******************************************************************
       01  RUIAV008.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPROJ-RURAL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMIN-FAIXA-PROTV     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 CTPO-OPER-ORCAM      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPCELA-LIBRC         PIC X(3).
      *    *************************************************************
           10 DINIC-VGCIA-PCELA    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 QREFT-PCELA-ORCAM    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPCELA-LIBRC         PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
