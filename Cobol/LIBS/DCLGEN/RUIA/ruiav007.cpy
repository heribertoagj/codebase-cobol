      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01ORCAMENTO_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUIAV007))                        *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUIAV007)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01ORCAMENTO_RURAL TABLE
           ( CPRODT_RURAL                   DECIMAL(3, 0) NOT NULL,
             CPROJ_RURAL                    DECIMAL(3, 0) NOT NULL,
             QMIN_FAIXA_PROTV               DECIMAL(9, 0) NOT NULL,
             DINIC_VGCIA                    DATE NOT NULL,
             CVALDC_ORCAM                   CHAR(1),
             PCPCAO_ORCAM                   DECIMAL(5, 2),
             RORIGE_ORCAM                   CHAR(40) NOT NULL,
             QTOT_REFT_ORCAM                DECIMAL(15, 2) NOT NULL,
             VTOT_ORCAM                     DECIMAL(15, 2) NOT NULL,
             VBSICO_COMCZ_PRODT             DECIMAL(9, 2),
             QMAX_FAIXA_PROTV               DECIMAL(9, 0) NOT NULL,
             CUND_MEDD                      DECIMAL(3, 0) NOT NULL,
             CFUNC_BDSCO                    DECIMAL(9, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01ORCAMENTO_RURAL          *
      ******************************************************************
       01  RUIAV007.
      *    *************************************************************
           10 CPRODT-RURAL         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CPROJ-RURAL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMIN-FAIXA-PROTV     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 CVALDC-ORCAM         PIC X(1).
      *    *************************************************************
           10 PCPCAO-ORCAM         PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 RORIGE-ORCAM         PIC X(40).
      *    *************************************************************
           10 QTOT-REFT-ORCAM      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTOT-ORCAM           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VBSICO-COMCZ-PRODT   PIC S9(7)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 QMAX-FAIXA-PROTV     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CUND-MEDD            PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
