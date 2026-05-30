      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PARM_VALDC_ORPA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV018))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV018)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PARM_VALDC_ORPA TABLE
           ( DINIC_VGCIA                    DATE NOT NULL,
             CTPO_CAPTC_ORPAG               DECIMAL(2, 0) NOT NULL,
             TDIA_VERIF_DUPDD               DECIMAL(2, 0) NOT NULL,
             PEXCED_LIM_OPER                DECIMAL(2, 0) NOT NULL,
             QTRANS_CATAO_ESTBL             DECIMAL(2, 0) NOT NULL,
             TDIA_VERIF_DSBTO               DECIMAL(2, 0) NOT NULL,
             QMAX_PCELA_CATAO               DECIMAL(2, 0) NOT NULL,
             QMAX_PCELA_ESTBL               DECIMAL(2, 0) NOT NULL,
             PTOLRC_LIM_CATAO               DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PARM_VALDC_ORPA          *
      ******************************************************************
       01  BACCV018.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 CTPO-CAPTC-ORPAG     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 TDIA-VERIF-DUPDD     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 PEXCED-LIM-OPER      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QTRANS-CATAO-ESTBL   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 TDIA-VERIF-DSBTO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-PCELA-CATAO     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-PCELA-ESTBL     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 PTOLRC-LIM-CATAO     PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
