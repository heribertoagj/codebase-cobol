***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DETLH_BONIF_PCOTE)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB042))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB042)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DETLH_BONIF_PCOTE TABLE
           ( CTARIF_PCOTE                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_COMPS              DATE NOT NULL,
             CSEQ_COMPO_PCOTE               DECIMAL(3, 0) NOT NULL,
             CTPO_BONIF                     DECIMAL(3, 0) NOT NULL,
             CCRITR_BONIF                   DECIMAL(3, 0) NOT NULL,
             DINIC_REGRA_BONIF              DATE NOT NULL,
             CSEQ_BONIF_PCOTE               DECIMAL(4, 0) NOT NULL,
             CIDTFD_TPO_SERVC               DECIMAL(1, 0) NOT NULL,
             CSERVC_RLCDO_BONIF             DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_REST               DATE NOT NULL,
             DFIM_VGCIA_REST                DATE NOT NULL,
             QMIN_BONIF_ESPCL               DECIMAL(5, 0) NOT NULL,
             QMAX_BONIF_ESPCL               DECIMAL(5, 0) NOT NULL,
             VMIN_BONIF_ESPCL               DECIMAL(11, 2) NOT NULL,
             VMAX_BONIF_ESPCL               DECIMAL(11, 2) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DETLH_BONIF_PCOTE           *
      ******************************************************************
       01  GFCTB042.
      *    *************************************************************
           10 CTARIF-PCOTE         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-COMPS    PIC X(10).
      *    *************************************************************
           10 CSEQ-COMPO-PCOTE     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-BONIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCRITR-BONIF         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-REGRA-BONIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-BONIF-PCOTE     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-TPO-SERVC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-RLCDO-BONIF   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-REST     PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REST      PIC X(10).
      *    *************************************************************
           10 QMIN-BONIF-ESPCL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-BONIF-ESPCL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 VMIN-BONIF-ESPCL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMAX-BONIF-ESPCL     PIC S9(9)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
