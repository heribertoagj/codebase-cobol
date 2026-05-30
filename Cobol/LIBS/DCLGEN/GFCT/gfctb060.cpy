***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.FLEXZ_VIP_CTA)                             *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB060))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB060)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.FLEXZ_VIP_CTA TABLE
           ( CBCO_CTA_CLI                   DECIMAL(3, 0) NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             DINIC_VGCIA_VIP                DATE NOT NULL,
             DFIM_VGCIA_VIP                 DATE NOT NULL,
             PDESC_VIP                      DECIMAL(7, 4) NOT NULL,
             CTPO_OPER_MOTVO                DECIMAL(3, 0) NOT NULL,
             CMOTVO_JUSTF                   DECIMAL(2, 0) NOT NULL,
             RCOMPL_JUSTF                   VARCHAR(255) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CTPO_OPER_CANCT                DECIMAL(3, 0) NOT NULL,
             CMOTVO_CANCT                   DECIMAL(2, 0) NOT NULL,
             RJUSTF_CANCT                   VARCHAR(255) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.FLEXZ_VIP_CTA               *
      ******************************************************************
       01  GFCTB060.
      *    *************************************************************
           10 CBCO-CTA-CLI         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-VIP      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-VIP       PIC X(10).
      *    *************************************************************
           10 PDESC-VIP            PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 CTPO-OPER-MOTVO      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-JUSTF         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RCOMPL-JUSTF.
              49 RCOMPL-JUSTF-LEN  PIC S9(4) USAGE COMP.
              49 RCOMPL-JUSTF-TEXT  PIC X(255).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CTPO-OPER-CANCT      PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CMOTVO-CANCT         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-CANCT.
              49 RJUSTF-CANCT-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-CANCT-TEXT  PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 16      *
      ******************************************************************
