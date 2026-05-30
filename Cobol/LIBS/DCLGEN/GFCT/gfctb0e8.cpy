      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SUSP_TARIF_CTA)                            *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0E8))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0E8)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SUSP_TARIF_CTA TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_SUSP_COBR                DATE NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             DFIM_SUSP_COBR                 DATE NOT NULL,
             CINDCD_LIBRC_COBR              CHAR(1) NOT NULL,
             DCOBR_SUSP_LIBRD               DATE NOT NULL,
             RJUSTF_SUSP_COBR               VARCHAR(255) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CTERM_INCL                     CHAR(8) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CTERM_MANUT                    CHAR(8) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SUSP_TARIF_CTA              *
      ******************************************************************
       01  GFCTB0E8.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-SUSP-COBR      PIC X(10).
      *    *************************************************************
           10 CFUNC-MANUT          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-SUSP-COBR       PIC X(10).
      *    *************************************************************
           10 CINDCD-LIBRC-COBR    PIC X(1).
      *    *************************************************************
           10 DCOBR-SUSP-LIBRD     PIC X(10).
      *    *************************************************************
           10 RJUSTF-SUSP-COBR.
              49 RJUSTF-SUSP-COBR-LEN  PIC S9(4) USAGE COMP.
              49 RJUSTF-SUSP-COBR-TEXT  PIC X(255).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CTERM-INCL           PIC X(8).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CTERM-MANUT          PIC X(8).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
