      ******************************************************************
      * DCLGEN TABLE(DB2PRD.SUSP_TARIF_AGPTO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0E7))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0E7)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.SUSP_TARIF_AGPTO TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             DINIC_SUSP_COBR                DATE NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT                    DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             DFIM_SUSP_COBR                 DATE NOT NULL,
             CINDCD_LIBRC_COBR              CHAR(1) NOT NULL,
             DCOBR_SUSP_LIBRD               DATE NOT NULL,
             RJUSTF_SUSP_COBR               VARCHAR(255) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.SUSP_TARIF_AGPTO            *
      ******************************************************************
       01  GFCTB0E7.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-SUSP-COBR      PIC X(10).
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
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
           10 CINDCD-AGPTO-TOT     PIC X(1).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
