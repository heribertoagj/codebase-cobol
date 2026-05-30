      ******************************************************************
      * DCLGEN TABLE(DB2PRD.ADSAO_COMP_PCOTE)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB001))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB001)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.ADSAO_COMP_PCOTE TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_ADSAO_COMP               DATE NOT NULL,
             DFIM_ADSAO_COMP                DATE NOT NULL,
             CFUNC_MANUT_INCL               DECIMAL(9, 0) NOT NULL,
             CFUNC_MANUT_EXCL               DECIMAL(9, 0) NOT NULL,
             CINDCD_AGPTO_TOT               CHAR(1) NOT NULL,
             HINCL_REG_SIST                 TIMESTAMP NOT NULL,
             HMANUT_REG                     TIMESTAMP NOT NULL,
             CPSSOA_SERVC_TARIF             CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.ADSAO_COMP_PCOTE            *
      ******************************************************************
       01  GFCTB001.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ADSAO-COMP     PIC X(10).
      *    *************************************************************
           10 DFIM-ADSAO-COMP      PIC X(10).
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-EXCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-AGPTO-TOT     PIC X(1).
      *    *************************************************************
           10 HINCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CPSSOA-SERVC-TARIF   PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
