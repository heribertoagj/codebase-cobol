***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.EXCEC_ADSAO_COMP)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB048))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB048)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.EXCEC_ADSAO_COMP TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_AGPTO_CTA                 DECIMAL(9, 0) NOT NULL,
             DINIC_ADSAO_COMP               DATE NOT NULL,
             CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCTA_CLI                       DECIMAL(7, 0) NOT NULL,
             HINCL_EXCEC_ADSAO              TIMESTAMP NOT NULL,
             CFUNC_INCL_EXCEC               DECIMAL(9, 0) NOT NULL,
             CFUNC_EXCL_EXCEC               DECIMAL(9, 0) NOT NULL,
             HEXCL_EXCEC_ADSAO              TIMESTAMP NOT NULL,
             CINDCD_EXCL_REG                DECIMAL(1, 0) NOT NULL,
             DINIC_EXCEC_ADSAO              DATE NOT NULL,
             DFIM_EXCEC_ADSAO               DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.EXCEC_ADSAO_COMP            *
      ******************************************************************
       01  GFCTB048.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-AGPTO-CTA       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-ADSAO-COMP     PIC X(10).
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CLI             PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-EXCEC-ADSAO    PIC X(26).
      *    *************************************************************
           10 CFUNC-INCL-EXCEC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-EXCL-EXCEC     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HEXCL-EXCEC-ADSAO    PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCL-REG      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-EXCEC-ADSAO    PIC X(10).
      *    *************************************************************
           10 DFIM-EXCEC-ADSAO     PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
