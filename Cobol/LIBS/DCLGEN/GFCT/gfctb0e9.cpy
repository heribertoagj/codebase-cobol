      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TARIF_AGPTO_AGNDA)                         *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0E9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0E9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TARIF_AGPTO_AGNDA TABLE
           ( CAGPTO_CTA                     DECIMAL(3, 0) NOT NULL,
             CSEQ_FLEXZ_CTA                 DECIMAL(2, 0) NOT NULL,
             HIDTFD_FLEXZ_AGPTO             TIMESTAMP NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_AGNDA              DATE NOT NULL,
             DFIM_VGCIA_AGNDA               DATE NOT NULL,
             DDIA_AGNDA                     DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TARIF_AGPTO_AGNDA           *
      ******************************************************************
       01  GFCTB0E9.
      *    *************************************************************
           10 CAGPTO-CTA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ-FLEXZ-CTA       PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HIDTFD-FLEXZ-AGPTO   PIC X(26).
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-AGNDA    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-AGNDA     PIC X(10).
      *    *************************************************************
           10 DDIA-AGNDA           PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
