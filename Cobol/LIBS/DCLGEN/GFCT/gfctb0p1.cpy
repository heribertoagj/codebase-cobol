***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TTARIF_EXCVD_GSTOR)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P1))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P1)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TTARIF_EXCVD_GSTOR TABLE
           ( CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_TARIF              DATE NOT NULL,
             CSEQ_MOVTO                     DECIMAL(11, 0) NOT NULL,
             DFIM_VGCIA_TARIF               DATE,
             CDEPDC_AUTRZ_ACSSO             DECIMAL(5, 0),
             CFUNC_MANUT_INCL               DECIMAL(9, 0),
             HINCL_REG                      TIMESTAMP,
             CFUNC_MANUT_EXCL               DECIMAL(9, 0),
             HMANUT_REG                     TIMESTAMP,
             CINDCD_EXCL_LOGCA              DECIMAL(1, 0),
             RJUSTF_INCL_AUTRZ              VARCHAR(256),
             RJUSTF_EXCL_AUTRZ              VARCHAR(256)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TTARIF_EXCVD_GSTOR          *
      ******************************************************************
       01  GFCTB0P1.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-TARIF    PIC X(10).
      *    *************************************************************
           10 CSEQ-MOVTO           PIC S9(11)V USAGE COMP-3.
      *    *************************************************************
           10 DFIM-VGCIA-TARIF     PIC X(10).
      *    *************************************************************
           10 CDEPDC-AUTRZ-ACSSO   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CFUNC-MANUT-INCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CFUNC-MANUT-EXCL     PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CINDCD-EXCL-LOGCA    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RJUSTF-INCL-AUTRZ.
              49 RJUSTF-INCL-AUTRZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-INCL-AUTRZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 RJUSTF-EXCL-AUTRZ.
              49 RJUSTF-EXCL-AUTRZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-EXCL-AUTRZ-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
