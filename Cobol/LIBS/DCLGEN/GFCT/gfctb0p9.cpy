***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFLEXZ_SERVC_ESPEC)                        *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0P9))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0P9)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFLEXZ_SERVC_ESPEC TABLE
           ( CSERVC_ESPEC                   DECIMAL(2, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA_FLEXZ              DATE NOT NULL,
             DFIM_VGCIA_FLEXZ               DATE NOT NULL,
             CSIT_FLEXZ_SERVC               DECIMAL(1, 0) NOT NULL,
             PDESC_FLEXZ_SERVC              DECIMAL(5, 2) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             RJUSTF_INCL_FLEXZ              VARCHAR(256) NOT NULL,
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(9),
             RJUSTF_MANUT_FLEXZ             VARCHAR(256),
             CUSUAR_VALDC_REG               CHAR(9),
             HVALDC_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFLEXZ_SERVC_ESPEC          *
      ******************************************************************
       01  GFCTB0P9.
      *    *************************************************************
           10 CSERVC-ESPEC         PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-FLEXZ    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-FLEXZ     PIC X(10).
      *    *************************************************************
           10 CSIT-FLEXZ-SERVC     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 PDESC-FLEXZ-SERVC    PIC S9(3)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 RJUSTF-INCL-FLEXZ.
              49 RJUSTF-INCL-FLEXZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-INCL-FLEXZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 RJUSTF-MANUT-FLEXZ.
              49 RJUSTF-MANUT-FLEXZ-LEN
                 PIC S9(4) USAGE COMP.
              49 RJUSTF-MANUT-FLEXZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 CUSUAR-VALDC-REG     PIC X(9).
      *    *************************************************************
           10 HVALDC-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
