***************************** Top of Data ******************************
      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TFLEXZ_VLR_REDZD)                          *
      *        LIBRARY(AD.DB2.DCLGEN(GFCTB0PA))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(GFCTB0PA)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TFLEXZ_VLR_REDZD TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA                     DECIMAL(13, 0) NOT NULL,
             CSERVC_TARIF                   DECIMAL(5, 0) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0) NOT NULL,
             VREDZD_NEGOC                   DECIMAL(13, 2) NOT NULL,
             DINIC_VGCIA_REDZD              DATE NOT NULL,
             DFIM_VGCIA_REDZD               DATE NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             RJUSTF_INCL_FLEXZ              VARCHAR(256) NOT NULL,
             HEXCL_REG_SIST                 TIMESTAMP NOT NULL,
             CUSUAR_EXCL                    CHAR(9) NOT NULL,
             RJUSTF_EXCL_FLEXZ              VARCHAR(256) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TFLEXZ_VLR_REDZD            *
      ******************************************************************
       01  GFCTB0PA.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA           PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CSERVC-TARIF         PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 VREDZD-NEGOC         PIC S9(11)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA-REDZD    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REDZD     PIC X(10).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 RJUSTF-INCL-FLEXZ.
              49 RJUSTF-INCL-FLEXZ-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RJUSTF-INCL-FLEXZ-TEXT
                 PIC X(256).
      *    *************************************************************
           10 HEXCL-REG-SIST       PIC X(26).
      *    *************************************************************
           10 CUSUAR-EXCL          PIC X(9).
      *    *************************************************************
           10 RJUSTF-EXCL-FLEXZ.
              49 RJUSTF-EXCL-FLEXZ-LEN
                 PIC S9(4) USAGE COMP-5.
              49 RJUSTF-EXCL-FLEXZ-TEXT
                 PIC X(256).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
