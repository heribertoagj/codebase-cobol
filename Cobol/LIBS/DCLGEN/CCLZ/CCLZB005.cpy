      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TDETLH_PROCM_RECTA_FEDRL)                  *
      *        LIBRARY(AD.DB2.DCLGEN(CCLZB005))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CCLZB005)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TDETLH_PROCM_RECTA_FEDRL TABLE
           ( DANO_MES_REFT                  DECIMAL(6, 0) NOT NULL,
             NDETLH_PROCM_RECTA             DECIMAL(5, 0) NOT NULL,
             CSIT_PROCM_RECTA               DECIMAL(2, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP,
             RMOTVO_SOLTC_RETRD             VARCHAR(500) NOT NULL,
             CUSUAR_SOLTC                   CHAR(9) NOT NULL,
             HINCL_SOLTC                    TIMESTAMP NOT NULL,
             CSIT_RETRD_INCON               DECIMAL(1, 0) NOT NULL,
             RMOTVO_SIT_SOLTC               VARCHAR(500) NOT NULL,
             CUSUAR_APROV                   CHAR(9),
             HAPROV_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TDETLH_PROCM_RECTA_FEDRL    *
      ******************************************************************
       01  CCLZB005.
      *    *************************************************************
           10 DANO-MES-REFT        PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 NDETLH-PROCM-RECTA   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-PROCM-RECTA     PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 RMOTVO-SOLTC-RETRD.
              49 RMOTVO-SOLTC-RETRD-LEN
                 PIC S9(4) USAGE COMP.
              49 RMOTVO-SOLTC-RETRD-TEXT
                 PIC X(500).
      *    *************************************************************
           10 CUSUAR-SOLTC         PIC X(9).
      *    *************************************************************
           10 HINCL-SOLTC          PIC X(26).
      *    *************************************************************
           10 CSIT-RETRD-INCON     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 RMOTVO-SIT-SOLTC.
              49 RMOTVO-SIT-SOLTC-LEN
                 PIC S9(4) USAGE COMP.
              49 RMOTVO-SIT-SOLTC-TEXT
                 PIC X(500).
      *    *************************************************************
           10 CUSUAR-APROV         PIC X(9).
      *    *************************************************************
           10 HAPROV-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
