      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PERIF_SBIN)                                *
      *        LIBRARY(AD.DB2.DCLGEN(SBINB026))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SBINB026)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PERIF_SBIN TABLE
           ( CTPO_PERIF                     DECIMAL(3, 0) NOT NULL,
             ITPO_PERIF                     CHAR(30) NOT NULL,
             ITPO_PERIF_ABREV               CHAR(5) NOT NULL,
             CANTIG_TPO_PERIF               CHAR(1) NOT NULL,
             DINCL_TPO_PERIF                DATE NOT NULL,
             DALT_TPO_PERIF                 DATE NOT NULL,
             CJUNC_DEPTO_GSTOR              DECIMAL(5, 0) NOT NULL,
             CUSUAR_SIST                    CHAR(8) NOT NULL,
             ROBS_TPO_PERIF                 CHAR(80) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PERIF_SBIN                  *
      ******************************************************************
       01  SBINB026.
      *    *************************************************************
           10 CTPO-PERIF           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ITPO-PERIF           PIC X(30).
      *    *************************************************************
           10 ITPO-PERIF-ABREV     PIC X(5).
      *    *************************************************************
           10 CANTIG-TPO-PERIF     PIC X(1).
      *    *************************************************************
           10 DINCL-TPO-PERIF      PIC X(10).
      *    *************************************************************
           10 DALT-TPO-PERIF       PIC X(10).
      *    *************************************************************
           10 CJUNC-DEPTO-GSTOR    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-SIST          PIC X(8).
      *    *************************************************************
           10 ROBS-TPO-PERIF       PIC X(80).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
