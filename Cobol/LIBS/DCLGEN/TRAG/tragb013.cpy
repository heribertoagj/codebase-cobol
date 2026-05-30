      ******************************************************************
      * DCLGEN TABLE(DB2PRD.PROCS_TRAG_CONCL)                          *
      *        LIBRARY(AD.DB2.DCLGEN(TRAGB013))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(TRAGB013)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.PROCS_TRAG_CONCL TABLE
           ( CIDTFD_TRAG                    DECIMAL(9, 0) NOT NULL,
             CBCO                           DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA                      DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_ATUAL               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL                    DECIMAL(3, 0) NOT NULL,
             CCTA_CORR_ORIGE                DECIMAL(13, 0) NOT NULL,
             CPOSTO_SERVC                   DECIMAL(3, 0) NOT NULL,
             CDIG_CTA_ORIGE                 CHAR(2) NOT NULL,
             CBCO_DSTNA                     DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_DSTNO                DECIMAL(5, 0) NOT NULL,
             CGRP_CTBIL_DSTNO               DECIMAL(3, 0) NOT NULL,
             CSGRP_CTBIL_DSTNO              DECIMAL(3, 0) NOT NULL,
             CCTA_MOVTC_DSTNO               DECIMAL(13, 0) NOT NULL,
             CDIG_CTA_DSTNO                 CHAR(2) NOT NULL,
             CPOSTO_DSTNO                   DECIMAL(3, 0) NOT NULL,
             CIDTFD_ENTRD_CTA               DECIMAL(1, 0) NOT NULL,
             DCAD_PREVT                     DATE NOT NULL,
             DCTBIL_PREVT                   DATE NOT NULL,
             CIDTFD_TPO_TRAG                CHAR(1) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.PROCS_TRAG_CONCL            *
      ******************************************************************
       01  TRAGB013.
      *    *************************************************************
           10 CIDTFD-TRAG          PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-ATUAL     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL          PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-CORR-ORIGE      PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-ORIGE       PIC X(2).
      *    *************************************************************
           10 CBCO-DSTNA           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-DSTNO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CGRP-CTBIL-DSTNO     PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CSGRP-CTBIL-DSTNO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-MOVTC-DSTNO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-DSTNO       PIC X(2).
      *    *************************************************************
           10 CPOSTO-DSTNO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-ENTRD-CTA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DCAD-PREVT           PIC X(10).
      *    *************************************************************
           10 DCTBIL-PREVT         PIC X(10).
      *    *************************************************************
           10 CIDTFD-TPO-TRAG      PIC X(1).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
