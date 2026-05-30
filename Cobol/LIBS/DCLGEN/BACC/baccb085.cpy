      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TRNSM_DEB_CREDT)                           *
      *        LIBRARY(AD.DB2.DCLGEN(BACCB085))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCB085)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TRNSM_DEB_CREDT TABLE
           ( CBCO                           DECIMAL(3, 0) NOT NULL,
             CDEPTO                         DECIMAL(4, 0) NOT NULL,
             CSETOR_DEPTO                   DECIMAL(3, 0) NOT NULL,
             CAG_MULTI                      DECIMAL(5, 0) NOT NULL,
             DMOVTO                         DATE NOT NULL,
             CDOCTO                         DECIMAL(7, 0) NOT NULL,
             CLCTO                          DECIMAL(5, 0) NOT NULL,
             CSEQ                           DECIMAL(9, 0) NOT NULL,
             CDIG_DEPDC                     CHAR(1) NOT NULL,
             CDEB_CREDT                     DECIMAL(1, 0),
             CRZ_CTA                        DECIMAL(6, 0) NOT NULL,
             CDIG_AG_MULTI                  CHAR(1) NOT NULL,
             CCTA_MULTI                     DECIMAL(13, 0) NOT NULL,
             CDIG_CTA_MULTI                 CHAR(2) NOT NULL,
             VOPER                          DECIMAL(15, 2) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TRNSM_DEB_CREDT             *
      ******************************************************************
       01  BACCB085.
      *    *************************************************************
           10 CBCO                 PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPTO               PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CSETOR-DEPTO         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-MULTI            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 CDOCTO               PIC S9(7)V USAGE COMP-3.
      *    *************************************************************
           10 CLCTO                PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CSEQ                 PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-DEPDC           PIC X(1).
      *    *************************************************************
           10 CDEB-CREDT           PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CRZ-CTA              PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-AG-MULTI        PIC X(1).
      *    *************************************************************
           10 CCTA-MULTI           PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CDIG-CTA-MULTI       PIC X(2).
      *    *************************************************************
           10 VOPER                PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
