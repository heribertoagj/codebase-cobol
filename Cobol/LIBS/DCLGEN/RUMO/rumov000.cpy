      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01PARCELA_PGTO)                           *
      *        LIBRARY(AD.DB2.DCLGEN(RUMOV000))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUMOV000)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01PARCELA_PGTO TABLE
           ( CCONTR                         DECIMAL(9, 0) NOT NULL,
             DVCTO                          DATE NOT NULL,
             VPCELA                         DECIMAL(15, 2) NOT NULL,
             VPROVS_JURO                    DECIMAL(15, 2) NOT NULL,
             VJURO                          DECIMAL(15, 2) NOT NULL,
             VPROVS_CM                      DECIMAL(15, 2) NOT NULL,
             VCORRC_ATULZ_MONET             DECIMAL(15, 2) NOT NULL,
             VRECTA                         DECIMAL(15, 2) NOT NULL,
             CSTTUS_BLOQ                    DECIMAL(1, 0) NOT NULL,
             CSTTUS_SIT_PCELA               DECIMAL(1, 0) NOT NULL,
             CSTTUS_BLOQ_TEMPR              DECIMAL(1, 0) NOT NULL,
             DULT_CALC                      DATE NOT NULL,
             COCOR_IMPED_REST               CHAR(25),
             VPROVS_JURO_ADCIO              DECIMAL(15, 2) NOT NULL,
             CINDCD_BAIXA_TRNST             CHAR(1) NOT NULL,
             VAPROP_PCELA_VENCD             DECIMAL(15, 2) NOT NULL,
             DTRNSF_CREDT_ATRSO             DATE NOT NULL,
             VJURO_REMUN_ATRSO              DECIMAL(15, 2) NOT NULL,
             VJURO_MORA_PCELA               DECIMAL(15, 2) NOT NULL,
             VMULTA_CONTR_ATRSO             DECIMAL(15, 2) NOT NULL,
             VJURO_CREDT_ATRSO              DECIMAL(15, 2) NOT NULL,
             VJURO_MORA_CREDT               DECIMAL(15, 2) NOT NULL,
             VMULTA_CREDT_ATRSO             DECIMAL(15, 2) NOT NULL,
             VTRNSF_RAP_VENCD               DECIMAL(15, 2)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01PARCELA_PGTO             *
      ******************************************************************
       01  RUMOV000.
      *    *************************************************************
           10 CCONTR               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 DVCTO                PIC X(10).
      *    *************************************************************
           10 VPCELA               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPROVS-JURO          PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO                PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VPROVS-CM            PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCORRC-ATULZ-MONET   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VRECTA               PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-BLOQ          PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-SIT-PCELA     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSTTUS-BLOQ-TEMPR    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 DULT-CALC            PIC X(10).
      *    *************************************************************
           10 COCOR-IMPED-REST     PIC X(25).
      *    *************************************************************
           10 VPROVS-JURO-ADCIO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-BAIXA-TRNST   PIC X(1).
      *    *************************************************************
           10 VAPROP-PCELA-VENCD   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DTRNSF-CREDT-ATRSO   PIC X(10).
      *    *************************************************************
           10 VJURO-REMUN-ATRSO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-MORA-PCELA     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMULTA-CONTR-ATRSO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-CREDT-ATRSO    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VJURO-MORA-CREDT     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VMULTA-CREDT-ATRSO   PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VTRNSF-RAP-VENCD     PIC S9(13)V9(2) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
