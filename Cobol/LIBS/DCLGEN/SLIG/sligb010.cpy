      ******************************************************************
      * DCLGEN TABLE(DB2PRD.DEPDC_BANDA_MAGNT)                         *
      *        LIBRARY(AD.DB2.DCLGEN(SLIGB010))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(SLIGB010)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.DEPDC_BANDA_MAGNT TABLE
           ( CEMPR_INC                      DECIMAL(5, 0) NOT NULL,
             CDEPDC                         DECIMAL(5, 0) NOT NULL,
             CBCO_SACDO                     DECIMAL(3, 0) NOT NULL,
             CAG_BCRIA_SACDO                DECIMAL(5, 0) NOT NULL,
             CCTA_BCRIA_SACDO               DECIMAL(13, 0) NOT NULL,
             CCGC_CPF_SACDO                 DECIMAL(9, 0) NOT NULL,
             CFLIAL_CGC_SACDO               DECIMAL(5, 0) NOT NULL,
             CIDTFD_CART_LIQDZ              DECIMAL(2, 0) NOT NULL,
             CCTRL_CGC_SACDO                DECIMAL(2, 0) NOT NULL,
             CINDCD_PSSOA_SACDO             CHAR(1) NOT NULL,
             VVCTO_PER_PG                   DECIMAL(15, 2),
             VVCTO_PER_NAO_PG               DECIMAL(15, 2),
             VVCTO_PER_BXADO                DECIMAL(15, 2),
             VVCTO_APOS_PER_PG              DECIMAL(15, 2),
             VAPOS_PER_NAO_PG               DECIMAL(15, 2),
             VAPOS_PER_BXADO                DECIMAL(15, 2),
             VCHEQ_PRIM_DEVLC               DECIMAL(15, 2),
             VCHEQ_REAP                     DECIMAL(15, 2),
             VCHEQ_REAP_DEVOL               DECIMAL(15, 2),
             VCHEQ_REAP_PG                  DECIMAL(15, 2),
             VCHEQ_DEVOL_ANTER              DECIMAL(15, 2),
             CCGC_CPF_SACDO_ST              CHAR(9) NOT NULL,
             CFLIAL_SACDO_ST                CHAR(5) NOT NULL,
             CCTRL_CGC_SACDO_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.DEPDC_BANDA_MAGNT           *
      ******************************************************************
       01  SLIGB010.
      *    *************************************************************
           10 CEMPR-INC            PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CDEPDC               PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CBCO-SACDO           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CAG-BCRIA-SACDO      PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCTA-BCRIA-SACDO     PIC S9(13)V USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-SACDO       PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CGC-SACDO     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-CART-LIQDZ    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CGC-SACDO      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CINDCD-PSSOA-SACDO   PIC X(1).
      *    *************************************************************
           10 VVCTO-PER-PG         PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VVCTO-PER-NAO-PG     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VVCTO-PER-BXADO      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VVCTO-APOS-PER-PG    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAPOS-PER-NAO-PG     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VAPOS-PER-BXADO      PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCHEQ-PRIM-DEVLC     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCHEQ-REAP           PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCHEQ-REAP-DEVOL     PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCHEQ-REAP-PG        PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VCHEQ-DEVOL-ANTER    PIC S9(13)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CCGC-CPF-SACDO-ST    PIC X(9).
      *    *************************************************************
           10 CFLIAL-SACDO-ST      PIC X(5).
      *    *************************************************************
           10 CCTRL-CGC-SACDO-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 24      *
      ******************************************************************
