      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCARAC_ESTRT_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB060))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB060)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCARAC_ESTRT_CATAO TABLE
           ( NESTRT_INFO_BACEN              DECIMAL(3, 0) NOT NULL,
             NVRSAO_ESTRT_INFO              DECIMAL(3, 0) NOT NULL,
             NCARAC_ESTRT_INFO              DECIMAL(3, 0) NOT NULL,
             ICARAC_ESTRT_INFO              CHAR(100) NOT NULL,
             CIDTFD_TPO_CARAC               CHAR(1) NOT NULL,
             CIDTFD_SUB_TPO                 CHAR(50),
             CIDTFD_ITEM_GRP                CHAR(1),
             QPOSIC_INTEI                   DECIMAL(6, 0),
             QPOSIC_DEC                     DECIMAL(2, 0),
             NNVEL_BAIXA_PLATF              DECIMAL(1, 0),
             NNVEL_MNFRM_CATAO              DECIMAL(9, 0),
             QMAX_REPTC                     DECIMAL(3, 0),
             QMIN_REPTC                     DECIMAL(3, 0),
             RCARAC_ESTRT_INFO              CHAR(200)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCARAC_ESTRT_CATAO          *
      ******************************************************************
       01  BVVEB060.
      *    *************************************************************
           10 NESTRT-INFO-BACEN    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NVRSAO-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 NCARAC-ESTRT-INFO    PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 ICARAC-ESTRT-INFO    PIC X(100).
      *    *************************************************************
           10 CIDTFD-TPO-CARAC     PIC X(1).
      *    *************************************************************
           10 CIDTFD-SUB-TPO       PIC X(50).
      *    *************************************************************
           10 CIDTFD-ITEM-GRP      PIC X(1).
      *    *************************************************************
           10 QPOSIC-INTEI         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 QPOSIC-DEC           PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 NNVEL-BAIXA-PLATF    PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 NNVEL-MNFRM-CATAO    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 QMAX-REPTC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 QMIN-REPTC           PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 RCARAC-ESTRT-INFO    PIC X(200).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
