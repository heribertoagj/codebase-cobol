      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCONTR_AGNDA_FINCR_RECPC)                  *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB09J))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB09J)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCONTR_AGNDA_FINCR_RECPC TABLE
           ( DMOVTO                         DATE NOT NULL,
             NIMAGE_AGNDA_RECPC             DECIMAL(9, 0) NOT NULL,
             COPER_EFETU_CIPAG              CHAR(20) NOT NULL,
             NSEQ_AGNDA_RCBVL               DECIMAL(5, 0) NOT NULL,
             NPRIOR_NEGOC_RCBVL             DECIMAL(9, 0) NOT NULL,
             CCNPJ_ENTID_REG                DECIMAL(9, 0) NOT NULL,
             CFLIAL_ENTID_REG               DECIMAL(4, 0) NOT NULL,
             CCTRL_ENTID_REG                DECIMAL(2, 0) NOT NULL,
             VUND_RCBVL_UTLZD               DECIMAL(17, 2) NOT NULL,
             VUND_RCBVL_FUTUR               DECIMAL(17, 2) NOT NULL,
             DVCTO_OPER_CIPAG               DATE NOT NULL,
             CCNPJ_ENTID_REG_ST             CHAR(9) NOT NULL,
             CFLIAL_ENTID_REG_ST            CHAR(4) NOT NULL,
             CCTRL_ENTID_REG_ST             DECIMAL(2, 0) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCONTR_AGNDA_FINCR_RECPC    *
      ******************************************************************
       01  BVVEB09J.
      *    *************************************************************
           10 DMOVTO               PIC X(10).
      *    *************************************************************
           10 NIMAGE-AGNDA-RECPC   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 COPER-EFETU-CIPAG    PIC X(20).
      *    *************************************************************
           10 NSEQ-AGNDA-RCBVL     PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 NPRIOR-NEGOC-RCBVL   PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-ENTID-REG      PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-ENTID-REG     PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-ENTID-REG      PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 VUND-RCBVL-UTLZD     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 VUND-RCBVL-FUTUR     PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 DVCTO-OPER-CIPAG     PIC X(10).
      *    *************************************************************
           10 CCNPJ-ENTID-REG-ST   PIC X(9).
      *    *************************************************************
           10 CFLIAL-ENTID-REG-ST  PIC X(4).
      *    *************************************************************
           10 CCTRL-ENTID-REG-ST   PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 14      *
      ******************************************************************
