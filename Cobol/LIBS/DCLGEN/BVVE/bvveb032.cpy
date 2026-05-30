      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCERTF_DGTAL_CRIPT)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB032))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB032)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCERTF_DGTAL_CRIPT TABLE
           ( CAUTDD_CERTD                   DECIMAL(1, 0) NOT NULL,
             CID_CERTF_DGTAL                CHAR(40) NOT NULL,
             HATIVO_CERTF_DGTAL             TIMESTAMP NOT NULL,
             HINIC_VALDD_CERTF              TIMESTAMP NOT NULL,
             HFNAL_VALDD_CERTF              TIMESTAMP NOT NULL,
             CPAIS_CERTF_DGTAL              CHAR(2) NOT NULL,
             RORGNZ_CERTF_DGTAL             CHAR(20) NOT NULL,
             IINSTC_CERTF_DGTAL             CHAR(100) NOT NULL,
             RLOC_CERTF_DGTAL               CHAR(15) NOT NULL,
             RUF_CERTF_DGTAL                CHAR(15) NOT NULL,
             NTMNHO_CHAVE_CERTF             DECIMAL(4, 0) NOT NULL,
             ICHAVE_CRIPT_CERTF             CHAR(64) NOT NULL,
             NEXPNT_CHAVE_CERTF             DECIMAL(5, 0) NOT NULL,
             IMDULO_CERTF_DGTAL             VARCHAR(1024) NOT NULL,
             WHASH_CERTF_DGTAL              CHAR(20) NOT NULL,
             WHASH_CERTF_ORIGE              CHAR(20) NOT NULL,
             CAUTDD_CERTD_ORIGE             DECIMAL(1, 0) NOT NULL,
             CID_CERTF_ORIGE                CHAR(40) NOT NULL,
             CISPB_PARTC_MSGEM              DECIMAL(9, 0) NOT NULL,
             CSIT_CERTF                     CHAR(1) NOT NULL,
             CMOTVO_CERTF                   DECIMAL(1, 0) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(9),
             CTPO_CERTF_DGTAL               DECIMAL(1, 0) NOT NULL,
             CISPB_PARTC_MSGEM_ST           CHAR(9) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCERTF_DGTAL_CRIPT          *
      ******************************************************************
       01  BVVEB032.
      *    *************************************************************
           10 CAUTDD-CERTD         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CID-CERTF-DGTAL      PIC X(40).
      *    *************************************************************
           10 HATIVO-CERTF-DGTAL   PIC X(26).
      *    *************************************************************
           10 HINIC-VALDD-CERTF    PIC X(26).
      *    *************************************************************
           10 HFNAL-VALDD-CERTF    PIC X(26).
      *    *************************************************************
           10 CPAIS-CERTF-DGTAL    PIC X(2).
      *    *************************************************************
           10 RORGNZ-CERTF-DGTAL   PIC X(20).
      *    *************************************************************
           10 IINSTC-CERTF-DGTAL   PIC X(100).
      *    *************************************************************
           10 RLOC-CERTF-DGTAL     PIC X(15).
      *    *************************************************************
           10 RUF-CERTF-DGTAL      PIC X(15).
      *    *************************************************************
           10 NTMNHO-CHAVE-CERTF   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 ICHAVE-CRIPT-CERTF   PIC X(64).
      *    *************************************************************
           10 NEXPNT-CHAVE-CERTF   PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 IMDULO-CERTF-DGTAL.
              49 IMDULO-CERTF-DGTAL-LEN
                 PIC S9(4) USAGE COMP-5.
              49 IMDULO-CERTF-DGTAL-TEXT
                 PIC X(1024).
      *    *************************************************************
           10 WHASH-CERTF-DGTAL    PIC X(20).
      *    *************************************************************
           10 WHASH-CERTF-ORIGE    PIC X(20).
      *    *************************************************************
           10 CAUTDD-CERTD-ORIGE   PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CID-CERTF-ORIGE      PIC X(40).
      *    *************************************************************
           10 CISPB-PARTC-MSGEM    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CSIT-CERTF           PIC X(1).
      *    *************************************************************
           10 CMOTVO-CERTF         PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 CTPO-CERTF-DGTAL     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CISPB-PARTC-MSGEM-ST
              PIC X(9).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 27      *
      ******************************************************************
