      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCRDRA_ESTBL_COML)                         *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB040))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB040)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCRDRA_ESTBL_COML TABLE
           ( CCRDRA_ESTBL_COML              DECIMAL(4, 0) NOT NULL,
             CCNPJ_CRDRA_ESTBL              DECIMAL(9, 0),
             CFLIAL_CRDRA_ESTBL             DECIMAL(4, 0),
             CCTRL_CRDRA_ESTBL              DECIMAL(2, 0),
             IREDZD_CRDRA_ESTBL             CHAR(30),
             ICRDRA_ESTBL_COML              CHAR(50),
             HINCL_REG                      TIMESTAMP,
             CUSUAR_INCL                    CHAR(9),
             HMANUT_REG                     TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7),
             CCNPJ_CRDRA_ESTBL_ST           CHAR(9),
             CFLIAL_CRDRA_ESTBL_ST          CHAR(4),
             CCTRL_CRDRA_ESTBL_ST           DECIMAL(2, 0)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCRDRA_ESTBL_COML           *
      ******************************************************************
       01  BVVEB040.
      *    *************************************************************
           10 CCRDRA-ESTBL-COML    PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCNPJ-CRDRA-ESTBL    PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 CFLIAL-CRDRA-ESTBL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CCTRL-CRDRA-ESTBL    PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 IREDZD-CRDRA-ESTBL   PIC X(30).
      *    *************************************************************
           10 ICRDRA-ESTBL-COML    PIC X(50).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 CCNPJ-CRDRA-ESTBL-ST
              PIC X(9).
      *    *************************************************************
           10 CFLIAL-CRDRA-ESTBL-ST
              PIC X(4).
      *    *************************************************************
           10 CCTRL-CRDRA-ESTBL-ST
              PIC S9(2)V USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
