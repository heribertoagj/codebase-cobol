      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TCLASF_EMPTO_RURAL)                        *
      *        LIBRARY(AD.DB2.DCLGEN(CRURB056))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(CRURB056)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TCLASF_EMPTO_RURAL TABLE
           ( CEMPTO_RURAL                   DECIMAL(6, 0) NOT NULL,
             CCLASF_EMPTO_RURAL             DECIMAL(4, 0) NOT NULL,
             ICLASF_EMPTO_RURAL             CHAR(40) NOT NULL,
             RCLASF_EMPTO_RURAL             VARCHAR(256),
             DINIC_VGCIA_REG                DATE NOT NULL,
             DFIM_VGCIA_REG                 DATE,
             CCLASF_PRODT_CONAB             CHAR(6) NOT NULL,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TCLASF_EMPTO_RURAL          *
      ******************************************************************
       01  CRURB056.
      *    *************************************************************
           10 CEMPTO-RURAL         PIC S9(6)V USAGE COMP-3.
      *    *************************************************************
           10 CCLASF-EMPTO-RURAL   PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 ICLASF-EMPTO-RURAL   PIC X(40).
      *    *************************************************************
           10 RCLASF-EMPTO-RURAL.
              49 RCLASF-EMPTO-RURAL-LEN
                 PIC S9(4) USAGE COMP.
              49 RCLASF-EMPTO-RURAL-TEXT
                 PIC X(256).
      *    *************************************************************
           10 DINIC-VGCIA-REG      PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-REG       PIC X(10).
      *    *************************************************************
           10 CCLASF-PRODT-CONAB   PIC X(6).
      *    *************************************************************
           10 CSIT-REG-RURAL       PIC X(1).
      *    *************************************************************
           10 CUSUAR-INCL          PIC X(9).
      *    *************************************************************
           10 HINCL-REG            PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(9).
      *    *************************************************************
           10 HMANUT-REG           PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
