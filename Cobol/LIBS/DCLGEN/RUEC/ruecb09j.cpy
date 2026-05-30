      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TAPOLC_SEGUR_OBRIG)                        *
      *        LIBRARY(AD.DB2.DCLGEN(RUECB09J))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(RUECB09J)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TAPOLC_SEGUR_OBRIG TABLE
           ( CJUNC_DEPDC                    DECIMAL(5, 0) NOT NULL,
             CCDULA                         DECIMAL(9, 0) NOT NULL,
             NAPOLC_SEGUR_OBRIG             DECIMAL(3, 0) NOT NULL,
             CTPO_SEGUR_CONTT               DECIMAL(1, 0) NOT NULL,
             CSEGDR                         DECIMAL(4, 0),
             PALIQT_SEGUR_BACEN             DECIMAL(7, 4),
             VCOBER_APOLC_SEGUR             DECIMAL(17, 2),
             CAPOLC_SEGUR_RURAL             CHAR(20),
             DINIC_VGCIA_SEGUR              DATE,
             DFIM_VGCIA_SEGUR               DATE,
             CSIT_REG_RURAL                 CHAR(1) NOT NULL,
             CUSUAR_INCL                    CHAR(9) NOT NULL,
             HINCL_REG                      TIMESTAMP NOT NULL,
             CUSUAR_MANUT                   CHAR(9),
             HMANUT_REG                     TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TAPOLC_SEGUR_OBRIG          *
      ******************************************************************
       01  RUECB09J.
      *    *************************************************************
           10 CJUNC-DEPDC          PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 CCDULA               PIC S9(9)V USAGE COMP-3.
      *    *************************************************************
           10 NAPOLC-SEGUR-OBRIG   PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CTPO-SEGUR-CONTT     PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 CSEGDR               PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 PALIQT-SEGUR-BACEN   PIC S9(3)V9(4) USAGE COMP-3.
      *    *************************************************************
           10 VCOBER-APOLC-SEGUR   PIC S9(15)V9(2) USAGE COMP-3.
      *    *************************************************************
           10 CAPOLC-SEGUR-RURAL   PIC X(20).
      *    *************************************************************
           10 DINIC-VGCIA-SEGUR    PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA-SEGUR     PIC X(10).
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
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 15      *
      ******************************************************************
