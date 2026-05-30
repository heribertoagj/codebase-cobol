      ******************************************************************
      * DCLGEN TABLE(DB2PRD.V01EMIS_BOLET_VISA)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BACCV022))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BACCV022)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.V01EMIS_BOLET_VISA TABLE
           ( CBOLET_VISA_CANCT              DECIMAL(5, 0) NOT NULL,
             DINIC_VGCIA                    DATE NOT NULL,
             DFIM_VGCIA                     DATE NOT NULL,
             DPROCM                         DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.V01EMIS_BOLET_VISA          *
      ******************************************************************
       01  BACCV022.
      *    *************************************************************
           10 CBOLET-VISA-CANCT    PIC S9(5)V USAGE COMP-3.
      *    *************************************************************
           10 DINIC-VGCIA          PIC X(10).
      *    *************************************************************
           10 DFIM-VGCIA           PIC X(10).
      *    *************************************************************
           10 DPROCM               PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 4       *
      ******************************************************************
