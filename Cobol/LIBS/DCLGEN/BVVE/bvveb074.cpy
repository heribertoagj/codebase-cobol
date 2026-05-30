      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TETAPA_TRFGO_CATAO)                        *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB074))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB074)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TETAPA_TRFGO_CATAO TABLE
           ( NMNTRC_TRFGO_ARQ               DECIMAL(18, 0) NOT NULL,
             NETAPA_MNTRC_TRFGO             DECIMAL(2, 0) NOT NULL,
             CPTCAO_TBELA                   DECIMAL(3, 0),
             CFASE_PROCM                    DECIMAL(4, 0),
             CIDTFD_SIT_FASE                DECIMAL(1, 0),
             HINIC_ETAPA_MNTRC              TIMESTAMP,
             HFNAL_ETAPA_MNTRC              TIMESTAMP,
             RERRO_ETAPA_MNTRC              CHAR(255),
             WEXCEC_ERRO_BAIXA              VARCHAR(32000),
             NMNTRC_TRFGO_ORIGE             DECIMAL(18, 0),
             NETAPA_MNTRC_ORIGE             DECIMAL(2, 0),
             HENVIO_REG_PLATF               TIMESTAMP
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TETAPA_TRFGO_CATAO          *
      ******************************************************************
       01  BVVEB074.
      *    *************************************************************
           10 NMNTRC-TRFGO-ARQ     PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 NETAPA-MNTRC-TRFGO   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CPTCAO-TBELA         PIC S9(3)V USAGE COMP-3.
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SIT-FASE      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 HFNAL-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 RERRO-ETAPA-MNTRC    PIC X(255).
      *    *************************************************************
           10 WEXCEC-ERRO-BAIXA.
              49 WEXCEC-ERRO-BAIXA-LEN
                 PIC S9(4) USAGE COMP.
              49 WEXCEC-ERRO-BAIXA-TEXT
                 PIC X(32000).
      *    *************************************************************
           10 NMNTRC-TRFGO-ORIGE   PIC S9(18)V USAGE COMP-3.
      *    *************************************************************
           10 NETAPA-MNTRC-ORIGE   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 HENVIO-REG-PLATF     PIC X(26).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 12      *
      ******************************************************************
