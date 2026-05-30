      ******************************************************************
      * DCLGEN TABLE(DB2PRD.TETAPA_DIA_CATAO)                          *
      *        LIBRARY(AD.DB2.DCLGEN(BVVEB072))                        *
      *        ACTION(REPLACE)                                         *
      *        LANGUAGE(COBOL)                                         *
      *        STRUCTURE(BVVEB072)                                     *
      *        APOST                                                   *
      *        LABEL(YES)                                              *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE DB2PRD.TETAPA_DIA_CATAO TABLE
           ( CMNTRC_MSGRA                   CHAR(23) NOT NULL,
             NETAPA_MNTRC_MSGRA             DECIMAL(2, 0) NOT NULL,
             CFASE_PROCM                    DECIMAL(4, 0),
             CIDTFD_SIT_FASE                DECIMAL(1, 0) NOT NULL,
             HINIC_ETAPA_MNTRC              TIMESTAMP NOT NULL,
             HFNAL_ETAPA_MNTRC              TIMESTAMP,
             CUSUAR_MANUT                   CHAR(7),
             RERRO_ETAPA_MNTRC              CHAR(255)
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DB2PRD.TETAPA_DIA_CATAO            *
      ******************************************************************
       01  BVVEB072.
      *    *************************************************************
           10 CMNTRC-MSGRA         PIC X(23).
      *    *************************************************************
           10 NETAPA-MNTRC-MSGRA   PIC S9(2)V USAGE COMP-3.
      *    *************************************************************
           10 CFASE-PROCM          PIC S9(4)V USAGE COMP-3.
      *    *************************************************************
           10 CIDTFD-SIT-FASE      PIC S9(1)V USAGE COMP-3.
      *    *************************************************************
           10 HINIC-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 HFNAL-ETAPA-MNTRC    PIC X(26).
      *    *************************************************************
           10 CUSUAR-MANUT         PIC X(7).
      *    *************************************************************
           10 RERRO-ETAPA-MNTRC    PIC X(255).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
