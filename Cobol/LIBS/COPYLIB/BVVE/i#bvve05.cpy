      ******************************************************************
      *  NOME DA INC:     I#BVVE05                                     *
      *  ROTINA:          BVVE                                         *
      *  DATA DA CRIACAO: 05/03/1999                                   *
      *  LRECL:           100                                          *
      *  OBJETIVO:        CADASTRO REDUZIDO DE PARCELADOS ENVIADO      *
      *                   PELA VISANET                                 *
      *                   CAMPOS NUMERICOS COMPACTADOS                 *
      *                                                                *
      *   HEADER:   AGENCIA E CONTA ZERADAS                            *
      *   DETALHES: AGENCIA E CONTA DIFERENTES DE ZEROS E NOVES        *
      *   TRAILLER: AGENCIA E CONTA IGUAIS A NOVES                     *
      *                                                                *
      *                                              KAMBARA           *
      ******************************************************************
      **
       01  CAD-HEADER.
      **
           03  HEA-AGENCIA                PIC S9(05)    COMP-3.
           03  HEA-DIG-AGEN               PIC  X(01).
           03  HEA-CONTA-CORR             PIC S9(09)    COMP-3.
           03  HEA-DIG-CCORR              PIC  X(01).
           03  HEA-TIPO                   PIC  9(02).
           03  HEA-NOME-ARQUIVO           PIC  X(08).
           03  HEA-DATA-MOVTO             PIC S9(09)    COMP-3.
           03  HEA-DATA-PROC              PIC S9(09)    COMP-3.
           03  HEA-REMESSA-VISANET        PIC S9(05)    COMP-3.
           03  HEA-REMESSA-BANCO          PIC  9(04).
           03  FILLER                     PIC  X(58).
           03  HEA-SEQ-HEADER             PIC S9(09)    COMP-3.


      *===============================================================*

      **
       01  CAD-DETALHE.
      **
           03  DET-AGENCIA                PIC S9(05)    COMP-3.
           03  DET-DIG-AGEN               PIC  X(01).
           03  DET-CONTA-CORR             PIC S9(09)    COMP-3.
           03  DET-DIG-CCORR              PIC  X(01).
           03  DET-BI005WK1.
               05  DET-BI005-PARPRINC     PIC S9(03)    COMP-3.
               05  DET-BI005-PARASSOC     PIC S9(03)    COMP-3.
               05  DET-BI005-PARCHAIN     PIC S9(03)    COMP-3.
               05  DET-BI005-PARESTAB     PIC S9(10)    COMP-3.
               05  DET-BI005-PARREOPE     PIC S9(07)    COMP-3.
               05  DET-BI005-PARCDINT     PIC S9(11)    COMP-3.
               05  DET-BI005-PARCDTVCT    PIC X(10).
               05  DET-BI005-PARNUPAR     PIC S9(02)    COMP-3.
               05  DET-BI005-PARPAABE     PIC S9(02)    COMP-3.
               05  DET-BI005-PARCDBCO     PIC S9(04)    COMP-3.
               05  DET-BI005-PARSTATUS    PIC X(02).
               05  DET-BI005-PARVLBRU     PIC S9(09)V99 COMP-3.
               05  DET-BI005-PARVLLIQ     PIC S9(09)V99 COMP-3.
               05  DET-BI005-PARNRREF     PIC S9(09)    COMP-3.
               05  DET-BI005-PARCDICV     PIC S9(11)    COMP-3.
               05  DET-BI005-PARESTSB     PIC S9(10)    COMP-3.
               05  FILLER                 PIC X(15).
               05  DET-SEQ-HEADER         PIC S9(09)    COMP-3.


      *===============================================================*

      **
       01  CAD-TRAILER.
      **
           03  TRA-AGENCIA                PIC S9(05)    COMP-3.
           03  TRA-DIG-AGEN               PIC  X(01).
           03  TRA-CONTA-CORR             PIC S9(09)    COMP-3.
           03  TRA-DIG-CCORR              PIC  X(01).
           03  TRA-VLR-TOTAL              PIC S9(15)V99 COMP-3.
           03  FILLER                     PIC  X(76).
           03  TRA-SEQ-TRAILER            PIC S9(09)    COMP-3.



      *===============================================================*
