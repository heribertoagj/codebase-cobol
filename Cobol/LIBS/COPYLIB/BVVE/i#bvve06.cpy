      ******************************************************************
      *  NOME DA INC:     I#BVVE06                                     *
      *  ROTINA:          BVVE                                         *
      *  DATA DA CRIACAO: 05/03/1999                                   *
      *  LRECL:           150                                          *
      *  OBJETIVO:        CADASTRO REDUZIDO DE PARCELADOS ENVIADO      *
      *                   PELA VISANET                                 *
      *                   CAMPOS NUMERICOS ZONADOS                     *
      *                   FORMATO DA FITA PARA O 4025-BRADESCO CARTOES *
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
           03  HEA-AGENCIA                PIC 9(05).
           03  HEA-DIG-AGEN               PIC X(01).
           03  HEA-CONTA-CORR             PIC 9(09).
           03  HEA-DIG-CCORR              PIC X(01).
           03  HEA-TIPO                   PIC 9(02).
           03  HEA-NOME-ARQUIVO           PIC X(08).
           03  HEA-DATA-MOVTO             PIC 9(09).
           03  HEA-DATA-PROC              PIC 9(09).
           03  HEA-REMESSA-VISANET        PIC 9(05).
           03  HEA-REMESSA-BANCO          PIC 9(04).
           03  HEA-BRANCOS                PIC X(88).
           03  HEA-SEQ-HEADER             PIC 9(09).


      *********************------------------------------**************

      **
       01  CAD-DETALHE.
      **
           03  DET-AGENCIA                PIC 9(05).
           03  DET-DIG-AGEN               PIC X(01).
           03  DET-CONTA-CORR             PIC 9(09).
           03  DET-DIG-CCORR              PIC X(01).
           03  DET-BI005WK1.
               05  DET-BI005-PARPRINC     PIC 9(03).
               05  DET-BI005-PARASSOC     PIC 9(03).
               05  DET-BI005-PARCHAIN     PIC 9(03).
               05  DET-BI005-PARESTAB     PIC 9(10).
               05  DET-BI005-PARREOPE     PIC 9(07).
               05  DET-BI005-PARCDINT     PIC 9(11).
               05  DET-BI005-PARCDTVCT    PIC X(10).
               05  DET-BI005-PARNUPAR     PIC 9(02).
               05  DET-BI005-PARPAABE     PIC 9(02).
               05  DET-BI005-PARCDBCO     PIC 9(04).
               05  DET-BI005-PARSTATUS    PIC X(02).
               05  DET-BI005-PARVLBRU     PIC 9(09)V99.
               05  DET-BI005-PARVLLIQ     PIC 9(09)V99.
               05  DET-BI005-PARNRREF     PIC 9(09).
               05  DET-BI005-PARCDICV     PIC 9(11).
               05  DET-BI005-PARESTSB     PIC 9(10).
               05  DET-BI05-BRANCOS       PIC X(16).
               05  DET-SEQ-HEADER         PIC 9(09).


      *********************------------------------------**************

      **
       01  CAD-TRAILER.
      **
           03  TRA-AGENCIA                PIC 9(05).
           03  TRA-DIG-AGEN               PIC X(01).
           03  TRA-CONTA-CORR             PIC 9(09).
           03  TRA-DIG-CCORR              PIC X(01).
           03  TRA-VLR-TOTAL              PIC 9(15)V99.
           03  TRA-BRANCOS                PIC X(108).
           03  TRA-SEQ-TRAILER            PIC 9(09).



      *===============================================================*
