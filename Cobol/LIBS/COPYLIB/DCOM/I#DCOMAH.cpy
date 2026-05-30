      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0PB - DB2PRD.TCONS_REJTD_DESC                       *
      *    - DCOMB0PC - DB2PRD.TOPER_REJTD_DESC                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7PAS - LISTA                                         *
      *===============================================================*
       01  AH-ARGUMENTOS-ENTRADA.
      *---------------------------------------------------------------*
      * CASO A CONEXAO DB2 UTILIZADA SEJA CKRS DEVERA INFORMAR
      * PARCIAL-TI  CASO CONTRARIO SERA UTILIZADO BRAD
      *---------------------------------------------------------------*
       05  AH-INSTRUCAO                PIC  X(010).
      *---------------------------------------------------------------*
      *  INFORMAR A PAGINA QUE DESEJA SER EXIBIDA
      *---------------------------------------------------------------*
       05  AH-PAGINA                   PIC S9(005) COMP-3.
       05  AH-LIN-SOLCTD               PIC S9(002) COMP-3.
       05  AH-FILTRO.
           10 AH-CCNPJ-ENTID           PIC S9(09)  COMP-3.
           10 AH-CFLIAL-CNPJ           PIC S9(04)  COMP-3.
           10 AH-CCTRL-CNPJ            PIC S9(02)  COMP-3.
           10 AH-DANO-INIC             PIC S9(06)  COMP-3.
           10 AH-DANO-FIM              PIC S9(06)  COMP-3.
           10 AH-E-REFT-OPER-RURAL      PIC X(11).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  AH-RETORNO.
       05  AH-COD-RETORNO              PIC  9(004).
       05  AH-MENSAGEM                 PIC  X(079).

      *****************************************************************
      * AREA DA LISTA                                                 *
      *****************************************************************
       01  AH-ARGUMENTOS-SAIDA.
       05  AH-QTD-CEDULA              PIC S9(07) COMP-3.
       05  AH-TOT-REG                 PIC S9(07) COMP-3.
       05  AH-QTD-REG                 PIC S9(02) COMP-3.
       05  AH-LISTA                   OCCURS 50 TIMES.
           10 AH-NCONS-REJTD-DESC     PIC S9(05) COMP-3.
           10 AH-NOPER-REJTD-DESC     PIC S9(07) COMP-3.
           10 AH-S-CREFT-OPER-RURAL   PIC X(11).
           10 AH-NORD-DSTNO-ORIGN     PIC S9(03) COMP-3.
           10 AH-DANO-REFT-REJTD      PIC S9(06) COMP-3.
           10 AH-CSIT-OPER-RURAL      PIC X(02).
           10 AH-CMOTVO-RCUSA-DESC    PIC X(03).
           10 AH-HIMPOR-ARQ-CREDT     PIC X(26).
           10 AH-HINCL-REG            PIC X(26).
