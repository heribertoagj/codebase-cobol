      ******************************************************************
      * NOME BOOK : BVVEW70S - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : MANUTENCAO (INCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : JANEIRO/2022                                       *
      * AUTOR     : MARCELO / MARIANI                                  *
      * TAMANHO   : 060 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW70S-QTD-CRED             : TOTAL DE CREDENCIADORAS        *
      * BVVEW70S-TOT-REG              : TOTAL DE REGISTROS             *
      * BVVEW70S-FLAG-GRADE-CIP       : GRADE CIP ABERTA OU FECHADA    *
      *                                 S - SIM - GRADE ABERTA         *
      *                                 N - NAO - GRADE FECHADA        *
      * BVVEW70S-DATA-CONTRATACAO     : DATA DA GRADE CIP              *
      *                                 DD/MM/AAAA                     *
      * BVVEW70S-HORA-CONTRATACAO     : HORA DA GRADE CIP              *
      *                                 HH:MM:SS                       *
      * BVVEW70S-STATUS-ENVIO-CIP     : SITUACAO ENVIO CIP             *
      *                                 0 - A PROCESSAR                *
      *                                 1 - ENVIADO                    *
      *                                 2 - PROCESSADO                 *
      *                                 3 - NAO OK                     *
      * BVVEW70S-DATA-INCLUSAO-REG    : DATA GRAVACAO REGISTRO NA BASE *
      *                                 DD/MM/AAAA HH:MM:SS            *
      *----------------------------------------------------------------*
       05 BVVEW70S-HEADER.
          10 BVVEW70S-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW70S'.
          10 BVVEW70S-TAM-LAYOUT           PIC  9(05) VALUE 00060.

       05 BVVEW70S-REGISTRO.
          10 BVVEW70S-BLOCO-SAIDA.
             15 BVVEW70S-QTD-CRED          PIC  9(04).
             15 BVVEW70S-TOT-REG           PIC  9(04).
             15 BVVEW70S-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW70S-DATA-CONTRATACAO  PIC  X(10).
             15 BVVEW70S-HORA-CONTRATACAO  PIC  X(08).
             15 BVVEW70S-STATUS-ENVIO-CIP  PIC  9(01).
             15 BVVEW70S-DATA-INCLUSAO-REG PIC  X(19).
