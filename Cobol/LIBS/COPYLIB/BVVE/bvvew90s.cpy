      ******************************************************************
      * NOME BOOK : BVVEW90S - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : MANUTENCAO (EXCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTOUT)             *
      * DATA      : SETEMBRO/2022                                      *
      * AUTOR     : MARCELO                                            *
      * TAMANHO   : 060 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW90S-QTD-CRED             : TOTAL DE CREDENCIADORAS        *
      * BVVEW90S-TOT-REG              : TOTAL DE REGISTROS             *
      * BVVEW90S-FLAG-GRADE-CIP       : GRADE CIP ABERTA OU FECHADA    *
      *                                 S - SIM - GRADE ABERTA         *
      *                                 N - NAO - GRADE FECHADA        *
      * BVVEW90S-DATA-CONTRATACAO     : DATA DA GRADE CIP              *
      *                                 DD/MM/AAAA                     *
      * BVVEW90S-HORA-CONTRATACAO     : HORA DA GRADE CIP              *
      *                                 HH:MM:SS                       *
      * BVVEW90S-STATUS-ENVIO-CIP     : SITUACAO ENVIO CIP             *
      *                                 0 - A PROCESSAR                *
      *                                 1 - ENVIADO                    *
      *                                 2 - PROCESSADO                 *
      *                                 3 - NAO OK                     *
      * BVVEW90S-DATA-INCLUSAO-REG    : DATA GRAVACAO REGISTRO NA BASE *
      *                                 DD/MM/AAAA HH:MM:SS            *
      *----------------------------------------------------------------*
       05 BVVEW90S-HEADER.
          10 BVVEW90S-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW90S'.
          10 BVVEW90S-TAM-LAYOUT           PIC  9(05) VALUE 00060.

       05 BVVEW90S-REGISTRO.
          10 BVVEW90S-BLOCO-SAIDA.
             15 BVVEW90S-QTD-CRED          PIC  9(04).
             15 BVVEW90S-TOT-REG           PIC  9(04).
             15 BVVEW90S-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW90S-DATA-CONTRATACAO  PIC  X(10).
             15 BVVEW90S-HORA-CONTRATACAO  PIC  X(08).
             15 BVVEW90S-STATUS-ENVIO-CIP  PIC  9(01).
             15 BVVEW90S-DATA-INCLUSAO-REG PIC  X(19).
