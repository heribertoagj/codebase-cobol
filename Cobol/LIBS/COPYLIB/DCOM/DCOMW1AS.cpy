      ******************************************************************
      * NOME BOOK : DCOMW1AS                                           *
      * DESCRICAO : BOOK DE SAIDA DA PESQUISA DE CONTRATO POR AGENCIA  *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 20176 BYTES                                        *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW1AS-NUM-CONSULTAS   = NUMERO DE CONTRATOS PESQUISADOS     *
      * DCOMW1AS-CCONTR-LIM-DESC = NUMERO DO CONTRATO DE LIMITE        *
      * DCOMW1AS-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW1AS-CCTA-CLIE       = NUMERO DA CONTA                     *
      * DCOMW1AS-DIG-CCTA-CLIE   = DIGITO DA CONTA                     *
      * DCOMW1AS-IPSSOA-CLIE     = NOME DO CLIENTE                     *
      * DCOMW1AS-DVCTO-CONTR-LIM = DIGO DO PRODUTO                     *
      * DCOMW1AS-VCONTR-LIM-DESC = VALOR DO CONTRATO DE LIMITE         *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1AS-HEADER.
               10 DCOMW1AS-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW1AS'.
               10 DCOMW1AS-TAM-LAYOUT       PIC  9(05) VALUE 20176.
           05  DCOMW1AS-BLOCO-SAIDA.
               10  DCOMW1AS-NUM-CONSULTAS         PIC  9(03).
               10  DCOMW1AS-DADOS-SAIDA           OCCURS 210 TIMES.
                   15  DCOMW1AS-CCONTR-LIM-DESC   PIC  9(09).
                   15  DCOMW1AS-CJUNC-DEPDC       PIC  9(05).
                   15  DCOMW1AS-CCTA-CLIE         PIC  9(13).
                   15  DCOMW1AS-DIG-CCTA-CLIE     PIC  X(02).
                   15  DCOMW1AS-IPSSOA-CLIE       PIC  X(40).
                   15  DCOMW1AS-DVCTO-CONTR-LIM   PIC  X(10).
                   15  DCOMW1AS-VCONTR-LIM-DESC   PIC  9(15)V99.
