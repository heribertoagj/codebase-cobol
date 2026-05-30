      ******************************************************************
      * NOME BOOK : DCOMW10S                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE OPERACAO POR AGENCIA*
      *             UTILIZADO NOS PROGRAMAS DCOM1100 E DCOM1000        *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 18916 BYTES                                        *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW10S-NUM-CONSULTAS     = NUMERO DE OPERACOES RETORNADAS    *
      * DCOMW10S-CJUNC-DEPDC       = CODIGO DA AGENCIA                 *
      * DCOMW10S-CCTA-CLIE         = NUMERO DA CONTA                   *
      * DCOMW10S-DIG-CCTA-CLIE     = DIGITO DA CONTA                   *
      * DCOMW10S-IPSSOA-CLIE       = NOME DO CLIENTE                   *
      * DCOMW10S-CPRODT-SERVC      = CODIGO DO PRODUTO                 *
      * DCOMW10S-IABREV-PRODT      = DESCRICAO DO PRODUTO              *
      * DCOMW10S-CSPRODT-SERVC     = CODIGO DO SUBPRODUTO              *
      * DCOMW10S-IRSUMO-SPROD-DESC = DESCRICAO DO SUBPRODUTO           *
      * DCOMW10S-COPER-DESC        = CODIGO DA OPERACAO DE DESCONTO    *
      * DCOMW10S-VOPER-DESC-COML   = VALOR DA OPERACAO DE DESCONTO     *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW10S-HEADER.
               10 DCOMW10S-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW10S'.
               10 DCOMW10S-TAM-LAYOUT       PIC  9(05) VALUE 18916.
           05  DCOMW10S-SAIDA.
               10 DCOMW10S-NUM-CONSULTAS    PIC  9(03).
               10 DCOMW10S-DADOS-SAIDA OCCURS 150 TIMES.
                  15  DCOMW10S-CJUNC-DEPDC       PIC  9(05).
                  15  DCOMW10S-CCTA-CLIE         PIC  9(13).
                  15  DCOMW10S-DIG-CCTA-CLIE     PIC  X(02).
                  15  DCOMW10S-IPSSOA-CLIE       PIC  X(40).
                  15  DCOMW10S-CPRODT-SERVC      PIC  9(03).
                  15  DCOMW10S-IABREV-PRODT      PIC  X(15).
                  15  DCOMW10S-CSPRODT-SERVC     PIC  9(03).
                  15  DCOMW10S-IRSUMO-SPROD-DESC PIC  X(15).
                  15  DCOMW10S-COPER-DESC        PIC  9(13).
                  15  DCOMW10S-VOPER-DESC-COML   PIC  9(15)V99.
