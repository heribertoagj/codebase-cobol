      ******************************************************************
      * NOME BOOK : DCOMW10I                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE OPERACAO POR AGENCIA*
      *             UTILIZADO NO PROGRAMA DCOM1100 E DCOM3100          *
      * DATA      : 28/09/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 18974 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW10I-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW10I-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW10I-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW10I-E-DTINI-PESQ    = DATA INICIO PARA PESQUISA           *
      * DCOMW10I-E-DTFIM-PESQ    = DATA FIM PARA PESQUESA              *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW10I-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      * DCOMW10I-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMW10I-I-DANO-OPER-DESC = ANO DA OPERACAO PESQUISADA         *
      * DCOMW10I-I-NSEQ-OPER-DESC = SEQUENCIA DA OPERACAO PESQUISADA   *
      * DCOMW10I-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMW10I-F-DANO-OPER-DESC = ANO DA OPERACAO PESQUISADA         *
      * DCOMW10I-F-NSEQ-OPER-DESC = SEQUENCIA DA OPERACAO PESQUISADA   *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW10I-S-NUM-CONSULTAS     = NUMERO DE OPERACOES PESQUISADAS *
      * DCOMW10I-S-CJUNC-DEPDC       = CODIGO DA AGENCIA               *
      * DCOMW10I-S-CCTA-CLIE         = NUMERO DA CONTA                 *
      * DCOMW10I-S-DIG-CCTA-CLIE     = DIGITO DA CONTA                 *
      * DCOMW10I-S-IPSSOA-CLIE       = NOME DO CLIENTE                 *
      * DCOMW10I-S-CPRODT-SERVC      = CODIGO DO PRODUTO               *
      * DCOMW10I-S-IABREV-PRODT      = DESCRICAO DO PRODUTO            *
      * DCOMW10I-S-CSPRODT-SERVC     = CODIGO DO SUBPRODUTO            *
      * DCOMW10I-S-IRSUMO-SPROD-DESC = DESCRICAO DO SUBPRODUTO         *
      * DCOMW10I-S-COPER-DESC        = CODIGO DA OPERACAO DE DESCONTO  *
      * DCOMW10I-S-VOPER-DESC-COML   = VALOR DA OPERACAO DE DESCONTO   *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW10I-HEADER.
               10  DCOMW10I-COD-LAYOUT      PIC  X(08) VALUE 'DCOMW10I'.
               10  DCOMW10I-TAM-LAYOUT      PIC  9(05) VALUE 18974.
           05  DCOMW10I-REGISTRO.
               10  DCOMW10I-BLOCO-ENTRADA.
                   15  DCOMW10I-E-CJUNC-DEPDC         PIC  9(05).
                   15  DCOMW10I-E-CPRODT-SERVC        PIC  9(03).
                   15  DCOMW10I-E-CSPRODT-SERVC       PIC  9(03).
                   15  DCOMW10I-E-DTINI-PESQ          PIC  X(10).
                   15  DCOMW10I-E-DTFIM-PESQ          PIC  X(10).
               10  DCOMW10I-BLOCO-PAGINACAO.
                   15  DCOMW10I-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMW10I-P-INICIAL              VALUE 'I'.
                       88  DCOMW10I-P-PRIMEIRA             VALUE 'P'.
                       88  DCOMW10I-P-SEGUINTE             VALUE 'S'.
                       88  DCOMW10I-P-ANTERIOR             VALUE 'A'.
                       88  DCOMW10I-P-ULTIMA               VALUE 'U'.
                   15  DCOMW10I-CHAVE-INI.
                       20  DCOMW10I-I-DANO-OPER-DESC  PIC  9(04).
                       20  DCOMW10I-I-NSEQ-OPER-DESC  PIC  9(09).
                   15  DCOMW10I-CHAVE-FIM.
                       20  DCOMW10I-F-DANO-OPER-DESC  PIC  9(04).
                       20  DCOMW10I-F-NSEQ-OPER-DESC  PIC  9(09).
               10  DCOMW10I-BLOCO-SAIDA.
                   15  DCOMW10I-S-NUM-CONSULTAS       PIC  9(03).
                   15  DCOMW10I-S-DADOS-SAIDA OCCURS 150 TIMES.
                       20  DCOMW10I-S-CJUNC-DEPDC     PIC  9(05).
                       20  DCOMW10I-S-CCTA-CLIE       PIC  9(13).
                       20  DCOMW10I-S-DIG-CCTA-CLIE   PIC  X(02).
                       20  DCOMW10I-S-IPSSOA-CLIE     PIC  X(40).
                       20  DCOMW10I-S-CPRODT-SERVC    PIC  9(03).
                       20  DCOMW10I-S-IABREV-PRODT    PIC  X(15).
                       20  DCOMW10I-S-CSPRODT-SERVC   PIC  9(03).
                       20  DCOMW10I-S-IRSUMO-SPROD-DESC
                                                      PIC  X(15).
                       20  DCOMW10I-S-COPER-DESC      PIC  9(13).
                       20  DCOMW10I-S-COPER-DESC-X
                                     REDEFINES    DCOMW10I-S-COPER-DESC.
                           25  DCOMW10I-S-DANO-OPER-DESC
                                                      PIC 9(04).
                           25  DCOMW10I-S-NSEQ-OPER-DESC
                                                      PIC 9(09).
                       20  DCOMW10I-S-VOPER-DESC-COML PIC 9(15)V99.
