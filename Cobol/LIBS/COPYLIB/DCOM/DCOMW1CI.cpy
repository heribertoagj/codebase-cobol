      ******************************************************************
      * NOME BOOK : DCOMW1CI                                           *
      * DESCRICAO : BOOK DE ENTRADA DA LISTA DE OPERACOES POR AGENCIA, *
      *             CNPJ/CPJ E PRODUTO (OPCIONAL)                      *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 19002 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1CI-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW1CI-E-CCNPJ-CPF     = CODIGO DO CNPJ OU CPF               *
      * DCOMW1CI-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW1CI-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW1CI-E-DTINI-PESQ    = DATA INICIO PARA PESQUISA           *
      * DCOMW1CI-E-DTFIM-PESQ    = DATA FIM PARA PESQUESA              *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1CI-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      * DCOMW1CI-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMW1CI-I-DANO-OPER-DESC = ANO DA OPERACAO PESQUISADA         *
      * DCOMW1CI-I-NSEQ-OPER-DESC = SEQUENCIA DA OPERACAO PESQUISADA   *
      * DCOMW1CI-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMW1CI-F-DANO-OPER-DESC = ANO DA OPERACAO PESQUISADA         *
      * DCOMW1CI-F-NSEQ-OPER-DESC = SEQUENCIA DA OPERACAO PESQUISADA   *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW1CI-S-NUM-CONSULTAS     = NUMERO DE OPERACOES PESQUISADAS *
      * DCOMW1CI-S-CJUNC-DEPDC       = CODIGO DA AGENCIA               *
      * DCOMW1CI-S-CCTA-CLIE         = NUMERO DA CONTA                 *
      * DCOMW1CI-S-DIG-CCTA-CLIE     = DIGITO DA CONTA                 *
      * DCOMW1CI-S-IPSSOA-CLIE       = NOME DO CLIENTE                 *
      * DCOMW1CI-S-CPRODT-SERVC      = CODIGO DO PRODUTO               *
      * DCOMW1CI-S-IABREV-PRODT      = DESCRICAO DO PRODUTO            *
      * DCOMW1CI-S-CSPRODT-SERVC     = CODIGO DO SUBPRODUTO            *
      * DCOMW1CI-S-IRSUMO-SPROD-DESC = DESCRICAO DO SUBPRODUTO         *
      * DCOMW1CI-S-COPER-DESC        = CODIGO DA OPERACAO DE DESCONTO  *
      * DCOMW1CI-S-VOPER-DESC-COML   = VALOR DA OPERACAO DE DESCONTO   *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1CI-HEADER.
               10  DCOMW1CI-COD-LAYOUT      PIC  X(08) VALUE 'DCOMW1CI'.
               10  DCOMW1CI-TAM-LAYOUT      PIC  9(05) VALUE 19002.
           05  DCOMW1CI-REGISTRO.
               10  DCOMW1CI-BLOCO-ENTRADA.
                   15  DCOMW1CI-E-CJUNC-DEPDC         PIC  9(05).
                   15  DCOMW1CI-E-CCNPJ-CPF           PIC  9(15).
                   15  DCOMW1CI-E-CCONTA              PIC  9(13).
                   15  DCOMW1CI-E-CPRODT-SERVC        PIC  9(03).
                   15  DCOMW1CI-E-CSPRODT-SERVC       PIC  9(03).
                   15  DCOMW1CI-E-DTINI-PESQ          PIC  X(10).
                   15  DCOMW1CI-E-DTFIM-PESQ          PIC  X(10).
               10  DCOMW1CI-BLOCO-PAGINACAO.
                   15  DCOMW1CI-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMW1CI-P-INICIAL              VALUE 'I'.
                       88  DCOMW1CI-P-PRIMEIRA             VALUE 'P'.
                       88  DCOMW1CI-P-SEGUINTE             VALUE 'S'.
                       88  DCOMW1CI-P-ANTERIOR             VALUE 'A'.
                       88  DCOMW1CI-P-ULTIMA               VALUE 'U'.
                   15  DCOMW1CI-CHAVE-INI.
                       20  DCOMW1CI-I-DANO-OPER-DESC  PIC  9(04).
                       20  DCOMW1CI-I-NSEQ-OPER-DESC  PIC  9(09).
                   15  DCOMW1CI-CHAVE-FIM.
                       20  DCOMW1CI-F-DANO-OPER-DESC  PIC  9(04).
                       20  DCOMW1CI-F-NSEQ-OPER-DESC  PIC  9(09).
               10  DCOMW1CI-BLOCO-SAIDA.
                   15  DCOMW1CI-S-NUM-CONSULTAS       PIC  9(03).
                   15  DCOMW1CI-S-DADOS-SAIDA OCCURS 150 TIMES.
                       20  DCOMW1CI-S-CJUNC-DEPDC     PIC  9(05).
                       20  DCOMW1CI-S-CCTA-CLIE       PIC  9(13).
                       20  DCOMW1CI-S-DIG-CCTA-CLIE   PIC  X(02).
                       20  DCOMW1CI-S-IPSSOA-CLIE     PIC  X(40).
                       20  DCOMW1CI-S-CPRODT-SERVC    PIC  9(03).
                       20  DCOMW1CI-S-IABREV-PRODT    PIC  X(15).
                       20  DCOMW1CI-S-CSPRODT-SERVC   PIC  9(03).
                       20  DCOMW1CI-S-IRSUMO-SPROD-DESC
                                                      PIC  X(15).
                       20  DCOMW1CI-S-COPER-DESC      PIC  9(13).
                       20  DCOMW1CI-S-COPER-DESC-X
                                     REDEFINES    DCOMW1CI-S-COPER-DESC.
                           25  DCOMW1CI-S-DANO-OPER-DESC
                                                      PIC 9(04).
                           25  DCOMW1CI-S-NSEQ-OPER-DESC
                                                      PIC 9(09).
                       20  DCOMW1CI-S-VOPER-DESC-COML PIC 9(15)V99.
