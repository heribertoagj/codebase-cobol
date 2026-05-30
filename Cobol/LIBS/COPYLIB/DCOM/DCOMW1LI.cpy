      ******************************************************************
      * NOME BOOK : DCOMW1LE                                           *
      * DESCRICAO : BOOK DE INTERFACE PARA TRATAR CONTRATO POR AGENCIA *
      *             E CONTA OU AGENCIA E CNPJ/CPF                      *
      * DATA      : 09/11/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 072 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1LI-E-CJUNC-DEPDC    = CODIGO DA AGENCIA                  *
      * DCOMW1LI-E-CCTA-BCRIA-CLI = NUMERO DA CONTA                    *
      * DCOMW1LI-E-CCNPJ-CPF      = CORPO DO CNPJ/CPF                  *
      * DCOMW1LI-E-CFLIAL-CNPJ    = FILIAL DO CNPJ OU ZEROS PARA CPF   *
      * DCOMW1LI-E-CCTRL-CNPJ-CPF = CONTROLE DO CNPJ/CPF               *
      * DCOMW1LI-E-CPRODT-SERVC   = CODIGO DO PRODUTO                  *
      * DCOMW1LI-E-CSPRODT-SERVC  = CODIGO DO SUBPRODUTO               *
      * DCOMW1LI-E-DTINI-PESQ     = DATA INICIO DE PESQUISA            *
      * DCOMW1LI-E-DTFIM-PESQ     = DATA FIM DE PESQUISA               *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1LI-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: LSTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      * DCOMW1LI-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMW1LI-I-CCONTR-LIM-DESC  = NUMERO DO CONTRATO INICIAL       *
      * DCOMW1LI-I-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO INICIAL       *
      * DCOMW1LI-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMW1LI-F-CCONTR-LIM-DESC  = NUMERO DO CONTRATO FINAL         *
      * DCOMW1LI-F-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO FINAL         *
      ******************************************************************
      * DCOMW1LI-S-NUM-CONSULTAS    = NUMERO DE OPERACOES PESQUISADAS  *
      * DCOMW1LI-S-CCONTR-LIM-DESC  = NUMERO DO CONTRATO FINAL         *
      * DCOMW1LI-S-CJUNC-DEPDC      = CODIGO DA AGENCIA                *
      * DCOMW1LI-S-CCTA-CLIE        = NUMERO DA CONTA                  *
      * DCOMW1LI-S-DIG-CCTA-CLIE    = DIGITO DA CONTA                  *
      * DCOMW1LI-S-IPSSOA-CLIE      = NOME DO CLIENTE                  *
      * DCOMW1LI-S-DVCTO-CONTR-LIM  = DATA DE VENCIMENTO DO CONTRATO   *
      * DCOMW1LI-S-VCONTR-LIM-DESC  = VALOR DO CONTRATO DE LIMITE      *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1LI-REGISTRO.
               10 DCOMW1LI-E-HEADER.
                   15 DCOMW1LI-E-COD-LAYOUT PIC  X(08) VALUE 'DCOMW1LE'.
                   15 DCOMW1LI-E-TAM-LAYOUT PIC  9(05) VALUE 72.
               10 DCOMW1LI-BLOCO-ENTRADA.
                   15 DCOMW1LI-E-CJUNC-DEPDC           PIC  9(05).
                   15 DCOMW1LI-E-CCTA-BCRIA-CLI        PIC  9(13).
                   15 DCOMW1LI-E-CCNPJ-CPF             PIC  9(09).
                   15 DCOMW1LI-E-CFLIAL-CNPJ           PIC  9(04).
                   15 DCOMW1LI-E-CCTRL-CNPJ-CPF        PIC  9(02).
                   15 DCOMW1LI-E-CPRODT-SERVC          PIC  9(03).
                   15 DCOMW1LI-E-CSPRODT-SERVC         PIC  9(03).
                   15 DCOMW1LI-E-DTINI-PESQ            PIC  X(10).
                   15 DCOMW1LI-E-DTFIM-PESQ            PIC  X(10).
               10  DCOMW1LI-BLOCO-PAGINACAO.
                   15  DCOMW1LI-INDICADOR-PAGINACAO    PIC  X(01).
                       88  DCOMW1LI-P-INICIAL          VALUE 'I'.
                       88  DCOMW1LI-P-PRIMEIRA         VALUE 'P'.
                       88  DCOMW1LI-P-SEGUINTE         VALUE 'S'.
                       88  DCOMW1LI-P-ANTERIOR         VALUE 'A'.
                       88  DCOMW1LI-P-ULTIMA           VALUE 'U'.
                   15  DCOMW1LI-CHAVE-INI.
                       20  DCOMW1LI-I-CCONTR-LIM-DESC  PIC  9(09).
                       20  DCOMW1LI-I-CVRSAO-CONTR-LIM PIC  9(03).
                   15  DCOMW1LI-CHAVE-FIM.
                       20  DCOMW1LI-F-CCONTR-LIM-DESC  PIC  9(09).
                       20  DCOMW1LI-F-CVRSAO-CONTR-LIM PIC  9(03).
               10  DCOMW1LI-BLOCO-SAIDA.
                   15  DCOMW1LI-S-NUM-CONSULTAS        PIC  9(03).
                   15  DCOMW1LI-S-DADOS-SAIDA          OCCURS 150 TIMES.
                       20  DCOMW1LI-S-CCONTR-LIM-DESC  PIC  9(09).
                       20  DCOMW1LI-S-CJUNC-DEPDC      PIC  9(05).
                       20  DCOMW1LI-S-CCTA-CLIE        PIC  9(13).
                       20  DCOMW1LI-S-DIG-CCTA-CLIE    PIC  X(02).
                       20  DCOMW1LI-S-IPSSOA-CLIE      PIC  X(40).
                       20  DCOMW1LI-S-DVCTO-CONTR-LIM  PIC  X(10).
                       20  DCOMW1LI-S-VCONTR-LIM-DESC  PIC  9(15)V99.
