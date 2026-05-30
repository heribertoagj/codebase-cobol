      ******************************************************************
      * NOME BOOK : DCOMW1CW                                           *
      * DESCRICAO : BOOK DE TRABALHO DA LISTA DE OPERACOES POR AGENCIA *
      *             CNPJ OU CPF E PRODUTO(OPCIONAL)                    *
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 088 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1CW-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW1CW-E-CCPF-CNPJ     = CODIGO DO CNPJ OU CPF               *
      * DCOMW1CW-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW1CW-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW1CW-E-DTINI-PESQ    = DATA INICIO DE PESQUISA             *
      * DCOMW1CW-E-DTFIM-PESQ    = DATA FIM DE PESQUISA                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1CW-INDICADOR-PAGINACAO = INDICA DIRECAO DA PAGINACAO     *
      * OBS.: PARA O PROCESSO NO TF A PAGINACAO EH INCIAL OU SEGUINTE  *
      * DCOMW1CW-I-DANO-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW1CW-I-NSEQ-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW1CW-F-DANO-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      * DCOMW1CW-F-NSEQ-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      *********************** DADOS DE RETORNO DA PAGINACAO ************
      * DCOMW1CW-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05 DCOMW1CW-HEADER.
              10 DCOMW1CW-COD-LAYOUT        PIC  X(08) VALUE 'DCOMW1CW'.
              10 DCOMW1CW-TAM-LAYOUT        PIC  9(05) VALUE 88.
           05 DCOMW1CW-REGISTRO.
              10 DCOMW1CW-BLOCO-ENTRADA.
                 15 DCOMW1CW-E-CJUNC-DEPDC       PIC  9(05).
                 15 DCOMW1CW-E-CCPF-CNPJ         PIC  9(15).
                 15 DCOMW1CW-E-CPRODT-SERVC      PIC  9(03).
                 15 DCOMW1CW-E-CSPRODT-SERVC     PIC  9(03).
                 15 DCOMW1CW-E-DTINI-PESQ        PIC  X(10).
                 15 DCOMW1CW-E-DTFIM-PESQ        PIC  X(10).
             10 DCOMW1CW-BLOCO-PAGINACAO.
                 15 DCOMW1CW-INDICADOR-PAGINACAO PIC  X(01).
                   88 DCOMW1CW-P-INICIAL              VALUE 'I'.
                   88 DCOMW1CW-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMW1CW-P-SEGUINTE             VALUE 'S'.
                   88 DCOMW1CW-P-ANTERIOR             VALUE 'A'.
                   88 DCOMW1CW-P-ULTIMA               VALUE 'U'.
                15 DCOMW1CW-CHAVE-INI.
                   20 DCOMW1CW-I-DANO-OPER-DESC  PIC  9(04).
                   20 DCOMW1CW-I-NSEQ-OPER-DESC  PIC  9(09).
                15 DCOMW1CW-CHAVE-FIM.
                   20 DCOMW1CW-F-DANO-OPER-DESC  PIC  9(04).
                   20 DCOMW1CW-F-NSEQ-OPER-DESC  PIC  9(09).
             10 DCOMW1CW-BLOCO-RETORNO.
                15 DCOMW1CW-COD-RETORNO          PIC  9(02).
