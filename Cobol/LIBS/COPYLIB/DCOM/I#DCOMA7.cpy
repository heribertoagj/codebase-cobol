      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0A7 - DB2PRD.TGRAL_CRITR_REDE                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7035 - SELECT         - DCOM7036 - FETCH             *
      *    - DCOM7037 - INSERT         - DCOM7038 - UPDATE            *
      *    - DCOM7039 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  A7-ARGUMENTOS-ENTRADA.

       05  A7-COLUNAS-DA-TABELA.
           10  A7-CCONVE-GRAL-DESC             PIC S9(9)V COMP-3.
           10  A7-CVRSAO-CONVE-GRAL            PIC S9(3)V COMP-3.
           10  A7-CINDCD-PSSOA-TOMAD           PIC X(1).
           10  A7-CSGMTO-CLI                   PIC S9(3)V COMP-3.
           10  A7-CRTING                       PIC X(3).
DTS001     10  A7-NNVEL-RTING                  PIC S9(2)V COMP-3.
DTS001     10  A7-HINIC-VGCIA-NVEL             PIC X(26).
           10  A7-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  A7-DINIC-CRITR-REDE             PIC X(10).
           10  A7-DFIM-CRITR-TX-REDE           PIC X(10).
           10  A7-CIDTFD-TX-MAX-REDE           PIC S9(5)V COMP-3.
           10  A7-CTERM                        PIC X(8).
           10  A7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  A7-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  A7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  A7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  A7-RETORNO.
       05  A7-COD-RETORNO                         PIC 9(04).
       05  A7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  A7-PROXIMO-RESTART                     PIC 9(05).
       05  A7-QTDE-RETORNADA                      PIC 9(03).
       05  A7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  A7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TA7.
           10  A7RT-CCONVE-GRAL-DESC           PIC S9(9)V COMP-3.
           10  A7RT-CVRSAO-CONVE-GRAL          PIC S9(3)V COMP-3.
           10  A7RT-CINDCD-PSSOA-TOMAD         PIC X(1).
           10  A7RT-CSGMTO-CLI                 PIC S9(3)V COMP-3.
           10  A7RT-CRTING                     PIC X(3).
DTS001     10  A7RT-NNVEL-RTING                PIC S9(2)V COMP-3.
DTS001     10  A7RT-HINIC-VGCIA-NVEL           PIC X(26).
           10  A7RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  A7RT-DINIC-CRITR-REDE           PIC X(10).
           10  A7RT-DFIM-CRITR-TX-REDE         PIC X(10).
           10  A7RT-CIDTFD-TX-MAX-REDE         PIC S9(5)V COMP-3.
           10  A7RT-CTERM                      PIC X(8).
           10  A7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  A7RT-HULT-ATULZ                 PIC X(26).
