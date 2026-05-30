      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0E9 - DB2PRD.TSPROD_CRITR_REDE                      *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7245 - SELECT         - DCOM7246 - FETCH             *
      *    - DCOM7247 - INSERT         - DCOM7248 - UPDATE            *
      *    - DCOM7249 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  E9-ARGUMENTOS-ENTRADA.

       05  E9-COLUNAS-DA-TABELA.
           10  E9-CPRODT                       PIC S9(3)V COMP-3.
           10  E9-CSPROD-DESC-COML             PIC S9(3)V COMP-3.
           10  E9-CINDCD-PSSOA-TOMAD           PIC X(1).
           10  E9-CSGMTO-CLI                   PIC S9(3)V COMP-3.
           10  E9-CRTING                       PIC X(3).
DTS001     10  E9-NNVEL-RTING                  PIC S9(2)V COMP-3.
DTS001     10  E9-HINIC-VGCIA-NVEL             PIC X(26).
           10  E9-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  E9-DINIC-CRITR-REDE             PIC X(10).
           10  E9-DFIM-CRITR-TX-REDE           PIC X(10).
           10  E9-CIDTFD-TX-MAX-REDE           PIC S9(5)V COMP-3.
           10  E9-CTERM                        PIC X(8).
           10  E9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E9-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E9-RETORNO.
       05  E9-COD-RETORNO                         PIC 9(04).
       05  E9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E9-PROXIMO-RESTART                     PIC 9(05).
       05  E9-QTDE-RETORNADA                      PIC 9(03).
       05  E9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE9.
           10  E9RT-CPRODT                     PIC S9(3)V COMP-3.
           10  E9RT-CSPROD-DESC-COML           PIC S9(3)V COMP-3.
           10  E9RT-CINDCD-PSSOA-TOMAD         PIC X(1).
           10  E9RT-CSGMTO-CLI                 PIC S9(3)V COMP-3.
           10  E9RT-CRTING                     PIC X(3).
DTS001     10  E9RT-NNVEL-RTING                PIC S9(2)V COMP-3.
DTS001     10  E9RT-HINIC-VGCIA-NVEL           PIC X(26).
           10  E9RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  E9RT-DINIC-CRITR-REDE           PIC X(10).
           10  E9RT-DFIM-CRITR-TX-REDE         PIC X(10).
           10  E9RT-CIDTFD-TX-MAX-REDE         PIC S9(5)V COMP-3.
           10  E9RT-CTERM                      PIC X(8).
           10  E9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E9RT-HULT-ATULZ                 PIC X(26).
