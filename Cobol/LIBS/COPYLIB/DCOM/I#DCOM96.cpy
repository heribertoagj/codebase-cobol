      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB096 - DB2PRD.TCLI_CRITR_REDE                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6980 - SELECT         - DCOM6981 - FETCH             *
      *    - DCOM6982 - INSERT         - DCOM6983 - UPDATE            *
      *    - DCOM6984 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  96-ARGUMENTOS-ENTRADA.

       05  96-COLUNAS-DA-TABELA.
           10  96-CCONVE-CLI-DESC              PIC S9(9)V COMP-3.
           10  96-CVRSAO-CONVE-CLI             PIC S9(3)V COMP-3.
           10  96-CINDCD-PSSOA-TOMAD           PIC X(1).
           10  96-CSGMTO-CLI                   PIC S9(3)V COMP-3.
           10  96-CRTING                       PIC X(3).
DTS001     10  96-NNVEL-RTING                  PIC S9(2)V COMP-3.
DTS001     10  96-HINIC-VGCIA-NVEL             PIC X(26).
           10  96-CMEIO-ENTRD-DESC             PIC S9(3)V COMP-3.
           10  96-DINIC-CRITR-REDE             PIC X(10).
           10  96-DFIM-CRITR-TX-REDE           PIC X(10).
           10  96-CIDTFD-TX-MAX-REDE           PIC S9(5)V COMP-3.
           10  96-CTERM                        PIC X(8).
           10  96-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  96-HULT-ATULZ                   PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  96-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  96-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  96-RETORNO.
       05  96-COD-RETORNO                         PIC 9(04).
       05  96-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  96-PROXIMO-RESTART                     PIC 9(05).
       05  96-QTDE-RETORNADA                      PIC 9(03).
       05  96-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  96-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T96.
           10  96RT-CCONVE-CLI-DESC            PIC S9(9)V COMP-3.
           10  96RT-CVRSAO-CONVE-CLI           PIC S9(3)V COMP-3.
           10  96RT-CINDCD-PSSOA-TOMAD         PIC X(1).
           10  96RT-CSGMTO-CLI                 PIC S9(3)V COMP-3.
           10  96RT-CRTING                     PIC X(3).
DTS001     10  96RT-NNVEL-RTING                PIC S9(2)V COMP-3.
DTS001     10  96RT-HINIC-VGCIA-NVEL           PIC X(26).
           10  96RT-CMEIO-ENTRD-DESC           PIC S9(3)V COMP-3.
           10  96RT-DINIC-CRITR-REDE           PIC X(10).
           10  96RT-DFIM-CRITR-TX-REDE         PIC X(10).
           10  96RT-CIDTFD-TX-MAX-REDE         PIC S9(5)V COMP-3.
           10  96RT-CTERM                      PIC X(8).
           10  96RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  96RT-HULT-ATULZ                 PIC X(26).
