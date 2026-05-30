      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB071 - DB2PRD.TSPROD_TX_DFNDO                        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM6855 - SELECT         - DCOM6856 - FETCH             *
      *    - DCOM6857 - INSERT         - DCOM6858 - UPDATE            *
      *    - DCOM6859 - DELETE                                        *
      *---------------------------------------------------------------*
DTS001* ALTERACOES.........:                                          *
DTS001*    - 03/07/2012 - INCLUIR NIVEL DE RATING                     *
DTS001*===============================================================*

       01  71-ARGUMENTOS-ENTRADA.

       05  71-COLUNAS-DA-TABELA.
           10  71-CPRODT                   PIC S9(3)V COMP-3.
           10  71-CSPROD-DESC-COML         PIC S9(3)V COMP-3.
           10  71-CINDCD-PSSOA-TOMAD       PIC X(1).
           10  71-CSGMTO-CLI               PIC S9(3)V COMP-3.
           10  71-CRTING                   PIC X(3).
DTS001     10  71-NNVEL-RTING              PIC S9(2)V COMP-3.
DTS001     10  71-HINIC-VGCIA-NVEL         PIC X(26).
           10  71-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  71-DINIC-TX-JURO            PIC X(10).
           10  71-DFIM-TX-JURO             PIC X(10).
           10  71-PTX-JURO-DFNDO           PIC S9(3)V9(5) COMP-3.
           10  71-CINDCD-FREQ-TX           PIC X(2).
           10  71-CIDTFD-TX-DFNDO          PIC S9(5)V COMP-3.
           10  71-HULT-ATULZ               PIC X(26).
           10  71-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  71-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  71-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  71-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  71-RETORNO.
       05  71-COD-RETORNO                         PIC 9(04).
       05  71-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  71-PROXIMO-RESTART                     PIC 9(05).
       05  71-QTDE-RETORNADA                      PIC 9(03).
       05  71-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  71-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T71.
           10  71RT-CPRODT                 PIC S9(3)V COMP-3.
           10  71RT-CSPROD-DESC-COML       PIC S9(3)V COMP-3.
           10  71RT-CINDCD-PSSOA-TOMAD     PIC X(1).
           10  71RT-CSGMTO-CLI             PIC S9(3)V COMP-3.
           10  71RT-CRTING                 PIC X(3).
DTS001     10  71RT-NNVEL-RTING            PIC S9(2)V COMP-3.
DTS001     10  71RT-HINIC-VGCIA-NVEL       PIC X(26).
           10  71RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  71RT-DINIC-TX-JURO          PIC X(10).
           10  71RT-DFIM-TX-JURO           PIC X(10).
           10  71RT-PTX-JURO-DFNDO         PIC S9(3)V9(5) COMP-3.
           10  71RT-CINDCD-FREQ-TX         PIC X(2).
           10  71RT-CIDTFD-TX-DFNDO        PIC S9(5)V COMP-3.
           10  71RT-HULT-ATULZ             PIC X(26).
           10  71RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  71RT-CTERM                  PIC X(8).
